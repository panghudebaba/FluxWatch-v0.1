# =========================================================
# fct_ingest.R
# Workbook ingest / validation / mapping / flow completion
# fixed ranges:
# - Info:         B4:N
# - Precip:       B4:E
# - Flow:         B4:E
# - WaterQuality: B4:unlimited columns
# B4 row is the header row
# =========================================================

library(readxl)
library(dplyr)
library(cellranger)

`%||%` <- function(a, b) if (!is.null(a)) a else b


# ---------------------------------------------------------
# Basic helpers
# ---------------------------------------------------------
fw_trim_na <- function(x) {
  x <- trimws(as.character(x))
  x[x %in% c("", "NA", "N/A", "NULL", "null")] <- NA_character_
  x
}

fw_clean_names <- function(x) {
  x <- fw_trim_na(x)
  x <- gsub("\\s+", "", x)
  x <- gsub("\u00A0", "", x, fixed = TRUE)
  x
}

fw_as_numeric <- function(x) {
  if (is.numeric(x)) return(x)
  x <- fw_trim_na(x)
  x <- gsub(",", "", x)
  x <- gsub("\uff0c", "", x)
  suppressWarnings(as.numeric(x))
}


# ---------------------------------------------------------
# Parse mixed date formats
# ---------------------------------------------------------
fw_parse_date <- function(x) {
  if (inherits(x, "Date")) {
    return(x)
  }

  out <- rep(as.Date(NA), length(x))

  if (is.numeric(x)) {
    return(as.Date(x, origin = "1899-12-30"))
  }

  x_chr <- fw_trim_na(x)
  x_std <- gsub("\\.", "-", x_chr)
  x_std <- gsub("/", "-", x_std)

  suppressWarnings({
    out <- as.Date(x_std, format = "%Y-%m-%d")
  })

  idx_na <- is.na(out) & !is.na(x_std)
  if (any(idx_na)) {
    num_try <- suppressWarnings(as.numeric(x_std[idx_na]))
    ok_num <- !is.na(num_try)
    if (any(ok_num)) {
      out_idx <- which(idx_na)[ok_num]
      out[out_idx] <- as.Date(num_try[ok_num], origin = "1899-12-30")
    }
  }

  out
}


# ---------------------------------------------------------
# Drop empty rows / cols
# ---------------------------------------------------------
fw_drop_empty_df <- function(df) {
  df <- as.data.frame(df, stringsAsFactors = FALSE)

  if (ncol(df) > 0) {
    keep_col <- vapply(
      df,
      function(z) !all(is.na(z) | trimws(as.character(z)) == ""),
      logical(1)
    )
    df <- df[, keep_col, drop = FALSE]
  }

  if (nrow(df) > 0 && ncol(df) > 0) {
    keep_row <- apply(
      df,
      1,
      function(z) !all(is.na(z) | trimws(as.character(z)) == "")
    )
    df <- df[keep_row, , drop = FALSE]
  }

  rownames(df) <- NULL
  df
}


# ---------------------------------------------------------
# Fixed-range sheet readers
# ---------------------------------------------------------
fw_read_sheet_fixed <- function(path, sheet, start_row = 4, start_col = 2, end_col) {
  dat <- readxl::read_excel(
    path = path,
    sheet = sheet,
    range = cellranger::cell_limits(
      c(start_row, start_col),
      c(NA, end_col)
    ),
    guess_max = 5000
  )
  dat <- as.data.frame(dat, stringsAsFactors = FALSE)
  dat <- fw_drop_empty_df(dat)
  dat
}

fw_read_sheet_open <- function(path, sheet, start_row = 4, start_col = 2) {
  dat <- readxl::read_excel(
    path = path,
    sheet = sheet,
    range = cellranger::cell_limits(c(start_row, start_col), c(NA, NA)),
    guess_max = 5000
  )
  dat <- as.data.frame(dat, stringsAsFactors = FALSE)
  dat <- fw_drop_empty_df(dat)
  dat
}


# ---------------------------------------------------------
# Generic cleaner
# ---------------------------------------------------------
fw_clean_sheet <- function(df) {
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  names(df) <- fw_clean_names(names(df))
  rownames(df) <- NULL
  df
}


# ---------------------------------------------------------
# Build topology from Info
# ---------------------------------------------------------
fw_build_topology_from_info <- function(info) {
  if (is.null(info) || nrow(info) == 0) return(NULL)

  req <- c("NAME_WQ", "WYBM_WQ", "WYBM_QF")
  miss <- setdiff(req, names(info))
  if (length(miss) > 0) return(NULL)

  topo <- info[, req, drop = FALSE]
  topo <- topo[!is.na(topo$WYBM_WQ) & !is.na(topo$WYBM_QF), , drop = FALSE]
  topo$NAME_WQ <- fw_trim_na(topo$NAME_WQ)
  topo$WYBM_WQ <- fw_trim_na(topo$WYBM_WQ)
  topo$WYBM_QF <- fw_trim_na(topo$WYBM_QF)
  topo <- unique(topo)
  rownames(topo) <- NULL
  topo
}


# ---------------------------------------------------------
# ★ 识别 WaterQuality 中的水质指标列
# ---------------------------------------------------------

#' 从 WaterQuality data.frame 中自动识别水质指标列
#' 排除元数据列（TM, WYBM, STNM 等），保留含有效数值的列
#' @param df  WaterQuality data.frame（已 clean_names）
#' @return character vector：指标列名
fw_detect_wq_constituents <- function(df) {
  if (is.null(df) || ncol(df) == 0) return(character(0))

  # 元数据列（不是水质指标的列）

  meta_cols <- c(
    "TM", "tm", "date", "Date", "DATE", "datetime", "Datetime",
    "WYBM", "wybm",
    "STNM", "stnm", "station", "Station", "STATION",
    "site", "Site", "site_no",
    "\u65e5\u671f", "\u7ad9\u70b9", "\u7ad9\u540d", "\u6c34\u6e90\u6807\u7801",
    "doy", "DOY", "year", "month", "day",
    "flow_row_missing", "is_imputed", "Q_original", "Q_imputed"
  )

  all_cols <- names(df)
  candidate_cols <- setdiff(all_cols, meta_cols)

  if (length(candidate_cols) == 0) return(character(0))

  # 进一步筛选：必须至少有 1 个有效数值
  valid_cols <- character(0)
  for (cc in candidate_cols) {
    vals <- suppressWarnings(as.numeric(as.character(df[[cc]])))
    if (any(is.finite(vals))) {
      valid_cols <- c(valid_cols, cc)
    }
  }

  valid_cols
}


# ---------------------------------------------------------
fw_format_ingest_log <- function(
    sheets,
    issues = character(0),
    flow_info = NULL,
    info_meta = NULL,
    wq_meta = NULL
) {
  lines <- c(
    "Workbook loaded / \u5de5\u4f5c\u7c3f\u5df2\u52a0\u8f7d",
    paste0("Sheets detected / \u68c0\u6d4b\u5230\u5de5\u4f5c\u8868: ",
           paste(sheets, collapse = ", "))
  )

  if (!is.null(info_meta)) {
    if (!is.null(info_meta$station_names) && length(info_meta$station_names) > 0) {
      lines <- c(lines, paste0("Stations / \u7ad9\u70b9: ",
                               paste(info_meta$station_names, collapse = ", ")))
    }
    # ★ 删除了 info_meta$parameter_names 的输出
  }

  # ★ 只保留 WQ 实测指标
  if (!is.null(wq_meta)) {
    if (!is.null(wq_meta$constituent_cols) && length(wq_meta$constituent_cols) > 0) {
      lines <- c(lines, paste0("Parameter / \u6307\u6807: ",
                               paste(wq_meta$constituent_cols, collapse = ", ")))
      lines <- c(lines, paste0("WQ constituents count / \u6c34\u8d28\u6307\u6807\u6570: ",
                               length(wq_meta$constituent_cols)))
    }
    if (!is.null(wq_meta$n_samples) && is.finite(wq_meta$n_samples)) {
      lines <- c(lines, paste0("WQ sample rows / \u6c34\u8d28\u6837\u54c1\u884c\u6570: ",
                               wq_meta$n_samples))
    }
    if (!is.null(wq_meta$wq_stations) && length(wq_meta$wq_stations) > 0) {
      lines <- c(lines, paste0("WQ stations / \u6c34\u8d28\u7ad9\u70b9: ",
                               paste(wq_meta$wq_stations, collapse = ", ")))
    }
  }

  if (!is.null(flow_info)) {
    lines <- c(lines, paste0("Flow stations / \u6d41\u91cf\u7ad9\u70b9\u6570: ",
                             flow_info$n_station %||% NA))
    lines <- c(lines, paste0("Flow original rows / \u539f\u59cb\u6d41\u91cf\u884c\u6570: ",
                             flow_info$n_rows_original %||% NA))
    lines <- c(lines, paste0("Flow valid-date rows / \u6709\u6548\u65e5\u671f\u6d41\u91cf\u884c\u6570: ",
                             flow_info$n_rows_valid_date %||% NA))
    lines <- c(lines, paste0("Flow completed rows / \u8865\u9f50\u540e\u6d41\u91cf\u884c\u6570: ",
                             flow_info$n_rows_completed %||% NA))
    lines <- c(lines, paste0("Flow imputed rows / \u63d2\u8865\u6d41\u91cf\u884c\u6570: ",
                             flow_info$n_imputed %||% NA))
  }

  if (length(issues) == 0) {
    lines <- c(lines, "", "No validation issues / \u672a\u53d1\u73b0\u6821\u9a8c\u95ee\u9898")
  } else {
    lines <- c(lines, "", "Validation messages / \u6821\u9a8c\u4fe1\u606f:")
    lines <- c(lines, paste0(" - ", unique(issues)))
  }

  paste(lines, collapse = "\n")
}



# ---------------------------------------------------------
# Validate Info
# ---------------------------------------------------------
fw_validate_info <- function(df) {
  issues <- character(0)

  req_cols <- c(
    "shortName", "stationName", "parameterCd", "paramShortName", "paramUnits",
    "NAME_PP", "NAME_WQ", "NAME_QF", "WYBM_PP", "WYBM_QF", "WYBM_WQ"
  )
  miss <- setdiff(req_cols, names(df))
  if (length(miss) > 0) {
    issues <- c(
      issues,
      paste0("Info missing columns / Info \u7f3a\u5c11\u5b57\u6bb5: ",
             paste(miss, collapse = ", "))
    )
  }

  df <- as.data.frame(df, stringsAsFactors = FALSE)

  chr_cols <- intersect(
    c("shortName", "stationName", "parameterCd", "paramShortName", "paramUnits",
      "NAME_PP", "NAME_WQ", "NAME_QF", "WYBM_PP", "WYBM_QF", "WYBM_WQ"),
    names(df)
  )
  for (cc in chr_cols) df[[cc]] <- fw_trim_na(df[[cc]])

  if ("drainageArea" %in% names(df)) {
    df$drainageArea <- fw_as_numeric(df$drainageArea)
  }

  station_names <- if ("stationName" %in% names(df))
    unique(stats::na.omit(df$stationName)) else character(0)
  parameter_names <- if ("paramShortName" %in% names(df))
    unique(stats::na.omit(df$paramShortName)) else character(0)

  list(
    data = df,
    issues = unique(issues),
    info_meta = list(
      station_names = station_names,
      parameter_names = parameter_names
    )
  )
}


# ---------------------------------------------------------
# Validate Precip
# ---------------------------------------------------------
fw_validate_precip <- function(df) {
  issues <- character(0)

  req_cols <- c("TM", "WYBM", "STNM", "P")
  miss <- setdiff(req_cols, names(df))
  if (length(miss) > 0) {
    issues <- c(
      issues,
      paste0("Precip missing columns / Precip \u7f3a\u5c11\u5b57\u6bb5: ",
             paste(miss, collapse = ", "))
    )
    return(list(data = df, issues = unique(issues)))
  }

  df <- as.data.frame(df, stringsAsFactors = FALSE)
  df$TM <- fw_parse_date(df$TM)
  df$WYBM <- fw_trim_na(df$WYBM)
  df$STNM <- fw_trim_na(df$STNM)
  df$P <- fw_as_numeric(df$P)

  if (any(is.na(df$TM))) {
    issues <- c(
      issues,
      paste0("Precip contains invalid dates / Precip \u5b58\u5728\u65e0\u6cd5\u89e3\u6790\u7684\u65e5\u671f\uff0c\u884c\u6570: ",
             sum(is.na(df$TM)))
    )
  }

  if (any(df$P < 0, na.rm = TRUE)) {
    issues <- c(issues, "Precip contains negative values / Precip \u5b58\u5728\u8d1f\u503c")
  }

  list(data = df, issues = unique(issues))
}


# ---------------------------------------------------------
# Flow complete dates by station
# ---------------------------------------------------------
fw_complete_flow_dates <- function(df) {
  issues <- character(0)

  req_cols <- c("TM", "WYBM", "STNM", "Q")
  miss <- setdiff(req_cols, names(df))
  if (length(miss) > 0) {
    issues <- c(
      issues,
      paste0("Flow missing columns for continuity check / Flow \u8fde\u7eed\u6027\u68c0\u67e5\u7f3a\u5c11\u5b57\u6bb5: ",
             paste(miss, collapse = ", "))
    )
    return(list(data = df, issues = unique(issues), gap_summary = NULL))
  }

  df <- as.data.frame(df, stringsAsFactors = FALSE)
  df$TM <- fw_parse_date(df$TM)
  df$WYBM <- fw_trim_na(df$WYBM)
  df$STNM <- fw_trim_na(df$STNM)
  df$Q <- fw_as_numeric(df$Q)

  n_invalid_tm <- sum(is.na(df$TM))
  if (n_invalid_tm > 0) {
    issues <- c(
      issues,
      paste0("Flow invalid TM rows excluded before completion / \u8865\u9f50\u524d\u5254\u9664\u65e0\u6548\u65e5\u671f\u884c\u6570: ",
             n_invalid_tm)
    )
  }

  df_valid <- df[!is.na(df$TM) & !is.na(df$WYBM), , drop = FALSE]

  if (nrow(df_valid) == 0) {
    issues <- c(issues, "Flow has no valid dates / Flow \u6ca1\u6709\u53ef\u7528\u4e8e\u8fde\u7eed\u6027\u68c0\u67e5\u7684\u6709\u6548\u65e5\u671f")
    return(list(data = df, issues = unique(issues), gap_summary = NULL))
  }

  split_list <- split(df_valid, df_valid$WYBM)

  out_list <- lapply(split_list, function(x) {
    x <- x[order(x$TM), , drop = FALSE]

    full_tm <- seq(min(x$TM, na.rm = TRUE), max(x$TM, na.rm = TRUE), by = "day")
    full_df <- data.frame(TM = full_tm, stringsAsFactors = FALSE)

    y <- merge(full_df, x, by = "TM", all.x = TRUE, sort = TRUE)

    y$WYBM <- unique(x$WYBM)[1]
    stnm_ok <- stats::na.omit(x$STNM)
    y$STNM <- if (length(stnm_ok) > 0) stnm_ok[1] else NA_character_

    if (!"flow_row_missing" %in% names(y)) {
      y$flow_row_missing <- is.na(y$Q)
    }

    y
  })

  out <- do.call(rbind, out_list)
  rownames(out) <- NULL

  gap_summary <- aggregate(
    flow_row_missing ~ WYBM + STNM,
    data = out,
    FUN = sum
  )
  names(gap_summary)[3] <- "n_gap"
  gap_summary <- gap_summary[gap_summary$n_gap > 0, , drop = FALSE]

  if (nrow(gap_summary) > 0) {
    issues <- c(
      issues,
      paste0("Flow date gaps detected / \u53d1\u73b0\u6d41\u91cf\u65e5\u671f\u4e0d\u8fde\u7eed\uff0c\u6d89\u53ca\u7ad9\u70b9\u6570: ",
             nrow(gap_summary))
    )
  }

  list(
    data = out,
    issues = unique(issues),
    gap_summary = gap_summary
  )
}


# ---------------------------------------------------------
# Flow imputation
# ---------------------------------------------------------
fw_impute_flow_q <- function(df) {
  issues <- character(0)

  req_cols <- c("TM", "WYBM", "STNM", "Q", "flow_row_missing")
  miss <- setdiff(req_cols, names(df))
  if (length(miss) > 0) {
    issues <- c(
      issues,
      paste0("Flow missing columns for imputation / Flow \u63d2\u8865\u7f3a\u5c11\u5b57\u6bb5: ",
             paste(miss, collapse = ", "))
    )
    return(list(data = df, issues = unique(issues)))
  }

  df <- as.data.frame(df, stringsAsFactors = FALSE)
  df$TM <- fw_parse_date(df$TM)
  df$Q  <- fw_as_numeric(df$Q)
  df$WYBM <- fw_trim_na(df$WYBM)
  df$STNM <- fw_trim_na(df$STNM)

  split_list <- split(df, df$WYBM)

  out_list <- lapply(split_list, function(x) {
    x <- x[order(x$TM), , drop = FALSE]

    x$Q_original <- x$Q
    x$Q_imputed  <- x$Q
    x$is_imputed <- FALSE

    idx_ok <- which(!is.na(x$Q))

    if (length(idx_ok) >= 2) {
      approx_res <- stats::approx(
        x = as.numeric(x$TM[idx_ok]),
        y = x$Q[idx_ok],
        xout = as.numeric(x$TM),
        method = "linear",
        rule = 2
      )

      x$Q_imputed <- approx_res$y
      x$is_imputed <- is.na(x$Q_original) & !is.na(x$Q_imputed)
      x$Q <- x$Q_imputed

    } else if (length(idx_ok) == 1) {
      x$Q_imputed <- x$Q
      x$Q_imputed[is.na(x$Q_imputed)] <- x$Q[idx_ok]
      x$is_imputed <- is.na(x$Q_original) & !is.na(x$Q_imputed)
      x$Q <- x$Q_imputed

    } else {
      x$is_imputed <- FALSE
    }

    x
  })

  out <- do.call(rbind, out_list)
  rownames(out) <- NULL

  n_imp <- sum(out$is_imputed, na.rm = TRUE)
  if (n_imp > 0) {
    issues <- c(issues, paste0("Flow imputed Q values / \u5df2\u63d2\u8865\u6d41\u91cf\u503c\u884c\u6570: ", n_imp))
  }

  list(data = out, issues = unique(issues))
}


# ---------------------------------------------------------
# Validate Flow
# ---------------------------------------------------------
fw_validate_flow <- function(df, auto_impute = TRUE) {
  issues <- character(0)

  req_cols <- c("TM", "WYBM", "STNM", "Q")
  miss <- setdiff(req_cols, names(df))
  if (length(miss) > 0) {
    issues <- c(
      issues,
      paste0("Flow missing columns / Flow \u7f3a\u5c11\u5b57\u6bb5: ",
             paste(miss, collapse = ", "))
    )
    return(list(
      data = df,
      issues = unique(issues),
      flow_info = list(
        n_station = NA_integer_,
        n_rows_original = nrow(df),
        n_rows_valid_date = NA_integer_,
        n_rows_completed = nrow(df),
        n_imputed = NA_integer_
      )
    ))
  }

  df <- as.data.frame(df, stringsAsFactors = FALSE)
  n_rows_original <- nrow(df)

  df$TM <- fw_parse_date(df$TM)
  df$WYBM <- fw_trim_na(df$WYBM)
  df$STNM <- fw_trim_na(df$STNM)
  df$Q <- fw_as_numeric(df$Q)

  n_invalid_tm <- sum(is.na(df$TM))
  n_rows_valid_date <- n_rows_original - n_invalid_tm

  if (n_invalid_tm > 0) {
    issues <- c(
      issues,
      paste0("Flow contains invalid dates / Flow \u5b58\u5728\u65e0\u6cd5\u89e3\u6790\u7684\u65e5\u671f\uff0c\u884c\u6570: ",
             n_invalid_tm)
    )
  }

  if (any(is.na(df$Q))) {
    issues <- c(issues, "Flow contains missing or non-numeric Q / Flow \u7684 Q \u5b58\u5728\u7f3a\u5931\u6216\u975e\u6570\u503c")
  }

  if (any(df$Q <= 0, na.rm = TRUE)) {
    issues <- c(issues, "Flow contains Q <= 0 / Flow \u5b58\u5728 Q <= 0")
  }

  dup <- duplicated(df[, c("TM", "WYBM")])
  if (any(dup, na.rm = TRUE)) {
    issues <- c(
      issues,
      paste0("Flow contains duplicated date-station rows / Flow \u5b58\u5728\u91cd\u590d\u65e5\u671f-\u7ad9\u70b9\u8bb0\u5f55: ",
             sum(dup, na.rm = TRUE))
    )
  }

  res_complete <- fw_complete_flow_dates(df)
  df2 <- res_complete$data
  issues <- c(issues, res_complete$issues)

  if (isTRUE(auto_impute)) {
    res_imp <- fw_impute_flow_q(df2)
    df2 <- res_imp$data
    issues <- c(issues, res_imp$issues)
  } else {
    df2$Q_original <- df2$Q
    df2$Q_imputed  <- df2$Q
    df2$is_imputed <- FALSE
  }

  flow_info <- list(
    n_station = length(unique(stats::na.omit(df2$WYBM))),
    n_rows_original = n_rows_original,
    n_rows_valid_date = n_rows_valid_date,
    n_rows_completed = nrow(df2),
    n_imputed = sum(df2$is_imputed, na.rm = TRUE)
  )

  list(
    data = df2,
    issues = unique(issues),
    flow_info = flow_info
  )
}


# ---------------------------------------------------------
# Validate WaterQuality  ★ 重写：自动识别指标列
# ---------------------------------------------------------
fw_validate_wq <- function(df) {
  issues <- character(0)

  req_cols <- c("TM", "WYBM", "STNM")
  miss <- setdiff(req_cols, names(df))
  if (length(miss) > 0) {
    issues <- c(
      issues,
      paste0("WaterQuality missing columns / WaterQuality \u7f3a\u5c11\u5b57\u6bb5: ",
             paste(miss, collapse = ", "))
    )
    return(list(
      data = df,
      issues = unique(issues),
      wq_meta = list(
        constituent_cols = character(0),
        n_samples = NA_integer_,
        wq_stations = character(0)
      )
    ))
  }

  df <- as.data.frame(df, stringsAsFactors = FALSE)
  df$TM <- fw_parse_date(df$TM)
  df$WYBM <- fw_trim_na(df$WYBM)
  df$STNM <- fw_trim_na(df$STNM)

  if (any(is.na(df$TM))) {
    issues <- c(
      issues,
      paste0("WaterQuality contains invalid dates / WaterQuality \u5b58\u5728\u65e0\u6cd5\u89e3\u6790\u7684\u65e5\u671f\uff0c\u884c\u6570: ",
             sum(is.na(df$TM)))
    )
  }

  # ★ 自动识别所有水质指标列（不再硬编码 TN/TP/...）
  conc_cols <- fw_detect_wq_constituents(df)

  if (length(conc_cols) == 0) {
    issues <- c(issues,
                "No concentration columns found in WaterQuality / WaterQuality \u672a\u627e\u5230\u6c34\u8d28\u6307\u6807\u5217")
  } else {
    # 逐列转数值 & 检查负值
    for (cc in conc_cols) {
      df[[cc]] <- fw_as_numeric(df[[cc]])
      neg_n <- sum(!is.na(df[[cc]]) & df[[cc]] < 0)
      if (neg_n > 0) {
        issues <- c(
          issues,
          paste0("WaterQuality ", cc, " contains negative values / ", cc,
                 " \u5b58\u5728\u8d1f\u503c\u884c\u6570: ", neg_n)
        )
      }
    }

    all_empty <- vapply(conc_cols, function(cc) all(is.na(df[[cc]])), logical(1))
    if (all(all_empty)) {
      issues <- c(issues,
                  "All concentration columns are empty / \u6240\u6709\u6d53\u5ea6\u5217\u5747\u4e3a\u7a7a")
    }

    # 每个指标的有效样本数
    for (cc in conc_cols) {
      n_valid <- sum(is.finite(df[[cc]]))
      if (n_valid == 0) {
        issues <- c(issues,
                    paste0("WaterQuality ", cc, " has no valid values / ", cc, " \u65e0\u6709\u6548\u503c"))
      }
    }
  }

  # ★ 构建 wq_meta
  wq_stations <- if ("STNM" %in% names(df))
    unique(stats::na.omit(df$STNM)) else character(0)

  wq_meta <- list(
    constituent_cols = conc_cols,
    n_samples        = nrow(df),
    wq_stations      = wq_stations
  )

  list(
    data    = df,
    issues  = unique(issues),
    wq_meta = wq_meta
  )
}


# ---------------------------------------------------------
# Validate topology
# ---------------------------------------------------------
fw_validate_topology <- function(df) {
  issues <- character(0)

  req_cols <- c("NAME_WQ", "WYBM_WQ", "WYBM_QF")
  miss <- setdiff(req_cols, names(df))
  if (length(miss) > 0) {
    issues <- c(
      issues,
      paste0("Topology missing columns / Topology \u7f3a\u5c11\u5b57\u6bb5: ",
             paste(miss, collapse = ", "))
    )
    return(list(data = df, issues = unique(issues)))
  }

  df <- as.data.frame(df, stringsAsFactors = FALSE)
  df$NAME_WQ <- fw_trim_na(df$NAME_WQ)
  df$WYBM_WQ <- fw_trim_na(df$WYBM_WQ)
  df$WYBM_QF <- fw_trim_na(df$WYBM_QF)

  if (any(is.na(df$WYBM_WQ) | df$WYBM_WQ == "")) {
    issues <- c(issues, "Topology contains empty WYBM_WQ / Topology \u5b58\u5728\u7a7a\u7684 WYBM_WQ")
  }

  if (any(is.na(df$WYBM_QF) | df$WYBM_QF == "")) {
    issues <- c(issues, "Topology contains empty WYBM_QF / Topology \u5b58\u5728\u7a7a\u7684 WYBM_QF")
  }

  dup <- duplicated(df[, c("WYBM_QF", "WYBM_WQ")])
  if (any(dup, na.rm = TRUE)) {
    issues <- c(
      issues,
      paste0("Topology contains duplicated mapping rows / Topology \u5b58\u5728\u91cd\u590d\u6620\u5c04\u884c\u6570: ",
             sum(dup, na.rm = TRUE))
    )
  }

  list(data = unique(df), issues = unique(issues))
}


# ---------------------------------------------------------
# Cross-sheet validation
# ---------------------------------------------------------
fw_validate_cross_sheet <- function(lst) {
  issues <- character(0)

  if (all(c("Flow", "WaterQuality") %in% names(lst))) {
    flow_tm <- lst$Flow$TM
    wq_tm <- lst$WaterQuality$TM

    if (all(is.na(flow_tm)) || all(is.na(wq_tm))) {
      issues <- c(issues,
                  "Flow or WaterQuality date is all NA / Flow \u6216 WaterQuality \u65e5\u671f\u5168\u4e3a\u7a7a")
    } else {
      flow_range <- range(flow_tm, na.rm = TRUE)
      wq_range <- range(wq_tm, na.rm = TRUE)

      overlap_start <- max(flow_range[1], wq_range[1])
      overlap_end <- min(flow_range[2], wq_range[2])

      if (isTRUE(overlap_start > overlap_end)) {
        issues <- c(issues,
                    "Flow and WaterQuality have no overlapping date range / Flow \u4e0e WaterQuality \u65f6\u95f4\u8303\u56f4\u6ca1\u6709\u91cd\u53e0")
      }
    }
  }

  list(issues = unique(issues))
}


# ---------------------------------------------------------
# Build flux input by topology mapping (QF -> WQ)
# ★ target 改为动态参数，默认 "TN"
# ---------------------------------------------------------
fw_build_flux_input <- function(lst, target = "TN") {
  if (!all(c("Flow", "WaterQuality", "Topology") %in% names(lst))) {
    return(NULL)
  }

  flow <- lst$Flow
  wq <- lst$WaterQuality
  topo <- lst$Topology

  if (!(target %in% names(wq))) {
    return(NULL)
  }

  topo2 <- unique(topo[, c("WYBM_QF", "WYBM_WQ", "NAME_WQ"), drop = FALSE])

  keep_cols <- intersect(
    c("TM", "WYBM", "STNM", "Q", "Q_original", "Q_imputed", "is_imputed", "flow_row_missing"),
    names(flow)
  )
  flow2 <- flow[, keep_cols, drop = FALSE]
  flow2$WYBM_QF <- flow2$WYBM
  flow2 <- merge(flow2, topo2, by = "WYBM_QF", all.x = TRUE, sort = FALSE)

  wq2 <- wq[, c("TM", "WYBM", "STNM", target), drop = FALSE]
  names(wq2) <- c("TM", "WYBM_WQ", "STNM_WQ", "conc")

  dat <- merge(
    flow2,
    wq2,
    by = c("TM", "WYBM_WQ"),
    all.x = TRUE,
    sort = TRUE
  )

  if (!"STNM" %in% names(dat)) dat$STNM <- NA_character_
  if (!"STNM_WQ" %in% names(dat)) dat$STNM_WQ <- NA_character_

  dat$station_match_name <- dplyr::coalesce(dat$STNM_WQ, dat$NAME_WQ, dat$STNM)
  dat$doy <- as.integer(format(dat$TM, "%j"))

  dat_calib <- subset(
    dat,
    !is.na(Q) & Q > 0 & !is.na(conc) & conc > 0 & !is.na(WYBM_WQ)
  )

  newdata_day <- subset(
    dat,
    !is.na(Q) & Q > 0 & !is.na(WYBM_WQ)
  )

  list(
    target = target,
    dat = dat,
    dat_calib = dat_calib,
    newdata_day = newdata_day
  )
}


# ---------------------------------------------------------
# Main function  ★ 收集 wq_meta 并传入 log
# ---------------------------------------------------------
fw_read_validate_workbook <- function(path, auto_impute_flow = TRUE) {
  stopifnot(file.exists(path))

  sheets <- readxl::excel_sheets(path)

  raw_list <- list()

  if ("Info" %in% sheets) {
    raw_list[["Info"]] <- fw_read_sheet_fixed(
      path = path,
      sheet = "Info",
      start_row = 4,
      start_col = 2,
      end_col = 14
    )
  }

  if ("Precip" %in% sheets) {
    raw_list[["Precip"]] <- fw_read_sheet_fixed(
      path = path,
      sheet = "Precip",
      start_row = 4,
      start_col = 2,
      end_col = 5
    )
  }

  if ("Flow" %in% sheets) {
    raw_list[["Flow"]] <- fw_read_sheet_fixed(
      path = path,
      sheet = "Flow",
      start_row = 4,
      start_col = 2,
      end_col = 5
    )
  }

  if ("WaterQuality" %in% sheets) {
    raw_list[["WaterQuality"]] <- fw_read_sheet_open(
      path = path,
      sheet = "WaterQuality",
      start_row = 4,
      start_col = 2
    )
  }

  other_sheets <- setdiff(sheets, names(raw_list))
  for (s in other_sheets) {
    raw_list[[s]] <- as.data.frame(
      readxl::read_excel(path, sheet = s, guess_max = 5000),
      stringsAsFactors = FALSE
    )
  }

  clean_list <- lapply(raw_list, fw_clean_sheet)

  issues <- character(0)
  flow_info <- NULL
  info_meta <- NULL
  wq_meta   <- NULL    # ★ 新增

  required_sheets <- c("Info", "Flow", "WaterQuality")
  missing_sheets <- setdiff(required_sheets, names(clean_list))
  if (length(missing_sheets) > 0) {
    issues <- c(
      issues,
      paste0("Missing required sheets / \u7f3a\u5c11\u5fc5\u9700\u5de5\u4f5c\u8868: ",
             paste(missing_sheets, collapse = ", "))
    )
  }

  if ("Info" %in% names(clean_list)) {
    res <- fw_validate_info(clean_list$Info)
    clean_list$Info <- res$data
    issues <- c(issues, res$issues)
    info_meta <- res$info_meta
  }

  if ("Precip" %in% names(clean_list)) {
    res <- fw_validate_precip(clean_list$Precip)
    clean_list$Precip <- res$data
    issues <- c(issues, res$issues)
  }

  if ("Flow" %in% names(clean_list)) {
    res <- fw_validate_flow(clean_list$Flow, auto_impute = auto_impute_flow)
    clean_list$Flow <- res$data
    issues <- c(issues, res$issues)
    flow_info <- res$flow_info
  }

  if ("WaterQuality" %in% names(clean_list)) {
    res <- fw_validate_wq(clean_list$WaterQuality)
    clean_list$WaterQuality <- res$data
    issues <- c(issues, res$issues)
    wq_meta <- res$wq_meta    # ★ 新增
  }

  cross <- fw_validate_cross_sheet(clean_list)
  issues <- c(issues, cross$issues)

  log <- fw_format_ingest_log(
    sheets    = sheets,
    issues    = unique(issues),
    flow_info = flow_info,
    info_meta = info_meta,
    wq_meta   = wq_meta    # ★ 新增
  )

  list(
    sheets     = sheets,
    raw_list   = raw_list,
    clean_list = clean_list,
    issues     = unique(issues),
    flow_info  = flow_info,
    info_meta  = info_meta,
    wq_meta    = wq_meta,    # ★ 新增：暴露给外部使用
    log        = log
  )
}
