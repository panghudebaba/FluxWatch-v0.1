# =====================================================================
# utils_flux.R
# 通量计算模块 —— 共享工具函数（类型转换/列识别/数据辅助）
# =====================================================================

if (!exists("%||%", mode = "function")) {
  `%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x
}

# ---------- 类型转换 ----------

fw_as_num <- function(x) {
  if (is.null(x)) return(numeric(0))
  if (is.factor(x)) x <- as.character(x)
  if (is.character(x)) {
    z <- trimws(x)
    z[z %in% c("", "NA", "NaN", "NULL", "null", "-", "--")] <- NA_character_
    z <- gsub(",", ".", z, fixed = TRUE)
    suppressWarnings(as.numeric(z))
  } else {
    suppressWarnings(as.numeric(x))
  }
}

fw_as_date <- function(x) {
  if (is.null(x)) return(as.Date(character(0)))
  if (inherits(x, "Date")) return(as.Date(x))
  if (inherits(x, "POSIXt")) return(as.Date(x))
  if (is.numeric(x)) {
    out <- as.Date(x, origin = "1970-01-01")
    bad <- is.na(out) | out < as.Date("1900-01-01") | out > as.Date("2200-12-31")
    if (any(bad)) out[bad] <- as.Date(x[bad], origin = "1899-12-30")
    return(out)
  }
  if (is.factor(x)) x <- as.character(x)
  xx <- trimws(as.character(x))
  xx[xx %in% c("", "NA", "NaN", "NULL", "null")] <- NA_character_
  out <- rep(as.Date(NA), length(xx))
  fmts <- c("%Y-%m-%d", "%Y/%m/%d", "%Y.%m.%d", "%Y%m%d",
            "%Y-%m-%d %H:%M:%S", "%Y/%m/%d %H:%M:%S",
            "%Y-%m-%d %H:%M", "%Y/%m/%d %H:%M")
  for (f in fmts) {
    idx <- is.na(out) & !is.na(xx)
    if (!any(idx)) break
    tmp <- suppressWarnings(as.Date(xx[idx], format = f))
    out[idx] <- tmp
  }
  idx <- is.na(out) & !is.na(xx)
  if (any(idx)) {
    tmp <- suppressWarnings(as.POSIXct(xx[idx], tz = "UTC"))
    out[idx] <- as.Date(tmp)
  }
  idx <- is.na(out) & !is.na(xx)
  if (any(idx)) {
    num <- suppressWarnings(as.numeric(xx[idx]))
    ok <- is.finite(num)
    if (any(ok)) {
      ii <- which(idx)[ok]
      out[ii] <- fw_as_date(num[ok])
    }
  }
  out
}

fw_trim_na <- function(x) {
  if (is.null(x)) return(character(0))
  z <- as.character(x)
  z <- trimws(z)
  z[z %in% c("", "NA", "NaN", "NULL", "null")] <- NA_character_
  z
}



# ---------------------------------------------------------
# fw_find_qf_cols
# 在 QF (Flow) data.frame 中自动识别关键列
# 返回命名列表：date, station_id, station_name, Q, ...
# ---------------------------------------------------------

# =============================================================
# fw_find_qf_cols
# 在 QF (Flow) data.frame 中自动识别所有关键列
# 返回命名列表，包含主名称和别名，兼容 $dt / $q 等访问方式
# =============================================================
fw_find_qf_cols <- function(df) {

  empty <- list(
    date = NA_character_, dt = NA_character_,
    station_id = NA_character_, station = NA_character_,
    station_name = NA_character_,
    Q = NA_character_, q = NA_character_,
    Q_original = NA_character_,
    Q_imputed  = NA_character_,
    is_imputed = NA_character_,
    flow_row_missing = NA_character_,
    found   = FALSE,
    missing = c("date", "station_id", "Q")
  )

  if (is.null(df) || !is.data.frame(df) || ncol(df) == 0) return(empty)

  nms <- names(df)


  # ---- 日期列 ----
  date_cands <- c("TM", "tm", "Tm",
                  "date", "Date", "DATE",
                  "datetime", "Datetime", "DATETIME",
                  "time", "Time", "TIME",
                  "\u65e5\u671f")
  date_col <- intersect(date_cands, nms)
  date_col <- if (length(date_col) > 0) date_col[1] else NA_character_


  # ---- 站点编码列（唯一标识） ----
  id_cands <- c("WYBM", "wybm",
                "station_id", "StationID", "stationId",
                "site_no", "site", "Site", "SITE",
                "station_code", "StationCode",
                "\u6c34\u6e90\u6807\u7801")
  id_col <- intersect(id_cands, nms)
  id_col <- if (length(id_col) > 0) id_col[1] else NA_character_


  # ---- 站点名称列 ----
  name_cands <- c("STNM", "stnm",
                  "station", "Station", "STATION",
                  "station_name", "StationName", "stationName",
                  "site_name", "SiteName",
                  "\u7ad9\u70b9", "\u7ad9\u540d")
  name_col <- intersect(name_cands, nms)
  name_col <- if (length(name_col) > 0) name_col[1] else NA_character_


  # ---- 流量列 ----
  q_cands <- c("Q", "q",
               "flow", "Flow", "FLOW",
               "discharge", "Discharge",
               "Q_m3s", "q_m3s",
               "\u6d41\u91cf")
  q_col <- intersect(q_cands, nms)
  q_col <- if (length(q_col) > 0) q_col[1] else NA_character_


  # ---- 原始流量列 ----
  qorig_col <- intersect(c("Q_original", "q_original", "Qorig", "Q_orig"), nms)
  qorig_col <- if (length(qorig_col) > 0) qorig_col[1] else NA_character_

  # ---- 插补流量列 ----
  qimp_col <- intersect(c("Q_imputed", "q_imputed", "Qimp"), nms)
  qimp_col <- if (length(qimp_col) > 0) qimp_col[1] else NA_character_

  # ---- 是否插补标记 ----
  imp_col <- intersect(c("is_imputed", "imputed", "Is_Imputed"), nms)
  imp_col <- if (length(imp_col) > 0) imp_col[1] else NA_character_

  # ---- 缺失行标记 ----
  miss_col <- intersect(c("flow_row_missing", "row_missing", "gap"), nms)
  miss_col <- if (length(miss_col) > 0) miss_col[1] else NA_character_


  # ---- 汇总 ----
  required     <- c(date = date_col, station_id = id_col, Q = q_col)
  missing_keys <- names(required)[is.na(required)]
  found        <- length(missing_keys) == 0

  list(
    # 主名称
    date             = date_col,
    station_id       = id_col,
    station_name     = name_col,
    Q                = q_col,
    Q_original       = qorig_col,
    Q_imputed        = qimp_col,
    is_imputed       = imp_col,
    flow_row_missing = miss_col,

    # 别名（兼容调用方使用 $dt / $q / $station）
    dt               = date_col,
    q                = q_col,
    station          = id_col,

    found            = found,
    missing          = missing_keys
  )
}


# =============================================================
# fw_find_wq_dt_col
# 在 WQ (WaterQuality) data.frame 中自动识别日期列
# 返回: 列名字符串（长度1），找不到时返回 NA_character_
# =============================================================
fw_find_wq_dt_col <- function(df) {

  if (is.null(df) || !is.data.frame(df) || ncol(df) == 0) return(NA_character_)

  nms <- names(df)

  # ---------- 按优先级匹配候选列名 ----------
  dt_cands <- c(
    "TM", "tm", "Tm",
    "date", "Date", "DATE",
    "datetime", "Datetime", "DATETIME",
    "SampleDate", "sampleDate", "sample_date",
    "SamplingDate", "sampling_date",
    "time", "Time", "TIME",
    "\u65e5\u671f",            # 日期
    "\u91c7\u6837\u65e5\u671f" # 采样日期
  )

  hit <- intersect(dt_cands, nms)

  if (length(hit) > 0) return(hit[1])

  # ---------- 回退：按列类型猜测 ----------
  for (nm in nms) {
    col <- df[[nm]]
    if (inherits(col, "POSIXct") || inherits(col, "POSIXt") || inherits(col, "Date")) {
      return(nm)
    }
  }

  NA_character_
}













# ---------- 列识别 ----------

fw_find_flux_cols <- function(df) {
  if (is.null(df) || !is.data.frame(df) || ncol(df) == 0) {
    return(list(dt = NULL, q = NULL, c = NULL, station = NULL, wybm = NULL))
  }
  nm <- names(df)
  nml <- tolower(gsub("[^a-z0-9]", "", nm))
  pick <- function(patterns, exclude = NULL) {
    hit <- rep(FALSE, length(nml))
    for (p in patterns) hit <- hit | grepl(p, nml, perl = TRUE)
    if (!is.null(exclude)) {
      for (p in exclude) hit <- hit & !grepl(p, nml, perl = TRUE)
    }
    if (any(hit)) nm[which(hit)[1]] else NULL
  }
  list(
    dt      = pick(c("^tm$", "^date$", "datetime", "timestamp", "^time$", "riqi", "^rq$")),
    q       = pick(c("^q$", "^flow$", "discharge", "streamflow", "liuliang")),
    c       = pick(c("^c$", "cobs", "conc", "concentration", "^tn$", "^tp$",
                     "nh4", "no3", "cod", "toc", "nh3n"),
                   exclude = c("code", "station", "stnm")),
    station = pick(c("^station$", "stnm", "sitename", "^site$", "siteid")),
    wybm    = pick(c("^wybm$", "basincode", "watershed", "subbasin"))
  )
}

# ---------- 数据辅助 ----------

fw_rv_get <- function(x, name) {
  tryCatch(x[[name]], error = function(e) NULL)
}

fw_pick_first_text <- function(df, cand_cols) {
  if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) return(NA_character_)
  for (nm in cand_cols) {
    if (nm %in% names(df)) {
      z <- fw_trim_na(df[[nm]])
      z <- z[!is.na(z)]
      if (length(z) > 0) return(as.character(z[1]))
    }
  }
  NA_character_
}

fw_pick_first_df <- function(x) {
  if (is.null(x)) return(NULL)
  if (is.data.frame(x)) return(x)
  if (is.list(x)) {
    ok <- vapply(x, is.data.frame, logical(1))
    if (any(ok)) return(x[[which(ok)[1]]])
  }
  NULL
}

fw_as_named_table_list <- function(x, default_name = "data") {
  if (is.null(x)) return(list())
  if (is.data.frame(x)) {
    out <- list(x); names(out) <- default_name; return(out)
  }
  if (!is.list(x)) return(list())
  ok <- vapply(x, is.data.frame, logical(1))
  out <- x[ok]
  if (length(out) == 0) return(list())
  nm <- names(out)
  if (is.null(nm)) nm <- rep("", length(out))
  miss <- which(is.na(nm) | nm == "")
  if (length(miss) > 0) nm[miss] <- paste0(default_name, "_", miss)
  names(out) <- nm
  out
}

fw_rbind_fill <- function(x, y) {
  if (is.null(x) || !is.data.frame(x) || nrow(x) == 0) return(y)
  if (is.null(y) || !is.data.frame(y) || nrow(y) == 0) return(x)
  cols <- union(names(x), names(y))
  for (nm in setdiff(cols, names(x))) x[[nm]] <- NA
  for (nm in setdiff(cols, names(y))) y[[nm]] <- NA
  x <- x[, cols, drop = FALSE]; y <- y[, cols, drop = FALSE]
  rbind(x, y)
}

# ---------- 第一步 QF/WQ 获取 ----------

fw_get_step1_qf_wq <- function(rv) {
  qf0 <- fw_rv_get(rv, "QF"); wq0 <- fw_rv_get(rv, "WQ")
  if (!is.null(qf0) && !is.null(wq0)) {
    return(list(QF = fw_as_named_table_list(qf0, "QF"),
                WQ = fw_as_named_table_list(wq0, "WQ"), source = "rv$QF + rv$WQ"))
  }
  cand_names <- c("step1", "step1_data", "step1_result", "clean_list")
  for (nm in cand_names) {
    obj <- fw_rv_get(rv, nm)
    if (is.null(obj) || !is.list(obj)) next
    qf <- obj[["QF"]]; wq <- obj[["WQ"]]
    if (!is.null(qf) && !is.null(wq)) {
      return(list(QF = fw_as_named_table_list(qf, "QF"),
                  WQ = fw_as_named_table_list(wq, "WQ"),
                  source = paste0("rv$", nm, "$QF/WQ")))
    }
    flow <- obj[["Flow"]]; wq2 <- obj[["WaterQuality"]]
    if (!is.null(flow) && !is.null(wq2)) {
      return(list(QF = fw_as_named_table_list(flow, "Flow"),
                  WQ = fw_as_named_table_list(wq2, "WaterQuality"),
                  source = paste0("rv$", nm, "$Flow/WaterQuality")))
    }
  }
  NULL
}

fw_get_wq_constituents <- function(wq_df) {
  if (is.null(wq_df) || !is.data.frame(wq_df) || nrow(wq_df) == 0) return(character(0))
  ex <- tolower(c("TM", "time", "date", "datetime", "timestamp",
                  "WYBM", "STNM", "WYBM_WQ", "STNM_WQ",
                  "year", "month", "day", "doy", "station"))
  cols <- names(wq_df)
  keep <- !(tolower(cols) %in% ex)
  cols <- cols[keep]
  if (length(cols) == 0) return(character(0))
  ok <- vapply(cols, function(cc) {
    x <- fw_as_num(wq_df[[cc]]); any(is.finite(x))
  }, logical(1))
  cols[ok]
}

fw_build_flux_df_from_clean_list <- function(clean_list, target = "TN") {
  if (is.null(clean_list) || !is.list(clean_list)) return(NULL)
  flow_src <- clean_list$Flow %||% clean_list$QF %||% clean_list$flow
  wq_src   <- clean_list$WaterQuality %||% clean_list$WQ %||% clean_list$waterquality
  flow_df <- fw_pick_first_df(flow_src)
  pick_wq_df <- function(x, target_col) {
    if (is.null(x)) return(NULL)
    if (is.data.frame(x)) return(x)
    if (is.list(x)) {
      idx <- which(vapply(x, function(df) is.data.frame(df) && target_col %in% names(df), logical(1)))
      if (length(idx) > 0) return(x[[idx[1]]])
      ok <- vapply(x, is.data.frame, logical(1))
      if (any(ok)) return(x[[which(ok)[1]]])
    }
    NULL
  }
  wq_df <- pick_wq_df(wq_src, target)
  if (is.null(flow_df) || is.null(wq_df)) return(NULL)
  cf <- fw_find_flux_cols(flow_df); cw <- fw_find_flux_cols(wq_df)
  dcol_f <- if ("TM" %in% names(flow_df)) "TM" else cf$dt
  qcol   <- if ("Q"  %in% names(flow_df)) "Q"  else cf$q
  dcol_w <- if ("TM" %in% names(wq_df))   "TM" else cw$dt
  if (is.null(dcol_f) || is.null(qcol) || is.null(dcol_w)) return(NULL)
  ccol <- target
  if (!(ccol %in% names(wq_df))) {
    cands <- fw_get_wq_constituents(wq_df)
    if (length(cands) == 0) return(NULL)
    ccol <- cands[1]
  }
  q <- data.frame(TM = fw_as_date(flow_df[[dcol_f]]), Q = fw_as_num(flow_df[[qcol]]),
                  stringsAsFactors = FALSE)
  cc <- data.frame(TM = fw_as_date(wq_df[[dcol_w]]), C_obs = fw_as_num(wq_df[[ccol]]),
                   stringsAsFactors = FALSE)
  q <- q[!is.na(q$TM), , drop = FALSE]; cc <- cc[!is.na(cc$TM), , drop = FALSE]
  if (nrow(q) == 0) return(NULL)
  agg_mean <- function(x) if (all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE)
  q <- stats::aggregate(Q ~ TM, data = q, FUN = agg_mean)
  if (nrow(cc) > 0) cc <- stats::aggregate(C_obs ~ TM, data = cc, FUN = agg_mean)
  out <- merge(q, cc, by = "TM", all.x = TRUE, sort = TRUE)
  station <- fw_pick_first_text(flow_df, c("station", "STNM", "STNM_WQ", "site", "SITENAME"))
  if (is.na(station) || station == "")
    station <- fw_pick_first_text(wq_df, c("station", "STNM", "STNM_WQ", "site", "SITENAME"))
  if (is.na(station) || station == "") station <- "ALL"
  wybm <- fw_pick_first_text(flow_df, c("WYBM", "WYBM_WQ", "wybm"))
  if (is.na(wybm) || wybm == "")
    wybm <- fw_pick_first_text(wq_df, c("WYBM", "WYBM_WQ", "wybm"))
  out$station <- station
  out$WYBM <- if (is.na(wybm) || wybm == "") NA_character_ else wybm
  out
}

fw_fill_daily_id_from_source <- function(daily, src, target_col, cand_cols) {
  if (is.null(daily) || !is.data.frame(daily)) return(daily)
  if (!(target_col %in% names(daily))) daily[[target_col]] <- NA_character_
  miss <- is.na(daily[[target_col]]) | daily[[target_col]] == ""
  if (!any(miss)) return(daily)
  fill_val <- NA_character_
  if (!is.null(src) && is.data.frame(src)) {
    for (nm in cand_cols) {
      if (nm %in% names(src)) {
        z <- fw_trim_na(src[[nm]]); z <- z[!is.na(z)]
        if (length(z) > 0) { fill_val <- as.character(z[1]); break }
      }
    }
  }
  if (is.na(fill_val) || fill_val == "") {
    fill_val <- if (identical(target_col, "station")) "ALL" else NA_character_
  }
  daily[[target_col]][miss] <- fill_val
  daily
}


# =============================================================
# 通量汇总表（按月/按年汇总 daily 表）
# =============================================================
fw_make_flux_summary_table <- function(daily) {
  if (is.null(daily) || !is.data.frame(daily) || nrow(daily) == 0)
    return(data.frame(period = character(0), total_flux_kg = numeric(0),
                      mean_Q = numeric(0), mean_C_est = numeric(0),
                      n_days = integer(0), stringsAsFactors = FALSE))

  daily$date <- fw_as_date(daily$date)
  daily <- daily[!is.na(daily$date), , drop = FALSE]
  if (nrow(daily) == 0)
    return(data.frame(period = character(0), total_flux_kg = numeric(0),
                      mean_Q = numeric(0), mean_C_est = numeric(0),
                      n_days = integer(0), stringsAsFactors = FALSE))

  daily$ym <- format(daily$date, "%Y-%m")
  daily$yr <- format(daily$date, "%Y")

  flux_col <- if ("flux" %in% names(daily)) "flux" else
    if ("load_kgd" %in% names(daily)) "load_kgd" else NULL
  q_col    <- if ("Q" %in% names(daily)) "Q" else NULL
  c_col    <- if ("C_est" %in% names(daily)) "C_est" else NULL

  # ---- 按月汇总 ----
  monthly <- do.call(rbind, lapply(split(daily, daily$ym), function(sub) {
    data.frame(
      period       = sub$ym[1],
      total_flux_kg = if (!is.null(flux_col)) round(sum(fw_as_num(sub[[flux_col]]), na.rm = TRUE), 4) else NA_real_,
      mean_Q       = if (!is.null(q_col))    round(mean(fw_as_num(sub[[q_col]]), na.rm = TRUE), 4) else NA_real_,
      mean_C_est   = if (!is.null(c_col))    round(mean(fw_as_num(sub[[c_col]]), na.rm = TRUE), 6) else NA_real_,
      n_days       = nrow(sub),
      stringsAsFactors = FALSE)
  }))

  # ---- 按年汇总 ----
  yearly <- do.call(rbind, lapply(split(daily, daily$yr), function(sub) {
    data.frame(
      period       = paste0(sub$yr[1], " \u5e74\u5408\u8ba1"),
      total_flux_kg = if (!is.null(flux_col)) round(sum(fw_as_num(sub[[flux_col]]), na.rm = TRUE), 4) else NA_real_,
      mean_Q       = if (!is.null(q_col))    round(mean(fw_as_num(sub[[q_col]]), na.rm = TRUE), 4) else NA_real_,
      mean_C_est   = if (!is.null(c_col))    round(mean(fw_as_num(sub[[c_col]]), na.rm = TRUE), 6) else NA_real_,
      n_days       = nrow(sub),
      stringsAsFactors = FALSE)
  }))

  # ---- 总计行 ----
  total_row <- data.frame(
    period       = "\u603b\u8ba1",
    total_flux_kg = if (!is.null(flux_col)) round(sum(fw_as_num(daily[[flux_col]]), na.rm = TRUE), 4) else NA_real_,
    mean_Q       = if (!is.null(q_col))    round(mean(fw_as_num(daily[[q_col]]), na.rm = TRUE), 4) else NA_real_,
    mean_C_est   = if (!is.null(c_col))    round(mean(fw_as_num(daily[[c_col]]), na.rm = TRUE), 6) else NA_real_,
    n_days       = nrow(daily),
    stringsAsFactors = FALSE)

  out <- rbind(monthly, yearly, total_row)
  rownames(out) <- NULL
  out
}
