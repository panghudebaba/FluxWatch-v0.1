# =========================================================
# utils_viz.R
# Utility helpers for the Visualization module
# =========================================================

library(dplyr)
library(readxl)

`%||%` <- function(a, b) if (!is.null(a)) a else b


# ---------------------------------------------------------
# 日期转 POSIXct
# ---------------------------------------------------------
fw_viz_as_posix <- function(x, tz = "Asia/Shanghai") {

  if (inherits(x, "POSIXct")) return(x)
  if (inherits(x, "POSIXt"))  return(as.POSIXct(x, tz = tz))
  if (inherits(x, "Date"))    return(as.POSIXct(x, tz = tz))
  as.POSIXct(x, tz = tz)
}


# ---------------------------------------------------------
# 通用：清洗列名
# ---------------------------------------------------------
fw_viz_clean_names <- function(df) {
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  names(df) <- trimws(names(df))
  names(df) <- gsub("\\s+", "", names(df))
  df
}


# ---------------------------------------------------------
# 通用：混合日期解析
# ---------------------------------------------------------
fw_viz_parse_date <- function(x) {
  if (inherits(x, "POSIXct")) return(x)
  if (inherits(x, "POSIXt"))  return(as.POSIXct(x))
  if (inherits(x, "Date"))    return(as.POSIXct(x))

  if (is.numeric(x)) {
    return(as.POSIXct(as.Date(x, origin = "1899-12-30")))
  }

  x_chr <- trimws(as.character(x))
  x_chr[x_chr == ""] <- NA_character_

  x_std <- gsub("\\.", "-", x_chr)
  x_std <- gsub("/", "-", x_std)

  out <- suppressWarnings(as.POSIXct(x_std, format = "%Y-%m-%d", tz = "Asia/Shanghai"))

  idx_na <- is.na(out) & !is.na(x_std)
  if (any(idx_na)) {
    out2 <- suppressWarnings(as.POSIXct(x_std[idx_na],
                                        format = "%Y-%m-%d %H:%M:%S",
                                        tz = "Asia/Shanghai"))
    out[idx_na] <- out2
  }

  idx_na <- is.na(out) & !is.na(x_std)
  if (any(idx_na)) {
    num_try <- suppressWarnings(as.numeric(x_std[idx_na]))
    ok_num  <- !is.na(num_try)
    if (any(ok_num)) {
      out_idx <- which(idx_na)[ok_num]
      out[out_idx] <- as.POSIXct(as.Date(num_try[ok_num], origin = "1899-12-30"),
                                 tz = "Asia/Shanghai")
    }
  }

  out
}


# ---------------------------------------------------------
# 根据数据猜测类型
# ---------------------------------------------------------
fw_viz_guess_sheet_type <- function(df) {
  nms <- names(df)

  if (all(c("TM", "WYBM", "STNM", "Q") %in% nms)) return("Flow")
  if (all(c("TM", "WYBM", "STNM", "P") %in% nms)) return("Rain")

  wq_core <- all(c("TM", "WYBM", "STNM") %in% nms)
  wq_vars <- intersect(c("TN", "TP", "NH4N", "NO3N", "COD", "TOC", "DO", "PH"), nms)
  if (wq_core && length(wq_vars) > 0) return("WaterQuality")

  if (all(c("NAME_WQ", "WYBM_WQ", "WYBM_QF") %in% nms)) return("Topology")

  "Unknown"
}


# ---------------------------------------------------------
# 标准化 clean_list
# ---------------------------------------------------------
fw_viz_normalize_clean_list <- function(clean_list) {
  if (is.null(clean_list) || length(clean_list) == 0) return(list())

  out <- clean_list

  for (nm in names(out)) {
    df <- out[[nm]]
    if (!is.data.frame(df)) next

    df <- fw_viz_clean_names(df)

    if ("TM"   %in% names(df)) df$TM   <- fw_viz_parse_date(df$TM)
    if ("WYBM"  %in% names(df)) df$WYBM  <- as.character(df$WYBM)
    if ("STNM"  %in% names(df)) {
      df$STNM <- as.character(df$STNM)
      df$STNM[is.na(df$STNM) | df$STNM == ""] <-
        df$WYBM[is.na(df$STNM) | df$STNM == ""]
    }
    if ("Q" %in% names(df)) df$Q <- suppressWarnings(as.numeric(df$Q))
    if ("P" %in% names(df)) df$P <- suppressWarnings(as.numeric(df$P))

    out[[nm]] <- df
  }

  out
}


# ---------------------------------------------------------
# 上传文件 -> clean_list
# ---------------------------------------------------------
fw_viz_read_uploaded_data <- function(path) {
  ext <- tolower(tools::file_ext(path))

  if (ext %in% c("xlsx", "xls")) {
    if (!exists("fw_read_validate_workbook", mode = "function")) {
      stop("fw_read_validate_workbook() not found. Please source fct_ingest.R first.")
    }
    res <- fw_read_validate_workbook(path, auto_impute_flow = TRUE, target = "TN")
    return(res$clean_list)
  }

  if (ext == "rds") {
    obj <- readRDS(path)
    if (is.list(obj) && any(names(obj) %in% c("Flow", "WaterQuality", "Rain", "Topology", "PP"))) {
      return(fw_viz_normalize_clean_list(obj))
    }
    if (is.data.frame(obj)) {
      tp <- fw_viz_guess_sheet_type(obj)
      if (tp == "Unknown") stop("Cannot infer RDS data type.")
      out <- list(); out[[tp]] <- obj
      return(fw_viz_normalize_clean_list(out))
    }
    stop("Unsupported RDS object.")
  }

  if (ext == "csv") {
    df <- read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
    df <- fw_viz_clean_names(df)
    tp <- fw_viz_guess_sheet_type(df)
    if (tp == "Unknown") stop("Cannot infer CSV data type.")
    out <- list(); out[[tp]] <- df
    return(fw_viz_normalize_clean_list(out))
  }

  stop("Unsupported file type.")
}


# ---------------------------------------------------------
# 从 clean_list 中抽取数据
# ---------------------------------------------------------
fw_viz_get_plot_data <- function(clean_list) {
  clean_list <- fw_viz_normalize_clean_list(clean_list)

  out <- list(rain = NULL, flow = NULL, wq = NULL, topo = NULL)
  nm  <- names(clean_list)

  rain_name <- intersect(c("Rain", "Rainfall", "Precip", "Precipitation", "PP"), nm)
  if (length(rain_name) > 0) {
    rain  <- clean_list[[rain_name[1]]]
    p_col <- intersect(c("P", "PP", "RAIN", "Rain", "PREC"), names(rain))
    if (length(p_col) > 0) names(rain)[names(rain) == p_col[1]] <- "P"
    if (!("STNM" %in% names(rain))) rain$STNM <- rain$WYBM %||% "Rain"
    out$rain <- rain
  }

  if ("Flow"         %in% nm) out$flow <- clean_list$Flow
  if ("WaterQuality" %in% nm) out$wq   <- clean_list$WaterQuality
  if ("Topology"     %in% nm) out$topo <- clean_list$Topology

  out
}


# ---------------------------------------------------------
# 水质指标候选
# ---------------------------------------------------------
fw_viz_get_wq_vars <- function(wq_df) {
  if (is.null(wq_df) || nrow(wq_df) == 0) return(character(0))
  exclude <- c("TM", "WYBM", "STNM")
  cand <- setdiff(names(wq_df), exclude)
  cand[vapply(wq_df[cand], function(x) is.numeric(x) || is.integer(x), logical(1))]
}


# ---------------------------------------------------------
# 站点 choices
# ---------------------------------------------------------
fw_viz_get_site_choices <- function(df) {
  if (is.null(df) || nrow(df) == 0) return(character(0))
  if (!all(c("WYBM", "STNM") %in% names(df))) return(character(0))

  tmp <- df %>%
    distinct(WYBM, STNM) %>%
    mutate(label = paste0(STNM, " [", WYBM, "]"))

  stats::setNames(as.character(tmp$WYBM), tmp$label)
}


# ---------------------------------------------------------
# 过滤
# ---------------------------------------------------------
fw_viz_filter_time <- function(df, start_time, end_time) {
  if (is.null(df) || nrow(df) == 0) return(df)
  if (!("TM" %in% names(df))) return(df)
  st <- fw_viz_as_posix(start_time)
  et <- fw_viz_as_posix(end_time)
  df %>% filter(TM >= st, TM <= et)
}

fw_viz_filter_site <- function(df, site_ids) {
  if (is.null(df) || nrow(df) == 0) return(df)
  if (is.null(site_ids) || length(site_ids) == 0) return(df)
  if (!("WYBM" %in% names(df))) return(df)
  df %>% filter(as.character(WYBM) %in% as.character(site_ids))
}


# ---------------------------------------------------------
# Topology: WQ -> Flow 站点映射
# ---------------------------------------------------------
fw_viz_match_flow_sites_by_topology <- function(clean_list, wq_sites) {
  dd   <- fw_viz_get_plot_data(clean_list)
  topo <- dd$topo
  if (is.null(topo) || nrow(topo) == 0) return(character(0))
  if (!all(c("WYBM_WQ", "WYBM_QF") %in% names(topo))) return(character(0))

  topo %>%
    mutate(WYBM_WQ = as.character(WYBM_WQ),
           WYBM_QF = as.character(WYBM_QF)) %>%
    filter(WYBM_WQ %in% as.character(wq_sites)) %>%
    pull(WYBM_QF) %>%
    unique()
}


# ---------------------------------------------------------
# 水质长表转换
# ---------------------------------------------------------
fw_viz_wq_to_long <- function(df, wq_vars) {
  if (is.null(df) || nrow(df) == 0) return(NULL)
  if (is.null(wq_vars) || length(wq_vars) == 0) return(NULL)

  keep <- intersect(c("TM", "WYBM", "STNM", wq_vars), names(df))
  if (length(setdiff(c("TM", "WYBM", "STNM"), keep)) > 0) return(NULL)

  out <- do.call(rbind, lapply(wq_vars, function(vv) {
    if (!(vv %in% names(df))) return(NULL)
    data.frame(
      TM        = df$TM,
      WYBM      = df$WYBM,
      STNM      = df$STNM,
      Indicator = vv,
      Value     = suppressWarnings(as.numeric(df[[vv]])),
      stringsAsFactors = FALSE
    )
  }))

  out
}


# ---------------------------------------------------------
# 内部辅助：添加水质标准线
# ---------------------------------------------------------
.wq_add_std_lines <- function(p, add_standard_lines, std_lines) {
  if (!isTRUE(add_standard_lines) || is.null(std_lines) || length(std_lines) == 0) return(p)
  if (!is.null(std_lines[["third"]]))
    p <- p + ggplot2::geom_hline(yintercept = std_lines[["third"]],
                                 colour = "brown", linetype = 3, linewidth = 0.8)
  if (!is.null(std_lines[["fourth"]]))
    p <- p + ggplot2::geom_hline(yintercept = std_lines[["fourth"]],
                                 colour = "blue",  linetype = 2, linewidth = 0.8)
  if (!is.null(std_lines[["fifth"]]))
    p <- p + ggplot2::geom_hline(yintercept = std_lines[["fifth"]],
                                 colour = "black", linetype = 1, linewidth = 0.8)
  p
}


# ---------------------------------------------------------
# 根据面板数量推算图高（像素）
# ---------------------------------------------------------
fw_viz_calc_plot_height <- function(plot_mode       = "flow_wq",
                                    single_type     = "flow",
                                    rain_sites      = NULL,
                                    flow_sites      = NULL,
                                    wq_sites        = NULL,
                                    wq_vars         = NULL,
                                    rain_panel_mode = "overlay",
                                    flow_panel_mode = "overlay",
                                    wq_panel_mode   = "facet_site_var",
                                    panel_height_px = 260,
                                    min_height_px   = 400,
                                    max_height_px   = 5000) {

  n_rain_sites <- max(length(rain_sites), 1)
  n_flow_sites <- max(length(flow_sites), 1)
  n_wq_sites   <- max(length(wq_sites), 1)
  n_wq_vars    <- max(length(wq_vars), 1)

  n_rain <- if (rain_panel_mode == "facet_site") n_rain_sites else 1
  n_flow <- if (flow_panel_mode == "facet_site") n_flow_sites else 1

  n_wq <- switch(
    wq_panel_mode,
    "facet_site"     = n_wq_sites,
    "facet_var"      = n_wq_vars,
    "overlay"        = max(n_wq_vars, 1),
    "facet_site_var" = n_wq_sites * n_wq_vars,
    1
  )

  total_panels <- switch(
    plot_mode,
    "rain_flow_wq"   = n_rain + n_flow + n_wq,
    "flow_wq"        = n_flow + n_wq,
    "linked_flow_wq" = n_flow + n_wq,
    "single" = switch(single_type,
                      "rain" = n_rain,
                      "flow" = n_flow,
                      "wq"   = n_wq,
                      1),
    1
  )

  height <- total_panels * panel_height_px
  max(min(height, max_height_px), min_height_px)
}
