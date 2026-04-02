
# =====================================================================
# fct_flux.R
# 通量计算模块 —— 共享计算函数
# （标签/原理/数据准备/汇总表/历史管理/绘图/总调度）
# =====================================================================

if (!exists("%||%", mode = "function")) {
  `%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x
}

# ---------- 方法标签 ----------

fw_flux_method_label <- function(key) {
  switch(key,
         weighted   = "加权平均法",
         interp     = "插值法",
         ratio      = "比率法",
         regression = "回归法",
         composite  = "复合方法",
         as.character(key))
}

# ---------- 方法原理 ----------

fw_flux_method_principle <- function(method) {
  switch(method,
         weighted   = "以有监测浓度日的流量作为权重计算代表浓度，缺测浓度日采用该代表值，再按 Q×C 计算日通量。",
         interp     = "对离散监测浓度按时间进行线性插值，获得连续日浓度序列，与日流量相乘得到日通量。",
         ratio      = "先在有监测日建立 C/Q 的平均比率，缺测浓度日按该比率与当日流量估算浓度。",
         regression = "调用第一步 QF/WQ 数据进行回归预测，得到逐日负荷并转换通量。",
         composite  = "复合法：回归'结构项' + 插值/Kalman '校正项'，包括 loadComp 四种子方法与 WRTDS-Kalman。",
         "未知方法")
}

# ---------- 原始数据准备 ----------

fw_prepare_flux_data <- function(raw_df) {
  if (is.null(raw_df) || !is.data.frame(raw_df) || nrow(raw_df) == 0)
    stop("原始数据为空。")
  cc <- fw_find_flux_cols(raw_df)
  if (is.null(cc$dt) || is.null(cc$q) || is.null(cc$c))
    stop("未识别到日期/流量/浓度列。")
  d <- data.frame(
    date    = fw_as_date(raw_df[[cc$dt]]),
    Q       = fw_as_num(raw_df[[cc$q]]),
    C_obs   = fw_as_num(raw_df[[cc$c]]),
    station = if (!is.null(cc$station)) as.character(raw_df[[cc$station]]) else NA_character_,
    WYBM    = if (!is.null(cc$wybm))    as.character(raw_df[[cc$wybm]])    else NA_character_,
    stringsAsFactors = FALSE)
  d <- d[!is.na(d$date), , drop = FALSE]
  if (nrow(d) == 0) stop("日期列解析失败，未得到有效记录。")
  d$station <- fw_trim_na(d$station); d$station[is.na(d$station)] <- "ALL"
  d$WYBM    <- fw_trim_na(d$WYBM)
  grp <- data.frame(date = d$date, station = d$station,
                    WYBM = ifelse(is.na(d$WYBM), "", d$WYBM), stringsAsFactors = FALSE)
  agg_mean <- function(x) if (all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE)
  q1 <- stats::aggregate(d$Q,     by = grp, FUN = agg_mean); names(q1)[ncol(q1)] <- "Q"
  c1 <- stats::aggregate(d$C_obs, by = grp, FUN = agg_mean); names(c1)[ncol(c1)] <- "C_obs"
  out <- merge(q1, c1, by = c("date", "station", "WYBM"), all = TRUE, sort = TRUE)
  out$WYBM[out$WYBM == ""] <- NA_character_
  out <- out[order(out$date), c("date", "Q", "C_obs", "station", "WYBM"), drop = FALSE]
  rownames(out) <- NULL
  out
}

# ---------- 汇总表 ----------

fw_make_flux_summary_table <- function(daily) {
  if (is.null(daily) || !is.data.frame(daily) || nrow(daily) == 0) {
    return(data.frame(calc_result = character(0), date = as.Date(character(0)),
                      station = character(0), WYBM = character(0),
                      Q = numeric(0), flux = numeric(0), stringsAsFactors = FALSE))
  }
  d <- as.data.frame(daily, stringsAsFactors = FALSE)
  if (!("date"    %in% names(d)) && "TM" %in% names(d)) d$date <- fw_as_date(d$TM)
  if (!("station" %in% names(d))) d$station <- "ALL"
  if (!("WYBM"    %in% names(d))) d$WYBM    <- NA_character_
  if (!("Q"       %in% names(d))) d$Q       <- NA_real_
  if (!("flux"    %in% names(d))) d$flux    <- NA_real_
  out <- data.frame(
    calc_result = "日值", date = fw_as_date(d$date),
    station = as.character(d$station), WYBM = as.character(d$WYBM),
    Q = fw_as_num(d$Q), flux = fw_as_num(d$flux), stringsAsFactors = FALSE)
  out <- out[order(out$date), , drop = FALSE]; rownames(out) <- NULL
  out
}

fw_make_flux_daily_summary <- function(daily) {
  fw_make_flux_summary_table(daily)
}

# ---------- 历史管理 ----------

fw_init_flux_history <- function(method_keys = c("weighted", "interp", "ratio", "regression", "composite")) {
  out <- setNames(vector("list", length(method_keys)), method_keys)
  for (k in method_keys) out[[k]] <- list(meta = data.frame(stringsAsFactors = FALSE), items = list())
  out
}

fw_append_flux_history <- function(history, method, res, station = "ALL") {
  if (is.null(history) || !is.list(history)) history <- fw_init_flux_history()
  if (is.null(history[[method]])) history[[method]] <- list(meta = data.frame(stringsAsFactors = FALSE), items = list())
  if (is.null(history[[method]]$meta)  || !is.data.frame(history[[method]]$meta))  history[[method]]$meta  <- data.frame(stringsAsFactors = FALSE)
  if (is.null(history[[method]]$items) || !is.list(history[[method]]$items))        history[[method]]$items <- list()
  rid <- paste0(format(Sys.time(), "%Y%m%d%H%M%S"), "_", sprintf("%03d", sample.int(999, 1)))
  while (rid %in% names(history[[method]]$items)) {
    rid <- paste0(format(Sys.time(), "%Y%m%d%H%M%S"), "_", sprintf("%03d", sample.int(999, 1)))
  }
  d <- if (!is.null(res) && is.list(res) && is.data.frame(res$daily)) res$daily else data.frame()
  p <- res$params %||% list()
  rec <- data.frame(
    rid = rid, created_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    method = method, method_label = fw_flux_method_label(method),
    station = as.character(station %||% "ALL"),
    n_days     = if (is.data.frame(d)) nrow(d) else NA_integer_,
    total_flux = if (is.data.frame(d) && "flux" %in% names(d)) round(sum(d$flux, na.rm = TRUE), 4) else NA_real_,
    mean_flux  = if (is.data.frame(d) && "flux" %in% names(d)) round(mean(d$flux, na.rm = TRUE), 4) else NA_real_,
    # ← 已删除 param1 / param2 列
    model_choice = p$model_choice %||% NA_character_,
    data_source  = p$data_source_label %||% p$data_source %||% NA_character_,
    date_start   = p$date_start   %||% NA_character_,
    date_end     = p$date_end     %||% NA_character_,
    stringsAsFactors = FALSE)
  history[[method]]$meta <- fw_rbind_fill(history[[method]]$meta, rec)
  history[[method]]$items[[rid]] <- res
  history
}

# ---------- 绘图 ----------

fw_plot_flux_ts <- function(daily, title = "通量时序") {
  if (is.null(daily) || !is.data.frame(daily) || nrow(daily) == 0) return(plotly::plot_ly())
  d <- as.data.frame(daily, stringsAsFactors = FALSE)
  if (!("date" %in% names(d)) && "TM" %in% names(d)) d$date <- fw_as_date(d$TM)
  d$date <- fw_as_date(d$date); d$flux <- fw_as_num(d$flux)
  d <- d[!is.na(d$date), , drop = FALSE]; d <- d[order(d$date), , drop = FALSE]
  if (nrow(d) == 0) return(plotly::plot_ly())
  p <- plotly::plot_ly(d, x = ~date, y = ~flux, type = "scatter", mode = "lines+markers",
                       name = "Flux", hovertemplate = "日期: %{x}<br>通量: %{y:.4f} kg/d<extra></extra>")
  plotly::layout(p, title = title, xaxis = list(title = "日期"), yaxis = list(title = "通量 (kg/d)"))
}

fw_plot_flux_diag <- function(res) {
  if (is.null(res) || is.null(res$daily) || !is.data.frame(res$daily) || nrow(res$daily) == 0) {
    graphics::plot.new(); graphics::text(0.5, 0.5, "No diagnostic data"); return(invisible(NULL))
  }
  d <- as.data.frame(res$daily, stringsAsFactors = FALSE)
  if (!("date" %in% names(d)) && "TM" %in% names(d)) d$date <- fw_as_date(d$TM)
  q <- fw_as_num(d$Q); c_obs <- fw_as_num(d$C_obs); c_est <- fw_as_num(d$C_est)
  flux <- fw_as_num(d$flux); dt <- fw_as_date(d$date)
  op <- graphics::par(no.readonly = TRUE); on.exit(graphics::par(op), add = TRUE)
  graphics::par(mfrow = c(1, 2), mar = c(4, 4, 3, 1))
  graphics::plot(q, c_est, pch = 1, col = "#2c7fb8", xlab = "Q (m3/s)", ylab = "浓度 (mg/L)", main = "浓度诊断")
  graphics::points(q, c_obs, pch = 16, col = grDevices::rgb(0.85, 0.2, 0.2, 0.6))
  graphics::legend("topleft", legend = c("C_est", "C_obs"), pch = c(1, 16),
                   col = c("#2c7fb8", grDevices::rgb(0.85, 0.2, 0.2, 0.6)), bty = "n", cex = 0.9)
  graphics::plot(dt, flux, type = "h", lwd = 2, col = "#1b9e77", xlab = "日期", ylab = "通量 (kg/d)", main = "日通量")
  if (any(is.finite(flux))) graphics::abline(h = mean(flux, na.rm = TRUE), lty = 2, col = "#555555")
  invisible(NULL)
}


# ---------- 总调度入口 ----------

fw_run_flux_with_config <- function(dat_daily = NULL,
                                    method = c("weighted", "interp", "ratio",
                                               "regression", "composite"),
                                    date_range = NULL,
                                    conv_factor = 86.4,
                                    step1_data = NULL,
                                    regression_cfg = list(),
                                    composite_cfg  = list(),
                                    interp_cfg     = list(),
                                    weighted_cfg   = list(),
                                    ratio_cfg      = list()) {   # ← 新增
  # ← 新增参数
  method <- match.arg(method)

  # ---- 回归法 ----
  if (identical(method, "regression") && !is.null(step1_data)) {
    return(fw_run_flux_regression_loadflex(
      step1_data   = step1_data,
      qf_sheet     = regression_cfg$qf_sheet     %||% NULL,
      wq_sheet     = regression_cfg$wq_sheet     %||% NULL,
      constituent  = regression_cfg$constituent  %||% "TN",
      date_range   = date_range,
      model_choice = regression_cfg$model_choice %||% "loadLm_season"))
  }

  # ---- 插值方法 ----
  if (identical(method, "interp") && !is.null(step1_data)) {
    return(fw_run_flux_interp(
      step1_data  = step1_data,
      qf_sheet    = interp_cfg$qf_sheet    %||% NULL,
      wq_sheet    = interp_cfg$wq_sheet    %||% NULL,
      constituent = interp_cfg$constituent %||% "TN",
      date_range  = date_range,
      sub_method  = interp_cfg$sub_method  %||% "method6",
      conv_factor = conv_factor))
  }
  if (identical(method, "interp") && !is.null(dat_daily)) {
    return(fw_run_flux_interp(
      dat_daily   = dat_daily,
      constituent = interp_cfg$constituent %||% "TN",
      date_range  = date_range,
      sub_method  = interp_cfg$sub_method  %||% "method6",
      conv_factor = conv_factor))
  }

  # ---- 比率估计（Method 7/8） ----
  if (identical(method, "ratio") && !is.null(step1_data)) {
    return(fw_run_flux_ratio(
      step1_data  = step1_data,
      qf_sheet    = ratio_cfg$qf_sheet    %||% NULL,
      wq_sheet    = ratio_cfg$wq_sheet    %||% NULL,
      constituent = ratio_cfg$constituent %||% "TN",
      date_range  = date_range,
      sub_method  = ratio_cfg$sub_method  %||% "method7",
      conv_factor = conv_factor))
  }
  if (identical(method, "ratio") && !is.null(dat_daily)) {
    return(fw_run_flux_ratio(
      dat_daily   = dat_daily,
      constituent = ratio_cfg$constituent %||% "TN",
      date_range  = date_range,
      sub_method  = ratio_cfg$sub_method  %||% "method7",
      conv_factor = conv_factor))
  }


  # ---- 加权平均法（Method 1–5） ----
  if (identical(method, "weighted") && !is.null(step1_data)) {
    return(fw_run_flux_weighted(
      step1_data  = step1_data,
      qf_sheet    = weighted_cfg$qf_sheet    %||% NULL,
      wq_sheet    = weighted_cfg$wq_sheet    %||% NULL,
      constituent = weighted_cfg$constituent %||% "TN",
      date_range  = date_range,
      sub_method  = weighted_cfg$sub_method  %||% "method1",
      conv_factor = conv_factor))
  }
  if (identical(method, "weighted") && !is.null(dat_daily)) {
    return(fw_run_flux_weighted(
      dat_daily   = dat_daily,
      constituent = weighted_cfg$constituent %||% "TN",
      date_range  = date_range,
      sub_method  = weighted_cfg$sub_method  %||% "method1",
      conv_factor = conv_factor))
  }


  # ---- 复合法 ----
  if (identical(method, "composite") && length(composite_cfg) > 0) {
    return(fw_run_composite_method(
      dat_daily     = dat_daily,
      sub_method    = composite_cfg$sub_method    %||% "abs_linear",
      reg_model     = composite_cfg$reg_model     %||% "lm_season",
      interp_method = composite_cfg$interp_method %||% "linearInterpolation",
      conv_factor   = conv_factor,
      wrtds_windowY = composite_cfg$wrtds_windowY %||% 7,
      wrtds_windowQ = composite_cfg$wrtds_windowQ %||% 2,
      wrtds_windowS = composite_cfg$wrtds_windowS %||% 0.5,
      kalman_rho    = composite_cfg$kalman_rho    %||% 0.90,
      kalman_niter  = composite_cfg$kalman_niter  %||% 200,
      date_range    = date_range))
  }

}
