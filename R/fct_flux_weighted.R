# =====================================================================
# fct_flux_weighted.R
# 加权平均法 —— 核心计算（Method 1–5 逐日估算 + RiverLoad 总负荷校验）
# =====================================================================

if (!exists("%||%", mode = "function")) {
  `%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x
}

#' 加权平均法主计算函数
#'
#' @param step1_data  list(QF, WQ)
#' @param dat_daily   data.frame(date, Q, C_obs, ...) —— 备用直接输入
#' @param qf_sheet    QF 表名
#' @param wq_sheet    WQ 表名
#' @param constituent 水质指标
#' @param date_range  日期范围 c(start, end)
#' @param sub_method  子方法 "method1"~"method5"
#' @param conv_factor 换算系数 (默认 86.4)
#' @return list(method, method_label, daily, summary, diag, params)
fw_run_flux_weighted <- function(step1_data = NULL,
                                 dat_daily = NULL,
                                 qf_sheet = NULL,
                                 wq_sheet = NULL,
                                 constituent = "TN",
                                 date_range = NULL,
                                 sub_method = c("method1", "method2",
                                                "method3", "method4",
                                                "method5"),
                                 conv_factor = 86.4) {

  sub_method <- match.arg(sub_method)

  # ================================================================
  # A. 数据准备
  # ================================================================
  if (!is.null(step1_data)) {
    prep <- fw_prepare_weighted_input(
      step1_data  = step1_data,
      qf_sheet    = qf_sheet,
      wq_sheet    = wq_sheet,
      constituent = constituent,
      date_range  = date_range
    )
    dat         <- prep$dat
    dat_rl      <- prep$dat_rl
    station     <- prep$station
    wybm        <- prep$wybm %||% NA_character_
    qf_sheet    <- prep$qf_sheet
    wq_sheet    <- prep$wq_sheet
    constituent <- prep$constituent

  } else if (!is.null(dat_daily)) {
    d <- as.data.frame(dat_daily, stringsAsFactors = FALSE)
    d$TM   <- fw_as_date(d$date %||% d$TM)
    d$Q    <- fw_as_num(d$Q)
    d$conc <- fw_as_num(d$C_obs %||% d$conc)

    if (!is.null(date_range) && length(date_range) == 2 && all(!is.na(date_range))) {
      s <- as.Date(date_range[1]); e <- as.Date(date_range[2])
      if (s > e) { tmp <- s; s <- e; e <- tmp }
      d <- d[d$TM >= s & d$TM <= e, , drop = FALSE]
    }
    if (nrow(d) == 0) stop("\u65f6\u95f4\u8303\u56f4\u5185\u65e0\u53ef\u7528\u6570\u636e\u3002")

    dat <- d[, c("TM", "Q", "conc"), drop = FALSE]
    dat <- dat[order(dat$TM), , drop = FALSE]
    rownames(dat) <- NULL

    dat_rl <- data.frame(
      datetime = as.POSIXct(dat$TM, tz = "Asia/Shanghai"),
      flow     = dat$Q,
      conc     = dat$conc,
      stringsAsFactors = FALSE
    )
    names(dat_rl)[3] <- constituent
    dat_rl <- dat_rl[!duplicated(dat_rl$datetime), , drop = FALSE]

    station <- if ("station" %in% names(d)) as.character(d$station[1]) else "ALL"
    wybm    <- if ("WYBM" %in% names(d)) as.character(d$WYBM[1]) else NA_character_

  } else {
    stop("\u9700\u8981\u63d0\u4f9b step1_data \u6216 dat_daily\u3002")
  }

  n_obs <- sum(!is.na(dat$conc))
  if (n_obs < 1) stop("\u65e0\u6709\u6548\u6d53\u5ea6\u76d1\u6d4b\u6570\u636e\u3002")

  # ================================================================
  # B. 逐日浓度估算 + 通量
  # ================================================================
  if (sub_method == "method1") {
    C_est <- fw_weighted_method1_daily(dat)
    flux  <- dat$Q * C_est * conv_factor
  } else if (sub_method == "method2") {
    m <- fw_weighted_method2_daily(dat, conv_factor = conv_factor)
    C_est <- m$C_est
    flux  <- m$flux
  } else if (sub_method == "method3") {
    m <- fw_weighted_method3_daily(dat, conv_factor = conv_factor)
    C_est <- m$C_est
    flux  <- m$flux
  } else if (sub_method == "method4") {
    m <- fw_weighted_method4_daily(dat, conv_factor = conv_factor)
    C_est <- m$C_est
    flux  <- m$flux
  } else if (sub_method == "method5") {
    m <- fw_weighted_method5_daily(dat, conv_factor = conv_factor)
    C_est <- m$C_est
    flux  <- m$flux
  }

  # ================================================================
  # C. RiverLoad 总负荷对比
  # ================================================================
  rl_total_g  <- fw_weighted_riverload_total(dat_rl, ncomp = 1)
  rl_total_kg <- rl_total_g / 1e3

  rl_this_kg   <- rl_total_kg[[sub_method]]
  daily_sum_kg <- sum(flux, na.rm = TRUE)

  # ================================================================
  # D. 构建 daily 输出
  # ================================================================
  daily <- data.frame(
    station  = station,
    WYBM     = wybm,
    date     = dat$TM,
    Q        = dat$Q,
    C_obs    = dat$conc,
    C_est    = C_est,
    flux     = flux,
    method   = sub_method,
    C_source = ifelse(!is.na(dat$conc), "observed", "estimated"),
    stringsAsFactors = FALSE
  )
  daily <- daily[order(daily$date), , drop = FALSE]
  rownames(daily) <- NULL

  # ================================================================
  # E. 诊断信息
  # ================================================================
  sub_label <- switch(sub_method,
                      method1 = "\u65f6\u95f4\u52a0\u6743 Q\u4e0eC (Method 1)",
                      method2 = "\u6d41\u91cf\u52a0\u6743\u6d53\u5ea6 (Method 2)",
                      method3 = "\u76f8\u90bb\u533a\u95f4\u5e73\u5747\u6d41\u91cf\u52a0\u6743 (Method 3)",
                      method4 = "\u65f6\u95f4\u52a0\u6743\u6d53\u5ea6+\u5168\u671f\u5747\u6d41\u91cf (Method 4)",
                      method5 = "\u65f6\u95f4\u4e0e\u6d41\u91cf\u52a0\u6743 (Method 5)")

  model_compare <- data.frame(
    method        = names(rl_total_kg),
    total_load_kg = round(unname(rl_total_kg), 4),
    stringsAsFactors = FALSE
  )

  d_start <- if (nrow(daily) > 0 && any(!is.na(daily$date)))
    as.character(min(daily$date, na.rm = TRUE)) else NA_character_
  d_end   <- if (nrow(daily) > 0 && any(!is.na(daily$date)))
    as.character(max(daily$date, na.rm = TRUE)) else NA_character_

  # ================================================================
  # F. 返回
  # ================================================================
  list(
    method       = "weighted",
    method_label = paste0("\u52a0\u6743\u5e73\u5747\u6cd5\uff08", sub_label, "\uff09"),
    daily   = daily,
    summary = fw_make_flux_summary_table(daily),
    diag = list(
      sub_method    = sub_method,
      sub_label     = sub_label,
      n_obs         = n_obs,
      n_total_days  = nrow(daily),
      daily_sum_kg  = round(daily_sum_kg, 4),
      rl_this_kg    = round(rl_this_kg %||% NA_real_, 4),
      model_compare = model_compare
    ),
    params = list(
      qf_sheet     = qf_sheet,
      wq_sheet     = wq_sheet,
      constituent  = constituent,
      sub_method   = sub_method,
      conv_factor  = conv_factor,
      data_source  = "step1_qf_wq",
      data_source_label = paste0("\u7b2c\u4e00\u6b65\u6570\u636e: QF$", qf_sheet,
                                 " + WQ$", wq_sheet,
                                 " [", constituent, "]"),
      date_start = d_start,
      date_end   = d_end
    )
  )
}
