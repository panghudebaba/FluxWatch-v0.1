# =====================================================================
# fct_flux_ratio.R
# 比率估计 —— 核心计算（RiverLoad Method 7/8 + 逐日分配 + 校验）
# =====================================================================

if (!exists("%||%", mode = "function")) {
  `%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x
}

#' 比率估计主计算函数
#'
#' @param step1_data  list(QF, WQ)
#' @param dat_daily   data.frame —— 备用直接输入
#' @param qf_sheet    QF 表名
#' @param wq_sheet    WQ 表名
#' @param constituent 水质指标
#' @param date_range  日期范围 c(start, end)
#' @param sub_method  子方法 "method7" / "method8"
#' @param conv_factor 换算系数 (默认 86.4)
#' @return list(method, method_label, daily, summary, diag, params)
fw_run_flux_ratio <- function(step1_data = NULL,
                              dat_daily = NULL,
                              qf_sheet = NULL,
                              wq_sheet = NULL,
                              constituent = "TN",
                              date_range = NULL,
                              sub_method = c("method7", "method8"),
                              conv_factor = 86.4) {

  sub_method <- match.arg(sub_method)

  # ================================================================
  # A. 数据准备
  # ================================================================
  if (!is.null(step1_data)) {
    prep <- fw_prepare_ratio_input(
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
  # B. 逐日估算 + 通量
  # ================================================================
  beale_detail <- NULL
  period_detail <- NULL

  if (sub_method == "method7") {
    m <- fw_ratio_method7_daily(dat, conv_factor = conv_factor)
    C_est <- m$C_est
    flux  <- m$flux
    beale_detail <- m$beale_info

  } else if (sub_method == "method8") {
    m <- fw_ratio_method8_daily(dat, conv_factor = conv_factor, period = "month")
    C_est <- m$C_est
    flux  <- m$flux
    period_detail <- m$period_info
  }

  daily_sum_kg <- sum(flux, na.rm = TRUE)


  # ================================================================
  # C. RiverLoad 总负荷对比（method1-6 + rating + rating.period）
  # ================================================================
  rl_compare_g  <- fw_ratio_riverload_compare(dat_rl, ncomp = 1, period = "month")
  rl_compare_kg <- rl_compare_g / 1e3

  # 当前子方法对应的 RiverLoad 总负荷
  rl_this_key <- switch(sub_method,
                        method7 = "rating",
                        method8 = "rating.period"
  )
  rl_this_kg <- rl_compare_kg[[rl_this_key]]


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
    C_source = ifelse(!is.na(dat$conc), "observed", "ratio_estimated"),
    stringsAsFactors = FALSE
  )
  daily <- daily[order(daily$date), , drop = FALSE]
  rownames(daily) <- NULL

  # ================================================================
  # E. 诊断信息
  # ================================================================
  sub_label <- switch(sub_method,
                      method7 = "Beale \u6bd4\u7387\u4f30\u8ba1 (Method 7)",
                      method8 = "Beale \u6bd4\u7387\u5206\u671f (Method 8)")

  model_compare <- data.frame(
    method        = names(rl_compare_kg),
    total_load_kg = round(unname(rl_compare_kg), 4),
    stringsAsFactors = FALSE
  )

  d_start <- if (nrow(daily) > 0 && any(!is.na(daily$date)))
    as.character(min(daily$date, na.rm = TRUE)) else NA_character_
  d_end   <- if (nrow(daily) > 0 && any(!is.na(daily$date)))
    as.character(max(daily$date, na.rm = TRUE)) else NA_character_

  # Beale 特有诊断
  beale_diag <- list()
  if (!is.null(beale_detail)) {
    beale_diag <- list(
      beale_ratio = round(beale_detail$beale_ratio %||% NA_real_, 6),
      q_bar       = round(beale_detail$q_bar %||% NA_real_, 4),
      l_bar       = round(beale_detail$l_bar %||% NA_real_, 4),
      cov_lq      = round(beale_detail$cov_lq %||% NA_real_, 4),
      var_q       = round(beale_detail$var_q %||% NA_real_, 4),
      n_sample    = beale_detail$n %||% 0,
      fallback    = beale_detail$fallback %||% FALSE
    )
  }

  # 分期详情
  period_diag <- NULL
  if (!is.null(period_detail) && length(period_detail) > 0) {
    period_diag <- do.call(rbind, lapply(period_detail, function(x) {
      data.frame(
        period      = x$period,
        n_obs       = x$n_obs,
        n_days      = x$n_days,
        Q_total     = round(x$Q_total, 2),
        L_total_kg  = x$L_total,
        beale_ratio = x$beale_ratio,
        stringsAsFactors = FALSE
      )
    }))
    rownames(period_diag) <- NULL
  }

  # ================================================================
  # F. 返回
  # ================================================================
  list(
    method       = "ratio",
    method_label = paste0("\u6bd4\u7387\u4f30\u8ba1\uff08", sub_label, "\uff09"),
    daily   = daily,
    summary = fw_make_flux_summary_table(daily),
    diag = list(
      sub_method    = sub_method,
      sub_label     = sub_label,
      n_obs         = n_obs,
      n_total_days  = nrow(daily),
      daily_sum_kg  = round(daily_sum_kg, 4),
      rl_this_kg    = round(rl_this_kg %||% NA_real_, 4),
      model_compare = model_compare,
      beale_diag    = beale_diag,
      period_diag   = period_diag
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
