# =====================================================================
# fct_flux_interp.R
# 插值方法 —— 核心计算（RiverLoad Method 6 + loadflex 4种插值）
# =====================================================================

if (!exists("%||%", mode = "function")) {
  `%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x
}

#' 插值方法主计算函数
#'
#' @param step1_data  list(QF, WQ)
#' @param dat_daily   data.frame —— 备用直接输入
#' @param qf_sheet    QF 表名
#' @param wq_sheet    WQ 表名
#' @param constituent 水质指标
#' @param date_range  日期范围 c(start, end)
#' @param sub_method  子方法
#' @param conv_factor 换算系数 (默认 86.4)
#' @return list(method, method_label, daily, summary, diag, params)
fw_run_flux_interp <- function(step1_data = NULL,
                               dat_daily = NULL,
                               qf_sheet = NULL,
                               wq_sheet = NULL,
                               constituent = "TN",
                               date_range = NULL,
                               sub_method = c("method6",
                                              "linearInterp",
                                              "rectangularInterp",
                                              "triangularInterp",
                                              "distanceWeightedInterp"),
                               conv_factor = 86.4) {

  sub_method <- match.arg(sub_method)

  # ================================================================
  # A. 数据准备
  # ================================================================
  if (!is.null(step1_data)) {
    prep <- fw_prepare_interp_input(
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
  # B. 逐日浓度估算
  # ================================================================
  # loadflex 插值函数名映射
  loadflex_fun_map <- c(
    linearInterp          = "linearInterpolation",
    rectangularInterp     = "rectangularInterpolation",
    triangularInterp      = "triangularInterpolation",
    distanceWeightedInterp = "distanceWeightedInterpolation"
  )

  C_est        <- NULL
  source_label <- "manual"

  # 优先尝试 loadflex（对 4 种 loadflex 子方法）
  if (sub_method %in% names(loadflex_fun_map)) {
    lf_fun_name <- loadflex_fun_map[[sub_method]]
    C_est <- fw_interp_loadflex_predict(dat, constituent = constituent,
                                        interp_fun_name = lf_fun_name)
    if (!is.null(C_est) && length(C_est) == nrow(dat)) {
      source_label <- "loadflex"
    } else {
      C_est <- NULL
    }
  }

  # 如果 loadflex 不可用或失败，用手动实现
  if (is.null(C_est)) {
    C_est <- switch(sub_method,
                    method6                = fw_interp_linear_daily(dat),
                    linearInterp           = fw_interp_linear_daily(dat),
                    rectangularInterp      = fw_interp_rectangular_daily(dat),
                    triangularInterp       = fw_interp_triangular_daily(dat),
                    distanceWeightedInterp = fw_interp_distweight_daily(dat)
    )
    source_label <- if (sub_method == "method6") "manual(RiverLoad_equiv)" else "manual(fallback)"
  }

  # 有实测浓度的日期保留实测值
  obs_ok <- which(!is.na(dat$conc))
  if (length(obs_ok) > 0) {
    C_est[obs_ok] <- dat$conc[obs_ok]
  }

  # 计算通量
  flux <- dat$Q * C_est * conv_factor
  flux[!is.finite(flux)] <- NA_real_

  daily_sum_kg <- sum(flux, na.rm = TRUE)

  # ================================================================
  # C. RiverLoad 总负荷对比（method1-6）
  # ================================================================
  rl_compare_g  <- fw_interp_riverload_compare(dat_rl, ncomp = 1)
  rl_compare_kg <- rl_compare_g / 1e3

  # 当前方法对应的 RiverLoad 总负荷
  rl_this_kg <- if (sub_method == "method6") {
    rl_compare_kg[["method6"]]
  } else {
    # loadflex 子方法没有直接对应的 RiverLoad 方法
    # 但线性插值 ≈ method6，作为参考
    rl_compare_kg[["method6"]]
  }

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
    C_source = ifelse(!is.na(dat$conc), "observed", "interpolated"),
    stringsAsFactors = FALSE
  )
  daily <- daily[order(daily$date), , drop = FALSE]
  rownames(daily) <- NULL

  # ================================================================
  # E. 诊断信息
  # ================================================================
  sub_label <- switch(sub_method,
                      method6                = "\u6d53\u5ea6\u7ebf\u6027\u63d2\u503c / RiverLoad Method 6",
                      linearInterp           = "\u7ebf\u6027\u63d2\u503c (linearInterpolation)",
                      rectangularInterp      = "\u77e9\u5f62\u63d2\u503c (rectangularInterpolation)",
                      triangularInterp       = "\u4e09\u89d2\u6838\u63d2\u503c (triangularInterpolation)",
                      distanceWeightedInterp = "\u8ddd\u79bb\u52a0\u6743\u63d2\u503c (distanceWeightedInterpolation)")

  model_compare <- data.frame(
    method        = names(rl_compare_kg),
    total_load_kg = round(unname(rl_compare_kg), 4),
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
    method       = "interp",
    method_label = paste0("\u63d2\u503c\u65b9\u6cd5\uff08", sub_label, "\uff09"),
    daily   = daily,
    summary = fw_make_flux_summary_table(daily),
    diag = list(
      sub_method    = sub_method,
      sub_label     = sub_label,
      source_label  = source_label,
      n_obs         = n_obs,
      n_total_days  = nrow(daily),
      n_interpolated = sum(is.na(dat$conc) & is.finite(C_est)),
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
