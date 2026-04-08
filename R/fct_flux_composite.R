# =====================================================================
# fct_flux_composite.R
# 复合法 —— 核心计算（直接调用 loadflex / EGRET / EGRETci）
# =====================================================================

if (!exists("%||%", mode = "function")) {
  `%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x
}

# ====================================================================
#  1. loadComp 四种子方法（直接调用 loadflex）
# ====================================================================

#' 用 loadflex 运行 loadComp 复合法
#'
#' @param d          data.frame(date, Q, C_obs, ...)
#' @param sub_method "abs_linear"/"rel_linear"/"abs_log"/"rel_log"
#' @param reg_model  "lm_season" / "lm_simple"
#' @param interp_method loadflex 插值函数名
#' @return list(pred_df, comp_model, sub_method, backend, ...)

fw_run_loadcomp <- function(d,
                            sub_method    = c("abs_linear", "rel_linear",
                                              "abs_log", "rel_log"),
                            reg_model     = "lm_season",
                            interp_method = "linearInterpolation") {

  sub_method <- match.arg(sub_method)

  if (!requireNamespace("loadflex", quietly = TRUE))
    stop("loadflex \u5305\u672a\u5b89\u88c5\u3002")

  # ---------- 准备数据 ----------
  lf_data <- fw_to_loadflex_data(d)
  meta    <- fw_build_loadflex_meta()
  calib   <- lf_data$calib
  alldata <- lf_data$all

  use_log <- sub_method %in% c("abs_log", "rel_log")

  # ---------- 插值函数 ----------
  interp_fun <- switch(
    interp_method,
    linearInterpolation      = loadflex::linearInterpolation,
    rectangularInterpolation = loadflex::rectangularInterpolation,
    loadflex::linearInterpolation
  )

  # =================================================================
  # Step A: 回归拟合（用 base R lm，不用 loadflex::loadLm）
  # =================================================================
  calib$log_Q   <- log(pmax(calib$Q, 1e-8))
  alldata$log_Q <- log(pmax(alldata$Q, 1e-8))

  if (use_log) {
    # ---- log 空间：响应变量 = log(conc) ----
    calib$log_conc <- log(pmax(calib$conc, 1e-10))

    fit <- if (identical(reg_model, "lm_simple")) {
      stats::lm(log_conc ~ log_Q, data = calib)
    } else {
      stats::lm(log_conc ~ log_Q +
                  sin(2 * pi * doy / 365) + cos(2 * pi * doy / 365),
                data = calib)
    }

    log_C_reg_calib <- stats::predict(fit, newdata = calib)
    log_C_reg_all   <- stats::predict(fit, newdata = alldata)

    C_reg_calib <- exp(log_C_reg_calib)
    C_reg_all   <- exp(log_C_reg_all)

  } else {
    # ---- 线性空间：响应变量 = conc ----
    fit <- if (identical(reg_model, "lm_simple")) {
      stats::lm(conc ~ Q, data = calib)
    } else {
      stats::lm(conc ~ Q +
                  sin(2 * pi * doy / 365) + cos(2 * pi * doy / 365),
                data = calib)
    }

    C_reg_calib <- stats::predict(fit, newdata = calib)
    C_reg_all   <- stats::predict(fit, newdata = alldata)
  }

  # =================================================================
  # Step B: 计算残差 & 插值
  # =================================================================
  C_obs_calib <- fw_as_num(calib$conc)

  if (sub_method == "abs_linear") {
    # ---- 绝对残差 + 线性空间 ----
    R_calib <- C_obs_calib - C_reg_calib

  } else if (sub_method == "abs_log") {
    # ---- 绝对残差 + log 空间 ----
    # R = log(C_obs) - log(C_reg)  (在 log 空间是加性)
    R_calib <- log(pmax(C_obs_calib, 1e-10)) - log(pmax(C_reg_calib, 1e-10))

  } else if (sub_method == "rel_linear") {
    # ---- 相对残差 + 线性空间 ----
    safe_denom <- ifelse(abs(C_reg_calib) < 1e-10, 1e-10, C_reg_calib)
    R_calib <- (C_obs_calib - C_reg_calib) / safe_denom

  } else {
    # ---- rel_log：相对残差 + log 空间 ----
    safe_denom <- ifelse(abs(C_reg_calib) < 1e-10, 1e-10, C_reg_calib)
    R_calib <- (C_obs_calib - C_reg_calib) / safe_denom
  }

  # 用 loadInterp 插值残差
  resid_df <- data.frame(
    date = fw_as_date(calib$date),
    Q    = calib$Q,
    conc = R_calib,    # 借用 "conc" 列存残差
    stringsAsFactors = FALSE
  )

  interp_obj <- loadflex::loadInterp(
    interp.format = "conc",
    interp.fun    = interp_fun,
    data          = resid_df,
    metadata      = meta
  )

  pred_R <- loadflex::predictSolute(
    load.model   = interp_obj,
    flux.or.conc = "conc",
    newdata      = alldata,
    date         = TRUE,
    by           = "unit"
  )
  R_all <- fw_as_num(as.data.frame(pred_R, stringsAsFactors = FALSE)[, 2])

  # =================================================================
  # Step C: 合成浓度
  # =================================================================
  if (sub_method == "abs_linear") {
    # C_comp = C_reg + R
    C_comp <- C_reg_all + R_all

  } else if (sub_method == "abs_log") {
    # log(C_comp) = log(C_reg) + R  =>  C_comp = C_reg * exp(R)
    C_comp <- C_reg_all * exp(R_all)

  } else if (sub_method == "rel_linear") {
    # C_comp = C_reg * (1 + R)
    C_comp <- C_reg_all * (1 + R_all)

  } else {
    # rel_log: log(C_comp) = log(C_reg) + log(1 + R)
    R_safe <- pmax(R_all, -0.999)
    C_comp <- ifelse(
      is.finite(C_reg_all) & C_reg_all > 0,
      exp(log(pmax(C_reg_all, 1e-10)) + log(1 + R_safe)),
      C_reg_all
    )
  }

  # 负浓度修正
  C_comp <- pmax(C_comp, 0)

  # =================================================================
  # Step D: 计算通量
  # =================================================================
  flux_comp <- ifelse(
    is.finite(alldata$Q) & is.finite(C_comp),
    alldata$Q * C_comp * 86.4,
    NA_real_
  )

  pred_df <- data.frame(
    date     = fw_as_date(alldata$date),
    fit      = flux_comp,
    C_reg    = C_reg_all,
    C_comp   = C_comp,
    R_interp = R_all,
    stringsAsFactors = FALSE
  )

  list(
    pred_df       = pred_df,
    comp_model    = interp_obj,
    reg_model_obj = fit,
    sub_method    = sub_method,
    backend       = "base::lm + loadflex::loadInterp (manual composite)"
  )
}

# ====================================================================
#  2. WRTDS-Kalman（直接调用 EGRET + EGRETci）
# ====================================================================


#' 用 EGRET 运行 WRTDS-Kalman
#'
#' @param d          data.frame(date, Q, C_obs, ...)
#' @param rho        AR(1) 参数
#' @param niter      Monte Carlo 迭代次数
#' @param windowY    WRTDS 时间窗口 (年)
#' @param windowQ    WRTDS log(Q) 窗口
#' @param windowS    WRTDS 季节窗口
#' @param station_nm 站名
#' @return list(eList, Daily_out, rho, niter, backend)
fw_run_wrtds_kalman <- function(d,
                                rho        = 0.90,
                                niter      = 200,
                                windowY    = 7,
                                windowQ    = 2,
                                windowS    = 0.5,
                                station_nm = "Station") {

  if (!requireNamespace("EGRET", quietly = TRUE))
    stop("EGRET \u5305\u672a\u5b89\u88c5\u3002\u8bf7\u8fd0\u884c install.packages('EGRET')")

  # ── ★ 探测 WRTDSKalman 函数位置 ──
  wrtds_k_fn <- NULL
  wrtds_k_backend <- ""


  # 优先级 1：EGRET 包（新版 >= 3.0.7）
  if (exists("WRTDSKalman", where = asNamespace("EGRET"), mode = "function")) {
    wrtds_k_fn <- EGRET::WRTDSKalman
    wrtds_k_backend <- "EGRET::WRTDSKalman"
  }

  # 优先级 2：EGRETci 包（旧版）
  if (is.null(wrtds_k_fn) && requireNamespace("EGRETci", quietly = TRUE)) {
    if (exists("WRTDSKalman", where = asNamespace("EGRETci"), mode = "function")) {
      wrtds_k_fn <- EGRETci::WRTDSKalman
      wrtds_k_backend <- "EGRETci::WRTDSKalman"
    }
  }

  # 优先级 3：用 getFromNamespace 尝试非导出函数

  if (is.null(wrtds_k_fn)) {
    wrtds_k_fn <- tryCatch(
      utils::getFromNamespace("WRTDSKalman", "EGRET"),
      error = function(e) NULL
    )
    if (!is.null(wrtds_k_fn)) wrtds_k_backend <- "EGRET:::WRTDSKalman (internal)"
  }

  if (is.null(wrtds_k_fn)) {
    # 优先级 4：尝试 EGRETci 非导出
    wrtds_k_fn <- tryCatch(
      utils::getFromNamespace("WRTDSKalman", "EGRETci"),
      error = function(e) NULL
    )
    if (!is.null(wrtds_k_fn)) wrtds_k_backend <- "EGRETci:::WRTDSKalman (internal)"
  }

  if (is.null(wrtds_k_fn))
    stop("WRTDSKalman \u51fd\u6570\u672a\u627e\u5230\u3002\u8bf7\u5347\u7ea7 EGRET \u5305\uff1a\n",
         "  install.packages('EGRET')\n",
         "\u6216\u5b89\u88c5 EGRETci\uff1a\n",
         "  remotes::install_github('USGS-R/EGRETci')")

  # ---------- 构建 eList ----------
  eList <- fw_build_elist(d, station_nm = station_nm, param_nm = "Conc")

  # 覆盖窗口参数
  eList$INFO$windowY     <- windowY
  eList$INFO$windowQ     <- windowQ
  eList$INFO$windowS     <- windowS
  eList$INFO$minNumObs   <- min(100, nrow(eList$Sample))
  eList$INFO$minNumUncen <- min(50, sum(eList$Sample$Uncen == 1))

  # ---------- Step 1: WRTDS 基础模型 ----------
  eList <- EGRET::modelEstimation(eList, verbose = FALSE)

  # ---------- Step 2: Kalman 校正 ----------
  eList <- wrtds_k_fn(eList, rho = rho, niter = niter)

  # ---------- 提取结果 ----------
  list(
    eList     = eList,
    Daily_out = eList$Daily,
    Sample    = eList$Sample,
    INFO      = eList$INFO,
    rho       = rho,
    niter     = niter,
    backend   = paste0("EGRET::modelEstimation + ", wrtds_k_backend)
  )
}


# ====================================================================
#  3. 总调度入口
# ====================================================================

#' 运行复合法（总入口）
#'
#' @param dat_daily     data.frame(date, Q, C_obs, station, WYBM)
#' @param sub_method    子方法
#' @param reg_model     回归模型 (loadComp 用)
#' @param interp_method 插值函数名 (loadComp 用)
#' @param conv_factor   换算系数
#' @param param1,param2 乘法参数
#' @param wrtds_windowY,wrtds_windowQ,wrtds_windowS  EGRET 窗口
#' @param kalman_rho,kalman_niter  Kalman 参数
#' @param date_range    c(start, end)
#' @return list (与 fw_run_flux_method 结构一致)
fw_run_composite_method <- function(dat_daily,
                                    sub_method    = c("abs_linear", "rel_linear",
                                                      "abs_log", "rel_log",
                                                      "wrtds_kalman"),
                                    reg_model     = "lm_season",
                                    interp_method = "linearInterpolation",
                                    conv_factor   = 86.4,
                                    param1        = 1,
                                    param2        = 1,
                                    wrtds_windowY = 7,
                                    wrtds_windowQ = 2,
                                    wrtds_windowS = 0.5,
                                    kalman_rho    = 0.90,
                                    kalman_niter  = 200,
                                    date_range    = NULL) {
  sub_method <- match.arg(sub_method)
  to_scalar <- function(x, d = 1) {
    x <- suppressWarnings(as.numeric(x))
    if (length(x) == 0 || !is.finite(x[1])) d else x[1]
  }
  p1 <- to_scalar(param1, 1); p2 <- to_scalar(param2, 1)

  # ---------- 数据准备 ----------
  d <- as.data.frame(dat_daily, stringsAsFactors = FALSE)
  if (!all(c("date", "Q", "C_obs") %in% names(d)))
    stop("dat_daily must contain: date, Q, C_obs")
  d$date  <- fw_as_date(d$date)
  d$Q     <- fw_as_num(d$Q)
  d$C_obs <- fw_as_num(d$C_obs)
  if (!("station" %in% names(d))) d$station <- "ALL"
  if (!("WYBM"    %in% names(d))) d$WYBM    <- NA_character_

  # 日期筛选
  if (!is.null(date_range) && length(date_range) == 2 && all(!is.na(date_range))) {
    s <- as.Date(date_range[1]); e <- as.Date(date_range[2])
    if (s > e) { tmp <- s; s <- e; e <- tmp }
    d <- d[d$date >= s & d$date <= e, , drop = FALSE]
  }
  d <- d[!is.na(d$date), , drop = FALSE]
  d <- d[order(d$date), , drop = FALSE]
  if (nrow(d) == 0) stop("无可用日数据。")

  station_first <- as.character(unique(stats::na.omit(d$station))[1])
  if (is.na(station_first)) station_first <- "ALL"

  # ==============================================================
  #  分发计算
  # ==============================================================

  flux     <- rep(NA_real_, nrow(d))
  C_est    <- rep(NA_real_, nrow(d))
  C_source <- rep("composite", nrow(d))
  diag_extra <- list()

  if (sub_method %in% c("abs_linear", "rel_linear", "abs_log", "rel_log")) {
    # ---- loadComp 路线 ----
    lc_res <- fw_run_loadcomp(
      d = d, sub_method = sub_method,
      reg_model = reg_model, interp_method = interp_method)

    pred <- lc_res$pred_df

    # 提取通量
    if ("fit" %in% names(pred)) {
      flux_raw <- fw_as_num(pred$fit)
    } else {
      num_cols <- names(pred)[vapply(pred, is.numeric, logical(1))]
      num_cols <- setdiff(num_cols, "date")
      flux_raw <- if (length(num_cols) > 0) fw_as_num(pred[[num_cols[1]]])
      else rep(NA_real_, nrow(pred))
    }

    # 日期匹配
    pred_dates <- fw_as_date(pred[[1]])
    flux_matched <- rep(NA_real_, nrow(d))
    for (i in seq_len(nrow(d))) {
      idx <- which(pred_dates == d$date[i])
      if (length(idx) > 0) flux_matched[i] <- flux_raw[idx[1]]
    }

    flux <- flux_matched * p1 * p2

    # 有实测日直接算
    has_obs <- is.finite(d$C_obs) & is.finite(d$Q) & d$Q > 0
    flux[has_obs] <- d$Q[has_obs] * d$C_obs[has_obs] * conv_factor * p1 * p2
    C_source[has_obs] <- "observed"

    C_est <- ifelse(is.finite(d$Q) & d$Q > 0 & is.finite(flux),
                    flux / (d$Q * conv_factor), NA_real_)

    if ("C_reg"    %in% names(pred)) diag_extra$C_reg    <- fw_as_num(pred$C_reg)
    if ("R_interp" %in% names(pred)) diag_extra$R_interp <- fw_as_num(pred$R_interp)
    diag_extra$loadcomp_result <- lc_res

  } else if (identical(sub_method, "wrtds_kalman")) {
    # ---- WRTDS-Kalman 路线 ----
    wk_res <- fw_run_wrtds_kalman(
      d          = d,
      rho        = to_scalar(kalman_rho, 0.90),
      niter      = to_scalar(kalman_niter, 200),
      windowY    = to_scalar(wrtds_windowY, 7),
      windowQ    = to_scalar(wrtds_windowQ, 2),
      windowS    = to_scalar(wrtds_windowS, 0.5),
      station_nm = station_first)

    Dout <- wk_res$Daily_out
    egret_dates <- fw_as_date(Dout$Date)

    # 匹配回 d
    for (i in seq_len(nrow(d))) {
      idx <- which(egret_dates == d$date[i])
      if (length(idx) > 0) {
        j <- idx[1]
        if ("GenFlux" %in% names(Dout) && is.finite(Dout$GenFlux[j])) {
          flux[i] <- Dout$GenFlux[j] * p1 * p2
        } else if ("FluxDay" %in% names(Dout) && is.finite(Dout$FluxDay[j])) {
          flux[i] <- Dout$FluxDay[j] * p1 * p2
        }
      }
    }

    has_obs <- is.finite(d$C_obs) & is.finite(d$Q) & d$Q > 0
    flux[has_obs] <- d$Q[has_obs] * d$C_obs[has_obs] * conv_factor * p1 * p2
    C_source[has_obs] <- "observed"

    C_est <- ifelse(is.finite(d$Q) & d$Q > 0 & is.finite(flux),
                    flux / (d$Q * conv_factor), NA_real_)

    # 提取诊断用浓度
    C_wrtds <- rep(NA_real_, nrow(d))
    C_wk    <- rep(NA_real_, nrow(d))
    for (i in seq_len(nrow(d))) {
      idx <- which(egret_dates == d$date[i])
      if (length(idx) > 0) {
        j <- idx[1]
        if ("ConcDay" %in% names(Dout)) C_wrtds[i] <- Dout$ConcDay[j]
        if ("GenConc" %in% names(Dout)) C_wk[i]    <- Dout$GenConc[j]
      }
    }
    diag_extra$C_wrtds   <- C_wrtds
    diag_extra$C_wk      <- C_wk
    diag_extra$wk_result <- wk_res
  }

  # ---------- daily data.frame ----------
  daily <- data.frame(
    station  = as.character(d$station),
    WYBM     = as.character(d$WYBM),
    date     = d$date,
    Q        = d$Q,
    C_obs    = d$C_obs,
    C_est    = C_est,
    flux     = flux,
    method   = paste0("composite_", sub_method),
    C_source = C_source,
    stringsAsFactors = FALSE)

  if (!is.null(diag_extra$C_wrtds)) daily$C_wrtds <- diag_extra$C_wrtds
  if (!is.null(diag_extra$C_wk))    daily$C_wk    <- diag_extra$C_wk
  if (!is.null(diag_extra$C_reg))   daily$C_reg   <- diag_extra$C_reg

  daily <- daily[order(daily$date), , drop = FALSE]
  rownames(daily) <- NULL

  d_start <- if (any(!is.na(daily$date)))
    as.character(min(daily$date, na.rm = TRUE)) else NA_character_
  d_end   <- if (any(!is.na(daily$date)))
    as.character(max(daily$date, na.rm = TRUE)) else NA_character_

  list(
    method       = "composite",
    method_label = paste0("复合法（", fw_composite_sub_label(sub_method), "）"),
    daily   = daily,
    summary = fw_make_flux_summary_table(daily),
    diag = c(list(
      sub_method    = sub_method,
      reg_model     = if (sub_method != "wrtds_kalman") reg_model     else NA,
      interp_method = if (sub_method != "wrtds_kalman") interp_method else NA,
      n_obs         = sum(is.finite(d$C_obs) & is.finite(d$Q) & d$Q > 0)
    ), diag_extra),
    params = list(
      sub_method      = sub_method,
      reg_model       = reg_model,
      interp_method   = interp_method,
      param1          = p1,
      param2          = p2,
      conv_factor     = conv_factor,
      date_start      = d_start,
      date_end        = d_end,
      wrtds_windowY   = if (sub_method == "wrtds_kalman") wrtds_windowY else NA,
      wrtds_windowQ   = if (sub_method == "wrtds_kalman") wrtds_windowQ else NA,
      wrtds_windowS   = if (sub_method == "wrtds_kalman") wrtds_windowS else NA,
      kalman_rho      = if (sub_method == "wrtds_kalman") kalman_rho    else NA,
      kalman_niter    = if (sub_method == "wrtds_kalman") kalman_niter  else NA))
}

# ====================================================================
#  4. 诊断图（复合法专用）
# ====================================================================

fw_plot_composite_diag <- function(res) {
  if (is.null(res) || is.null(res$daily) || nrow(res$daily) == 0) {
    graphics::plot.new(); graphics::text(0.5, 0.5, "No data")
    return(invisible(NULL))
  }
  d   <- as.data.frame(res$daily, stringsAsFactors = FALSE)
  sub <- res$params$sub_method %||% res$diag$sub_method %||% "unknown"
  dt  <- fw_as_date(d$date)
  q   <- fw_as_num(d$Q)
  flux <- fw_as_num(d$flux)
  c_obs <- fw_as_num(d$C_obs)
  c_est <- fw_as_num(d$C_est)

  op <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(op), add = TRUE)
  is_wk <- identical(sub, "wrtds_kalman")

  if (is_wk) {
    graphics::par(mfrow = c(2, 2), mar = c(4, 4, 3, 1))

    # (1) C_obs vs C_wrtds vs C_wk
    c_wrtds <- fw_as_num(d$C_wrtds); c_wk <- fw_as_num(d$C_wk)
    ylim <- range(c(c_obs, c_wrtds, c_wk), na.rm = TRUE, finite = TRUE)
    graphics::plot(dt, c_wrtds, type = "l", col = "#bbb", lwd = 1.5,
                   xlab = "Date", ylab = "Conc (mg/L)",
                   main = "WRTDS vs Kalman Conc", ylim = ylim)
    graphics::lines(dt, c_wk, col = "#2c7fb8", lwd = 1.5)
    ok_obs <- which(is.finite(c_obs))
    if (length(ok_obs) > 0)
      graphics::points(dt[ok_obs], c_obs[ok_obs], pch = 16,
                       col = grDevices::rgb(0.85, 0.2, 0.2, 0.6), cex = 0.8)
    graphics::legend("topleft", c("WRTDS", "Kalman", "Obs"),
                     col = c("#bbb", "#2c7fb8", grDevices::rgb(.85,.2,.2)),
                     lty = c(1, 1, NA), pch = c(NA, NA, 16), bty = "n", cex = 0.8)

    # (2) WRTDS flux vs Kalman flux
    wk <- res$diag$wk_result
    if (!is.null(wk) && !is.null(wk$Daily_out)) {
      Dout <- wk$Daily_out; egd <- fw_as_date(Dout$Date)
      graphics::plot(egd, Dout$FluxDay, type = "l", col = "#bbb", lwd = 1.2,
                     xlab = "Date", ylab = "Flux (kg/d)", main = "Flux comparison")
      if ("GenFlux" %in% names(Dout))
        graphics::lines(egd, Dout$GenFlux, col = "#2c7fb8", lwd = 1.2)
      graphics::legend("topleft", c("WRTDS", "Kalman"),
                       col = c("#bbb", "#2c7fb8"), lty = 1, bty = "n", cex = 0.8)
    } else { graphics::plot.new(); graphics::text(.5, .5, "No EGRET output") }

    # (3) Q-C scatter
    graphics::plot(q, c_est, pch = 1, col = "#2c7fb8", cex = 0.7,
                   xlab = "Q (m3/s)", ylab = "Conc", main = "Q-C")
    if (length(ok_obs) > 0)
      graphics::points(q[ok_obs], c_obs[ok_obs], pch = 16,
                       col = grDevices::rgb(.85, .2, .2, .6), cex = .8)

    # (4) 日通量
    graphics::plot(dt, flux, type = "h", lwd = 1.5, col = "#1b9e77",
                   xlab = "Date", ylab = "Flux (kg/d)", main = "Daily Flux")
    if (any(is.finite(flux)))
      graphics::abline(h = mean(flux, na.rm = TRUE), lty = 2, col = "#555")

  } else {
    # ---------- loadComp 子方法 ----------
    graphics::par(mfrow = c(2, 2), mar = c(4, 4, 3, 1))
    c_reg <- if ("C_reg" %in% names(d)) fw_as_num(d$C_reg)
    else rep(NA_real_, nrow(d))

    # (1) C_reg / C_est / C_obs
    ylim <- range(c(c_obs, c_reg, c_est), na.rm = TRUE, finite = TRUE)
    graphics::plot(dt, c_reg, type = "l", col = "#bbb", lwd = 1.5, ylim = ylim,
                   xlab = "Date", ylab = "Conc",
                   main = paste0("Conc (", fw_composite_sub_label(sub), ")"))
    graphics::lines(dt, c_est, col = "#2c7fb8", lwd = 1.3)
    ok_obs <- which(is.finite(c_obs))
    if (length(ok_obs) > 0)
      graphics::points(dt[ok_obs], c_obs[ok_obs], pch = 16,
                       col = grDevices::rgb(.85, .2, .2, .6), cex = .8)
    graphics::legend("topleft", c("C_reg", "C_composite", "C_obs"),
                     col = c("#bbb", "#2c7fb8", grDevices::rgb(.85, .2, .2)),
                     lty = c(1, 1, NA), pch = c(NA, NA, 16), bty = "n", cex = 0.8)

    # (2) 插值残差
    R_int <- res$diag$R_interp
    if (!is.null(R_int) && length(R_int) == nrow(d)) {
      graphics::plot(dt, R_int, type = "l", col = "#d95f02", lwd = 1.2,
                     xlab = "Date", ylab = "R(t)", main = "Interp Residuals")
      graphics::abline(h = 0, lty = 2, col = "#999")
    } else {
      graphics::plot.new()
      graphics::text(.5, .5, "Residual N/A\n(only for relative methods)")
    }

    # (3) Q-C scatter
    graphics::plot(q, c_est, pch = 1, col = "#2c7fb8", cex = .7,
                   xlab = "Q", ylab = "Conc", main = "Q-C Diagnostic")
    if (length(ok_obs) > 0)
      graphics::points(q[ok_obs], c_obs[ok_obs], pch = 16,
                       col = grDevices::rgb(.85, .2, .2, .6), cex = .8)

    # (4) 日通量
    graphics::plot(dt, flux, type = "h", lwd = 1.5, col = "#1b9e77",
                   xlab = "Date", ylab = "Flux (kg/d)", main = "Daily Flux")
    if (any(is.finite(flux)))
      graphics::abline(h = mean(flux, na.rm = TRUE), lty = 2, col = "#555")
  }

  invisible(NULL)
}
