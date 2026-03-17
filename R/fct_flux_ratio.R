# =====================================================================
# fct_flux_ratio.R
# 比率法 —— 核心计算
# =====================================================================

#' 比率法：C/Q 比率估计
#' @param d  data.frame(date, Q, C_obs, station, WYBM)
#' @param conv_factor 换算系数
#' @return list(method, method_label, daily, summary, diag, params)
fw_calc_ratio <- function(d, conv_factor = 86.4) {
  d$date  <- fw_as_date(d$date)
  d$Q     <- fw_as_num(d$Q)
  d$C_obs <- fw_as_num(d$C_obs)
  if (!("station" %in% names(d))) d$station <- "ALL"
  if (!("WYBM"    %in% names(d))) d$WYBM    <- NA_character_
  d <- d[!is.na(d$date), , drop = FALSE]
  d <- d[order(d$date), , drop = FALSE]
  if (nrow(d) == 0) stop("无可用日数据。")

  # 流量加权浓度作兜底
  idx_obs <- which(is.finite(d$C_obs))
  weighted_conc <- NA_real_
  if (length(idx_obs) > 0) {
    w <- fw_as_num(d$Q[idx_obs])
    if (sum(w, na.rm = TRUE) > 0) {
      weighted_conc <- suppressWarnings(stats::weighted.mean(d$C_obs[idx_obs], w = w, na.rm = TRUE))
    } else {
      weighted_conc <- mean(d$C_obs[idx_obs], na.rm = TRUE)
    }
  }

  ok_ratio <- is.finite(d$C_obs) & is.finite(d$Q) & d$Q > 0
  ratio <- if (any(ok_ratio)) mean(d$C_obs[ok_ratio] / d$Q[ok_ratio], na.rm = TRUE) else NA_real_
  c_ratio <- ifelse(is.finite(d$Q), ratio * d$Q, NA_real_)

  C_est    <- ifelse(is.finite(d$C_obs), d$C_obs, c_ratio)
  C_source <- ifelse(is.finite(d$C_obs), "observed", "ratio")
  miss <- !is.finite(C_est)
  C_est[miss] <- weighted_conc
  C_source[miss] <- "weighted"

  flux <- ifelse(is.finite(d$Q) & is.finite(C_est), d$Q * C_est * conv_factor, NA_real_)

  daily <- data.frame(
    station = as.character(d$station), WYBM = as.character(d$WYBM),
    date = d$date, Q = d$Q, C_obs = d$C_obs, C_est = C_est,
    flux = flux, method = "ratio", C_source = C_source,
    stringsAsFactors = FALSE)

  list(method = "ratio", method_label = fw_flux_method_label("ratio"),
       daily = daily, summary = fw_make_flux_summary_table(daily),
       diag = list(weighted_conc = weighted_conc, n_obs = length(idx_obs), ratio = ratio),
       params = list(conv_factor = conv_factor))
}

#' 简单复合法（fallback）：优先实测 > 插值 > 内部回归 > 加权
#' 当 mod_flux.R 中 composite 页未使用 loadflex/EGRET 时的兜底
fw_calc_composite_simple <- function(d, conv_factor = 86.4) {
  d$date  <- fw_as_date(d$date)
  d$Q     <- fw_as_num(d$Q)
  d$C_obs <- fw_as_num(d$C_obs)
  if (!("station" %in% names(d))) d$station <- "ALL"
  if (!("WYBM"    %in% names(d))) d$WYBM    <- NA_character_
  d <- d[!is.na(d$date), , drop = FALSE]
  d <- d[order(d$date), , drop = FALSE]
  if (nrow(d) == 0) stop("无可用日数据。")

  idx_obs <- which(is.finite(d$C_obs))
  weighted_conc <- NA_real_
  if (length(idx_obs) > 0) {
    w <- fw_as_num(d$Q[idx_obs])
    if (sum(w, na.rm = TRUE) > 0) {
      weighted_conc <- suppressWarnings(stats::weighted.mean(d$C_obs[idx_obs], w = w, na.rm = TRUE))
    } else {
      weighted_conc <- mean(d$C_obs[idx_obs], na.rm = TRUE)
    }
  }

  # 插值
  c_interp <- fw_interp_conc_linear(d$date, d$C_obs)

  # 内部简单回归
  c_reg <- fw_regress_conc_simple(d$Q, d$C_obs, d$date)

  # 优先级合成
  C_est    <- d$C_obs
  C_source <- ifelse(is.finite(d$C_obs), "observed", NA_character_)

  miss <- !is.finite(C_est) & is.finite(c_interp)
  C_est[miss] <- c_interp[miss]; C_source[miss] <- "interp"

  miss <- !is.finite(C_est) & is.finite(c_reg)
  C_est[miss] <- c_reg[miss]; C_source[miss] <- "regression"

  miss <- !is.finite(C_est)
  C_est[miss] <- weighted_conc; C_source[miss] <- "weighted"

  flux <- ifelse(is.finite(d$Q) & is.finite(C_est), d$Q * C_est * conv_factor, NA_real_)

  daily <- data.frame(
    station = as.character(d$station), WYBM = as.character(d$WYBM),
    date = d$date, Q = d$Q, C_obs = d$C_obs, C_est = C_est,
    flux = flux, method = "composite", C_source = C_source,
    stringsAsFactors = FALSE)

  list(method = "composite", method_label = fw_flux_method_label("composite"),
       daily = daily, summary = fw_make_flux_summary_table(daily),
       diag = list(weighted_conc = weighted_conc, n_obs = length(idx_obs)),
       params = list(conv_factor = conv_factor))
}

#' 内部简单回归浓度预测（供 fw_calc_composite_simple 使用）
fw_regress_conc_simple <- function(Q, conc, date) {
  Q    <- fw_as_num(Q)
  conc <- fw_as_num(conc)
  doy  <- as.integer(format(fw_as_date(date), "%j"))
  ok <- is.finite(Q) & Q > 0 & is.finite(conc) & conc > 0 & is.finite(doy)
  if (sum(ok) < 5) return(rep(NA_real_, length(Q)))
  train <- data.frame(Q = Q[ok], conc = conc[ok], doy = doy[ok], stringsAsFactors = FALSE)
  newd  <- data.frame(Q = pmax(Q, .Machine$double.eps), doy = doy, stringsAsFactors = FALSE)
  fit <- tryCatch(
    stats::lm(log(conc) ~ log(Q) + sin(2 * pi * doy / 365) + cos(2 * pi * doy / 365), data = train),
    error = function(e) NULL)
  if (is.null(fit)) {
    fit <- tryCatch(stats::lm(log(conc) ~ log(Q), data = train), error = function(e) NULL)
  }
  if (is.null(fit)) return(rep(NA_real_, length(Q)))
  fw_as_num(suppressWarnings(exp(stats::predict(fit, newdata = newd))))
}
