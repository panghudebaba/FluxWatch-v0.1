# =====================================================================
# fct_flux_weighted.R
# 加权平均法 —— 核心计算
# =====================================================================

#' 加权平均法：流量加权浓度
#' @param d  data.frame(date, Q, C_obs, station, WYBM) 已排序
#' @param conv_factor 换算系数
#' @return list(method, method_label, daily, summary, diag, params)
fw_calc_weighted <- function(d, conv_factor = 86.4) {
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

  C_est    <- ifelse(is.finite(d$C_obs), d$C_obs, weighted_conc)
  C_source <- ifelse(is.finite(d$C_obs), "observed", "weighted")
  flux     <- ifelse(is.finite(d$Q) & is.finite(C_est), d$Q * C_est * conv_factor, NA_real_)

  daily <- data.frame(
    station = as.character(d$station), WYBM = as.character(d$WYBM),
    date = d$date, Q = d$Q, C_obs = d$C_obs, C_est = C_est,
    flux = flux, method = "weighted", C_source = C_source,
    stringsAsFactors = FALSE)

  list(method = "weighted", method_label = fw_flux_method_label("weighted"),
       daily = daily, summary = fw_make_flux_summary_table(daily),
       diag = list(weighted_conc = weighted_conc, n_obs = length(idx_obs)),
       params = list(conv_factor = conv_factor))
}
