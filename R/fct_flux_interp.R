# =====================================================================
# fct_flux_interp.R
# 插值法 —— 核心计算（线性插值）
# =====================================================================

#' 线性插值浓度（内部辅助）
fw_interp_conc_linear <- function(date, conc) {
  x <- as.numeric(fw_as_date(date))
  y <- fw_as_num(conc)
  ok <- is.finite(x) & is.finite(y)
  if (sum(ok) == 0) return(rep(NA_real_, length(y)))
  if (sum(ok) == 1) return(rep(y[ok][1], length(y)))
  xy <- data.frame(x = x[ok], y = y[ok], stringsAsFactors = FALSE)
  xy <- stats::aggregate(y ~ x, data = xy, FUN = mean)
  stats::approx(x = xy$x, y = xy$y, xout = x, method = "linear", rule = 2, ties = mean)$y
}

#' 插值法：线性插值浓度
#' @param d  data.frame(date, Q, C_obs, station, WYBM)
#' @param conv_factor 换算系数
#' @return list(method, method_label, daily, summary, diag, params)
fw_calc_interp <- function(d, conv_factor = 86.4) {
  d$date  <- fw_as_date(d$date)
  d$Q     <- fw_as_num(d$Q)
  d$C_obs <- fw_as_num(d$C_obs)
  if (!("station" %in% names(d))) d$station <- "ALL"
  if (!("WYBM"    %in% names(d))) d$WYBM    <- NA_character_
  d <- d[!is.na(d$date), , drop = FALSE]
  d <- d[order(d$date), , drop = FALSE]
  if (nrow(d) == 0) stop("\u65e0\u53ef\u7528\u65e5\u6570\u636e\u3002")

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

  c_interp <- fw_interp_conc_linear(d$date, d$C_obs)

  C_est    <- ifelse(is.finite(d$C_obs), d$C_obs, c_interp)
  C_source <- ifelse(is.finite(d$C_obs), "observed", "interp")
  miss <- !is.finite(C_est)
  C_est[miss] <- weighted_conc
  C_source[miss] <- "weighted"

  flux <- ifelse(is.finite(d$Q) & is.finite(C_est), d$Q * C_est * conv_factor, NA_real_)

  daily <- data.frame(
    station = as.character(d$station), WYBM = as.character(d$WYBM),
    date = d$date, Q = d$Q, C_obs = d$C_obs, C_est = C_est,
    flux = flux, method = "interp", C_source = C_source,
    stringsAsFactors = FALSE)

  list(method = "interp", method_label = fw_flux_method_label("interp"),
       daily = daily, summary = fw_make_flux_summary_table(daily),
       diag = list(weighted_conc = weighted_conc, n_obs = length(idx_obs)),
       params = list(conv_factor = conv_factor))
}


fw_interp_left_extra_ui <- function(ns, key = "interp") {
  sub_ch <- fw_interp_sub_labels()
  shiny::tagList(
    shiny::helpText("\u63d2\u503c\u6cd5\u56fa\u5b9a\u8c03\u7528\u7b2c\u4e00\u6b65\u5904\u7406\u540e\u7684 QF/WQ \u6570\u636e"),
    shiny::selectInput(ns(paste0("qf_sheet_", key)), "QF\u6570\u636e\u8868", choices = NULL),
    shiny::selectInput(ns(paste0("wq_sheet_", key)), "WQ\u6570\u636e\u8868", choices = NULL),
    shiny::selectInput(ns(paste0("constituent_", key)), "\u6c34\u8d28\u6307\u6807(j)", choices = NULL),
    shiny::selectInput(
      ns(paste0("interp_sub_", key)),
      "\u63d2\u503c\u5b50\u65b9\u6cd5",
      choices  = sub_ch,
      selected = sub_ch[1]
    )
  )
}
