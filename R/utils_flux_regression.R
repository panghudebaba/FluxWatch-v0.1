# =====================================================================
# utils_flux_regression.R
# 回归法 —— 数据准备 + 各子方法内核（rating / loadest / wrtds / loadflex）
# =====================================================================

if (!exists("%||%", mode = "function")) {
  `%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x
}

# =============================================================
# A. 数据准备
# =============================================================
fw_prepare_regression_input <- function(step1_data,
                                        qf_sheet = NULL,
                                        wq_sheet = NULL,
                                        constituent = "TN",
                                        date_range = NULL) {
  if (is.null(step1_data) || is.null(step1_data$QF) || is.null(step1_data$WQ))
    stop("第一步数据缺少 QF/WQ。")

  qf_list <- fw_as_named_table_list(step1_data$QF, "QF")
  wq_list <- fw_as_named_table_list(step1_data$WQ, "WQ")
  if (length(qf_list) == 0) stop("QF 数据为空。")
  if (length(wq_list) == 0) stop("WQ 数据为空。")

  if (is.null(qf_sheet) || !(qf_sheet %in% names(qf_list))) qf_sheet <- names(qf_list)[1]
  if (is.null(wq_sheet) || !(wq_sheet %in% names(wq_list))) wq_sheet <- names(wq_list)[1]

  qf_df <- as.data.frame(qf_list[[qf_sheet]], stringsAsFactors = FALSE)
  wq_df <- as.data.frame(wq_list[[wq_sheet]], stringsAsFactors = FALSE)

  q_cols <- fw_find_flux_cols(qf_df)
  w_cols <- fw_find_flux_cols(wq_df)

  q_date_col <- if ("TM" %in% names(qf_df)) "TM" else q_cols$dt
  q_flow_col <- if ("Q"  %in% names(qf_df)) "Q"  else q_cols$q
  w_date_col <- if ("TM" %in% names(wq_df)) "TM" else w_cols$dt

  if (is.null(q_date_col) || is.null(q_flow_col))
    stop("QF 数据中未识别到 TM/Q 列。")
  if (is.null(w_date_col))
    stop("WQ 数据中未识别到 TM 列。")

  if (!(constituent %in% names(wq_df))) {
    idx <- which(tolower(names(wq_df)) == tolower(constituent))
    if (length(idx) == 0) stop(paste0("WQ 数据中不存在指标列: ", constituent))
    constituent <- names(wq_df)[idx[1]]
  }

  q <- data.frame(TM = fw_as_date(qf_df[[q_date_col]]),
                  Q  = fw_as_num(qf_df[[q_flow_col]]), stringsAsFactors = FALSE)
  cc <- data.frame(TM   = fw_as_date(wq_df[[w_date_col]]),
                   conc = fw_as_num(wq_df[[constituent]]), stringsAsFactors = FALSE)

  q  <- q[!is.na(q$TM), , drop = FALSE]
  cc <- cc[!is.na(cc$TM), , drop = FALSE]

  if (!is.null(date_range) && length(date_range) == 2 && all(!is.na(date_range))) {
    s <- as.Date(date_range[1]); e <- as.Date(date_range[2])
    if (s > e) { tmp <- s; s <- e; e <- tmp }
    q  <- q[q$TM >= s & q$TM <= e, , drop = FALSE]
    cc <- cc[cc$TM >= s & cc$TM <= e, , drop = FALSE]
  }

  agg_mean <- function(x) if (all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE)
  if (nrow(q) > 0)  q  <- stats::aggregate(Q ~ TM, data = q, FUN = agg_mean)
  if (nrow(cc) > 0) cc <- stats::aggregate(conc ~ TM, data = cc, FUN = agg_mean)

  dat <- merge(q, cc, by = "TM", all.x = TRUE, sort = TRUE)
  dat$doy  <- as.integer(format(dat$TM, "%j"))
  dat$conc[is.nan(dat$conc)] <- NA_real_

  yr_num  <- as.numeric(format(dat$TM, "%Y"))
  is_leap <- (yr_num %% 4 == 0 & (yr_num %% 100 != 0 | yr_num %% 400 == 0))
  dat$dec_year <- yr_num + (dat$doy - 1) / ifelse(is_leap, 366, 365)
  dat$month    <- as.integer(format(dat$TM, "%m"))
  dat$year     <- as.integer(yr_num)
  dat$quarter  <- ceiling(dat$month / 3)

  dat_calib   <- dat[is.finite(dat$Q) & dat$Q > 0 &
                       is.finite(dat$conc) & dat$conc > 0, , drop = FALSE]
  newdata_day <- dat[is.finite(dat$Q) & dat$Q > 0, , drop = FALSE]

  if (nrow(dat_calib) < 3)
    stop(sprintf("有效校准数据仅 %d 行（需≥3 行同时有 Q>0 和浓度>0）。", nrow(dat_calib)))

  station <- fw_pick_first_text(qf_df, c("STNM_WQ", "STNM", "station", "site", "SITENAME"))
  if (is.na(station) || station == "")
    station <- fw_pick_first_text(wq_df, c("STNM_WQ", "STNM", "station", "site", "SITENAME"))
  if (is.na(station) || station == "") station <- qf_sheet

  wybm <- fw_pick_first_text(qf_df, c("WYBM_WQ", "WYBM", "wybm"))
  if (is.na(wybm) || wybm == "")
    wybm <- fw_pick_first_text(wq_df, c("WYBM_WQ", "WYBM", "wybm"))

  list(
    dat = dat, dat_calib = dat_calib, newdata_day = newdata_day,
    station = station,
    wybm = if (is.na(wybm) || wybm == "") NA_character_ else wybm,
    qf_sheet = qf_sheet, wq_sheet = wq_sheet, constituent = constituent)
}


# =============================================================
# B. RiverLoad Method 9: rating — 幂函数评级曲线
#    log10(C) = a + b*log10(Q)
# =============================================================
fw_reg_rating <- function(dat_calib, newdata_day) {
  fit <- stats::lm(log10(conc) ~ log10(Q), data = dat_calib)
  log10_C_hat <- stats::predict(fit, newdata = newdata_day)
  C_hat <- 10^log10_C_hat
  data.frame(TM = newdata_day$TM, C_est = C_hat,
             load_kgd = C_hat * newdata_day$Q * 86.4, stringsAsFactors = FALSE)
}


# =============================================================
# C. RiverLoad Method 10: rating.period — 分期评级曲线
# =============================================================
fw_reg_rating_period <- function(dat_calib, newdata_day, period_col = "month") {
  .ensure_period <- function(df) {
    if (!(period_col %in% names(df))) {
      df[[period_col]] <- switch(period_col,
                                 "month"   = as.integer(format(df$TM, "%m")),
                                 "year"    = as.integer(format(df$TM, "%Y")),
                                 "quarter" = ceiling(as.integer(format(df$TM, "%m")) / 3),
                                 as.integer(format(df$TM, "%m")))
    }
    df
  }
  dat_calib   <- .ensure_period(dat_calib)
  newdata_day <- .ensure_period(newdata_day)

  fit_global <- stats::lm(log10(conc) ~ log10(Q), data = dat_calib)

  result <- lapply(seq_len(nrow(newdata_day)), function(i) {
    p <- newdata_day[[period_col]][i]
    sub <- dat_calib[dat_calib[[period_col]] == p, , drop = FALSE]
    if (nrow(sub) < 3) {
      log10_c <- stats::predict(fit_global, newdata = newdata_day[i, , drop = FALSE])
    } else {
      fit_p <- tryCatch(stats::lm(log10(conc) ~ log10(Q), data = sub), error = function(e) NULL)
      log10_c <- if (is.null(fit_p))
        stats::predict(fit_global, newdata = newdata_day[i, , drop = FALSE])
      else
        stats::predict(fit_p, newdata = newdata_day[i, , drop = FALSE])
    }
    c_hat <- 10^log10_c
    data.frame(TM = newdata_day$TM[i], C_est = c_hat,
               load_kgd = c_hat * newdata_day$Q[i] * 86.4, stringsAsFactors = FALSE)
  })
  do.call(rbind, result)
}


# =============================================================
# D. RiverLoad Method 11: ferguson — Ferguson 修正
#    BCF = exp(2.651 * s²)
# =============================================================
fw_reg_ferguson <- function(dat_calib, newdata_day) {
  fit <- stats::lm(log10(conc) ~ log10(Q), data = dat_calib)
  s   <- summary(fit)$sigma
  bcf <- exp(2.651 * s^2)
  log10_C_hat <- stats::predict(fit, newdata = newdata_day)
  C_hat <- 10^log10_C_hat * bcf
  data.frame(TM = newdata_day$TM, C_est = C_hat,
             load_kgd = C_hat * newdata_day$Q * 86.4, stringsAsFactors = FALSE)
}


# =============================================================
# E. RiverLoad Method 12: ferguson.period — 分期 Ferguson
# =============================================================
fw_reg_ferguson_period <- function(dat_calib, newdata_day, period_col = "month") {
  .ensure_period <- function(df) {
    if (!(period_col %in% names(df))) {
      df[[period_col]] <- switch(period_col,
                                 "month"   = as.integer(format(df$TM, "%m")),
                                 "year"    = as.integer(format(df$TM, "%Y")),
                                 "quarter" = ceiling(as.integer(format(df$TM, "%m")) / 3),
                                 as.integer(format(df$TM, "%m")))
    }
    df
  }
  dat_calib   <- .ensure_period(dat_calib)
  newdata_day <- .ensure_period(newdata_day)

  fit_global <- stats::lm(log10(conc) ~ log10(Q), data = dat_calib)
  s_global   <- summary(fit_global)$sigma
  bcf_global <- exp(2.651 * s_global^2)

  result <- lapply(seq_len(nrow(newdata_day)), function(i) {
    p <- newdata_day[[period_col]][i]
    sub <- dat_calib[dat_calib[[period_col]] == p, , drop = FALSE]
    if (nrow(sub) < 3) {
      log10_c <- stats::predict(fit_global, newdata = newdata_day[i, , drop = FALSE])
      bcf <- bcf_global
    } else {
      fit_p <- tryCatch(stats::lm(log10(conc) ~ log10(Q), data = sub), error = function(e) NULL)
      if (is.null(fit_p)) {
        log10_c <- stats::predict(fit_global, newdata = newdata_day[i, , drop = FALSE])
        bcf <- bcf_global
      } else {
        s_p <- summary(fit_p)$sigma
        bcf <- exp(2.651 * s_p^2)
        log10_c <- stats::predict(fit_p, newdata = newdata_day[i, , drop = FALSE])
      }
    }
    c_hat <- 10^log10_c * bcf
    data.frame(TM = newdata_day$TM[i], C_est = c_hat,
               load_kgd = c_hat * newdata_day$Q[i] * 86.4, stringsAsFactors = FALSE)
  })
  do.call(rbind, result)
}


# =============================================================
# F. LOADEST 公式（Model 1–9）
# =============================================================
fw_reg_loadest_formula <- function(model_num = 9) {
  model_num <- as.integer(model_num)
  flist <- list(
    "1" = log(conc) ~ lnQ,
    "2" = log(conc) ~ lnQ + lnQ2,
    "3" = log(conc) ~ lnQ + dT,
    "4" = log(conc) ~ lnQ + sin2piT + cos2piT,
    "5" = log(conc) ~ lnQ + lnQ2 + dT,
    "6" = log(conc) ~ lnQ + lnQ2 + sin2piT + cos2piT,
    "7" = log(conc) ~ lnQ + sin2piT + cos2piT + dT,
    "8" = log(conc) ~ lnQ + lnQ2 + dT + dT2,
    "9" = log(conc) ~ lnQ + lnQ2 + sin2piT + cos2piT + dT + dT2)
  f <- flist[[as.character(model_num)]]
  if (is.null(f)) f <- flist[["9"]]
  f
}

fw_reg_loadest_add_vars <- function(df) {
  df$lnQ  <- log(df$Q)
  ctr_lnQ <- mean(df$lnQ, na.rm = TRUE)
  ctr_T   <- mean(df$dec_year, na.rm = TRUE)
  df$lnQ  <- df$lnQ - ctr_lnQ
  df$lnQ2 <- df$lnQ^2
  df$dT   <- df$dec_year - ctr_T
  df$dT2  <- df$dT^2
  df$sin2piT <- sin(2 * pi * df$dec_year)
  df$cos2piT <- cos(2 * pi * df$dec_year)
  attr(df, "ctr_lnQ") <- ctr_lnQ
  attr(df, "ctr_T")   <- ctr_T
  df
}


# =============================================================
# G. LOADEST（base R + Duan smearing）
# =============================================================
fw_reg_loadest <- function(dat_calib, newdata_day, model_num = 9) {
  calib <- fw_reg_loadest_add_vars(dat_calib)
  ctr_lnQ <- attr(calib, "ctr_lnQ")
  ctr_T   <- attr(calib, "ctr_T")

  nd <- newdata_day
  nd$lnQ     <- log(nd$Q) - ctr_lnQ
  nd$lnQ2    <- nd$lnQ^2
  nd$dT      <- nd$dec_year - ctr_T
  nd$dT2     <- nd$dT^2
  nd$sin2piT <- sin(2 * pi * nd$dec_year)
  nd$cos2piT <- cos(2 * pi * nd$dec_year)

  fml <- fw_reg_loadest_formula(model_num)
  fit <- tryCatch(stats::lm(fml, data = calib), error = function(e) NULL)
  if (is.null(fit)) stop(paste0("LOADEST Model ", model_num, " 拟合失败。"))

  resid_vals   <- stats::residuals(fit)
  smear_factor <- mean(exp(resid_vals), na.rm = TRUE)
  log_C_hat    <- stats::predict(fit, newdata = nd)
  C_hat        <- exp(log_C_hat) * smear_factor

  data.frame(TM = nd$TM, C_est = C_hat,
             load_kgd = C_hat * nd$Q * 86.4, stringsAsFactors = FALSE)
}


# =============================================================
# H. WRTDS tricube 权重
# =============================================================
fw_reg_tricube <- function(d, h) {
  u <- abs(d) / h
  ifelse(u >= 1, 0, (1 - u^3)^3)
}

fw_reg_wrtds_predict_one <- function(target_row, calib, h_T = 10, h_Q = 2, h_S = 0.5) {
  T0   <- target_row$dec_year
  lnQ0 <- log(target_row$Q)

  d_T <- abs(calib$dec_year - T0)
  d_Q <- abs(log(calib$Q) - lnQ0)
  d_S <- pmin(d_T %% 1, 1 - d_T %% 1)

  w <- fw_reg_tricube(d_T, h_T) * fw_reg_tricube(d_Q, h_Q) * fw_reg_tricube(d_S, h_S)

  if (sum(w > 0) < 5)
    w <- fw_reg_tricube(d_T, h_T * 2) * fw_reg_tricube(d_Q, h_Q * 2) * fw_reg_tricube(d_S, h_S * 2)
  if (sum(w > 0) < 3) return(list(C_est = NA_real_, se = NA_real_))

  local_df <- data.frame(
    lnC  = log(calib$conc),
    TT   = calib$dec_year,
    lnQ  = log(calib$Q),
    sinT = sin(2 * pi * calib$dec_year),
    cosT = cos(2 * pi * calib$dec_year),
    stringsAsFactors = FALSE)

  fit <- tryCatch(
    stats::lm(lnC ~ TT + lnQ + sinT + cosT, data = local_df, weights = w),
    error = function(e) NULL)
  if (is.null(fit)) return(list(C_est = NA_real_, se = NA_real_))

  nd <- data.frame(TT = T0, lnQ = lnQ0,
                   sinT = sin(2 * pi * T0), cosT = cos(2 * pi * T0), stringsAsFactors = FALSE)

  y_hat <- tryCatch(stats::predict(fit, newdata = nd), error = function(e) NA_real_)
  se    <- tryCatch(summary(fit)$sigma, error = function(e) NA_real_)
  bcf   <- if (is.finite(se)) exp(se^2 / 2) else 1
  list(C_est = exp(y_hat) * bcf, se = se)
}

fw_reg_wrtds <- function(dat_calib, newdata_day, h_T = 10, h_Q = 2, h_S = 0.5) {
  n <- nrow(newdata_day)
  C_est <- numeric(n)
  for (i in seq_len(n)) {
    res <- fw_reg_wrtds_predict_one(newdata_day[i, , drop = FALSE], dat_calib,
                                    h_T = h_T, h_Q = h_Q, h_S = h_S)
    C_est[i] <- res$C_est
  }
  data.frame(TM = newdata_day$TM, C_est = C_est,
             load_kgd = C_est * newdata_day$Q * 86.4, stringsAsFactors = FALSE)
}


# =============================================================
# I. loadflex 包装（尝试调用 loadflex 包）
# =============================================================
fw_reg_loadflex <- function(dat_calib, newdata_day, model_choice = "loadLm_season") {
  if (!requireNamespace("loadflex", quietly = TRUE)) return(NULL)

  tryCatch({
    md <- loadflex::metadata(
      constituent = "conc", flow = "Q", dates = "TM",
      conc.units = "mg L^-1", flow.units = "m^3 s^-1",
      load.units = "kg", load.rate.units = "kg d^-1")

    fml <- if (model_choice == "loadLm_simple") {
      stats::as.formula(log(conc) ~ log(Q))
    } else {
      stats::as.formula(log(conc) ~ log(Q) + sin(2 * pi * doy / 365) + cos(2 * pi * doy / 365))
    }

    m <- loadflex::loadLm(formula = fml, pred.format = "conc", data = dat_calib,
                          metadata = md, ylog = TRUE, retrans.function = exp)

    pr <- loadflex::predictSolute(load.model = m, flux.or.conc = "flux",
                                  newdata = newdata_day, agg.by = "unit", date = TRUE, na.rm = FALSE)
    pr <- as.data.frame(pr, stringsAsFactors = FALSE)
    if (nrow(pr) == 0) return(NULL)

    date_col <- names(pr)[1]
    tm <- fw_as_date(pr[[date_col]])
    if (all(is.na(tm)) && !is.null(rownames(pr))) tm <- fw_as_date(rownames(pr))
    num_cols <- names(pr)[vapply(pr, is.numeric, logical(1))]
    if (length(num_cols) == 0) return(NULL)

    load_vals <- fw_as_num(pr[[num_cols[1]]])
    C_est <- ifelse(is.finite(newdata_day$Q) & newdata_day$Q > 0,
                    load_vals / (newdata_day$Q * 86.4), NA_real_)

    data.frame(TM = tm, C_est = C_est, load_kgd = load_vals, stringsAsFactors = FALSE)
  }, error = function(e) NULL)
}


# =============================================================
# J. loadflex base lm 兜底（不依赖 loadflex 包）
# =============================================================
fw_reg_loadflex_base <- function(dat_calib, newdata_day, model_choice = "loadLm_season") {
  fml <- if (model_choice == "loadLm_simple") {
    log(conc) ~ log(Q)
  } else {
    log(conc) ~ log(Q) + sin(2 * pi * doy / 365) + cos(2 * pi * doy / 365)
  }

  fit <- tryCatch(stats::lm(fml, data = dat_calib), error = function(e) NULL)
  if (is.null(fit)) return(NULL)

  resid_vals   <- stats::residuals(fit)
  smear_factor <- mean(exp(resid_vals), na.rm = TRUE)
  log_c_hat    <- tryCatch(stats::predict(fit, newdata = newdata_day),
                           error = function(e) rep(NA_real_, nrow(newdata_day)))
  c_hat <- exp(log_c_hat) * smear_factor

  data.frame(TM = fw_as_date(newdata_day$TM), C_est = c_hat,
             load_kgd = fw_as_num(newdata_day$Q) * c_hat * 86.4, stringsAsFactors = FALSE)
}
