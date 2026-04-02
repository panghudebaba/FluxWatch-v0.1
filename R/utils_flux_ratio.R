# =====================================================================
# utils_flux_ratio.R
# 比率估计 —— 辅助工具函数（数据准备、Method 7/8 估算、RiverLoad 调用）
# =====================================================================

if (!exists("%||%", mode = "function")) {
  `%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x
}

# ---------- 数据准备 ----------

fw_prepare_ratio_input <- function(step1_data,
                                   qf_sheet = NULL,
                                   wq_sheet = NULL,
                                   constituent = "TN",
                                   date_range = NULL) {

  if (is.null(step1_data))
    stop("step1_data \u4e3a\u7a7a\u3002")

  qf_list <- fw_as_named_table_list(step1_data$QF, "QF")
  wq_list <- fw_as_named_table_list(step1_data$WQ, "WQ")
  if (length(qf_list) == 0 || length(wq_list) == 0)
    stop("QF \u6216 WQ \u6570\u636e\u4e3a\u7a7a\u3002")

  if (is.null(qf_sheet) || !(qf_sheet %in% names(qf_list))) qf_sheet <- names(qf_list)[1]
  if (is.null(wq_sheet) || !(wq_sheet %in% names(wq_list))) wq_sheet <- names(wq_list)[1]

  qf_df <- as.data.frame(qf_list[[qf_sheet]], stringsAsFactors = FALSE)
  wq_df <- as.data.frame(wq_list[[wq_sheet]], stringsAsFactors = FALSE)

  # ---------- 流量表 ----------
  qf_cols <- fw_find_qf_cols(qf_df)
  if (is.null(qf_cols$dt) || is.null(qf_cols$q))
    stop(paste0("QF \u8868\u4e2d\u672a\u8bc6\u522b\u5230\u65e5\u671f/\u6d41\u91cf\u5217\u3002",
                "\n\u5f53\u524d\u5217\u540d: ", paste(names(qf_df), collapse = ", "),
                "\n\u8bc6\u522b\u7ed3\u679c: dt=", qf_cols$dt %||% "NULL",
                ", q=", qf_cols$q %||% "NULL"))

  qf_clean <- data.frame(
    TM = fw_as_date(qf_df[[qf_cols$dt]]),
    Q  = fw_as_num(qf_df[[qf_cols$q]]),
    stringsAsFactors = FALSE
  )
  qf_clean <- qf_clean[!is.na(qf_clean$TM), , drop = FALSE]
  if (nrow(qf_clean) == 0)
    stop("QF \u8868\u65e5\u671f\u5217\u89e3\u6790\u540e\u65e0\u6709\u6548\u8bb0\u5f55\u3002")

  qf_daily <- stats::aggregate(Q ~ TM, data = qf_clean,
                               FUN = function(x) mean(x, na.rm = TRUE))
  qf_daily <- qf_daily[order(qf_daily$TM), , drop = FALSE]

  # ---------- 水质表 ----------
  wq_dt_col <- fw_find_wq_dt_col(wq_df)
  if (is.null(wq_dt_col))
    stop(paste0("WQ \u8868\u4e2d\u672a\u8bc6\u522b\u5230\u65e5\u671f\u5217\u3002",
                "\n\u5f53\u524d\u5217\u540d: ", paste(names(wq_df), collapse = ", ")))
  if (!(constituent %in% names(wq_df)))
    stop(paste0("WQ \u8868\u4e2d\u4e0d\u5b58\u5728\u6307\u6807\u5217: ", constituent,
                "\n\u53ef\u7528\u5217: ", paste(names(wq_df), collapse = ", ")))

  wq_clean <- data.frame(
    TM   = fw_as_date(wq_df[[wq_dt_col]]),
    conc = fw_as_num(wq_df[[constituent]]),
    stringsAsFactors = FALSE
  )
  wq_clean <- wq_clean[!is.na(wq_clean$TM), , drop = FALSE]

  wq_daily <- stats::aggregate(conc ~ TM, data = wq_clean,
                               FUN = function(x) mean(x, na.rm = TRUE))

  # ---------- 合并 ----------
  dat <- merge(qf_daily, wq_daily, by = "TM", all.x = TRUE, sort = TRUE)
  dat <- dat[order(dat$TM), , drop = FALSE]
  rownames(dat) <- NULL

  if (!is.null(date_range) && length(date_range) == 2 && all(!is.na(date_range))) {
    s <- as.Date(date_range[1]); e <- as.Date(date_range[2])
    if (s > e) { tmp <- s; s <- e; e <- tmp }
    dat <- dat[dat$TM >= s & dat$TM <= e, , drop = FALSE]
  }
  if (nrow(dat) == 0) stop("\u65f6\u95f4\u8303\u56f4\u5185\u65e0\u53ef\u7528\u6570\u636e\u3002")

  # RiverLoad 格式
  dat_rl <- data.frame(
    datetime = as.POSIXct(dat$TM, tz = "Asia/Shanghai"),
    flow     = dat$Q,
    conc     = dat$conc,
    stringsAsFactors = FALSE
  )
  names(dat_rl)[3] <- constituent
  dat_rl <- dat_rl[order(dat_rl$datetime), , drop = FALSE]
  dat_rl <- dat_rl[!duplicated(dat_rl$datetime), , drop = FALSE]

  # 站点 / WYBM
  station <- NA_character_; wybm <- NA_character_
  if (!is.null(qf_cols$station) && qf_cols$station %in% names(qf_df))
    station <- as.character(stats::na.omit(unique(qf_df[[qf_cols$station]])))[1]
  if (is.na(station)) station <- qf_sheet
  if (!is.null(qf_cols$wybm) && qf_cols$wybm %in% names(qf_df))
    wybm <- as.character(stats::na.omit(unique(qf_df[[qf_cols$wybm]])))[1]

  list(dat = dat, dat_rl = dat_rl, station = station, wybm = wybm,
       qf_sheet = qf_sheet, wq_sheet = wq_sheet, constituent = constituent)
}


# ========================================================================
# Beale 比率估计核心函数
# ========================================================================

#' 计算 Beale 比率估计的总负荷（对一组数据）
#' @param q_sample   采样日流量向量
#' @param c_sample   采样日浓度向量
#' @param Q_total    目标时段总流量（全部日流量之和，m3/s·day）
#' @param conv_factor 换算系数 (默认 86.4)
#' @return list(L_total = 总负荷kg, beale_ratio = 修正因子, q_bar, l_bar, cov_lq, var_q, n)
fw_beale_ratio_calc <- function(q_sample, c_sample, Q_total, conv_factor = 86.4) {
  ok <- which(is.finite(q_sample) & q_sample > 0 & is.finite(c_sample))
  n <- length(ok)
  if (n < 2) {
    # 退化：直接用 mean(C) × Q_total
    c_mean <- if (n >= 1) mean(c_sample[ok], na.rm = TRUE) else NA_real_
    L_total <- Q_total * c_mean * conv_factor
    return(list(L_total = L_total, beale_ratio = 1.0,
                q_bar = mean(q_sample[ok], na.rm = TRUE),
                l_bar = NA_real_, cov_lq = NA_real_, var_q = NA_real_, n = n))
  }

  qi <- q_sample[ok]
  ci <- c_sample[ok]
  li <- ci * qi * conv_factor   # 采样日日负荷 (kg/d)

  q_bar <- mean(qi)
  l_bar <- mean(li)

  # 协方差与方差（样本）
  cov_lq <- stats::cov(li, qi)
  var_q  <- stats::var(qi)

  # Beale 修正因子
  numerator   <- 1 + (1 / n) * (cov_lq / (l_bar * q_bar))
  denominator <- 1 + (1 / n) * (var_q / (q_bar^2))

  # 防止除零/极端值
  if (!is.finite(numerator) || !is.finite(denominator) || denominator == 0) {
    beale_ratio <- 1.0
  } else {
    beale_ratio <- numerator / denominator
  }

  # L = (Q_total / q_bar) * l_bar * beale_ratio
  # 注意 Q_total 是全部日流量之和(m3/s·day)，q_bar 是采样日平均流量(m3/s)
  # (Q_total / q_bar) 相当于"等效天数缩放因子"
  L_total <- (Q_total / q_bar) * l_bar * beale_ratio

  list(L_total = L_total, beale_ratio = beale_ratio,
       q_bar = q_bar, l_bar = l_bar, cov_lq = cov_lq, var_q = var_q, n = n)
}


# ========================================================================
# Method 7: Beale ratio — 全期一次性计算
# ========================================================================

#' Method 7: Beale ratio 逐日估算
#' @param dat data.frame(TM, Q, conc)
#' @param conv_factor 换算系数
#' @return list(C_est, flux, beale_info)
fw_ratio_method7_daily <- function(dat, conv_factor = 86.4) {
  n <- nrow(dat)
  obs_idx <- which(!is.na(dat$conc) & is.finite(dat$Q) & dat$Q > 0)

  if (length(obs_idx) < 1)
    return(list(C_est = rep(NA_real_, n), flux = rep(NA_real_, n),
                beale_info = list()))

  Q_total <- sum(dat$Q, na.rm = TRUE)   # m3/s·day（对应全期）

  beale <- fw_beale_ratio_calc(
    q_sample    = dat$Q[obs_idx],
    c_sample    = dat$conc[obs_idx],
    Q_total     = Q_total,
    conv_factor = conv_factor
  )

  L_total <- beale$L_total
  if (!is.finite(L_total) || L_total <= 0) {
    # 退化
    c_mean <- mean(dat$conc[obs_idx], na.rm = TRUE)
    flux_vec <- dat$Q * c_mean * conv_factor
    c_est    <- rep(c_mean, n)
    c_est[obs_idx] <- dat$conc[obs_idx]
    return(list(C_est = c_est, flux = flux_vec,
                beale_info = c(beale, list(fallback = TRUE))))
  }

  # 按日流量比例分配总负荷
  q_sum <- sum(dat$Q, na.rm = TRUE)
  if (q_sum > 0) {
    flux_vec <- (dat$Q / q_sum) * L_total
  } else {
    flux_vec <- rep(L_total / n, n)
  }

  # 反推 C_est
  c_est <- ifelse(is.finite(dat$Q) & dat$Q > 0,
                  flux_vec / (dat$Q * conv_factor), NA_real_)
  # 有实测日保留实测
  c_est[obs_idx] <- dat$conc[obs_idx]

  list(C_est = c_est, flux = flux_vec,
       beale_info = c(beale, list(fallback = FALSE)))
}


# ========================================================================
# Method 8: Beale ratio by period — 按月分期计算后汇总
# ========================================================================

#' Method 8: Beale ratio by period 逐日估算
#' @param dat data.frame(TM, Q, conc)
#' @param conv_factor 换算系数
#' @param period "month" 或 "year"
#' @return list(C_est, flux, period_info)
fw_ratio_method8_daily <- function(dat, conv_factor = 86.4, period = "month") {
  n <- nrow(dat)
  obs_idx <- which(!is.na(dat$conc) & is.finite(dat$Q) & dat$Q > 0)

  if (length(obs_idx) < 1)
    return(list(C_est = rep(NA_real_, n), flux = rep(NA_real_, n),
                period_info = list()))

  # 分期标签
  if (identical(period, "year")) {
    dat$period_key <- format(dat$TM, "%Y")
  } else {
    dat$period_key <- format(dat$TM, "%Y-%m")
  }

  # 全局 fallback 参数
  global_beale <- fw_beale_ratio_calc(
    q_sample    = dat$Q[obs_idx],
    c_sample    = dat$conc[obs_idx],
    Q_total     = sum(dat$Q, na.rm = TRUE),
    conv_factor = conv_factor
  )

  flux_vec <- rep(NA_real_, n)
  c_est    <- rep(NA_real_, n)
  period_details <- list()

  for (pk in unique(dat$period_key)) {
    idx_p <- which(dat$period_key == pk)
    obs_p <- intersect(idx_p, obs_idx)

    Q_total_p <- sum(dat$Q[idx_p], na.rm = TRUE)

    if (length(obs_p) >= 2) {
      # 本期有足够样本，用 Beale ratio
      beale_p <- fw_beale_ratio_calc(
        q_sample    = dat$Q[obs_p],
        c_sample    = dat$conc[obs_p],
        Q_total     = Q_total_p,
        conv_factor = conv_factor
      )
    } else if (length(obs_p) == 1) {
      # 仅1个样本，用简单比率
      beale_p <- fw_beale_ratio_calc(
        q_sample    = dat$Q[obs_p],
        c_sample    = dat$conc[obs_p],
        Q_total     = Q_total_p,
        conv_factor = conv_factor
      )
    } else {
      # 本期无样本，用全局参数
      # 按流量比例从全局分配
      q_ratio <- if (is.finite(global_beale$L_total) && sum(dat$Q, na.rm = TRUE) > 0) {
        Q_total_p / sum(dat$Q, na.rm = TRUE)
      } else 0
      L_period <- global_beale$L_total * q_ratio
      beale_p <- list(L_total = L_period, beale_ratio = global_beale$beale_ratio,
                      q_bar = global_beale$q_bar, l_bar = global_beale$l_bar,
                      n = 0, fallback = TRUE)
    }

    L_p <- beale_p$L_total
    if (!is.finite(L_p)) L_p <- 0

    # 本期日流量分配
    q_sum_p <- sum(dat$Q[idx_p], na.rm = TRUE)
    if (q_sum_p > 0 && is.finite(L_p)) {
      flux_vec[idx_p] <- (dat$Q[idx_p] / q_sum_p) * L_p
    } else {
      flux_vec[idx_p] <- if (length(idx_p) > 0 && is.finite(L_p)) L_p / length(idx_p) else 0
    }

    # 反推 C_est
    for (j in idx_p) {
      c_est[j] <- if (is.finite(dat$Q[j]) && dat$Q[j] > 0 && is.finite(flux_vec[j])) {
        flux_vec[j] / (dat$Q[j] * conv_factor)
      } else NA_real_
    }

    period_details[[pk]] <- list(
      period = pk, n_obs = length(obs_p), n_days = length(idx_p),
      Q_total = Q_total_p, L_total = round(L_p, 4),
      beale_ratio = round(beale_p$beale_ratio %||% NA_real_, 6)
    )
  }

  # 有实测日保留实测
  c_est[obs_idx] <- dat$conc[obs_idx]

  list(C_est = c_est, flux = flux_vec, period_info = period_details)
}


# ========================================================================
# RiverLoad method7/method8 总负荷 + method1-8 对比
# ========================================================================

#' 调用 RiverLoad method1-8 获取总负荷 (g)
# ========================================================================
# RiverLoad rating / rating.period 总负荷 + method1-6 对比
# ========================================================================

#' 调用 RiverLoad::rating 获取 Beale ratio 总负荷 (g)
#' @param dat_rl RiverLoad 格式 data.frame
#' @param ncomp  组分编号
#' @param period 时段 ("year", "month" 等)
#' @return numeric 总负荷 (g)，或 NA
fw_ratio_riverload_rating <- function(dat_rl, ncomp = 1, period = "year") {
  if (!requireNamespace("RiverLoad", quietly = TRUE)) return(NA_real_)
  tryCatch({
    out <- RiverLoad::rating(dat_rl, ncomp = ncomp, period = period)
    if (is.matrix(out) || is.data.frame(out)) as.numeric(out[1, 1])
    else if (is.numeric(out)) as.numeric(out[1])
    else NA_real_
  }, error = function(e) NA_real_)
}

#' 调用 RiverLoad::rating.period 获取 Beale ratio by period 总负荷 (g)
#' @param dat_rl RiverLoad 格式 data.frame
#' @param ncomp  组分编号
#' @param period 分期粒度 ("month", "year" 等)
#' @return numeric 总负荷 (g)，或 NA
fw_ratio_riverload_rating_period <- function(dat_rl, ncomp = 1, period = "month") {
  if (!requireNamespace("RiverLoad", quietly = TRUE)) return(NA_real_)
  tryCatch({
    out <- RiverLoad::rating.period(dat_rl, ncomp = ncomp, period = period)
    # rating.period 可能返回分期表或汇总值
    if (is.matrix(out) || is.data.frame(out)) {
      # 如果是分期表，取最后一行（汇总行）或求和
      if (nrow(out) > 1) {
        # 尝试对所有行求和（每行是一个分期）
        col_idx <- if (ncol(out) >= ncomp) ncomp else 1
        return(sum(as.numeric(out[, col_idx]), na.rm = TRUE))
      } else {
        return(as.numeric(out[1, 1]))
      }
    }
    else if (is.numeric(out)) sum(as.numeric(out), na.rm = TRUE)
    else NA_real_
  }, error = function(e) NA_real_)
}

#' 调用 RiverLoad method1-6 + rating + rating.period 获取总负荷 (g) 用于对比
#' @param dat_rl RiverLoad 格式 data.frame
#' @param ncomp  组分编号
#' @param period 分期粒度 (用于 rating / rating.period)
#' @return named numeric vector (g)
fw_ratio_riverload_compare <- function(dat_rl, ncomp = 1, period = "month") {
  if (!requireNamespace("RiverLoad", quietly = TRUE)) {
    return(c(method1 = NA_real_, method2 = NA_real_, method3 = NA_real_,
             method4 = NA_real_, method5 = NA_real_, method6 = NA_real_,
             rating = NA_real_, rating.period = NA_real_))
  }

  # method1-6：标准平均/插值方法
  avg_fns <- list(
    method1 = RiverLoad::method1,
    method2 = RiverLoad::method2,
    method3 = RiverLoad::method3,
    method4 = RiverLoad::method4,
    method5 = RiverLoad::method5,
    method6 = RiverLoad::method6
  )

  avg_results <- sapply(names(avg_fns), function(nm) {
    tryCatch({
      out <- avg_fns[[nm]](dat_rl, ncomp = ncomp)
      if (is.matrix(out) || is.data.frame(out)) as.numeric(out[1, 1])
      else if (is.numeric(out)) as.numeric(out[1])
      else NA_real_
    }, error = function(e) NA_real_)
  })

  # rating (Beale ratio, Method 7)
  rating_val <- tryCatch({
    out <- RiverLoad::rating(dat_rl, ncomp = ncomp, period = period)
    if (is.matrix(out) || is.data.frame(out)) as.numeric(out[1, 1])
    else if (is.numeric(out)) as.numeric(out[1])
    else NA_real_
  }, error = function(e) NA_real_)

  # rating.period (Beale ratio by period, Method 8)
  rating_period_val <- tryCatch({
    out <- RiverLoad::rating.period(dat_rl, ncomp = ncomp, period = period)
    if (is.matrix(out) || is.data.frame(out)) {
      col_idx <- if (ncol(out) >= ncomp) ncomp else 1
      sum(as.numeric(out[, col_idx]), na.rm = TRUE)
    } else if (is.numeric(out)) {
      sum(as.numeric(out), na.rm = TRUE)
    } else NA_real_
  }, error = function(e) NA_real_)

  c(avg_results,
    rating         = rating_val,
    rating.period  = rating_period_val)
}

