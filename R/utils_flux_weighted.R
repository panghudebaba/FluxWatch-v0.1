# =====================================================================
# utils_flux_weighted.R
# 加权平均法 —— 辅助工具函数（数据准备、Method 1-5 逐日估算、RiverLoad 调用）
# =====================================================================

if (!exists("%||%", mode = "function")) {
  `%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x
}

# ---------- 数据准备 ----------

fw_prepare_weighted_input <- function(step1_data,
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

  # ---------- 流量表：使用专用 QF 列识别 ----------
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
    stop(paste0("QF \u8868\u65e5\u671f\u5217 '", qf_cols$dt, "' \u89e3\u6790\u540e\u65e0\u6709\u6548\u8bb0\u5f55\u3002"))

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

  dat_rl <- data.frame(
    datetime = as.POSIXct(dat$TM, tz = "Asia/Shanghai"),
    flow     = dat$Q,
    conc     = dat$conc,
    stringsAsFactors = FALSE
  )
  names(dat_rl)[3] <- constituent
  dat_rl <- dat_rl[order(dat_rl$datetime), , drop = FALSE]
  dat_rl <- dat_rl[!duplicated(dat_rl$datetime), , drop = FALSE]

  station <- NA_character_
  wybm    <- NA_character_
  if (!is.null(qf_cols$station) && qf_cols$station %in% names(qf_df)) {
    station <- as.character(stats::na.omit(unique(qf_df[[qf_cols$station]])))[1]
  }
  if (is.na(station)) station <- qf_sheet
  if (!is.null(qf_cols$wybm) && qf_cols$wybm %in% names(qf_df)) {
    wybm <- as.character(stats::na.omit(unique(qf_df[[qf_cols$wybm]])))[1]
  }

  list(
    dat         = dat,
    dat_rl      = dat_rl,
    station     = station,
    wybm        = wybm,
    qf_sheet    = qf_sheet,
    wq_sheet    = wq_sheet,
    constituent = constituent
  )
}


# ---------- Method 1–5 逐日浓度估算 ----------

#' Method 1: 时间加权的 Q 与 C —— mean(C) × mean(Q)
#' 日尺度实现：用所有监测浓度算术均值作为每日 C_est
fw_weighted_method1_daily <- function(dat) {
  c_mean <- mean(dat$conc, na.rm = TRUE)
  if (!is.finite(c_mean)) c_mean <- 0
  rep(c_mean, nrow(dat))
}

#' Method 2: 流量加权浓度 —— mean(Ci*Qi)
#' 日尺度实现：直接返回 list(C_est, flux)
fw_weighted_method2_daily <- function(dat, conv_factor = 86.4) {
  obs_idx <- which(!is.na(dat$conc) & is.finite(dat$Q) & dat$Q > 0)
  if (length(obs_idx) == 0)
    return(list(C_est = rep(NA_real_, nrow(dat)),
                flux  = rep(NA_real_, nrow(dat))))
  flux_obs  <- dat$Q[obs_idx] * dat$conc[obs_idx] * conv_factor
  flux_mean <- mean(flux_obs, na.rm = TRUE)
  flux_vec  <- rep(flux_mean, nrow(dat))
  c_est     <- ifelse(is.finite(dat$Q) & dat$Q > 0,
                      flux_vec / (dat$Q * conv_factor), NA_real_)
  list(C_est = c_est, flux = flux_vec)
}

#' Method 3: 相邻区间平均流量加权 —— Ci × Q_bar(区间)
#' 日尺度实现：返回 list(C_est, flux)
fw_weighted_method3_daily <- function(dat, conv_factor = 86.4) {
  obs_idx <- which(!is.na(dat$conc))
  n <- nrow(dat)
  if (length(obs_idx) < 1)
    return(list(C_est = rep(NA_real_, n), flux = rep(NA_real_, n)))

  flux_vec <- rep(NA_real_, n)
  c_est    <- rep(NA_real_, n)

  # 区间边界：每个观测点"负责"前后两次采样中点之间的日期
  boundaries <- numeric(length(obs_idx) + 1)
  boundaries[1] <- 1
  boundaries[length(boundaries)] <- n
  for (k in seq_along(obs_idx)[-1]) {
    boundaries[k] <- floor((obs_idx[k - 1] + obs_idx[k]) / 2)
  }

  for (k in seq_along(obs_idx)) {
    seg_start <- if (k == 1) 1 else boundaries[k] + 1
    seg_end   <- boundaries[k + 1]
    seg_range <- seq(max(seg_start, 1), min(seg_end, n))

    ci <- dat$conc[obs_idx[k]]
    for (j in seg_range) {
      c_est[j]    <- ci
      flux_vec[j] <- ci * dat$Q[j] * conv_factor
    }
  }
  list(C_est = c_est, flux = flux_vec)
}

#' Method 4: 时间加权浓度 + 全期均流量 —— mean(C) × Q_bar
#' 日尺度实现：返回 list(C_est, flux)
fw_weighted_method4_daily <- function(dat, conv_factor = 86.4) {
  c_mean <- mean(dat$conc, na.rm = TRUE)
  q_bar  <- mean(dat$Q, na.rm = TRUE)
  if (!is.finite(c_mean) || !is.finite(q_bar))
    return(list(C_est = rep(NA_real_, nrow(dat)),
                flux  = rep(NA_real_, nrow(dat))))
  flux_day <- c_mean * q_bar * conv_factor
  flux_vec <- rep(flux_day, nrow(dat))
  c_est    <- rep(c_mean, nrow(dat))
  list(C_est = c_est, flux = flux_vec)
}

#' Method 5: 时间与流量加权 —— (sum(Ci*Qi)/sum(Qi)) × Q_bar
#' 日尺度实现：返回 list(C_est, flux)
fw_weighted_method5_daily <- function(dat, conv_factor = 86.4) {
  obs_idx <- which(!is.na(dat$conc) & is.finite(dat$Q) & dat$Q > 0)
  if (length(obs_idx) == 0)
    return(list(C_est = rep(NA_real_, nrow(dat)),
                flux  = rep(NA_real_, nrow(dat))))

  c_fw  <- sum(dat$conc[obs_idx] * dat$Q[obs_idx], na.rm = TRUE) /
    sum(dat$Q[obs_idx], na.rm = TRUE)
  q_bar <- mean(dat$Q, na.rm = TRUE)
  if (!is.finite(c_fw) || !is.finite(q_bar))
    return(list(C_est = rep(NA_real_, nrow(dat)),
                flux  = rep(NA_real_, nrow(dat))))
  flux_day <- c_fw * q_bar * conv_factor
  flux_vec <- rep(flux_day, nrow(dat))
  c_est    <- rep(c_fw, nrow(dat))
  list(C_est = c_est, flux = flux_vec)
}

# ---------- 调用 RiverLoad 原函数获取总负荷 ----------

fw_weighted_riverload_total <- function(dat_rl, ncomp = 1) {
  if (!requireNamespace("RiverLoad", quietly = TRUE)) {
    warning("RiverLoad \u5305\u672a\u5b89\u88c5\uff0c\u8df3\u8fc7\u603b\u8d1f\u8377\u5bf9\u6bd4\u3002")
    return(rep(NA_real_, 5) |> setNames(paste0("method", 1:5)))
  }

  fns <- list(
    method1 = RiverLoad::method1,
    method2 = RiverLoad::method2,
    method3 = RiverLoad::method3,
    method4 = RiverLoad::method4,
    method5 = RiverLoad::method5
  )

  sapply(names(fns), function(nm) {
    tryCatch({
      out <- fns[[nm]](dat_rl, ncomp = ncomp)
      if (is.matrix(out) || is.data.frame(out)) as.numeric(out[1, 1])
      else if (is.numeric(out)) as.numeric(out[1])
      else NA_real_
    }, error = function(e) NA_real_)
  })
}
