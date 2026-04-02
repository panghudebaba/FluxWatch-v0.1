# =====================================================================
# utils_flux_interp.R
# 插值方法 —— 辅助工具函数（数据准备、Method 6 + loadflex 逐日估算）
# =====================================================================

if (!exists("%||%", mode = "function")) {
  `%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x
}

# ---------- 数据准备 ----------

fw_prepare_interp_input <- function(step1_data,
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
# 5 种插值子方法的逐日浓度估算
# ========================================================================

#' Method 6 / linearInterp: 浓度线性插值
#' @param dat data.frame(TM, Q, conc)
#' @return numeric vector of C_est
fw_interp_linear_daily <- function(dat) {
  obs_idx <- which(!is.na(dat$conc))
  if (length(obs_idx) < 2)
    return(rep(mean(dat$conc, na.rm = TRUE), nrow(dat)))
  c_est <- stats::approx(
    x    = as.numeric(dat$TM[obs_idx]),
    y    = dat$conc[obs_idx],
    xout = as.numeric(dat$TM),
    rule  = 2   # 端点外推取最近值
  )$y
  c_est
}

#' rectangularInterp: 矩形（阶梯/前值保持）插值
#' @param dat data.frame(TM, Q, conc)
#' @return numeric vector of C_est
fw_interp_rectangular_daily <- function(dat) {
  obs_idx <- which(!is.na(dat$conc))
  if (length(obs_idx) == 0)
    return(rep(NA_real_, nrow(dat)))
  if (length(obs_idx) == 1)
    return(rep(dat$conc[obs_idx], nrow(dat)))
  # 前值保持：approx method = "constant"
  c_est <- stats::approx(
    x      = as.numeric(dat$TM[obs_idx]),
    y      = dat$conc[obs_idx],
    xout   = as.numeric(dat$TM),
    method = "constant",
    rule    = 2,
    f       = 0    # f=0 表示前值保持
  )$y
  c_est
}

#' triangularInterp: 三角核插值
#' @param dat data.frame(TM, Q, conc)
#' @param half_window 半窗宽天数 (默认 NULL 自动设为中位采样间隔)
#' @return numeric vector of C_est
fw_interp_triangular_daily <- function(dat, half_window = NULL) {
  obs_idx <- which(!is.na(dat$conc))
  n <- nrow(dat)
  if (length(obs_idx) == 0) return(rep(NA_real_, n))
  if (length(obs_idx) == 1) return(rep(dat$conc[obs_idx], n))

  t_obs <- as.numeric(dat$TM[obs_idx])
  c_obs <- dat$conc[obs_idx]

  # 自动半窗宽：中位采样间隔 × 1.5

  if (is.null(half_window) || !is.finite(half_window) || half_window <= 0) {
    gaps <- diff(t_obs)
    half_window <- if (length(gaps) > 0) stats::median(gaps) * 1.5 else 30
  }

  t_all <- as.numeric(dat$TM)
  c_est <- numeric(n)

  for (j in seq_len(n)) {
    d <- abs(t_all[j] - t_obs)
    w <- pmax(0, 1 - d / half_window)
    sw <- sum(w)
    c_est[j] <- if (sw > 0) sum(w * c_obs) / sw else NA_real_
  }
  # 如有 NA（超出所有窗口），用最近观测填充
  na_idx <- which(is.na(c_est))
  if (length(na_idx) > 0) {
    for (j in na_idx) {
      d <- abs(as.numeric(dat$TM[j]) - t_obs)
      c_est[j] <- c_obs[which.min(d)]
    }
  }
  c_est
}

#' distanceWeightedInterp: 距离加权插值（IDW）
#' @param dat data.frame(TM, Q, conc)
#' @param power 距离衰减幂次 (默认 2)
#' @return numeric vector of C_est
fw_interp_distweight_daily <- function(dat, power = 2) {
  obs_idx <- which(!is.na(dat$conc))
  n <- nrow(dat)
  if (length(obs_idx) == 0) return(rep(NA_real_, n))
  if (length(obs_idx) == 1) return(rep(dat$conc[obs_idx], n))

  t_obs <- as.numeric(dat$TM[obs_idx])
  c_obs <- dat$conc[obs_idx]
  t_all <- as.numeric(dat$TM)
  c_est <- numeric(n)

  for (j in seq_len(n)) {
    d <- abs(t_all[j] - t_obs)
    # 如果恰好在观测点上
    exact <- which(d == 0)
    if (length(exact) > 0) {
      c_est[j] <- mean(c_obs[exact])
      next
    }
    w <- 1 / (d ^ power)
    c_est[j] <- sum(w * c_obs) / sum(w)
  }
  c_est
}


# ========================================================================
# loadflex 集成：用 loadInterp 预测浓度
# ========================================================================

#' 尝试用 loadflex::loadInterp 预测逐日浓度
#' @param dat data.frame(TM, Q, conc)
#' @param constituent 指标名
#' @param interp_fun_name loadflex 插值函数名（字符串）
#' @return numeric vector of C_est，或 NULL（loadflex 不可用时）
fw_interp_loadflex_predict <- function(dat, constituent = "TN",
                                       interp_fun_name = "linearInterpolation") {
  if (!requireNamespace("loadflex", quietly = TRUE)) return(NULL)

  obs_idx <- which(!is.na(dat$conc) & is.finite(dat$Q) & dat$Q > 0)
  if (length(obs_idx) < 2) return(NULL)

  tryCatch({
    # 构建校准数据
    cal_df <- data.frame(
      date = as.POSIXct(dat$TM[obs_idx], tz = "UTC"),
      flow = dat$Q[obs_idx],
      conc = dat$conc[obs_idx],
      stringsAsFactors = FALSE
    )
    names(cal_df)[3] <- constituent

    # 构建预测数据
    est_df <- data.frame(
      date = as.POSIXct(dat$TM, tz = "UTC"),
      flow = dat$Q,
      stringsAsFactors = FALSE
    )

    # 获取插值函数
    interp_fn <- tryCatch(
      get(interp_fun_name, envir = asNamespace("loadflex")),
      error = function(e) NULL
    )
    if (is.null(interp_fn)) return(NULL)

    # 创建 metadata
    meta <- loadflex::metadata(
      constituent = constituent,
      flow       = "flow",
      dates      = "date",
      conc.units = "mg/L",
      flow.units = "cms",
      load.units = "kg",
      load.rate.units = "kg/d",
      station    = "station"
    )

    # 创建插值模型
    model <- loadflex::loadInterp(
      interp.format = "conc",
      interp.fun    = interp_fn,
      data          = cal_df,
      metadata      = meta
    )

    # 预测浓度
    preds <- loadflex::predictSolute(model, "conc", est_df)

    # 提取预测结果
    if (is.data.frame(preds)) {
      # 尝试常见列名
      pred_col <- intersect(names(preds), c("fit", "conc", constituent, "Pred"))
      if (length(pred_col) > 0) return(as.numeric(preds[[pred_col[1]]]))
    }
    if (is.numeric(preds)) return(as.numeric(preds))

    NULL
  }, error = function(e) {
    message("loadflex loadInterp \u9884\u6d4b\u5931\u8d25: ", e$message)
    NULL
  })
}


# ========================================================================
# RiverLoad method6 总负荷
# ========================================================================

#' 调用 RiverLoad::method6 获取总负荷（g）
#' @param dat_rl RiverLoad 格式 data.frame
#' @param ncomp 组分编号
#' @return numeric 总负荷 (g)，或 NA
fw_interp_riverload_method6 <- function(dat_rl, ncomp = 1) {
  if (!requireNamespace("RiverLoad", quietly = TRUE)) return(NA_real_)
  tryCatch({
    out <- RiverLoad::method6(dat_rl, ncomp = ncomp)
    if (is.matrix(out) || is.data.frame(out)) as.numeric(out[1, 1])
    else if (is.numeric(out)) as.numeric(out[1])
    else NA_real_
  }, error = function(e) NA_real_)
}

#' 调用 RiverLoad method1-6 获取总负荷用于对比
fw_interp_riverload_compare <- function(dat_rl, ncomp = 1) {
  if (!requireNamespace("RiverLoad", quietly = TRUE)) {
    return(rep(NA_real_, 6) |> setNames(paste0("method", 1:6)))
  }

  fns <- list(
    method1 = RiverLoad::method1,
    method2 = RiverLoad::method2,
    method3 = RiverLoad::method3,
    method4 = RiverLoad::method4,
    method5 = RiverLoad::method5,
    method6 = RiverLoad::method6
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
