# =====================================================================
# utils_flux_regression.R
# 回归法 —— 数据准备（QF/WQ → 回归输入）
# =====================================================================

if (!exists("%||%", mode = "function")) {
  `%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x
}

fw_prepare_regression_input <- function(step1_data,
                                        qf_sheet = NULL,
                                        wq_sheet = NULL,
                                        constituent = "TN",
                                        date_range = NULL) {
  if (is.null(step1_data) || is.null(step1_data$QF) || is.null(step1_data$WQ)) {
    stop("第一步数据缺少 QF/WQ。")
  }

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

  if (is.null(q_date_col) || is.null(q_flow_col)) stop("QF 数据中未识别到 TM/Q 列。")
  if (is.null(w_date_col)) stop("WQ 数据中未识别到 TM 列。")

  if (!(constituent %in% names(wq_df))) {
    idx <- which(tolower(names(wq_df)) == tolower(constituent))
    if (length(idx) == 0) stop(paste0("WQ 数据中不存在指标列: ", constituent))
    constituent <- names(wq_df)[idx[1]]
  }

  q <- data.frame(TM = fw_as_date(qf_df[[q_date_col]]), Q = fw_as_num(qf_df[[q_flow_col]]),
                  stringsAsFactors = FALSE)
  cc <- data.frame(TM = fw_as_date(wq_df[[w_date_col]]), conc = fw_as_num(wq_df[[constituent]]),
                   stringsAsFactors = FALSE)

  q <- q[!is.na(q$TM), , drop = FALSE]
  cc <- cc[!is.na(cc$TM), , drop = FALSE]

  if (!is.null(date_range) && length(date_range) == 2 && all(!is.na(date_range))) {
    s <- as.Date(date_range[1]); e <- as.Date(date_range[2])
    if (s > e) { tmp <- s; s <- e; e <- tmp }
    q  <- q[q$TM >= s & q$TM <= e, , drop = FALSE]
    cc <- cc[cc$TM >= s & cc$TM <= e, , drop = FALSE]
  }

  agg_mean <- function(x) if (all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE)
  if (nrow(q) > 0)  q  <- stats::aggregate(Q ~ TM,    data = q,  FUN = agg_mean)
  if (nrow(cc) > 0) cc <- stats::aggregate(conc ~ TM, data = cc, FUN = agg_mean)

  dat <- merge(q, cc, by = "TM", all.x = TRUE, sort = TRUE)
  dat$doy <- as.integer(format(dat$TM, "%j"))
  dat$conc[is.nan(dat$conc)] <- NA_real_

  dat_calib   <- dat[is.finite(dat$Q) & dat$Q > 0 & is.finite(dat$conc) & dat$conc > 0, , drop = FALSE]
  newdata_day <- dat[is.finite(dat$Q) & dat$Q > 0, c("TM", "Q", "doy"), drop = FALSE]

  station <- fw_pick_first_text(qf_df, c("STNM_WQ", "STNM", "station", "site", "SITENAME"))
  if (is.na(station) || station == "") {
    station <- fw_pick_first_text(wq_df, c("STNM_WQ", "STNM", "station", "site", "SITENAME"))
  }
  if (is.na(station) || station == "") station <- qf_sheet

  wybm <- fw_pick_first_text(qf_df, c("WYBM_WQ", "WYBM", "wybm"))
  if (is.na(wybm) || wybm == "") {
    wybm <- fw_pick_first_text(wq_df, c("WYBM_WQ", "WYBM", "wybm"))
  }

  list(
    dat         = dat,
    dat_calib   = dat_calib,
    newdata_day = newdata_day,
    station     = station,
    wybm        = if (is.na(wybm) || wybm == "") NA_character_ else wybm,
    qf_sheet    = qf_sheet,
    wq_sheet    = wq_sheet,
    constituent = constituent
  )
}
