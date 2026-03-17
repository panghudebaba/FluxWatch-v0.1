# =====================================================================
# utils_flux_composite.R
# 复合法 —— 数据格式转换工具（loadflex / EGRET 所需结构）
# =====================================================================

if (!exists("%||%", mode = "function")) {
  `%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x
}

# ---------- 子方法标签 / choices ----------

fw_composite_sub_label <- function(sub) {
  switch(sub,
         abs_linear   = "绝对残差+线性空间",
         rel_linear   = "相对残差+线性空间",
         abs_log      = "绝对残差+log空间",
         rel_log      = "相对残差+log空间",
         wrtds_kalman = "WRTDS-Kalman",
         as.character(sub))
}

fw_composite_sub_choices <- function() {
  c("绝对残差+线性空间" = "abs_linear",
    "相对残差+线性空间" = "rel_linear",
    "绝对残差+log空间"  = "abs_log",
    "相对残差+log空间"  = "rel_log",
    "WRTDS-Kalman"       = "wrtds_kalman")
}

# ---------- loadflex 数据准备 ----------

#' 构建 loadflex metadata 对象
fw_build_loadflex_meta <- function(constituent = "conc", flow = "Q", dates = "date",
                                   conc.units = "mg L^-1", flow.units = "m^3 s^-1",
                                   load.units = "kg", load.rate.units = "kg d^-1",
                                   station = "station01") {
  if (!requireNamespace("loadflex", quietly = TRUE))
    stop("loadflex 包未安装，请先 install.packages('loadflex')")
  loadflex::metadata(
    constituent = constituent, flow = flow, dates = dates,
    conc.units = conc.units, flow.units = flow.units,
    load.units = load.units, load.rate.units = load.rate.units,
    station = station)
}

#' 将 app 内部 data.frame 转为 loadflex 所需格式
#' @param d data.frame(date, Q, C_obs, ...)
#' @return list(calib, all) — calib 为有观测数据, all 为全日期
fw_to_loadflex_data <- function(d) {
  d$date  <- fw_as_date(d$date)
  d$Q     <- fw_as_num(d$Q)
  d$C_obs <- fw_as_num(d$C_obs)
  d$doy   <- as.numeric(format(d$date, "%j"))

  df_all <- data.frame(
    date = d$date,
    Q    = pmax(d$Q, 1e-8),
    conc = d$C_obs,
    doy  = d$doy,
    stringsAsFactors = FALSE)

  ok <- is.finite(df_all$Q) & df_all$Q > 0 &
    is.finite(df_all$conc) & df_all$conc > 0
  df_calib <- df_all[ok, , drop = FALSE]

  if (nrow(df_calib) < 5)
    stop("用于校准的有效样本不足 (< 5)。")

  list(calib = df_calib, all = df_all)
}

# ---------- EGRET 数据准备 ----------

#' 构建 EGRET Daily data.frame
fw_to_egret_daily <- function(d) {
  d$date <- fw_as_date(d$date)
  d$Q    <- fw_as_num(d$Q)
  Daily <- data.frame(
    Date     = d$date,
    Q        = d$Q,
    LogQ     = ifelse(is.finite(d$Q) & d$Q > 0, log(d$Q), NA_real_),
    Julian   = as.numeric(d$date),
    Month    = as.integer(format(d$date, "%m")),
    Day      = as.integer(format(d$date, "%d")),
    DecYear  = as.numeric(format(d$date, "%Y")) +
      (as.numeric(format(d$date, "%j")) - 0.5) / 365.25,
    MonthSeq = (as.integer(format(d$date, "%Y")) - 1850) * 12 +
      as.integer(format(d$date, "%m")),
    Qualifier = rep("", nrow(d)),
    i         = seq_len(nrow(d)),
    stringsAsFactors = FALSE)
  Daily <- Daily[!is.na(Daily$Date) & is.finite(Daily$Q), , drop = FALSE]
  Daily <- Daily[order(Daily$Date), , drop = FALSE]
  rownames(Daily) <- NULL
  Daily
}

#' 构建 EGRET Sample data.frame
fw_to_egret_sample <- function(d) {
  d$date  <- fw_as_date(d$date)
  d$Q     <- fw_as_num(d$Q)
  d$C_obs <- fw_as_num(d$C_obs)
  ok <- is.finite(d$C_obs) & d$C_obs > 0 &
    is.finite(d$Q) & d$Q > 0 & !is.na(d$date)
  ds <- d[ok, , drop = FALSE]
  Sample <- data.frame(
    Date     = ds$date,
    ConcLow  = ds$C_obs, ConcHigh = ds$C_obs, ConcAve = ds$C_obs,
    Uncen    = rep(1L, nrow(ds)),
    Q        = ds$Q,
    LogQ     = log(ds$Q),
    Julian   = as.numeric(ds$date),
    Month    = as.integer(format(ds$date, "%m")),
    Day      = as.integer(format(ds$date, "%d")),
    DecYear  = as.numeric(format(ds$date, "%Y")) +
      (as.numeric(format(ds$date, "%j")) - 0.5) / 365.25,
    MonthSeq = (as.integer(format(ds$date, "%Y")) - 1850) * 12 +
      as.integer(format(ds$date, "%m")),
    SinDY    = sin(2 * pi * (as.numeric(format(ds$date, "%j")) - 0.5) / 365.25),
    CosDY    = cos(2 * pi * (as.numeric(format(ds$date, "%j")) - 0.5) / 365.25),
    stringsAsFactors = FALSE)
  Sample <- Sample[order(Sample$Date), , drop = FALSE]
  rownames(Sample) <- NULL
  Sample
}

#' 构建 EGRET INFO data.frame
fw_to_egret_info <- function(station_nm = "Station01", param_nm = "Conc",
                             drainSqKm = NA_real_) {
  data.frame(
    shortName = station_nm, paramShortName = param_nm,
    staAbbrev = station_nm, constitAbbrev = param_nm,
    drainSqKm = drainSqKm, paStart = 10, paLong = 12,
    windowY = 7, windowQ = 2, windowS = 0.5,
    minNumObs = 100, minNumUncen = 50, edgeAdjust = TRUE,
    stringsAsFactors = FALSE)
}

#' 组装 EGRET eList
fw_build_elist <- function(d, station_nm = "Station01", param_nm = "Conc") {
  if (!requireNamespace("EGRET", quietly = TRUE))
    stop("EGRET 包未安装，请先 install.packages('EGRET')")
  EGRET::as.egret(
    fw_to_egret_info(station_nm, param_nm),
    fw_to_egret_daily(d),
    fw_to_egret_sample(d),
    NA)
}
