# =====================================================================
# fct_flux_regression.R
# 回归法 —— 模型目录 + 统一入口 + 比较表 + 外部包包装
# ★ 模型目录函数放在文件最顶部，确保在 mod UI 构建前可用
# =====================================================================

if (!exists("%||%", mode = "function")) {

  `%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x
}

# =============================================================
# A. 模型目录 / 标签 / 类别（唯一定义，全局可用）
# =============================================================

#' 全部可用回归模型（命名向量：中文标签 = key）
fw_reg_all_model_choices <- function() {

  c(
    # ---- RiverLoad ----
    "\u8bc4\u7ea7\u66f2\u7ebf (Method 9: rating)"                       = "rating",
    "\u5206\u671f\u8bc4\u7ea7\u66f2\u7ebf (Method 10: rating.period)"   = "rating_period",
    "Ferguson\u4fee\u6b63 (Method 11: ferguson)"                         = "ferguson",
    "\u5206\u671fFerguson\u4fee\u6b63 (Method 12: ferguson.period)"      = "ferguson_period",
    # ---- LOADEST ----
    "LOADEST-1: lnQ"                                                     = "loadest_1",
    "LOADEST-2: lnQ + lnQ\u00b2"                                        = "loadest_2",
    "LOADEST-3: lnQ + T"                                                 = "loadest_3",
    "LOADEST-4: lnQ + sin/cos"                                           = "loadest_4",
    "LOADEST-5: lnQ + lnQ\u00b2 + T"                                    = "loadest_5",
    "LOADEST-6: lnQ + lnQ\u00b2 + sin/cos"                              = "loadest_6",
    "LOADEST-7: lnQ + sin/cos + T"                                       = "loadest_7",
    "LOADEST-8: lnQ + lnQ\u00b2 + T + T\u00b2"                          = "loadest_8",
    "LOADEST-9: \u5b8c\u6574\u6a21\u578b (full)"                        = "loadest_9",
    # ---- loadflex ----
    "loadflex \u7ebf\u6027\u56de\u5f52 (loadLm_simple)"                 = "loadLm_simple",
    "loadflex \u5b63\u8282\u56de\u5f52 (loadLm_season)"                 = "loadLm_season",
    # ---- EGRET WRTDS ----
    "WRTDS \u52a0\u6743\u56de\u5f52 (EGRET)"                            = "wrtds"
  )
}

#' 模型 key → 中文标签
fw_reg_model_label <- function(key) {
  ch <- fw_reg_all_model_choices()
  idx <- which(ch == key)
  if (length(idx) > 0) names(ch)[idx[1]] else key
}

#' 判断模型 key 所属类别（支持向量化）
fw_reg_model_category <- function(key) {
  vapply(key, function(k) {
    if (k %in% c("rating", "rating_period", "ferguson", "ferguson_period")) return("riverload")
    if (grepl("^loadest_", k)) return("loadest")
    if (k %in% c("loadLm_simple", "loadLm_season")) return("loadflex")
    if (k == "wrtds") return("wrtds")
    "unknown"
  }, character(1), USE.NAMES = FALSE)
}


# =============================================================
# B. 主入口函数
# =============================================================
fw_run_flux_regression <- function(step1_data,
                                   qf_sheet      = NULL,
                                   wq_sheet      = NULL,
                                   constituent   = "TN",
                                   date_range    = NULL,
                                   model_choice  = "loadLm_season",
                                   period_col    = "month",
                                   wrtds_h_T     = 10,
                                   wrtds_h_Q     = 2,
                                   wrtds_h_S     = 0.5) {

  # ---- 1. 数据准备 ----
  prep <- fw_prepare_regression_input(
    step1_data = step1_data, qf_sheet = qf_sheet, wq_sheet = wq_sheet,
    constituent = constituent, date_range = date_range
  )

  dat         <- prep$dat
  dat_calib   <- prep$dat_calib
  newdata_day <- prep$newdata_day

  if (nrow(dat_calib) < 5)
    stop("\u7528\u4e8e\u56de\u5f52\u6821\u51c6\u7684\u6709\u6548\u6837\u672c\u4e0d\u8db3\uff08Q>0 \u4e14 conc>0 \u7684\u5929\u6570 < 5\uff09\u3002")
  if (nrow(newdata_day) == 0)
    stop("\u6ca1\u6709\u53ef\u9884\u6d4b\u7684\u65e5\u6d41\u91cf\u6570\u636e\uff08Q>0\uff09\u3002")

  # ---- 2. 分发到各子方法 ----
  category <- fw_reg_model_category(model_choice)
  backend  <- "base_R"
  pred_df  <- NULL

  if (category == "riverload") {
    pred_df <- switch(model_choice,
                      "rating"          = fw_reg_rating(dat_calib, newdata_day),
                      "rating_period"   = fw_reg_rating_period(dat_calib, newdata_day, period_col = period_col),
                      "ferguson"        = fw_reg_ferguson(dat_calib, newdata_day),
                      "ferguson_period" = fw_reg_ferguson_period(dat_calib, newdata_day, period_col = period_col),
                      NULL)
    backend <- "base_R_riverload"

  } else if (category == "loadest") {
    model_num <- as.integer(sub("^loadest_", "", model_choice))
    if (requireNamespace("rloadest", quietly = TRUE)) {
      pred_df <- tryCatch(fw_reg_rloadest(dat_calib, newdata_day, model_num), error = function(e) NULL)
      if (!is.null(pred_df)) backend <- "rloadest"
    }
    if (is.null(pred_df)) {
      pred_df <- fw_reg_loadest(dat_calib, newdata_day, model_num)
      backend <- "base_R_loadest"
    }

  } else if (category == "loadflex") {
    pred_df <- fw_reg_loadflex(dat_calib, newdata_day, model_choice)
    if (!is.null(pred_df) && nrow(pred_df) > 0) {
      backend <- "loadflex"
    } else {
      pred_df <- fw_reg_loadflex_base(dat_calib, newdata_day, model_choice)
      backend <- "base_lm"
    }

  } else if (category == "wrtds") {
    if (requireNamespace("EGRET", quietly = TRUE)) {
      pred_df <- tryCatch(fw_reg_egret_wrtds(dat_calib, newdata_day), error = function(e) NULL)
      if (!is.null(pred_df)) backend <- "EGRET"
    }
    if (is.null(pred_df)) {
      pred_df <- fw_reg_wrtds(dat_calib, newdata_day,
                              h_T = wrtds_h_T, h_Q = wrtds_h_Q, h_S = wrtds_h_S)
      backend <- "base_R_wrtds"
    }

  } else {
    stop(paste0("\u672a\u77e5\u6a21\u578b: ", model_choice))
  }

  if (is.null(pred_df) || nrow(pred_df) == 0)
    stop("\u56de\u5f52\u6a21\u578b\u672a\u4ea7\u751f\u6709\u6548\u9884\u6d4b\u7ed3\u679c\u3002")

  pred_df <- pred_df[!is.na(pred_df$TM), , drop = FALSE]
  if (nrow(pred_df) == 0) stop("\u56de\u5f52\u9884\u6d4b\u7ed3\u679c\u65e5\u671f\u5168\u90e8\u65e0\u6548\u3002")

  # ---- 3. 组装 daily ----
  obs <- dat[, c("TM", "conc"), drop = FALSE]

  daily <- merge(
    newdata_day[, c("TM", "Q"), drop = FALSE],
    pred_df[, c("TM", "C_est", "load_kgd"), drop = FALSE],
    by = "TM", all.x = TRUE, sort = TRUE)
  daily <- merge(daily, obs, by = "TM", all.x = TRUE, sort = TRUE)

  daily$C_obs    <- daily$conc
  daily$flux     <- fw_as_num(daily$load_kgd)
  daily$station  <- prep$station
  daily$WYBM     <- prep$wybm %||% NA_character_
  daily$date     <- fw_as_date(daily$TM)
  daily$method   <- model_choice
  daily$C_source <- ifelse(is.finite(daily$C_obs), "observed", "regression")

  daily <- daily[, c("station", "WYBM", "date", "Q", "C_obs", "C_est", "flux",
                     "load_kgd", "method", "C_source"), drop = FALSE]
  daily <- daily[order(daily$date), , drop = FALSE]
  rownames(daily) <- NULL

  # ---- 4. 诊断 ----
  fit_data <- data.frame(logQ = log(dat_calib$Q), logC = log(dat_calib$conc),
                         stringsAsFactors = FALSE)
  model_compare <- fw_reg_compare_all_models(dat_calib, newdata_day, model_choice, period_col)

  d_start <- if (nrow(daily) > 0 && any(!is.na(daily$date)))
    as.character(min(daily$date, na.rm = TRUE)) else NA_character_
  d_end <- if (nrow(daily) > 0 && any(!is.na(daily$date)))
    as.character(max(daily$date, na.rm = TRUE)) else NA_character_

  # ---- 5. 返回 ----
  list(
    method       = "regression",
    method_label = paste0("\u56de\u5f52\u6cd5\uff08", fw_reg_model_label(model_choice), "\uff09"),
    daily   = daily,
    summary = fw_make_flux_summary_table(daily),
    diag = list(fit_data = fit_data, model_choice = model_choice, model_compare = model_compare),
    params = list(
      qf_sheet = prep$qf_sheet, wq_sheet = prep$wq_sheet,
      constituent = prep$constituent, model_choice = model_choice,
      model_category = category, backend = backend, period_col = period_col,
      data_source = "step1_qf_wq",
      data_source_label = paste0("\u7b2c\u4e00\u6b65\u6570\u636e: QF$", prep$qf_sheet,
                                 " + WQ$", prep$wq_sheet, " [", prep$constituent, "]"),
      date_start = d_start, date_end = d_end))
}


# =============================================================
# C. 向后兼容别名
# =============================================================
fw_run_flux_regression_loadflex <- function(step1_data,
                                            qf_sheet     = NULL,
                                            wq_sheet     = NULL,
                                            constituent  = "TN",
                                            date_range   = NULL,
                                            model_choice = c("loadLm_season", "loadLm_simple")) {
  model_choice <- match.arg(model_choice)
  fw_run_flux_regression(step1_data = step1_data, qf_sheet = qf_sheet,
                         wq_sheet = wq_sheet, constituent = constituent,
                         date_range = date_range, model_choice = model_choice)
}


# =============================================================
# D. 多模型比较表
# =============================================================
fw_reg_compare_all_models <- function(dat_calib, newdata_day, selected_model, period_col = "month") {
  safe_total <- function(func, ...) {
    tryCatch({
      res <- func(...)
      if (is.null(res) || nrow(res) == 0) return(NA_real_)
      round(sum(res$load_kgd, na.rm = TRUE), 4)
    }, error = function(e) NA_real_)
  }

  models <- c("rating", "ferguson", "loadest_1", "loadest_9",
              "loadLm_simple", "loadLm_season")

  totals <- vapply(models, function(m) {
    cat_m <- fw_reg_model_category(m)
    if (cat_m == "riverload") {
      if (m == "rating")   return(safe_total(fw_reg_rating, dat_calib, newdata_day))
      if (m == "ferguson")  return(safe_total(fw_reg_ferguson, dat_calib, newdata_day))
    }
    if (cat_m == "loadest") {
      num <- as.integer(sub("^loadest_", "", m))
      return(safe_total(fw_reg_loadest, dat_calib, newdata_day, num))
    }
    if (cat_m == "loadflex") {
      return(safe_total(fw_reg_loadflex_base, dat_calib, newdata_day, m))
    }
    NA_real_
  }, numeric(1))

  out <- data.frame(method = models, total_flux_kg = totals,
                    is_selected = models == selected_model, stringsAsFactors = FALSE)
  out[is.finite(out$total_flux_kg), , drop = FALSE]
}


# =============================================================
# E. EGRET WRTDS 包装
# =============================================================
fw_reg_egret_wrtds <- function(dat_calib, newdata_day) {
  if (!requireNamespace("EGRET", quietly = TRUE)) return(NULL)
  tryCatch({
    Sample <- data.frame(
      dateTime = as.POSIXct(dat_calib$TM),
      ConcLow = dat_calib$conc, ConcHigh = dat_calib$conc, Uncen = 1,
      ConcAve = dat_calib$conc, Julian = dat_calib$doy,
      Month = dat_calib$month,
      Day = as.integer(format(dat_calib$TM, "%d")),
      DecYear = dat_calib$dec_year,
      MonthSeq = (dat_calib$year - min(dat_calib$year)) * 12 + dat_calib$month,
      SinDY = sin(2 * pi * dat_calib$dec_year),
      CosDY = cos(2 * pi * dat_calib$dec_year),
      LogQ = log(dat_calib$Q), Q = dat_calib$Q, stringsAsFactors = FALSE)

    Daily <- data.frame(
      Date = as.Date(newdata_day$TM), Q = newdata_day$Q,
      Julian = newdata_day$doy, Month = newdata_day$month,
      Day = as.integer(format(newdata_day$TM, "%d")),
      DecYear = newdata_day$dec_year,
      MonthSeq = (newdata_day$year - min(newdata_day$year)) * 12 + newdata_day$month,
      SinDY = sin(2 * pi * newdata_day$dec_year),
      CosDY = cos(2 * pi * newdata_day$dec_year),
      LogQ = log(newdata_day$Q), i = seq_len(nrow(newdata_day)),
      stringsAsFactors = FALSE)

    INFO <- data.frame(param.units = "mg/L", shortName = "Conc",
                       paramShortName = "Conc", drainSqKm = 1, stringsAsFactors = FALSE)

    eList <- EGRET::as.egret(INFO = INFO, Daily = Daily, Sample = Sample)
    eList <- EGRET::modelEstimation(eList, verbose = FALSE)
    daily_out <- eList$Daily
    C_est <- daily_out$ConcDay

    data.frame(TM = as.Date(daily_out$Date), C_est = C_est,
               load_kgd = C_est * daily_out$Q * 86.4, stringsAsFactors = FALSE)
  }, error = function(e) NULL)
}


# =============================================================
# F. rloadest 包装
# =============================================================
fw_reg_rloadest <- function(dat_calib, newdata_day, model_num = 9) {
  if (!requireNamespace("rloadest", quietly = TRUE)) return(NULL)
  tryCatch({
    calib_df <- data.frame(
      DATES = as.Date(dat_calib$TM), TIMES = rep("12:00", nrow(dat_calib)),
      FLOW = dat_calib$Q, conc = dat_calib$conc, stringsAsFactors = FALSE)
    est_df <- data.frame(
      DATES = as.Date(newdata_day$TM), TIMES = rep("12:00", nrow(newdata_day)),
      FLOW = newdata_day$Q, stringsAsFactors = FALSE)

    m <- rloadest::loadReg(conc ~ model(model_num), data = calib_df,
                           flow = "FLOW", dates = "DATES", time.step = "day", station = "station")
    pred <- rloadest::predLoad(m, newdata = est_df, load.units = "kg",
                               by = "unit", allow.incomplete = TRUE)
    load_vals <- if ("Flux" %in% names(pred)) pred$Flux else pred[[2]]
    C_est <- ifelse(is.finite(newdata_day$Q) & newdata_day$Q > 0,
                    load_vals / (newdata_day$Q * 86.4), NA_real_)

    data.frame(TM = as.Date(newdata_day$TM), C_est = C_est,
               load_kgd = load_vals, stringsAsFactors = FALSE)
  }, error = function(e) NULL)
}
