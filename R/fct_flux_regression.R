# =====================================================================
# fct_flux_regression.R
# 回归法 —— 核心计算（尝试 loadflex，fallback base lm）
# =====================================================================

if (!exists("%||%", mode = "function")) {
  `%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x
}

fw_run_flux_regression_loadflex <- function(step1_data,
                                            qf_sheet = NULL,
                                            wq_sheet = NULL,
                                            constituent = "TN",
                                            date_range = NULL,
                                            model_choice = c("loadLm_season", "loadLm_simple"),
                                            param1 = 1,
                                            param2 = 1) {
  model_choice <- match.arg(model_choice)

  to_scalar_num <- function(x, default = 1) {
    x <- suppressWarnings(as.numeric(x))
    if (length(x) == 0 || !is.finite(x[1])) return(default)
    x[1]
  }
  p1 <- to_scalar_num(param1, 1)
  p2 <- to_scalar_num(param2, 1)

  prep <- fw_prepare_regression_input(
    step1_data = step1_data, qf_sheet = qf_sheet, wq_sheet = wq_sheet,
    constituent = constituent, date_range = date_range)

  dat <- prep$dat; dat_calib <- prep$dat_calib; newdata_day <- prep$newdata_day
  if (nrow(dat_calib) < 5) stop("用于回归校准的有效样本不足（Q>0 且 conc>0 的天数 < 5）。")
  if (nrow(newdata_day) == 0) stop("没有可预测的日流量数据（Q>0）。")

  pred_all <- NULL
  backend <- "base_lm"

  # ---- 尝试 loadflex ----
  if (requireNamespace("loadflex", quietly = TRUE)) {
    pred_all <- tryCatch({
      md <- loadflex::metadata(
        constituent = "conc", flow = "Q", dates = "TM",
        conc.units = "mg L^-1", flow.units = "m^3 s^-1",
        load.units = "kg", load.rate.units = "kg d^-1")

      lm_formula_simple <- stats::as.formula(log(conc) ~ log(Q))
      lm_formula_season <- stats::as.formula(
        log(conc) ~ log(Q) + sin(2 * pi * doy / 365) + cos(2 * pi * doy / 365))

      m_simple <- loadflex::loadLm(
        formula = lm_formula_simple, pred.format = "conc", data = dat_calib,
        metadata = md, ylog = TRUE, retrans.function = exp)
      m_season <- loadflex::loadLm(
        formula = lm_formula_season, pred.format = "conc", data = dat_calib,
        metadata = md, ylog = TRUE, retrans.function = exp)

      parse_pred <- function(pr_obj) {
        pr <- as.data.frame(pr_obj, stringsAsFactors = FALSE)
        if (nrow(pr) == 0) return(NULL)
        date_col <- names(pr)[1]
        tm <- fw_as_date(pr[[date_col]])
        if (all(is.na(tm)) && !is.null(rownames(pr))) tm <- fw_as_date(rownames(pr))
        pr$TM <- tm
        num_cols <- names(pr)[vapply(pr, is.numeric, logical(1))]
        num_cols <- setdiff(num_cols, "TM")
        if (length(num_cols) == 0) {
          for (nm in names(pr)) pr[[nm]] <- fw_as_num(pr[[nm]])
          num_cols <- names(pr)[vapply(pr, is.numeric, logical(1))]
          num_cols <- setdiff(num_cols, "TM")
        }
        if (length(num_cols) == 0) return(NULL)
        data.frame(TM = fw_as_date(pr$TM), load_kgd = fw_as_num(pr[[num_cols[1]]]),
                   stringsAsFactors = FALSE)
      }

      models <- list(loadLm_simple = m_simple, loadLm_season = m_season)
      out <- lapply(names(models), function(nm) {
        pr <- tryCatch(
          loadflex::predictSolute(
            load.model = models[[nm]], flux.or.conc = "flux",
            newdata = newdata_day, agg.by = "unit", date = TRUE, na.rm = FALSE),
          error = function(e) NULL)
        x <- parse_pred(pr)
        if (is.null(x) || nrow(x) == 0) return(NULL)
        x$method <- nm; x
      })
      out <- Filter(Negate(is.null), out)
      if (length(out) == 0) NULL else do.call(rbind, out)
    }, error = function(e) NULL)

    if (!is.null(pred_all) && nrow(pred_all) > 0) backend <- "loadflex"
  }

  # ---- fallback：base lm ----
  if (is.null(pred_all) || nrow(pred_all) == 0) {
    mk_pred <- function(formula, name) {
      fit <- tryCatch(stats::lm(formula, data = dat_calib), error = function(e) NULL)
      if (is.null(fit)) return(NULL)
      c_hat <- tryCatch(exp(stats::predict(fit, newdata = newdata_day)),
                        error = function(e) rep(NA_real_, nrow(newdata_day)))
      data.frame(
        TM = fw_as_date(newdata_day$TM),
        load_kgd = fw_as_num(newdata_day$Q) * fw_as_num(c_hat) * 86.4,
        method = name, stringsAsFactors = FALSE)
    }
    pred_all <- rbind(
      mk_pred(log(conc) ~ log(Q), "loadLm_simple"),
      mk_pred(log(conc) ~ log(Q) + sin(2 * pi * doy / 365) + cos(2 * pi * doy / 365), "loadLm_season"))
    pred_all <- pred_all[!is.na(pred_all$TM), , drop = FALSE]
    if (nrow(pred_all) == 0) stop("回归模型未产生有效预测结果。")
  }

  if (!(model_choice %in% unique(pred_all$method))) {
    model_choice <- unique(pred_all$method)[1]
  }

  pred_sel <- pred_all[pred_all$method == model_choice, c("TM", "load_kgd", "method"), drop = FALSE]
  obs <- dat[, c("TM", "conc"), drop = FALSE]

  daily <- merge(newdata_day[, c("TM", "Q"), drop = FALSE], pred_sel, by = "TM", all.x = TRUE, sort = TRUE)
  daily <- merge(daily, obs, by = "TM", all.x = TRUE, sort = TRUE)

  daily$C_obs <- daily$conc
  daily$flux  <- fw_as_num(daily$load_kgd) * p1 * p2
  daily$C_est <- ifelse(
    is.finite(daily$Q) & daily$Q > 0 & is.finite(daily$flux),
    daily$flux / (daily$Q * 86.4), NA_real_)

  daily$station  <- prep$station
  daily$WYBM     <- prep$wybm %||% NA_character_
  daily$date     <- fw_as_date(daily$TM)
  daily$C_source <- ifelse(is.finite(daily$C_obs), "observed", "regression")

  daily <- daily[, c("station", "WYBM", "date", "Q", "C_obs", "C_est", "flux",
                     "load_kgd", "method", "C_source"), drop = FALSE]
  daily <- daily[order(daily$date), , drop = FALSE]
  rownames(daily) <- NULL

  fit_data <- data.frame(logQ = log(dat_calib$Q), logC = log(dat_calib$conc),
                         stringsAsFactors = FALSE)
  model_compare <- stats::aggregate(
    load_kgd ~ method, data = pred_all,
    FUN = function(x) round(sum(x, na.rm = TRUE), 4))
  names(model_compare)[2] <- "total_flux_kg"

  d_start <- if (nrow(daily) > 0 && any(!is.na(daily$date))) as.character(min(daily$date, na.rm = TRUE)) else NA_character_
  d_end   <- if (nrow(daily) > 0 && any(!is.na(daily$date))) as.character(max(daily$date, na.rm = TRUE)) else NA_character_

  list(
    method       = "regression",
    method_label = paste0("回归法（", ifelse(model_choice == "loadLm_season", "季节回归", "线性回归"), "）"),
    daily   = daily,
    summary = fw_make_flux_summary_table(daily),
    diag = list(fit_data = fit_data, model_choice = model_choice, model_compare = model_compare),
    params = list(
      qf_sheet = prep$qf_sheet, wq_sheet = prep$wq_sheet,
      constituent = prep$constituent, model_choice = model_choice,
      backend = backend, param1 = p1, param2 = p2,
      data_source = "step1_qf_wq",
      data_source_label = paste0("第一步数据: QF$", prep$qf_sheet,
                                 " + WQ$", prep$wq_sheet, " [", prep$constituent, "]"),
      date_start = d_start, date_end = d_end))
}
