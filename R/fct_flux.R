if (!exists("%||%", mode = "function")) {
  `%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x
}

fw_flux_method_label <- function(key) {
  switch(
    key,
    weighted = "加权平均法",
    interp = "插值法",
    ratio = "比率法",
    regression = "回归法",
    composite = "复合方法",
    as.character(key)
  )
}

fw_flux_method_principle <- function(method) {
  switch(
    method,
    weighted = "以有监测浓度日的流量作为权重计算代表浓度，缺测浓度日采用该代表值，再按 Q×C 计算日通量。",
    interp = "对离散监测浓度按时间进行线性插值，获得连续日浓度序列，与日流量相乘得到日通量。",
    ratio = "先在有监测日建立 C/Q 的平均比率，缺测浓度日按该比率与当日流量估算浓度。",
    regression = "调用第一步 QF/WQ 数据进行回归预测，得到逐日负荷并转换通量。",
    composite = "优先实测浓度，其次插值，再次回归，最后用加权平均兜底，提高连续性和稳健性。",
    "未知方法"
  )
}

fw_make_flux_summary_table <- function(daily) {
  if (is.null(daily) || !is.data.frame(daily) || nrow(daily) == 0) {
    return(data.frame(
      calc_result = character(0),
      date = as.Date(character(0)),
      station = character(0),
      WYBM = character(0),
      Q = numeric(0),
      flux = numeric(0),
      stringsAsFactors = FALSE
    ))
  }

  d <- as.data.frame(daily, stringsAsFactors = FALSE)
  if (!("date" %in% names(d)) && "TM" %in% names(d)) d$date <- fw_as_date(d$TM)
  if (!("station" %in% names(d))) d$station <- "ALL"
  if (!("WYBM" %in% names(d))) d$WYBM <- NA_character_
  if (!("Q" %in% names(d))) d$Q <- NA_real_
  if (!("flux" %in% names(d))) d$flux <- NA_real_

  out <- data.frame(
    calc_result = "日值",
    date = fw_as_date(d$date),
    station = as.character(d$station),
    WYBM = as.character(d$WYBM),
    Q = fw_as_num(d$Q),
    flux = fw_as_num(d$flux),
    stringsAsFactors = FALSE
  )
  out <- out[order(out$date), , drop = FALSE]
  rownames(out) <- NULL
  out
}

fw_make_flux_daily_summary <- function(daily) {
  fw_make_flux_summary_table(daily)
}

fw_prepare_flux_data <- function(raw_df) {
  if (is.null(raw_df) || !is.data.frame(raw_df) || nrow(raw_df) == 0) {
    stop("原始数据为空。")
  }

  cc <- fw_find_flux_cols(raw_df)
  if (is.null(cc$dt) || is.null(cc$q) || is.null(cc$c)) {
    stop("未识别到日期/流量/浓度列。")
  }

  d <- data.frame(
    date = fw_as_date(raw_df[[cc$dt]]),
    Q = fw_as_num(raw_df[[cc$q]]),
    C_obs = fw_as_num(raw_df[[cc$c]]),
    station = if (!is.null(cc$station)) as.character(raw_df[[cc$station]]) else NA_character_,
    WYBM = if (!is.null(cc$wybm)) as.character(raw_df[[cc$wybm]]) else NA_character_,
    stringsAsFactors = FALSE
  )

  d <- d[!is.na(d$date), , drop = FALSE]
  if (nrow(d) == 0) stop("日期列解析失败，未得到有效记录。")

  d$station <- fw_trim_na(d$station)
  d$station[is.na(d$station)] <- "ALL"
  d$WYBM <- fw_trim_na(d$WYBM)

  grp <- data.frame(
    date = d$date,
    station = d$station,
    WYBM = ifelse(is.na(d$WYBM), "", d$WYBM),
    stringsAsFactors = FALSE
  )

  agg_mean <- function(x) if (all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE)

  q1 <- stats::aggregate(d$Q, by = grp, FUN = agg_mean)
  c1 <- stats::aggregate(d$C_obs, by = grp, FUN = agg_mean)
  names(q1)[ncol(q1)] <- "Q"
  names(c1)[ncol(c1)] <- "C_obs"

  out <- merge(q1, c1, by = c("date", "station", "WYBM"), all = TRUE, sort = TRUE)
  out$WYBM[out$WYBM == ""] <- NA_character_
  out <- out[order(out$date), c("date", "Q", "C_obs", "station", "WYBM"), drop = FALSE]
  rownames(out) <- NULL
  out
}

fw_run_flux_method <- function(dat_daily,
                               method = c("weighted", "interp", "ratio", "regression", "composite"),
                               conv_factor = 86.4) {
  method <- match.arg(method)

  d <- as.data.frame(dat_daily, stringsAsFactors = FALSE)
  if (!all(c("date", "Q", "C_obs") %in% names(d))) {
    stop("dat_daily must contain: date, Q, C_obs")
  }

  d$date <- fw_as_date(d$date)
  d$Q <- fw_as_num(d$Q)
  d$C_obs <- fw_as_num(d$C_obs)

  if (!("station" %in% names(d))) d$station <- "ALL"
  if (!("WYBM" %in% names(d))) d$WYBM <- NA_character_

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

  interp_conc <- function(date, conc) {
    x <- as.numeric(fw_as_date(date))
    y <- fw_as_num(conc)
    ok <- is.finite(x) & is.finite(y)
    if (sum(ok) == 0) return(rep(NA_real_, length(y)))
    if (sum(ok) == 1) return(rep(y[ok][1], length(y)))

    xy <- data.frame(x = x[ok], y = y[ok], stringsAsFactors = FALSE)
    xy <- stats::aggregate(y ~ x, data = xy, FUN = mean)

    stats::approx(
      x = xy$x, y = xy$y,
      xout = x, method = "linear",
      rule = 2, ties = mean
    )$y
  }

  regress_conc <- function(Q, conc, date) {
    Q <- fw_as_num(Q)
    conc <- fw_as_num(conc)
    doy <- as.integer(format(fw_as_date(date), "%j"))

    ok <- is.finite(Q) & Q > 0 & is.finite(conc) & conc > 0 & is.finite(doy)
    if (sum(ok) < 5) return(rep(NA_real_, length(Q)))

    train <- data.frame(Q = Q[ok], conc = conc[ok], doy = doy[ok], stringsAsFactors = FALSE)
    newd <- data.frame(Q = pmax(Q, .Machine$double.eps), doy = doy, stringsAsFactors = FALSE)

    fit <- tryCatch(
      stats::lm(log(conc) ~ log(Q) + sin(2 * pi * doy / 365) + cos(2 * pi * doy / 365), data = train),
      error = function(e) NULL
    )
    if (is.null(fit)) {
      fit <- tryCatch(stats::lm(log(conc) ~ log(Q), data = train), error = function(e) NULL)
    }
    if (is.null(fit)) return(rep(NA_real_, length(Q)))

    fw_as_num(suppressWarnings(exp(stats::predict(fit, newdata = newd))))
  }

  c_interp <- interp_conc(d$date, d$C_obs)
  c_reg <- regress_conc(d$Q, d$C_obs, d$date)

  C_est <- rep(NA_real_, nrow(d))
  C_source <- rep(NA_character_, nrow(d))

  if (identical(method, "weighted")) {
    C_est <- ifelse(is.finite(d$C_obs), d$C_obs, weighted_conc)
    C_source <- ifelse(is.finite(d$C_obs), "observed", "weighted")
  }

  if (identical(method, "interp")) {
    C_est <- ifelse(is.finite(d$C_obs), d$C_obs, c_interp)
    C_source <- ifelse(is.finite(d$C_obs), "observed", "interp")
    miss <- !is.finite(C_est)
    C_est[miss] <- weighted_conc
    C_source[miss] <- "weighted"
  }

  if (identical(method, "ratio")) {
    ok_ratio <- is.finite(d$C_obs) & is.finite(d$Q) & d$Q > 0
    ratio <- if (any(ok_ratio)) mean(d$C_obs[ok_ratio] / d$Q[ok_ratio], na.rm = TRUE) else NA_real_
    c_ratio <- ifelse(is.finite(d$Q), ratio * d$Q, NA_real_)

    C_est <- ifelse(is.finite(d$C_obs), d$C_obs, c_ratio)
    C_source <- ifelse(is.finite(d$C_obs), "observed", "ratio")
    miss <- !is.finite(C_est)
    C_est[miss] <- weighted_conc
    C_source[miss] <- "weighted"
  }

  if (identical(method, "regression")) {
    C_est <- ifelse(is.finite(d$C_obs), d$C_obs, c_reg)
    C_source <- ifelse(is.finite(d$C_obs), "observed", "regression")
    miss <- !is.finite(C_est)
    C_est[miss] <- weighted_conc
    C_source[miss] <- "weighted"
  }

  if (identical(method, "composite")) {
    C_est <- d$C_obs
    C_source <- ifelse(is.finite(d$C_obs), "observed", NA_character_)

    miss <- !is.finite(C_est) & is.finite(c_interp)
    C_est[miss] <- c_interp[miss]
    C_source[miss] <- "interp"

    miss <- !is.finite(C_est) & is.finite(c_reg)
    C_est[miss] <- c_reg[miss]
    C_source[miss] <- "regression"

    miss <- !is.finite(C_est)
    C_est[miss] <- weighted_conc
    C_source[miss] <- "weighted"
  }

  flux <- ifelse(
    is.finite(d$Q) & is.finite(C_est),
    d$Q * C_est * conv_factor,
    NA_real_
  )

  daily <- data.frame(
    station = as.character(d$station),
    WYBM = as.character(d$WYBM),
    date = d$date,
    Q = d$Q,
    C_obs = d$C_obs,
    C_est = C_est,
    flux = flux,
    method = method,
    C_source = C_source,
    stringsAsFactors = FALSE
  )

  list(
    method = method,
    method_label = fw_flux_method_label(method),
    daily = daily,
    summary = fw_make_flux_summary_table(daily),
    diag = list(weighted_conc = weighted_conc, n_obs = length(idx_obs)),
    params = list(conv_factor = conv_factor)
  )
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
  q_flow_col <- if ("Q" %in% names(qf_df)) "Q" else q_cols$q
  w_date_col <- if ("TM" %in% names(wq_df)) "TM" else w_cols$dt

  if (is.null(q_date_col) || is.null(q_flow_col)) stop("QF 数据中未识别到 TM/Q 列。")
  if (is.null(w_date_col)) stop("WQ 数据中未识别到 TM 列。")

  if (!(constituent %in% names(wq_df))) {
    idx <- which(tolower(names(wq_df)) == tolower(constituent))
    if (length(idx) == 0) stop(paste0("WQ 数据中不存在指标列: ", constituent))
    constituent <- names(wq_df)[idx[1]]
  }

  q <- data.frame(
    TM = fw_as_date(qf_df[[q_date_col]]),
    Q = fw_as_num(qf_df[[q_flow_col]]),
    stringsAsFactors = FALSE
  )
  c <- data.frame(
    TM = fw_as_date(wq_df[[w_date_col]]),
    conc = fw_as_num(wq_df[[constituent]]),
    stringsAsFactors = FALSE
  )

  q <- q[!is.na(q$TM), , drop = FALSE]
  c <- c[!is.na(c$TM), , drop = FALSE]

  if (!is.null(date_range) && length(date_range) == 2 && all(!is.na(date_range))) {
    s <- as.Date(date_range[1]); e <- as.Date(date_range[2])
    if (s > e) { tmp <- s; s <- e; e <- tmp }
    q <- q[q$TM >= s & q$TM <= e, , drop = FALSE]
    c <- c[c$TM >= s & c$TM <= e, , drop = FALSE]
  }

  agg_mean <- function(x) if (all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE)
  if (nrow(q) > 0) q <- stats::aggregate(Q ~ TM, data = q, FUN = agg_mean)
  if (nrow(c) > 0) c <- stats::aggregate(conc ~ TM, data = c, FUN = agg_mean)

  dat <- merge(q, c, by = "TM", all.x = TRUE, sort = TRUE)
  dat$doy <- as.integer(format(dat$TM, "%j"))
  dat$conc[is.nan(dat$conc)] <- NA_real_

  dat_calib <- dat[is.finite(dat$Q) & dat$Q > 0 & is.finite(dat$conc) & dat$conc > 0, , drop = FALSE]
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
    dat = dat,
    dat_calib = dat_calib,
    newdata_day = newdata_day,
    station = station,
    wybm = if (is.na(wybm) || wybm == "") NA_character_ else wybm,
    qf_sheet = qf_sheet,
    wq_sheet = wq_sheet,
    constituent = constituent
  )
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
    step1_data = step1_data,
    qf_sheet = qf_sheet,
    wq_sheet = wq_sheet,
    constituent = constituent,
    date_range = date_range
  )

  dat <- prep$dat
  dat_calib <- prep$dat_calib
  newdata_day <- prep$newdata_day

  if (nrow(dat_calib) < 5) stop("用于回归校准的有效样本不足（Q>0 且 conc>0 的天数 < 5）。")
  if (nrow(newdata_day) == 0) stop("没有可预测的日流量数据（Q>0）。")

  pred_all <- NULL
  backend <- "base_lm"

  # 尝试 loadflex
  if (requireNamespace("loadflex", quietly = TRUE)) {
    pred_all <- tryCatch({
      md <- loadflex::metadata(
        constituent = "conc",
        flow = "Q",
        dates = "TM",
        conc.units = "mg L^-1",
        flow.units = "m^3 s^-1",
        load.units = "kg",
        load.rate.units = "kg d^-1"
      )

      lm_formula_simple <- stats::as.formula(log(conc) ~ log(Q))
      lm_formula_season <- stats::as.formula(log(conc) ~ log(Q) + sin(2 * pi * doy / 365) + cos(2 * pi * doy / 365))

      m_simple <- loadflex::loadLm(
        formula = lm_formula_simple,
        pred.format = "conc",
        data = dat_calib,
        metadata = md,
        ylog = TRUE,
        retrans.function = exp
      )

      m_season <- loadflex::loadLm(
        formula = lm_formula_season,
        pred.format = "conc",
        data = dat_calib,
        metadata = md,
        ylog = TRUE,
        retrans.function = exp
      )

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

        data.frame(
          TM = fw_as_date(pr$TM),
          load_kgd = fw_as_num(pr[[num_cols[1]]]),
          stringsAsFactors = FALSE
        )
      }

      models <- list(loadLm_simple = m_simple, loadLm_season = m_season)

      out <- lapply(names(models), function(nm) {
        pr <- tryCatch(
          loadflex::predictSolute(
            load.model = models[[nm]],
            flux.or.conc = "flux",
            newdata = newdata_day,
            agg.by = "unit",
            date = TRUE,
            na.rm = FALSE
          ),
          error = function(e) NULL
        )
        x <- parse_pred(pr)
        if (is.null(x) || nrow(x) == 0) return(NULL)
        x$method <- nm
        x
      })

      out <- Filter(Negate(is.null), out)
      if (length(out) == 0) NULL else do.call(rbind, out)
    }, error = function(e) NULL)

    if (!is.null(pred_all) && nrow(pred_all) > 0) backend <- "loadflex"
  }

  # fallback：base lm
  if (is.null(pred_all) || nrow(pred_all) == 0) {
    mk_pred <- function(formula, name) {
      fit <- tryCatch(stats::lm(formula, data = dat_calib), error = function(e) NULL)
      if (is.null(fit)) return(NULL)
      c_hat <- tryCatch(exp(stats::predict(fit, newdata = newdata_day)), error = function(e) rep(NA_real_, nrow(newdata_day)))
      data.frame(
        TM = fw_as_date(newdata_day$TM),
        load_kgd = fw_as_num(newdata_day$Q) * fw_as_num(c_hat) * 86.4,
        method = name,
        stringsAsFactors = FALSE
      )
    }

    pred_all <- rbind(
      mk_pred(log(conc) ~ log(Q), "loadLm_simple"),
      mk_pred(log(conc) ~ log(Q) + sin(2 * pi * doy / 365) + cos(2 * pi * doy / 365), "loadLm_season")
    )
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
  daily$flux <- fw_as_num(daily$load_kgd) * p1 * p2
  daily$C_est <- ifelse(
    is.finite(daily$Q) & daily$Q > 0 & is.finite(daily$flux),
    daily$flux / (daily$Q * 86.4),
    NA_real_
  )

  daily$station <- prep$station
  daily$WYBM <- prep$wybm %||% NA_character_
  daily$date <- fw_as_date(daily$TM)
  daily$C_source <- ifelse(is.finite(daily$C_obs), "observed", "regression")

  daily <- daily[, c("station", "WYBM", "date", "Q", "C_obs", "C_est", "flux", "load_kgd", "method", "C_source"), drop = FALSE]
  daily <- daily[order(daily$date), , drop = FALSE]
  rownames(daily) <- NULL

  fit_data <- data.frame(
    logQ = log(dat_calib$Q),
    logC = log(dat_calib$conc),
    stringsAsFactors = FALSE
  )

  model_compare <- stats::aggregate(
    load_kgd ~ method,
    data = pred_all,
    FUN = function(x) round(sum(x, na.rm = TRUE), 4)
  )
  names(model_compare)[2] <- "total_flux_kg"

  d_start <- if (nrow(daily) > 0 && any(!is.na(daily$date))) as.character(min(daily$date, na.rm = TRUE)) else NA_character_
  d_end   <- if (nrow(daily) > 0 && any(!is.na(daily$date))) as.character(max(daily$date, na.rm = TRUE)) else NA_character_

  list(
    method = "regression",
    method_label = paste0("回归法（", ifelse(model_choice == "loadLm_season", "季节回归", "线性回归"), "）"),
    daily = daily,
    summary = fw_make_flux_summary_table(daily),
    diag = list(
      fit_data = fit_data,
      model_choice = model_choice,
      model_compare = model_compare
    ),
    params = list(
      qf_sheet = prep$qf_sheet,
      wq_sheet = prep$wq_sheet,
      constituent = prep$constituent,
      model_choice = model_choice,
      backend = backend,
      param1 = p1,
      param2 = p2,
      data_source = "step1_qf_wq",
      data_source_label = paste0("第一步数据: QF$", prep$qf_sheet, " + WQ$", prep$wq_sheet, " [", prep$constituent, "]"),
      date_start = d_start,
      date_end = d_end
    )
  )
}

fw_run_flux_with_config <- function(dat_daily = NULL,
                                    method = c("weighted", "interp", "ratio", "regression", "composite"),
                                    date_range = NULL,
                                    param1 = 1,
                                    param2 = 1,
                                    conv_factor = 86.4,
                                    step1_data = NULL,
                                    regression_cfg = list()) {
  method <- match.arg(method)

  to_scalar_num <- function(x, default = 1) {
    x <- suppressWarnings(as.numeric(x))
    if (length(x) == 0 || !is.finite(x[1])) return(default)
    x[1]
  }
  p1 <- to_scalar_num(param1, 1)
  p2 <- to_scalar_num(param2, 1)

  if (identical(method, "regression") && !is.null(step1_data)) {
    return(fw_run_flux_regression_loadflex(
      step1_data = step1_data,
      qf_sheet = regression_cfg$qf_sheet %||% NULL,
      wq_sheet = regression_cfg$wq_sheet %||% NULL,
      constituent = regression_cfg$constituent %||% "TN",
      date_range = date_range,
      model_choice = regression_cfg$model_choice %||% "loadLm_season",
      param1 = p1,
      param2 = p2
    ))
  }

  d <- as.data.frame(dat_daily, stringsAsFactors = FALSE)
  if (!all(c("date", "Q", "C_obs") %in% names(d))) {
    stop("dat_daily must contain: date, Q, C_obs")
  }

  d$date <- fw_as_date(d$date)

  if (!is.null(date_range) && length(date_range) == 2 && all(!is.na(date_range))) {
    s <- as.Date(date_range[1]); e <- as.Date(date_range[2])
    if (s > e) { tmp <- s; s <- e; e <- tmp }
    d <- d[d$date >= s & d$date <= e, , drop = FALSE]
  }

  if (nrow(d) == 0) stop("当前时间范围内无可用数据。")

  res <- fw_run_flux_method(
    dat_daily = d,
    method = method,
    conv_factor = conv_factor
  )

  res$daily$C_est <- fw_as_num(res$daily$C_est) * p1
  res$daily$flux <- ifelse(
    is.finite(res$daily$Q) & is.finite(res$daily$C_est),
    res$daily$Q * res$daily$C_est * conv_factor * p2,
    NA_real_
  )

  if (exists("fw_fill_daily_id_from_source", mode = "function")) {
    res$daily <- fw_fill_daily_id_from_source(res$daily, d, "station", c("station", "STNM_WQ", "STNM"))
    res$daily <- fw_fill_daily_id_from_source(res$daily, d, "WYBM", c("WYBM", "WYBM_WQ"))
  }

  res$summary <- fw_make_flux_summary_table(res$daily)

  d_start <- if (nrow(res$daily) > 0 && any(!is.na(res$daily$date))) as.character(min(res$daily$date, na.rm = TRUE)) else NA_character_
  d_end   <- if (nrow(res$daily) > 0 && any(!is.na(res$daily$date))) as.character(max(res$daily$date, na.rm = TRUE)) else NA_character_

  res$params <- c(
    res$params %||% list(),
    list(
      param1 = p1,
      param2 = p2,
      conv_factor = conv_factor,
      date_start = d_start,
      date_end = d_end
    )
  )

  res
}

fw_rbind_fill <- function(x, y) {
  if (is.null(x) || !is.data.frame(x) || nrow(x) == 0) return(y)
  if (is.null(y) || !is.data.frame(y) || nrow(y) == 0) return(x)

  cols <- union(names(x), names(y))
  for (nm in setdiff(cols, names(x))) x[[nm]] <- NA
  for (nm in setdiff(cols, names(y))) y[[nm]] <- NA
  x <- x[, cols, drop = FALSE]
  y <- y[, cols, drop = FALSE]
  rbind(x, y)
}

fw_init_flux_history <- function(method_keys = c("weighted", "interp", "ratio", "regression", "composite")) {
  out <- setNames(vector("list", length(method_keys)), method_keys)
  for (k in method_keys) {
    out[[k]] <- list(
      meta = data.frame(stringsAsFactors = FALSE),
      items = list()
    )
  }
  out
}

fw_append_flux_history <- function(history, method, res, station = "ALL") {
  if (is.null(history) || !is.list(history)) history <- fw_init_flux_history()
  if (is.null(history[[method]])) history[[method]] <- list(meta = data.frame(stringsAsFactors = FALSE), items = list())
  if (is.null(history[[method]]$meta) || !is.data.frame(history[[method]]$meta)) history[[method]]$meta <- data.frame(stringsAsFactors = FALSE)
  if (is.null(history[[method]]$items) || !is.list(history[[method]]$items)) history[[method]]$items <- list()

  rid <- paste0(format(Sys.time(), "%Y%m%d%H%M%S"), "_", sprintf("%03d", sample.int(999, 1)))
  while (rid %in% names(history[[method]]$items)) {
    rid <- paste0(format(Sys.time(), "%Y%m%d%H%M%S"), "_", sprintf("%03d", sample.int(999, 1)))
  }

  d <- if (!is.null(res) && is.list(res) && is.data.frame(res$daily)) res$daily else data.frame()
  p <- res$params %||% list()

  rec <- data.frame(
    rid = rid,
    created_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    method = method,
    method_label = fw_flux_method_label(method),
    station = as.character(station %||% "ALL"),
    n_days = if (is.data.frame(d)) nrow(d) else NA_integer_,
    total_flux = if (is.data.frame(d) && "flux" %in% names(d)) round(sum(d$flux, na.rm = TRUE), 4) else NA_real_,
    mean_flux = if (is.data.frame(d) && "flux" %in% names(d)) round(mean(d$flux, na.rm = TRUE), 4) else NA_real_,
    param1 = p$param1 %||% NA_real_,
    param2 = p$param2 %||% NA_real_,
    model_choice = p$model_choice %||% NA_character_,
    data_source = p$data_source_label %||% p$data_source %||% NA_character_,
    date_start = p$date_start %||% NA_character_,
    date_end = p$date_end %||% NA_character_,
    stringsAsFactors = FALSE
  )

  history[[method]]$meta <- fw_rbind_fill(history[[method]]$meta, rec)
  history[[method]]$items[[rid]] <- res
  history
}

fw_plot_flux_ts <- function(daily, title = "通量时序") {
  if (is.null(daily) || !is.data.frame(daily) || nrow(daily) == 0) return(plotly::plot_ly())

  d <- as.data.frame(daily, stringsAsFactors = FALSE)
  if (!("date" %in% names(d)) && "TM" %in% names(d)) d$date <- fw_as_date(d$TM)

  d$date <- fw_as_date(d$date)
  d$flux <- fw_as_num(d$flux)
  d <- d[!is.na(d$date), , drop = FALSE]
  d <- d[order(d$date), , drop = FALSE]
  if (nrow(d) == 0) return(plotly::plot_ly())

  p <- plotly::plot_ly(
    d,
    x = ~date,
    y = ~flux,
    type = "scatter",
    mode = "lines+markers",
    name = "Flux",
    hovertemplate = "日期: %{x}<br>通量: %{y:.4f} kg/d<extra></extra>"
  )

  plotly::layout(
    p,
    title = title,
    xaxis = list(title = "日期"),
    yaxis = list(title = "通量 (kg/d)")
  )
}

fw_plot_flux_diag <- function(res) {
  if (is.null(res) || is.null(res$daily) || !is.data.frame(res$daily) || nrow(res$daily) == 0) {
    graphics::plot.new()
    graphics::text(0.5, 0.5, "No diagnostic data")
    return(invisible(NULL))
  }

  d <- as.data.frame(res$daily, stringsAsFactors = FALSE)
  if (!("date" %in% names(d)) && "TM" %in% names(d)) d$date <- fw_as_date(d$TM)

  q <- fw_as_num(d$Q)
  c_obs <- fw_as_num(d$C_obs)
  c_est <- fw_as_num(d$C_est)
  flux <- fw_as_num(d$flux)
  dt <- fw_as_date(d$date)

  op <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(op), add = TRUE)

  graphics::par(mfrow = c(1, 2), mar = c(4, 4, 3, 1))

  graphics::plot(
    q, c_est,
    pch = 1, col = "#2c7fb8",
    xlab = "Q (m3/s)", ylab = "浓度 (mg/L)",
    main = "浓度诊断"
  )
  graphics::points(q, c_obs, pch = 16, col = grDevices::rgb(0.85, 0.2, 0.2, 0.6))
  graphics::legend(
    "topleft",
    legend = c("C_est", "C_obs"),
    pch = c(1, 16),
    col = c("#2c7fb8", grDevices::rgb(0.85, 0.2, 0.2, 0.6)),
    bty = "n", cex = 0.9
  )

  graphics::plot(
    dt, flux,
    type = "h", lwd = 2, col = "#1b9e77",
    xlab = "日期", ylab = "通量 (kg/d)",
    main = "日通量"
  )
  if (any(is.finite(flux))) {
    graphics::abline(h = mean(flux, na.rm = TRUE), lty = 2, col = "#555555")
  }

  invisible(NULL)
}
