# =====================================================================
# fct_design.R
# 采样优化设计模块 —— 核心业务逻辑函数
# =====================================================================

if (!exists("%||%", mode = "function")) {
  `%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x
}

# -----------------------------------------------------------------
# 1) 计算排除行索引（各种抽稀方案）
# -----------------------------------------------------------------

fct_design_compute_excluded <- function(dat, scheme,
                                        sys_interval = 4,
                                        random_pct   = 50,
                                        random_seed  = 42,
                                        freq_str     = "24h") {
  n <- nrow(dat)
  if (n == 0) return(integer(0))

  if (scheme == "systematic") {
    k      <- as.integer(sys_interval)
    retain <- seq(1, n, by = k)
    return(setdiff(seq_len(n), retain))
  }

  if (scheme == "random") {
    set.seed(random_seed)
    n_retain <- max(1, round(n * random_pct / 100))
    retain   <- sort(sample(seq_len(n), n_retain))
    return(setdiff(seq_len(n), retain))
  }

  if (scheme == "frequency") {
    freq_sec <- tryCatch({
      val  <- as.numeric(gsub("[^0-9.]", "", freq_str))
      unit <- gsub("[0-9.]", "", freq_str)
      switch(unit, "h" = val * 3600, "d" = val * 86400, val * 3600)
    }, error = function(e) 86400)

    # 查找时间列
    tc <- NULL
    for (c_ in c("TM", "date", "datetime", "DateTime")) {
      if (c_ %in% names(dat)) { tc <- c_; break }
    }
    if (is.null(tc)) return(integer(0))

    times <- tryCatch(as.POSIXct(dat[[tc]]), error = function(e) NULL)
    if (is.null(times) || all(is.na(times))) return(integer(0))

    retain <- 1
    last_t <- times[1]
    for (i in 2:n) {
      if (!is.na(times[i]) &&
          as.numeric(difftime(times[i], last_t, units = "secs")) >= freq_sec) {
        retain <- c(retain, i)
        last_t <- times[i]
      }
    }
    return(setdiff(seq_len(n), retain))
  }

  # manual 或未知方案
  integer(0)
}

# -----------------------------------------------------------------
# 2) 清理数据帧——去除 DT 辅助列
# -----------------------------------------------------------------

fct_design_clean_df <- function(df) {
  drop <- c("Selected", ".row_id")
  df[, !(names(df) %in% drop), drop = FALSE]
}

# -----------------------------------------------------------------
# 3) 单方法通量计算（核心函数，含三层容错）
# -----------------------------------------------------------------

fct_design_run_one_method <- function(df, method,
                                      conv_factor    = 86.4,
                                      reg_submethod  = "abs_linear",
                                      reg_log        = FALSE,
                                      reg_smear      = TRUE,
                                      comp_submethod = "beale",
                                      comp_nstrata   = 3) {

  # ==== 数据预检 ====
  if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) return(NULL)
  if (!("Q" %in% names(df))) return(NULL)

  # ==== A) 基础方法: weighted / interp / ratio ====
  if (method %in% c("weighted", "interp", "ratio")) {
    if (exists("fw_run_flux_with_config", mode = "function")) {
      return(tryCatch(
        fw_run_flux_with_config(
          dat_daily   = df,
          method      = method,
          conv_factor = conv_factor
        ),
        error = function(e) {
          message("[fct_design] ", method, " 失败: ", e$message)
          NULL
        }
      ))
    }
    return(NULL)
  }

  # ==== B) 回归法 ====
  if (method == "regression") {

    # Layer 1: fw_run_flux_with_config
    if (exists("fw_run_flux_with_config", mode = "function")) {
      res <- tryCatch(
        fw_run_flux_with_config(
          dat_daily   = df,
          method      = "regression",
          conv_factor = conv_factor,
          reg_method  = reg_submethod,
          log_c       = reg_log,
          smearing    = reg_smear
        ),
        error = function(e) NULL
      )
      if (!is.null(res)) return(res)
    }

    # Layer 2: fw_calc_flux_regression
    if (exists("fw_calc_flux_regression", mode = "function")) {
      res <- tryCatch(
        fw_calc_flux_regression(
          dat_daily   = df,
          sub_method  = reg_submethod,
          log_c       = reg_log,
          smearing    = reg_smear,
          conv_factor = conv_factor
        ),
        error = function(e) NULL
      )
      if (!is.null(res)) return(res)
    }

    # Layer 3: 手动回归
    return(fct_design_regression_manual(
      df, reg_submethod, reg_log, reg_smear, conv_factor))
  }

  # ==== C) 复合法 ====
  if (method == "composite") {

    # Layer 1
    if (exists("fw_run_flux_with_config", mode = "function")) {
      res <- tryCatch(
        fw_run_flux_with_config(
          dat_daily   = df,
          method      = "composite",
          conv_factor = conv_factor,
          comp_method = comp_submethod,
          n_strata    = comp_nstrata
        ),
        error = function(e) NULL
      )
      if (!is.null(res)) return(res)
    }

    # Layer 2
    if (exists("fw_calc_flux_composite", mode = "function")) {
      res <- tryCatch(
        fw_calc_flux_composite(
          dat_daily   = df,
          sub_method  = comp_submethod,
          n_strata    = comp_nstrata,
          conv_factor = conv_factor
        ),
        error = function(e) NULL
      )
      if (!is.null(res)) return(res)
    }

    # Layer 3: 手动 Beale
    return(fct_design_composite_manual(
      df, comp_submethod, comp_nstrata, conv_factor))
  }

  NULL
}

# -----------------------------------------------------------------
# 3a) 手动回归法实现（修复 "对象不是矩阵" 错误）
# -----------------------------------------------------------------

fct_design_regression_manual <- function(df, sub_method, use_log, use_smear, conv_factor) {
  tryCatch({
    # 确保 Q 和 C 都是数值型
    df$Q <- as.numeric(df$Q)
    if ("C" %in% names(df)) {
      df$C <- as.numeric(df$C)
    } else if ("conc" %in% names(df)) {
      df$C <- as.numeric(df$conc)
    } else {
      message("[fct_design] regression: 找不到浓度列 C 或 conc")
      return(NULL)
    }

    # 校准子集：Q > 0 且 C 非 NA
    df_ok <- df[!is.na(df$Q) & is.finite(df$Q) & df$Q > 0 &
                  !is.na(df$C) & is.finite(df$C) & df$C > 0, , drop = FALSE]
    if (nrow(df_ok) < 3) {
      message("[fct_design] regression: 校准数据不足 (n=", nrow(df_ok), ")")
      return(NULL)
    }

    # ---- 构建校准 data.frame（仅含数值列）----
    calib <- data.frame(
      Q   = df_ok$Q,
      C   = df_ok$C,
      stringsAsFactors = FALSE
    )

    # ---- 预测用 data.frame ----
    pred_df <- data.frame(Q = df$Q, stringsAsFactors = FALSE)

    # ---- 拟合模型 ----
    fit     <- NULL
    is_log  <- FALSE
    r2      <- NA_real_

    if (use_log && all(calib$Q > 0) && all(calib$C > 0)) {
      # 对数模型
      calib$logQ <- log(calib$Q)
      calib$logC <- log(calib$C)
      fit    <- stats::lm(logC ~ logQ, data = calib)
      is_log <- TRUE

      pred_df$logQ <- log(pmax(pred_df$Q, 1e-10))
      df$C_est <- exp(stats::predict(fit, newdata = pred_df))

      if (use_smear) {
        sigma2   <- summary(fit)$sigma^2
        df$C_est <- df$C_est * exp(sigma2 / 2)
      }

    } else {
      # 非对数模型
      if (sub_method == "poly2") {
        calib$Q2 <- calib$Q^2
        fit <- stats::lm(C ~ Q + Q2, data = calib)
        pred_df$Q2 <- pred_df$Q^2
      } else if (sub_method == "power" && all(calib$Q > 0) && all(calib$C > 0)) {
        calib$logQ <- log(calib$Q)
        calib$logC <- log(calib$C)
        fit    <- stats::lm(logC ~ logQ, data = calib)
        is_log <- TRUE
        pred_df$logQ <- log(pmax(pred_df$Q, 1e-10))
        df$C_est <- exp(stats::predict(fit, newdata = pred_df))
        if (use_smear) {
          sigma2   <- summary(fit)$sigma^2
          df$C_est <- df$C_est * exp(sigma2 / 2)
        }
      } else {
        # 默认线性: C ~ Q
        fit <- stats::lm(C ~ Q, data = calib)
      }

      # 如果不是 log 模型，在这里 predict
      if (!is_log) {
        df$C_est <- stats::predict(fit, newdata = pred_df)
        df$C_est <- pmax(df$C_est, 0)  # 浓度不能为负
      }
    }

    r2 <- tryCatch(summary(fit)$r.squared, error = function(e) NA_real_)

    # 有实测浓度的优先使用实测值
    has_obs <- !is.na(df$C) & is.finite(df$C)
    df$C_est[has_obs] <- df$C[has_obs]

    # 计算通量
    df$C_est <- as.numeric(df$C_est)
    df$Q     <- as.numeric(df$Q)
    df$flux  <- df$Q * df$C_est * conv_factor / 1000
    df$flux[!is.finite(df$flux)] <- NA_real_

    list(
      daily        = df,
      method       = "regression",
      method_label = paste0("回归法(", sub_method, ")"),
      params       = list(sub_method = sub_method, log_c = use_log,
                          smearing = use_smear, r_squared = r2),
      summary      = NULL
    )
  }, error = function(e) {
    message("[fct_design] regression manual 失败: ", e$message)
    NULL
  })
}

# -----------------------------------------------------------------
# 3b) 手动复合法实现（Beale 比率估计）
# -----------------------------------------------------------------

fct_design_composite_manual <- function(df, sub_method, n_strata, conv_factor) {
  tryCatch({
    df$Q <- as.numeric(df$Q)
    if ("C" %in% names(df)) {
      df$C <- as.numeric(df$C)
    } else if ("conc" %in% names(df)) {
      df$C <- as.numeric(df$conc)
    } else {
      return(NULL)
    }

    df_ok <- df[!is.na(df$Q) & is.finite(df$Q) &
                  !is.na(df$C) & is.finite(df$C), , drop = FALSE]
    if (nrow(df_ok) < 3) return(NULL)

    n_s       <- nrow(df_ok)
    q_bar_s   <- mean(df_ok$Q, na.rm = TRUE)
    q_bar_all <- mean(df$Q, na.rm = TRUE)
    load_s    <- df_ok$Q * df_ok$C
    l_bar     <- mean(load_s, na.rm = TRUE)

    cov_ql <- stats::cov(df_ok$Q, load_s)
    var_q  <- stats::var(df_ok$Q)

    theta <- if (abs(var_q) > 1e-12 && abs(q_bar_s) > 1e-12 && abs(l_bar) > 1e-12) {
      (1 + (1 / n_s) * cov_ql / (q_bar_s * l_bar)) /
        (1 + (1 / n_s) * var_q  / (q_bar_s^2))
    } else 1.0

    ratio_adj <- (l_bar / q_bar_s) * theta

    # C_est: 有实测用实测，无实测用 Beale 估计
    df$C_est <- ratio_adj
    has_obs  <- !is.na(df$C) & is.finite(df$C)
    df$C_est[has_obs] <- df$C[has_obs]

    df$flux <- df$Q * df$C_est * conv_factor / 1000
    df$flux[!is.finite(df$flux)] <- NA_real_

    list(
      daily        = df,
      method       = "composite",
      method_label = paste0("复合法(", sub_method, ")"),
      params       = list(sub_method  = sub_method,
                          n_strata    = n_strata,
                          beale_theta = round(theta, 4)),
      summary      = NULL
    )
  }, error = function(e) {
    message("[fct_design] composite manual 失败: ", e$message)
    NULL
  })
}

# -----------------------------------------------------------------
# 4) 运行全量 vs 抽稀的完整对比
# -----------------------------------------------------------------

fct_design_run_comparison <- function(df_full, df_thin, methods,
                                      conv_factor    = 86.4,
                                      reg_submethod  = "abs_linear",
                                      reg_log        = FALSE,
                                      reg_smear      = TRUE,
                                      comp_submethod = "beale",
                                      comp_nstrata   = 3,
                                      progress_fn    = NULL) {

  results <- list()
  errors  <- character(0)
  total   <- length(methods) * 2
  step    <- 0

  for (m in methods) {
    # 全量
    step <- step + 1
    if (is.function(progress_fn))
      progress_fn(1 / total, paste0(utils_design_method_label(m), " (全量)"))

    res_full <- fct_design_run_one_method(
      df = df_full, method = m, conv_factor = conv_factor,
      reg_submethod = reg_submethod, reg_log = reg_log, reg_smear = reg_smear,
      comp_submethod = comp_submethod, comp_nstrata = comp_nstrata
    )

    # 抽稀
    step <- step + 1
    if (is.function(progress_fn))
      progress_fn(1 / total, paste0(utils_design_method_label(m), " (抽稀)"))

    res_thin <- fct_design_run_one_method(
      df = df_thin, method = m, conv_factor = conv_factor,
      reg_submethod = reg_submethod, reg_log = reg_log, reg_smear = reg_smear,
      comp_submethod = comp_submethod, comp_nstrata = comp_nstrata
    )

    if (is.null(res_full) && is.null(res_thin)) {
      errors <- c(errors, paste0(utils_design_method_label(m), ": 全量和抽稀均计算失败"))
    } else if (is.null(res_full)) {
      errors <- c(errors, paste0(utils_design_method_label(m), ": 全量计算失败"))
    } else if (is.null(res_thin)) {
      errors <- c(errors, paste0(utils_design_method_label(m), ": 抽稀计算失败"))
    }

    results[[m]] <- list(full = res_full, thin = res_thin)
  }

  list(results = results, errors = errors)
}

# -----------------------------------------------------------------
# 5) 构建对比汇总表
# -----------------------------------------------------------------

fct_design_build_summary <- function(results, methods,
                                     reg_submethod  = "abs_linear",
                                     reg_log        = FALSE,
                                     comp_submethod = "beale") {

  rows <- lapply(methods, function(m) {
    rf <- results[[m]]$full
    rt <- results[[m]]$thin

    flux_full <- if (!is.null(rf) && !is.null(rf$daily))
      sum(rf$daily$flux, na.rm = TRUE) else NA_real_
    flux_thin <- if (!is.null(rt) && !is.null(rt$daily))
      sum(rt$daily$flux, na.rm = TRUE) else NA_real_
    mean_full <- if (!is.null(rf) && !is.null(rf$daily))
      mean(rf$daily$flux, na.rm = TRUE) else NA_real_
    mean_thin <- if (!is.null(rt) && !is.null(rt$daily))
      mean(rt$daily$flux, na.rm = TRUE) else NA_real_

    abs_err <- flux_thin - flux_full
    re_pct  <- if (!is.na(flux_full) && abs(flux_full) > 1e-12)
      round(abs(abs_err) / abs(flux_full) * 100, 2) else NA_real_

    # 额外参数信息
    extra <- ""
    if (m == "regression") {
      r2 <- rf$params$r_squared %||% rt$params$r_squared %||% NA
      extra <- paste0(
        reg_submethod,
        if (reg_log) " [log]" else "",
        if (!is.na(r2)) sprintf(" R\u00b2=%.3f", r2) else ""
      )
    } else if (m == "composite") {
      theta <- rf$params$beale_theta %||% rt$params$beale_theta %||% NA
      extra <- paste0(
        comp_submethod,
        if (!is.na(theta)) sprintf(" \u03b8=%.3f", theta) else ""
      )
    }

    data.frame(
      method_key    = m,
      method_label  = utils_design_method_label(m),
      sub_info      = extra,
      n_full        = if (!is.null(rf) && !is.null(rf$daily)) nrow(rf$daily) else NA_integer_,
      n_thin        = if (!is.null(rt) && !is.null(rt$daily)) nrow(rt$daily) else NA_integer_,
      flux_full_kg  = round(flux_full, 4),
      flux_thin_kg  = round(flux_thin, 4),
      mean_full_kgd = round(mean_full, 4),
      mean_thin_kgd = round(mean_thin, 4),
      abs_error_kg  = round(abs_err, 4),
      RE_pct        = re_pct,
      stringsAsFactors = FALSE
    )
  })

  summary_df <- do.call(rbind, rows)
  rownames(summary_df) <- NULL
  summary_df
}

# -----------------------------------------------------------------
# 6) 最优方案文本生成
# -----------------------------------------------------------------

fct_design_best_text <- function(cr) {
  if (is.null(cr)) return("尚未运行计算。")

  tbl <- cr$summary
  ok  <- tbl[!is.na(tbl$RE_pct), , drop = FALSE]
  if (nrow(ok) == 0) return("无有效结果。")

  ok_sorted <- ok[order(ok$RE_pct), , drop = FALSE]
  best      <- ok_sorted[1, ]

  conclusion <- if (best$RE_pct <= 5) "\u2605 优秀，强烈推荐"
  else if (best$RE_pct <= 10) "\u2714 良好，可以采用"
  else if (best$RE_pct <= 15) "\u26a0 尚可，需注意误差"
  else "\u2718 误差过大，不推荐"

  scheme_label <- switch(
    cr$scheme %||% "?",
    "manual"     = "手动选择",
    "systematic" = "等间隔抽稀",
    "random"     = "随机抽稀",
    "frequency"  = "按频率抽稀",
    cr$scheme
  )

  ranking <- paste(sprintf(
    "  %d. %s%s: RE = %s%%",
    seq_len(nrow(ok_sorted)),
    ok_sorted$method_label,
    ifelse(ok_sorted$sub_info != "", paste0("(", ok_sorted$sub_info, ")"), ""),
    ok_sorted$RE_pct
  ), collapse = "\n")

  sprintf(
    paste0(
      "水质指标: %s\n",
      "方案: %s\n",
      "保留样本: %d / %d (%s%%)\n",
      "------------------------------\n",
      "最优方法: %s%s\n",
      "相对误差: %s%%\n",
      "全量通量: %s kg\n",
      "抽稀通量: %s kg\n",
      "------------------------------\n",
      "结论: %s\n",
      "\n各方法 RE 排名:\n%s"
    ),
    cr$target %||% "?",
    scheme_label,
    cr$n_thin, cr$n_full, cr$retain_pct,
    best$method_label,
    if (best$sub_info != "") paste0(" (", best$sub_info, ")") else "",
    best$RE_pct,
    best$flux_full_kg,
    best$flux_thin_kg,
    conclusion,
    ranking
  )
}
