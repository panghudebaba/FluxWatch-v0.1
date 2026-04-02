# =====================================================================
# mod_flux_ratio.R
# 比率估计 —— 专属 UI 参数 + Server 初始化 + 数据获取 + 计算调度
# RiverLoad Method 7 (Beale ratio) + Method 8 (Beale ratio by period)
# =====================================================================

if (!exists("%||%", mode = "function")) {
  `%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x
}

# ================================================================
# 原理文本（HTML + MathJax）
# ================================================================

fw_ratio_principle_text <- function(active_sub = NULL) {
  paste0(

    # ---------- 大标题 ----------
    "<h4>\u6bd4\u7387\u4f30\u8ba1\uff08Method 7 &ndash; 8\uff09</h4>",
    "<p>RiverLoad \u4e2d\u57fa\u4e8e Beale \u6bd4\u7387\u7684\u8d1f\u8377\u4f30\u7b97\u65b9\u6cd5\uff0c",
    "\u5229\u7528\u91c7\u6837\u65e5\u8d1f\u8377\u4e0e\u6d41\u91cf\u7684\u7edf\u8ba1\u5173\u7cfb\u4f30\u7b97\u603b\u8d1f\u8377\u3002</p>",

    # ================================================================
    # Method 7
    # ================================================================
    "<h5>Method 7\uff1aBeale ratio\uff08\u6bd4\u5c14\u6bd4\u7387\uff09</h5>",
    "\\[L=\\frac{Q}{\\overline{q}}\\;\\overline{l}\\;",
    "\\left[",
    "\\frac{1+\\frac{1}{n}\\left(\\frac{\\operatorname{Cov}(l,q)}{\\overline{l}\\,\\overline{q}}\\right)}",
    "{1+\\frac{1}{n}\\left(\\frac{\\operatorname{Var}(q)}{\\overline{q}^{\\,2}}\\right)}",
    "\\right]\\]",
    "<ul>",
    "<li>\\(L\\)\uff1a\u6307\u5b9a\u65f6\u6bb5\u603b\u8d1f\u8377</li>",
    "<li>\\(Q\\)\uff1a\u76ee\u6807\u65f6\u6bb5\u603b\u6d41\u91cf\uff08\u7531\u9ad8\u9891\u6d41\u91cf\u8bb0\u5f55\u79ef\u5206/\u7d2f\u52a0\u5f97\u5230\uff09</li>",
    "<li>\\(\\overline{q}\\)\uff1a\u91c7\u6837\u65e5\u7684\u5e73\u5747\u6d41\u91cf</li>",
    "<li>\\(\\overline{l}\\)\uff1a\u91c7\u6837\u65e5\u201c\u65e5\u8d1f\u8377\u201d \\(l\\) \u7684\u5e73\u5747\u503c\uff08\u901a\u5e38 \\(l=CQ\\)\uff09</li>",
    "<li>\\(n\\)\uff1a\u91c7\u6837\u6b21\u6570\uff08\u91c7\u6837\u65e5\u6570\uff09</li>",
    "<li>\\(\\operatorname{Cov}(l,q)\\)\uff1a\u91c7\u6837\u65e5\u8d1f\u8377 \\(l\\) \u4e0e\u91c7\u6837\u65e5\u6d41\u91cf \\(q\\) \u7684\u534f\u65b9\u5dee</li>",
    "<li>\\(\\operatorname{Var}(q)\\)\uff1a\u91c7\u6837\u65e5\u6d41\u91cf \\(q\\) \u7684\u65b9\u5dee</li>",
    "</ul>",
    "<p><b>\u539f\u7406\uff1a</b>\u7528 \\(\\frac{Q}{\\overline{q}}\\overline{l}\\) \u505a\u5c3a\u5ea6\u8f6c\u6362\uff08\u628a\u91c7\u6837\u65e5\u5e73\u5747\u8d1f\u8377\u6269\u5c55\u5230\u5168\u671f\uff09\uff0c",
    "\u518d\u7528\u62ec\u53f7\u4e2d\u7684\u6bd4\u7387\u4fee\u6b63\u9879\u5bf9\u6709\u9650\u6837\u672c\u504f\u5dee\u505a\u7edf\u8ba1\u6821\u6b63\uff08\u4f9d\u8d56\u534f\u65b9\u5dee\u3001\u65b9\u5dee\uff09\u3002",
    "\u5176\u5728\u7406\u8bba\u4e0a\u662f\u7edf\u8ba1\u65e0\u504f\u3001\u7a33\u5065\u7684\u3002",
    "\u4f46\u9700\u6ee1\u8db3\u8d1f\u8377\u4e0e\u6d41\u91cf\u4e4b\u95f4\u8fd1\u4f3c\u6b63\u7ebf\u6027\u4e14\u8fc7\u539f\u70b9\uff0c",
    "\u8d1f\u8377\u65b9\u5dee\u968f\u6d41\u91cf\u65b9\u5dee\u589e\u5927\u800c\u589e\u5927\u7684\u5047\u8bbe\u3002</p>",
    "<p><b>\u9002\u7528\u60c5\u51b5\uff1a</b>\u91c7\u6837\u65e5\u80fd\u4ee3\u8868\u603b\u4f53\u6d41\u91cf\u5206\u5e03\u3001\u6837\u672c\u91cf\u4e0d\u592a\u5c0f\uff1b",
    "\u82e5\u91c7\u6837\u504f\u5009\u4e25\u91cd\u6216 \\(n\\) \u5f88\u5c0f\uff0c\u534f\u65b9\u5dee/\u65b9\u5dee\u4f30\u8ba1\u4e0d\u7a33\uff0c\u4fee\u6b63\u53ef\u80fd\u53cd\u800c\u653e\u5927\u8bef\u5dee\u3002</p>",

    # ================================================================
    # Method 8
    # ================================================================
    "<h5>Method 8\uff1aBeale ratio by period\uff08\u6bd4\u7387\u5206\u671f\uff09</h5>",
    "<p>\u516c\u5f0f\u540c Method 7\uff0c\u4f46 \\(\\overline{q},\\overline{l},\\operatorname{Cov}(l,q),\\operatorname{Var}(q)\\) ",
    "\u5747\u6309\u201c\u6708/\u5e74\u201d\u7b49 period \u5206\u6bb5\u5206\u522b\u8ba1\u7b97\uff08\u518d\u6309 period \u6c47\u603b\u5f97\u5230\u5168\u671f \\(L\\)\uff09\u3002</p>",
    "<ul>",
    "<li><b>\u7b26\u53f7\u610f\u4e49\uff1a</b>\u540c Method 7</li>",
    "</ul>",
    "<p><b>\u539f\u7406\uff1a</b>\u5141\u8bb8\u4e0d\u540c\u5206\u6bb5\u671f\u5185 \\(l\\)\u2013\\(q\\) \u7684\u5173\u7cfb\u4e0d\u540c\uff0c",
    "\u7528\u5206\u6bb5\u7edf\u8ba1\u91cf\u964d\u4f4e\u5b63\u8282\u6027/\u5206\u671f\u5dee\u5f02\u5f15\u5165\u7684\u504f\u5dee\u3002</p>",
    "<p><b>\u9002\u7528\u60c5\u51b5\uff1a</b>\u5173\u7cfb\u5b58\u5728\u660e\u663e\u5b63\u8282/\u5e74\u5ea6\u5206\u6bb5\u7279\u5f81\uff1b",
    "\u4e0d\u9002\u5408\u5206\u6bb5\u540e\u6bcf\u6bb5\u6837\u672c\u8fc7\u5c11\u5bfc\u81f4\u7edf\u8ba1\u91cf\u66f4\u4e0d\u7a33\u5b9a\u3002</p>",

    "<hr>",

    # ---------- Beale 比率修正因子解读 ----------
    "<h5>Beale \u6bd4\u7387\u4fee\u6b63\u56e0\u5b50\u89e3\u8bfb</h5>",
    "<p>\u4fee\u6b63\u56e0\u5b50 = \\(\\frac{1+\\frac{1}{n}\\frac{\\operatorname{Cov}(l,q)}{\\overline{l}\\overline{q}}}",
    "{1+\\frac{1}{n}\\frac{\\operatorname{Var}(q)}{\\overline{q}^2}}\\)</p>",
    "<ul>",
    "<li>\u4fee\u6b63\u56e0\u5b50 &gt; 1\uff1a\u8868\u793a\u8d1f\u8377-\u6d41\u91cf\u534f\u65b9\u5dee\u8f83\u5f3a\uff0c\u5411\u4e0a\u4fee\u6b63\uff08\u9ad8\u6d41\u91cf\u5e26\u6765\u66f4\u591a\u8d1f\u8377\uff09</li>",
    "<li>\u4fee\u6b63\u56e0\u5b50 &asymp; 1\uff1a\u8d1f\u8377\u4e0e\u6d41\u91cf\u51e0\u4e4e\u6210\u6b63\u6bd4</li>",
    "<li>\u4fee\u6b63\u56e0\u5b50 &lt; 1\uff1a\u8d1f\u8377\u4e0e\u6d41\u91cf\u5173\u7cfb\u8f83\u5f31\u6216\u8d1f\u76f8\u5173</li>",
    "</ul>",

    # ---------- 逐日分配 ----------
    "<h5>\u9010\u65e5\u8d1f\u8377\u5206\u914d</h5>",
    "<p>Beale \u6bd4\u7387\u4f30\u7b97\u5f97\u5230\u603b\u8d1f\u8377 \\(L\\) \u540e\uff0c\u6309\u65e5\u6d41\u91cf\u6bd4\u4f8b\u5206\u914d\uff1a</p>",
    "\\[L_j = \\frac{Q_j}{\\sum_{j=1}^{N} Q_j} \\cdot L\\]",
    "<p>\u5176\u4e2d \\(Q_j\\) \u4e3a\u7b2c \\(j\\) \u5929\u6d41\u91cf\uff0c\\(N\\) \u4e3a\u603b\u5929\u6570\u3002</p>",

    "<hr>",
    "<p><i>Method 7/8 \u603b\u8d1f\u8377\u540c\u65f6\u7531 RiverLoad \u5305\u539f\u51fd\u6570\u8ba1\u7b97\uff0c",
    "\u4f9b\u4e0e\u9010\u65e5\u6c47\u603b\u7ed3\u679c\u4ea4\u53c9\u6821\u9a8c\u3002</i></p>"
  )
}

# ================================================================
# UI 参数块
# ================================================================

fw_ratio_left_extra_ui <- function(ns, key = "ratio") {
  shiny::tagList(
    shiny::helpText("\u6bd4\u7387\u4f30\u8ba1\u8c03\u7528\u7b2c\u4e00\u6b65 QF/WQ \u6570\u636e + RiverLoad Method 7\u20138"),
    shiny::selectInput(ns(paste0("qf_sheet_", key)), "QF\u6570\u636e\u8868", choices = NULL),
    shiny::selectInput(ns(paste0("wq_sheet_", key)), "WQ\u6570\u636e\u8868", choices = NULL),
    shiny::selectInput(ns(paste0("constituent_", key)), "\u6c34\u8d28\u6307\u6807(j)", choices = NULL),
    shiny::selectInput(
      ns(paste0("sub_method_", key)),
      "\u6bd4\u7387\u5b50\u65b9\u6cd5",
      choices = c(
        "Beale \u6bd4\u7387\u4f30\u8ba1 (Method 7)" = "method7",
        "Beale \u6bd4\u7387\u5206\u671f (Method 8)" = "method8"
      ),
      selected = "method7"
    )
  )
}

# ================================================================
# Server：QF/WQ 数据源 reactive
# ================================================================

fw_ratio_step1_reactive <- function(rv) {
  shiny::reactive({
    fw_get_step1_qf_wq(rv)
  })
}

# ================================================================
# Server：QF/WQ 表名与指标联动 observers
# ================================================================

fw_ratio_init_observers <- function(input, session, step1_qf_wq) {

  shiny::observe({
    s1 <- step1_qf_wq()
    if (is.null(s1)) {
      shiny::updateSelectInput(session, "qf_sheet_ratio", choices = character(0))
      shiny::updateSelectInput(session, "wq_sheet_ratio", choices = character(0))
      return()
    }

    qf_list <- fw_as_named_table_list(s1$QF, "QF")
    wq_list <- fw_as_named_table_list(s1$WQ, "WQ")
    if (length(qf_list) == 0 || length(wq_list) == 0) {
      shiny::updateSelectInput(session, "qf_sheet_ratio", choices = character(0))
      shiny::updateSelectInput(session, "wq_sheet_ratio", choices = character(0))
      return()
    }

    qf_nm <- names(qf_list); wq_nm <- names(wq_list)

    cur_qf <- shiny::isolate(input$qf_sheet_ratio)
    cur_wq <- shiny::isolate(input$wq_sheet_ratio)
    if (is.null(cur_qf) || !(cur_qf %in% qf_nm)) cur_qf <- qf_nm[1]
    if (is.null(cur_wq) || !(cur_wq %in% wq_nm)) cur_wq <- wq_nm[1]

    shiny::updateSelectInput(session, "qf_sheet_ratio", choices = qf_nm, selected = cur_qf)
    shiny::updateSelectInput(session, "wq_sheet_ratio", choices = wq_nm, selected = cur_wq)
  })

  shiny::observe({
    s1 <- step1_qf_wq()
    if (is.null(s1)) {
      shiny::updateSelectInput(session, "constituent_ratio", choices = character(0))
      return()
    }

    wq_list <- fw_as_named_table_list(s1$WQ, "WQ")
    if (length(wq_list) == 0) {
      shiny::updateSelectInput(session, "constituent_ratio", choices = character(0))
      return()
    }

    ws <- input$wq_sheet_ratio
    if (is.null(ws) || !(ws %in% names(wq_list))) ws <- names(wq_list)[1]

    cands <- fw_get_wq_constituents(wq_list[[ws]])
    if (length(cands) == 0) cands <- names(wq_list[[ws]])

    cur <- shiny::isolate(input$constituent_ratio)
    if (is.null(cur) || !(cur %in% cands)) cur <- cands[1]

    shiny::updateSelectInput(session, "constituent_ratio", choices = cands, selected = cur)
  })
}

# ================================================================
# Server：数据获取（daily_all）
# ================================================================

fw_ratio_get_daily_all <- function(input, step1_qf_wq) {
  s1 <- step1_qf_wq()
  if (is.null(s1)) return(NULL)

  prep <- tryCatch(
    fw_prepare_ratio_input(
      step1_data  = s1,
      qf_sheet    = input$qf_sheet_ratio,
      wq_sheet    = input$wq_sheet_ratio,
      constituent = input$constituent_ratio,
      date_range  = NULL
    ),
    error = function(e) NULL
  )
  if (is.null(prep)) return(NULL)

  data.frame(
    station = prep$station,
    WYBM    = prep$wybm %||% NA_character_,
    date    = prep$dat$TM,
    Q       = prep$dat$Q,
    C_obs   = prep$dat$conc,
    stringsAsFactors = FALSE
  )
}

# ================================================================
# Server：计算调度
# ================================================================

fw_ratio_run_calc <- function(input, step1_qf_wq, key = "ratio") {
  s1 <- step1_qf_wq()
  if (is.null(s1)) {
    shiny::showNotification("\u672a\u627e\u5230\u7b2c\u4e00\u6b65 QF/WQ \u6570\u636e\u3002", type = "error")
    return(NULL)
  }

  res <- tryCatch(
    fw_run_flux_ratio(
      step1_data  = s1,
      qf_sheet    = input[[paste0("qf_sheet_", key)]],
      wq_sheet    = input[[paste0("wq_sheet_", key)]],
      constituent = input[[paste0("constituent_", key)]],
      date_range  = input[[paste0("daterange_", key)]],
      sub_method  = input[[paste0("sub_method_", key)]] %||% "method7",
      conv_factor = 86.4
    ),
    error = function(e) {
      shiny::showNotification(e$message, type = "error")
      NULL
    }
  )
  res
}

# ================================================================
# Server：结算文本额外信息
# ================================================================

fw_ratio_settle_extra <- function(res) {
  if (is.null(res)) return("")
  diag <- res$diag %||% list()
  bd   <- diag$beale_diag %||% list()

  beale_lines <- ""
  if (length(bd) > 0) {
    beale_lines <- paste0(
      "\nBeale\u4fee\u6b63\u56e0\u5b50: ", bd$beale_ratio %||% "NA",
      "\n\u91c7\u6837\u65e5\u5747\u6d41\u91cf(m3/s): ", bd$q_bar %||% "NA",
      "\n\u91c7\u6837\u65e5\u5747\u8d1f\u8377(kg/d): ", bd$l_bar %||% "NA",
      "\nCov(l,q): ", bd$cov_lq %||% "NA",
      "\nVar(q): ", bd$var_q %||% "NA",
      "\n\u91c7\u6837\u6570: ", bd$n_sample %||% "NA",
      if (isTRUE(bd$fallback)) "\n[\u6ce8\u610f: \u5df2\u9000\u5316\u4e3a\u7b80\u5355\u4f30\u7b97]" else ""
    )
  }

  period_lines <- ""
  pd <- diag$period_diag
  if (!is.null(pd) && is.data.frame(pd) && nrow(pd) > 0) {
    period_lines <- paste0(
      "\n\u5206\u671f\u6570: ", nrow(pd),
      "\n\u5206\u671f\u6709\u6837\u672c\u6570: ", sum(pd$n_obs > 0),
      "\n\u5206\u671f\u65e0\u6837\u672c\u6570: ", sum(pd$n_obs == 0)
    )
  }

  paste0(
    "\nQF\u8868: ",   res$params$qf_sheet    %||% "",
    "\nWQ\u8868: ",   res$params$wq_sheet    %||% "",
    "\n\u6307\u6807: ", res$params$constituent %||% "",
    "\n\u5b50\u65b9\u6cd5: ", diag$sub_label  %||% res$params$sub_method %||% "",
    "\n\u76d1\u6d4b\u5929\u6570: ", diag$n_obs %||% "",
    "\n\u8ba1\u7b97\u5929\u6570: ", diag$n_total_days %||% "",
    beale_lines,
    period_lines,
    "\n\u9010\u65e5\u6c47\u603b(kg): ", diag$daily_sum_kg %||% "",
    "\nRiverLoad\u603b\u8d1f\u8377(kg): ", diag$rl_this_kg %||% "NA"
  )
}

# ================================================================
# Server：比率法专属诊断图
# ================================================================

fw_ratio_render_diag <- function(res) {
  if (is.null(res) || is.null(res$daily) || !is.data.frame(res$daily) || nrow(res$daily) == 0) {
    graphics::plot.new(); graphics::text(0.5, 0.5, "No diagnostic data")
    return(invisible(NULL))
  }

  d <- as.data.frame(res$daily, stringsAsFactors = FALSE)
  dt    <- fw_as_date(d$date)
  q     <- fw_as_num(d$Q)
  c_obs <- fw_as_num(d$C_obs)
  c_est <- fw_as_num(d$C_est)
  flux  <- fw_as_num(d$flux)

  op <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(op), add = TRUE)

  # Method 8 有分期表 → 2x3 布局，否则 2x2
  has_period <- !is.null(res$diag$period_diag) && is.data.frame(res$diag$period_diag) && nrow(res$diag$period_diag) > 0
  if (has_period) {
    graphics::par(mfrow = c(2, 3), mar = c(4, 4, 3, 1))
  } else {
    graphics::par(mfrow = c(2, 2), mar = c(4, 4, 3, 1))
  }

  # 1) 浓度时序：观测 vs 估算
  ylim <- range(c(c_obs, c_est), na.rm = TRUE, finite = TRUE)
  if (all(!is.finite(ylim))) ylim <- c(0, 1)
  graphics::plot(dt, c_est, type = "l", col = "#2c7fb8", lwd = 1.5,
                 xlab = "\u65e5\u671f", ylab = "\u6d53\u5ea6 (mg/L)",
                 main = "\u6d53\u5ea6\uff1a\u89c2\u6d4b vs \u4f30\u7b97", ylim = ylim)
  obs_ok <- which(!is.na(c_obs))
  if (length(obs_ok) > 0)
    graphics::points(dt[obs_ok], c_obs[obs_ok],
                     pch = 16, col = grDevices::rgb(0.85, 0.2, 0.2, 0.7), cex = 1.2)
  graphics::legend("topleft", legend = c("C_est (Beale)", "C_obs (\u5b9e\u6d4b)"),
                   lty = c(1, NA), pch = c(NA, 16),
                   col = c("#2c7fb8", grDevices::rgb(0.85, 0.2, 0.2, 0.7)),
                   bty = "n", cex = 0.85)

  # 2) 日通量柱状图
  graphics::plot(dt, flux, type = "h", lwd = 2, col = "#1b9e77",
                 xlab = "\u65e5\u671f", ylab = "\u901a\u91cf (kg/d)",
                 main = "\u9010\u65e5\u901a\u91cf")
  if (any(is.finite(flux)))
    graphics::abline(h = mean(flux, na.rm = TRUE), lty = 2, col = "#555555")

  # 3) Q vs 日负荷散点（采样日）
  obs_load <- c_obs * q * 86.4
  graphics::plot(q, obs_load, pch = 1, col = "#cccccc",
                 xlab = "Q (m\u00b3/s)", ylab = "\u65e5\u8d1f\u8377 l = CQ\u00d7K (kg/d)",
                 main = "Q\u2013l \u5173\u7cfb\uff08Beale \u57fa\u7840\uff09")
  if (length(obs_ok) > 0) {
    graphics::points(q[obs_ok], obs_load[obs_ok], pch = 16,
                     col = grDevices::rgb(0.85, 0.2, 0.2, 0.7), cex = 1.2)
    # 回归参考线
    if (length(obs_ok) >= 3) {
      fit <- tryCatch(stats::lm(obs_load[obs_ok] ~ q[obs_ok]), error = function(e) NULL)
      if (!is.null(fit)) graphics::abline(fit, lty = 2, col = "#e41a1c")
    }
  }
  graphics::legend("topleft",
                   legend = c("\u91c7\u6837\u65e5", "\u5168\u90e8\u65e5"),
                   pch = c(16, 1),
                   col = c(grDevices::rgb(0.85, 0.2, 0.2, 0.7), "#cccccc"),
                   bty = "n", cex = 0.85)

  # 4) RiverLoad method1-8 总负荷对比
  # 4) RiverLoad method1-6 + rating + rating.period 总负荷对比
  mc <- res$diag$model_compare
  if (!is.null(mc) && is.data.frame(mc) && nrow(mc) > 0 && any(is.finite(mc$total_load_kg))) {
    vals <- mc$total_load_kg
    nms  <- mc$method

    # 将当前 sub_method 映射到 model_compare 中的名称
    cur_rl_name <- switch(res$diag$sub_method,
                          method7 = "rating",
                          method8 = "rating.period",
                          res$diag$sub_method
    )
    cols <- ifelse(nms == cur_rl_name, "#e41a1c", "#4daf4a")

    bp <- graphics::barplot(vals, names.arg = nms, col = cols,
                            main = "RiverLoad \u603b\u8d1f\u8377\u5bf9\u6bd4 (kg)",
                            ylab = "\u603b\u8d1f\u8377 (kg)", las = 2, cex.names = 0.7)
    graphics::text(bp, vals, labels = round(vals, 1), pos = 3, cex = 0.65)

    dsk <- res$diag$daily_sum_kg
    if (is.finite(dsk %||% NA_real_)) {
      graphics::abline(h = dsk, lty = 2, col = "#ff7f00", lwd = 1.5)
      graphics::legend("topright",
                       legend = c(paste0("\u5f53\u524d: ", cur_rl_name), "\u5176\u4ed6 Method",
                                  paste0("\u9010\u65e5\u6c47\u603b: ", round(dsk, 1))),
                       fill = c("#e41a1c", "#4daf4a", NA),
                       border = c("black", "black", NA),
                       lty = c(NA, NA, 2), col = c(NA, NA, "#ff7f00"),
                       bty = "n", cex = 0.65)
    } else {
      graphics::legend("topright",
                       legend = c(paste0("\u5f53\u524d: ", cur_rl_name), "\u5176\u4ed6"),
                       fill = c("#e41a1c", "#4daf4a"), bty = "n", cex = 0.7)
    }
  } else {
    graphics::plot.new()
    graphics::text(0.5, 0.5, "RiverLoad \u5bf9\u6bd4\u4e0d\u53ef\u7528", cex = 0.9)
  }


  # 5) Method 8 分期详情（仅当有分期数据时）
  if (has_period) {
    pd <- res$diag$period_diag

    # 分期负荷柱状图
    cols_p <- ifelse(pd$n_obs > 0, "#377eb8", "#d95f02")
    bp2 <- graphics::barplot(pd$L_total_kg, names.arg = pd$period, col = cols_p,
                             main = "\u5206\u671f\u8d1f\u8377 (kg)",
                             ylab = "L (kg)", las = 2, cex.names = 0.65)
    graphics::text(bp2, pd$L_total_kg, labels = round(pd$L_total_kg, 1),
                   pos = 3, cex = 0.6)
    graphics::legend("topright",
                     legend = c("\u6709\u91c7\u6837", "\u65e0\u91c7\u6837(\u5168\u5c40\u9000\u5316)"),
                     fill = c("#377eb8", "#d95f02"), bty = "n", cex = 0.7)

    # 分期 Beale 修正因子
    br_vals <- pd$beale_ratio
    if (any(is.finite(br_vals))) {
      graphics::barplot(br_vals, names.arg = pd$period,
                        col = ifelse(br_vals > 1, "#e41a1c", "#4daf4a"),
                        main = "\u5206\u671f Beale \u4fee\u6b63\u56e0\u5b50",
                        ylab = "\u4fee\u6b63\u56e0\u5b50", las = 2, cex.names = 0.65)
      graphics::abline(h = 1, lty = 2, col = "#555555")
    } else {
      graphics::plot.new()
      graphics::text(0.5, 0.5, "Beale \u56e0\u5b50\u4e0d\u53ef\u7528", cex = 0.9)
    }
  }

  invisible(NULL)
}
