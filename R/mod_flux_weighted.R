# =====================================================================
# mod_flux_weighted.R
# 加权平均法 —— 专属 UI 参数 + Server 初始化 + 数据获取 + 计算调度
# RiverLoad Method 1–5 封装
# =====================================================================

if (!exists("%||%", mode = "function")) {
  `%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x
}

# ================================================================
# 原理文本（HTML + MathJax）—— Method 1–5
# ================================================================

fw_weighted_principle_text <- function(active_sub = NULL) {
  paste0(

    # ---------- 大标题 ----------
    "<h4>\u52a0\u6743\u5e73\u5747\u6cd5\uff08Method 1 &ndash; 5\uff09</h4>",
    "<p>RiverLoad 5 \u4e2a\u8d1f\u8377\u4f30\u7b97\u65b9\u6cd5\uff0c",
    "\u5206\u522b\u91c7\u7528\u4e0d\u540c\u7684\u65f6\u95f4/\u6d41\u91cf\u52a0\u6743\u7b56\u7565\u4ece\u79bb\u6563\u91c7\u6837\u4f30\u7b97\u603b\u8d1f\u8377\u3002</p>",

    # ================================================================
    # Method 1
    # ================================================================
    "<h5>Method 1\uff1atime-weighted Q and C\uff08\u65f6\u95f4\u52a0\u6743\u7684 Q \u4e0e C\uff09</h5>",
    "\\[L = K\\left(\\frac{1}{n}\\sum_{i=1}^{n} C_i\\right)",
    "\\left(\\frac{1}{n}\\sum_{i=1}^{n} Q_i\\right)\\]",
    "<ul>",
    "<li>\\(L\\)\uff1a\u6307\u5b9a\u65f6\u6bb5\u603b\u8d1f\u8377\uff08load\uff09</li>",
    "<li>\\(K\\)\uff1a\u5355\u4f4d/\u65f6\u6bb5\u6362\u7b97\u56e0\u5b50\uff08\u4e0e\u6d53\u5ea6\u3001\u6d41\u91cf\u5355\u4f4d\u53ca\u4f30\u7b97\u65f6\u6bb5\u6709\u5173\uff09</li>",
    "<li>\\(n\\)\uff1a\u91c7\u6837\u6b21\u6570</li>",
    "<li>\\(C_i\\)\uff1a\u91c7\u6837\u65f6\u523b \\(i\\) \u7684\u6d53\u5ea6</li>",
    "<li>\\(Q_i\\)\uff1a\u91c7\u6837\u65f6\u523b \\(i\\) \u7684\u6d41\u91cf</li>",
    "</ul>",
    "<p><b>\u539f\u7406\uff1a</b>\u5206\u522b\u7528\u91c7\u6837\u6d53\u5ea6\u7684\u65f6\u95f4\u5e73\u5747\u4e0e\u91c7\u6837\u6d41\u91cf\u7684\u65f6\u95f4\u5e73\u5747\u4ee3\u8868\u8be5\u671f\u5178\u578b\u6c34\u5e73\uff0c\u518d\u76f8\u4e58\u4f30\u7b97\u8d1f\u8377\u3002</p>",
    "<p><b>\u9002\u7528\u60c5\u51b5\uff1a</b>\u91c7\u6837\u8f83\u5747\u5300\u3001\u6d53\u5ea6\u4e0e\u6d41\u91cf\u76f8\u5173\u6027\u4e0d\u5f3a\u65f6\u8f83\u7a33\u5065\uff1b",
    "\u5ffd\u89c6\u6d41\u91cf\u4e0e\u6d53\u5ea6\u7684\u5173\u7cfb\uff0c\u4e0d\u9002\u5408\u9ad8\u6d41\u91cf\u4e8b\u4ef6\u8d21\u732e\u5927\u4f46\u672a\u88ab\u91c7\u5230\u7684\u60c5\u5f62\u3002</p>",

    # ================================================================
    # Method 2
    # ================================================================
    "<h5>Method 2\uff1adischarge-weighted C\uff08\u6d41\u91cf\u52a0\u6743\u6d53\u5ea6\uff09</h5>",
    "\\[L = K\\left(\\frac{1}{n}\\sum_{i=1}^{n} C_i Q_i\\right)\\]",
    "<ul>",
    "<li>\\(L\\)\uff1a\u6307\u5b9a\u65f6\u6bb5\u603b\u8d1f\u8377</li>",
    "<li>\\(K\\)\uff1a\u5355\u4f4d/\u65f6\u6bb5\u6362\u7b97\u56e0\u5b50</li>",
    "<li>\\(n\\)\uff1a\u91c7\u6837\u6b21\u6570</li>",
    "<li>\\(C_i\\)\uff1a\u91c7\u6837\u65f6\u523b \\(i\\) \u7684\u6d53\u5ea6</li>",
    "<li>\\(Q_i\\)\uff1a\u91c7\u6837\u65f6\u523b \\(i\\) \u7684\u6d41\u91cf</li>",
    "</ul>",
    "<p><b>\u539f\u7406\uff1a</b>\u5bf9\u91c7\u6837\u65f6\u523b\u7684\u201c\u77ac\u65f6\u8d1f\u8377\u9879\u201d \\(C_iQ_i\\) \u53d6\u5e73\u5747\u5e76\u6362\u7b97\u5230\u76ee\u6807\u65f6\u6bb5\u3002</p>",
    "<p><b>\u9002\u7528\u60c5\u51b5\uff1a</b>\u91c7\u6837\u80fd\u8986\u76d6\u9ad8\u6d41\u91cf\u8fc7\u7a0b\u65f6\u66f4\u5408\u7406\uff1b",
    "\u4f4e\u9891\u91c7\u6837\u6613\u4ea7\u751f\u504f\u5dee\uff0c\u82e5\u91c7\u6837\u504f\u5411\u67af\u6c34\u6216\u7f3a\u5c11\u6d2a\u5cf0\u6837\u672c\uff0c\u4f1a\u4ea7\u751f\u663e\u8457\u504f\u5dee\u3002</p>",

    # ================================================================
    # Method 3
    # ================================================================
    "<h5>Method 3\uff1amean discharge-weighted C\uff08\u76f8\u90bb\u533a\u95f4\u5e73\u5747\u6d41\u91cf\u52a0\u6743\uff09</h5>",
    "\\[L = K' \\sum_{i=1}^{n} C_i\\,\\overline{Q}_{i,i-1}\\]",
    "<ul>",
    "<li>\\(L\\)\uff1a\u6307\u5b9a\u65f6\u6bb5\u603b\u8d1f\u8377</li>",
    "<li>\\(K'\\)\uff1a\u5355\u4f4d/\u65f6\u6bb5\u6362\u7b97\u56e0\u5b50\uff08\u4e0e \\(\\Delta t\\) \u7684\u5b9a\u4e49\u3001\u5355\u4f4d\u6362\u7b97\u65b9\u5f0f\u6709\u5173\uff09</li>",
    "<li>\\(n\\)\uff1a\u91c7\u6837\u6b21\u6570</li>",
    "<li>\\(C_i\\)\uff1a\u91c7\u6837\u65f6\u523b \\(i\\) \u7684\u6d53\u5ea6</li>",
    "<li>\\(\\overline{Q}_{i,i-1}\\)\uff1a\u76f8\u90bb\u4e24\u6b21\u91c7\u6837 \\(i\\) \u4e0e \\(i-1\\) \u95f4\u9694\u5185\u7684\u5e73\u5747\u6d41\u91cf\uff08\u7531\u9ad8\u9891\u6d41\u91cf\u8bb0\u5f55\u8ba1\u7b97\uff09</li>",
    "</ul>",
    "<p><b>\u539f\u7406\uff1a</b>\u628a\u6bcf\u4e2a\u6d53\u5ea6\u89c2\u6d4b \\(C_i\\) \u89c6\u4f5c\u4ee3\u8868\u5176\u4e0e\u524d\u4e00\u6b21\u91c7\u6837\u4e4b\u95f4\u533a\u95f4\u7684\u6c34\u5e73\uff0c",
    "\u7528\u8be5\u533a\u95f4\u5e73\u5747\u6d41\u91cf\u52a0\u6743\u8fd1\u4f3c\u8be5\u6bb5\u8f93\u9001\u91cf\u3002</p>",
    "<p><b>\u9002\u7528\u60c5\u51b5\uff1a</b>\u6709\u8fde\u7eed/\u9ad8\u9891\u6d41\u91cf\u8bb0\u5f55\u3001\u6d53\u5ea6\u91c7\u6837\u8f83\u7a00\u758f\u4f46\u5e0c\u671b\u4f53\u73b0\u533a\u95f4\u6d41\u91cf\u53d8\u5316\uff1b",
    "\u4e0d\u9002\u5408\u6d53\u5ea6\u5728\u91c7\u6837\u95f4\u9694\u5185\u5267\u70c8\u6ce2\u52a8\u4e14\u91c7\u6837\u95f4\u9694\u5f88\u957f\u3002</p>",

    # ================================================================
    # Method 4
    # ================================================================
    "<h5>Method 4\uff1atime-weighted C\uff08\u65f6\u95f4\u52a0\u6743\u6d53\u5ea6 + \u5e74/\u671f\u5747\u6d41\u91cf\uff09</h5>",
    "\\[L = K\\,\\overline{Q}\\left(\\frac{1}{n}\\sum_{i=1}^{n} C_i\\right)\\]",
    "<ul>",
    "<li>\\(L\\)\uff1a\u6307\u5b9a\u65f6\u6bb5\u603b\u8d1f\u8377</li>",
    "<li>\\(K\\)\uff1a\u5355\u4f4d/\u65f6\u6bb5\u6362\u7b97\u56e0\u5b50</li>",
    "<li>\\(\\overline{Q}\\)\uff1a\uff08\u5e74/\u671f\uff09\u5e73\u5747\u6d41\u91cf\uff08\u7531\u9ad8\u9891\u6d41\u91cf\u8bb0\u5f55\u5f97\u5230\uff09</li>",
    "<li>\\(n\\)\uff1a\u91c7\u6837\u6b21\u6570</li>",
    "<li>\\(C_i\\)\uff1a\u91c7\u6837\u65f6\u523b \\(i\\) \u7684\u6d53\u5ea6</li>",
    "</ul>",
    "<p><b>\u539f\u7406\uff1a</b>\u7528\u65f6\u95f4\u5e73\u5747\u6d53\u5ea6\u4ee3\u8868\u6d53\u5ea6\u6c34\u5e73\uff0c\u7528\u5168\u671f\u5e73\u5747\u6d41\u91cf\u4ee3\u8868\u5178\u578b\u6d41\u91cf\u5f3a\u5ea6\uff0c\u518d\u76f8\u4e58\u6362\u7b97\u8d1f\u8377\u3002</p>",
    "<p><b>\u9002\u7528\u60c5\u51b5\uff1a</b>\u6d41\u91cf\u6ce2\u52a8\u76f8\u5bf9\u53ef\u7528\u201c\u5747\u503c\u201d\u6982\u62ec\u65f6\uff1b",
    "\u4f46\u5b8c\u5168\u5ffd\u7565\u6d53\u5ea6\u4e0e\u6d41\u91cf\u8026\u5408\u5173\u7cfb\uff0c\u4e0d\u9002\u5408\u8d1f\u8377\u7531\u5c11\u6570\u6781\u7aef\u6d41\u91cf\u4e8b\u4ef6\u4e3b\u5bfc\uff0c\u4e00\u822c\u4e0d\u63a8\u8350\u3002</p>",

    # ================================================================
    # Method 5
    # ================================================================
    "<h5>Method 5\uff1atime and discharge weighted\uff08\u65f6\u95f4\u4e0e\u6d41\u91cf\u52a0\u6743\uff09</h5>",
    "\\[L = K\\left(\\frac{\\sum_{i=1}^{n} C_iQ_i}{\\sum_{i=1}^{n} Q_i}\\right)\\overline{Q}\\]",
    "<ul>",
    "<li>\\(L\\)\uff1a\u6307\u5b9a\u65f6\u6bb5\u603b\u8d1f\u8377</li>",
    "<li>\\(K\\)\uff1a\u5355\u4f4d/\u65f6\u6bb5\u6362\u7b97\u56e0\u5b50</li>",
    "<li>\\(C_i\\)\uff1a\u91c7\u6837\u65f6\u523b \\(i\\) \u7684\u6d53\u5ea6</li>",
    "<li>\\(Q_i\\)\uff1a\u91c7\u6837\u65f6\u523b \\(i\\) \u7684\u6d41\u91cf</li>",
    "<li>\\(\\overline{Q}\\)\uff1a\uff08\u5e74/\u671f\uff09\u5e73\u5747\u6d41\u91cf</li>",
    "<li>\\(n\\)\uff1a\u91c7\u6837\u6b21\u6570</li>",
    "</ul>",
    "<p><b>\u539f\u7406\uff1a</b>\u5148\u5f97\u5230\u91c7\u6837\u70b9\u7684\u6d41\u91cf\u52a0\u6743\u5e73\u5747\u6d53\u5ea6 ",
    "\\(\\left(\\sum C_iQ_i\\right)/\\left(\\sum Q_i\\right)\\)\uff0c",
    "\u518d\u4e58\u4ee5\u76ee\u6807\u671f\u5747\u6d41\u91cf\u4f30\u7b97\u8d1f\u8377\u3002</p>",
    "<p><b>\u9002\u7528\u60c5\u51b5\uff1a</b>\u8003\u8651\u6d53\u5ea6\u548c\u6d41\u91cf\u5173\u7cfb\uff0c\u5728\u91c7\u6837\u8986\u76d6\u4e00\u5b9a\u6d41\u91cf\u8303\u56f4\u65f6\u8003\u8651\u3001",
    "\u5176\u66f4\u770b\u91cd\u9ad8\u6d41\u91cf\u91c7\u6837\u70b9\uff1b",
    "\u82e5\u91c7\u6837\u6d41\u91cf\u5206\u5e03\u4e0e\u771f\u5b9e\u671f\u5185\u6d41\u91cf\u5206\u5e03\u5dee\u5f02\u5f88\u5927\uff0c\u4f1a\u4ea7\u751f\u504f\u5dee\u3002</p>",

    "<hr>",
    "<p><i>\u4e0a\u8ff0 5 \u79cd\u65b9\u6cd5\u7684\u603b\u8d1f\u8377\u540c\u65f6\u7531 RiverLoad \u5305\u7684\u539f\u51fd\u6570\u8ba1\u7b97\uff0c",
    "\u4f9b\u4e0e\u9010\u65e5\u6c47\u603b\u7ed3\u679c\u4ea4\u53c9\u6821\u9a8c\u3002</i></p>"
  )
}

# ================================================================
# UI 参数块
# ================================================================

fw_weighted_left_extra_ui <- function(ns, key = "weighted") {
  shiny::tagList(
    shiny::helpText("\u52a0\u6743\u5e73\u5747\u6cd5\u8c03\u7528\u7b2c\u4e00\u6b65 QF/WQ \u6570\u636e + RiverLoad Method 1\u20135"),
    shiny::selectInput(ns(paste0("qf_sheet_", key)), "QF\u6570\u636e\u8868", choices = NULL),
    shiny::selectInput(ns(paste0("wq_sheet_", key)), "WQ\u6570\u636e\u8868", choices = NULL),
    shiny::selectInput(ns(paste0("constituent_", key)), "\u6c34\u8d28\u6307\u6807(j)", choices = NULL),
    shiny::selectInput(
      ns(paste0("sub_method_", key)),
      "\u52a0\u6743\u5b50\u65b9\u6cd5",
      choices = c(
        "\u65f6\u95f4\u52a0\u6743 Q\u4e0eC (Method 1)" = "method1",
        "\u6d41\u91cf\u52a0\u6743\u6d53\u5ea6 (Method 2)" = "method2",
        "\u76f8\u90bb\u533a\u95f4\u5e73\u5747\u6d41\u91cf\u52a0\u6743 (Method 3)" = "method3",
        "\u65f6\u95f4\u52a0\u6743\u6d53\u5ea6+\u5168\u671f\u5747\u6d41\u91cf (Method 4)" = "method4",
        "\u65f6\u95f4\u4e0e\u6d41\u91cf\u52a0\u6743 (Method 5)" = "method5"
      ),
      selected = "method1"
    )
  )
}

# ================================================================
# Server：QF/WQ 数据源 reactive
# ================================================================

fw_weighted_step1_reactive <- function(rv) {
  shiny::reactive({
    fw_get_step1_qf_wq(rv)
  })
}

# ================================================================
# Server：QF/WQ 表名与指标联动 observers
# ================================================================

fw_weighted_init_observers <- function(input, session, step1_qf_wq) {

  shiny::observe({
    s1 <- step1_qf_wq()
    if (is.null(s1)) {
      shiny::updateSelectInput(session, "qf_sheet_weighted", choices = character(0))
      shiny::updateSelectInput(session, "wq_sheet_weighted", choices = character(0))
      return()
    }

    qf_list <- fw_as_named_table_list(s1$QF, "QF")
    wq_list <- fw_as_named_table_list(s1$WQ, "WQ")
    if (length(qf_list) == 0 || length(wq_list) == 0) {
      shiny::updateSelectInput(session, "qf_sheet_weighted", choices = character(0))
      shiny::updateSelectInput(session, "wq_sheet_weighted", choices = character(0))
      return()
    }

    qf_nm <- names(qf_list)
    wq_nm <- names(wq_list)

    cur_qf <- shiny::isolate(input$qf_sheet_weighted)
    cur_wq <- shiny::isolate(input$wq_sheet_weighted)
    if (is.null(cur_qf) || !(cur_qf %in% qf_nm)) cur_qf <- qf_nm[1]
    if (is.null(cur_wq) || !(cur_wq %in% wq_nm)) cur_wq <- wq_nm[1]

    shiny::updateSelectInput(session, "qf_sheet_weighted", choices = qf_nm, selected = cur_qf)
    shiny::updateSelectInput(session, "wq_sheet_weighted", choices = wq_nm, selected = cur_wq)
  })

  shiny::observe({
    s1 <- step1_qf_wq()
    if (is.null(s1)) {
      shiny::updateSelectInput(session, "constituent_weighted", choices = character(0))
      return()
    }

    wq_list <- fw_as_named_table_list(s1$WQ, "WQ")
    if (length(wq_list) == 0) {
      shiny::updateSelectInput(session, "constituent_weighted", choices = character(0))
      return()
    }

    ws <- input$wq_sheet_weighted
    if (is.null(ws) || !(ws %in% names(wq_list))) ws <- names(wq_list)[1]

    cands <- fw_get_wq_constituents(wq_list[[ws]])
    if (length(cands) == 0) cands <- names(wq_list[[ws]])

    cur <- shiny::isolate(input$constituent_weighted)
    if (is.null(cur) || !(cur %in% cands)) cur <- cands[1]

    shiny::updateSelectInput(session, "constituent_weighted", choices = cands, selected = cur)
  })
}

# ================================================================
# Server：数据获取（daily_all）
# ================================================================

fw_weighted_get_daily_all <- function(input, step1_qf_wq) {
  s1 <- step1_qf_wq()
  if (is.null(s1)) return(NULL)

  prep <- tryCatch(
    fw_prepare_weighted_input(
      step1_data  = s1,
      qf_sheet    = input$qf_sheet_weighted,
      wq_sheet    = input$wq_sheet_weighted,
      constituent = input$constituent_weighted,
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

fw_weighted_run_calc <- function(input, step1_qf_wq, key = "weighted") {
  s1 <- step1_qf_wq()
  if (is.null(s1)) {
    shiny::showNotification("\u672a\u627e\u5230\u7b2c\u4e00\u6b65 QF/WQ \u6570\u636e\u3002", type = "error")
    return(NULL)
  }

  res <- tryCatch(
    fw_run_flux_weighted(
      step1_data  = s1,
      qf_sheet    = input[[paste0("qf_sheet_", key)]],
      wq_sheet    = input[[paste0("wq_sheet_", key)]],
      constituent = input[[paste0("constituent_", key)]],
      date_range  = input[[paste0("daterange_", key)]],
      sub_method  = input[[paste0("sub_method_", key)]] %||% "method1",
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

fw_weighted_settle_extra <- function(res) {
  if (is.null(res)) return("")
  diag <- res$diag %||% list()
  paste0(
    "\nQF\u8868: ",   res$params$qf_sheet    %||% "",
    "\nWQ\u8868: ",   res$params$wq_sheet    %||% "",
    "\n\u6307\u6807: ", res$params$constituent %||% "",
    "\n\u5b50\u65b9\u6cd5: ", diag$sub_label  %||% res$params$sub_method %||% "",
    "\n\u76d1\u6d4b\u5929\u6570: ", diag$n_obs %||% "",
    "\n\u8ba1\u7b97\u5929\u6570: ", diag$n_total_days %||% "",
    "\n\u9010\u65e5\u6c47\u603b(kg): ", diag$daily_sum_kg %||% "",
    "\nRiverLoad\u603b\u8d1f\u8377(kg): ", diag$rl_this_kg %||% "NA"
  )
}

# ================================================================
# Server：加权平均法专属诊断图
# ================================================================

fw_weighted_render_diag <- function(res) {
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
  graphics::par(mfrow = c(2, 2), mar = c(4, 4, 3, 1))

  # 1) C_obs vs C_est 时序
  ylim <- range(c(c_obs, c_est), na.rm = TRUE, finite = TRUE)
  if (all(!is.finite(ylim))) ylim <- c(0, 1)
  graphics::plot(dt, c_est, type = "l", col = "#2c7fb8", lwd = 1.5,
                 xlab = "\u65e5\u671f", ylab = "\u6d53\u5ea6 (mg/L)",
                 main = "\u6d53\u5ea6\uff1a\u89c2\u6d4b vs \u4f30\u7b97", ylim = ylim)
  graphics::points(dt[!is.na(c_obs)], c_obs[!is.na(c_obs)],
                   pch = 16, col = grDevices::rgb(0.85, 0.2, 0.2, 0.7), cex = 1.2)
  graphics::legend("topleft", legend = c("C_est (\u4f30\u7b97)", "C_obs (\u5b9e\u6d4b)"),
                   lty = c(1, NA), pch = c(NA, 16),
                   col = c("#2c7fb8", grDevices::rgb(0.85, 0.2, 0.2, 0.7)),
                   bty = "n", cex = 0.85)

  # 2) 日通量柱状图
  graphics::plot(dt, flux, type = "h", lwd = 2, col = "#1b9e77",
                 xlab = "\u65e5\u671f", ylab = "\u901a\u91cf (kg/d)",
                 main = "\u9010\u65e5\u901a\u91cf")
  if (any(is.finite(flux)))
    graphics::abline(h = mean(flux, na.rm = TRUE), lty = 2, col = "#555555")

  # 3) Q vs C_est 散点
  graphics::plot(q, c_est, pch = 1, col = "#2c7fb8",
                 xlab = "Q (m\u00b3/s)", ylab = "C_est (mg/L)",
                 main = "Q\u2013C \u5173\u7cfb")
  obs_ok <- which(!is.na(c_obs))
  if (length(obs_ok) > 0)
    graphics::points(q[obs_ok], c_obs[obs_ok], pch = 16,
                     col = grDevices::rgb(0.85, 0.2, 0.2, 0.6))

  # 4) RiverLoad 5 方法总负荷对比
  mc <- res$diag$model_compare
  if (!is.null(mc) && is.data.frame(mc) && nrow(mc) > 0) {
    vals <- mc$total_load_kg
    nms  <- mc$method
    cols <- ifelse(nms == res$diag$sub_method, "#e41a1c", "#4daf4a")
    bp <- graphics::barplot(vals, names.arg = nms, col = cols,
                            main = "RiverLoad \u603b\u8d1f\u8377\u5bf9\u6bd4 (kg)",
                            ylab = "\u603b\u8d1f\u8377 (kg)", las = 2, cex.names = 0.8)
    graphics::text(bp, vals, labels = round(vals, 1), pos = 3, cex = 0.75)
    graphics::legend("topright", legend = c("\u5f53\u524d\u9009\u4e2d", "\u5176\u4ed6\u65b9\u6cd5"),
                     fill = c("#e41a1c", "#4daf4a"), bty = "n", cex = 0.8)
  } else {
    graphics::plot.new()
    graphics::text(0.5, 0.5, "RiverLoad \u603b\u8d1f\u8377\u5bf9\u6bd4\u4e0d\u53ef\u7528", cex = 0.9)
  }

  invisible(NULL)
}
