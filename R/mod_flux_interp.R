# =====================================================================
# mod_flux_interp.R
# 插值方法 —— 专属 UI 参数 + Server 初始化 + 数据获取 + 计算调度
# RiverLoad Method 6 + loadflex loadInterp 4种插值
# =====================================================================

if (!exists("%||%", mode = "function")) {
  `%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x
}

# ================================================================
# 原理文本（HTML + MathJax）
# ================================================================

fw_interp_principle_text <- function(active_sub = NULL) {
  paste0(

    # ---------- 大标题 ----------
    "<h4>\u63d2\u503c\u65b9\u6cd5</h4>",
    "<p>\u901a\u8fc7\u63d2\u503c\u5c06\u79bb\u6563\u76d1\u6d4b\u6d53\u5ea6\u8865\u9f50\u4e3a\u8fde\u7eed\u65e5\u5e8f\u5217\uff0c",
    "\u518d\u4e0e\u9ad8\u9891\u6d41\u91cf\u9010\u6b65\u76f8\u4e58\u5e76\u7d2f\u52a0\uff0c\u8fd1\u4f3c\u79ef\u5206 \\(\\int QC\\,dt\\)\u3002",
    "\u5305\u542b RiverLoad Method 6 \u53ca loadflex \u63d2\u503c\u6a21\u578b\u7684 4 \u79cd\u63d2\u503c\u7b56\u7565\u3002</p>",

    # ================================================================
    # 1. RiverLoad Method 6
    # ================================================================
    "<h5>1. RiverLoad Method 6\uff1a\u6d53\u5ea6\u7ebf\u6027\u63d2\u503c (linear interpolation of C)</h5>",
    "\\[L = K'' \\sum_{j=1}^{n_{\\text{int}}} C^{\\text{int}}_j\\,Q_j\\]",
    "<ul>",
    "<li>\\(L\\)\uff1a\u6307\u5b9a\u65f6\u6bb5\u603b\u8d1f\u8377</li>",
    "<li>\\(K''\\)\uff1a\u5355\u4f4d/\u65f6\u6bb5\u6362\u7b97\u56e0\u5b50</li>",
    "<li>\\(n_{\\text{int}}\\)\uff1a\u63d2\u503c\u540e\u7684\u65f6\u95f4\u6b65\u6570\uff08\u5e38\u4e3a\u8be5\u671f\u5929\u6570\uff09</li>",
    "<li>\\(C^{\\text{int}}_j\\)\uff1a\u7ebf\u6027\u63d2\u503c\u5f97\u5230\u7684\u7b2c \\(j\\) \u5929\u6d53\u5ea6</li>",
    "<li>\\(Q_j\\)\uff1a\u7b2c \\(j\\) \u5929\u7684\u5e73\u5747\u6d41\u91cf</li>",
    "</ul>",
    "<p><b>\u539f\u7406\uff1a</b>\u628a\u6d53\u5ea6\u8865\u9f50\u6210\u8fde\u7eed\uff08\u9010\u65e5\uff09\u5e8f\u5217\uff0c\u518d\u4e0e\u9ad8\u9891\u6d41\u91cf\u9010\u6b65\u76f8\u4e58\u5e76\u7d2f\u52a0\u3002</p>",
    "<p><b>\u9002\u7528\u60c5\u51b5\uff1a</b>\u6d53\u5ea6\u53d8\u5316\u76f8\u5bf9\u5e73\u6ed1\u6216\u91c7\u6837\u95f4\u9694\u4e0d\u5927\u65f6\uff1b",
    "\u4e0d\u9002\u5408\u6d53\u5ea6\u7531\u77ed\u5386\u65f6\u5cf0\u503c/\u4e8b\u4ef6\u9a71\u52a8\u4e14\u4e8b\u4ef6\u672a\u91c7\u5230\uff08\u7ebf\u6027\u63d2\u503c\u4f1a\u6f0f\u5cf0\uff09\u3002</p>",

    # ================================================================
    # 2. loadflex loadInterp 插值模型
    # ================================================================
    "<h5>2. loadflex loadInterp \u63d2\u503c\u6a21\u578b</h5>",
    "<p>\u63d2\u503c\u6cd5\u7684\u6838\u5fc3\uff1a\u5148\u83b7\u5f97\u6bcf\u4e2a\u65f6\u95f4\u6b65\u7684 \\(\\widehat{C}(t)\\)\uff0c",
    "\u518d\u7528 \\(Q(t)\\) \u4e0e\u5355\u4f4d\u6362\u7b97\u5f97\u5230 \\(\\widehat{L}(t)\\)\u3002</p>",

    # ---- 2.1 线性插值 ----
    "<h5>2.1 \u7ebf\u6027\u63d2\u503c (linearInterpolation)</h5>",
    "<p>\u8bbe\u76f8\u90bb\u4e24\u6b21\u89c2\u6d4b\u5728 \\((t_1, C_1)\\) \u4e0e \\((t_2, C_2)\\)\uff0c\u5bf9 \\(t\\in[t_1,t_2]\\)\uff1a</p>",
    "\\[\\widehat{C}(t)=C_1+\\frac{t-t_1}{t_2-t_1}\\left(C_2-C_1\\right)\\]",
    "<ul>",
    "<li>\\(t_1,t_2\\)\uff1a\u76f8\u90bb\u4e24\u6b21\u91c7\u6837\u65f6\u95f4\uff08\\(t_2>t_1\\)\uff09</li>",
    "<li>\\(C_1,C_2\\)\uff1a\u5bf9\u5e94\u6d53\u5ea6\u89c2\u6d4b\u503c</li>",
    "<li>\\(\\widehat{C}(t)\\)\uff1a\u63d2\u503c\u5f97\u5230\u7684\u6d53\u5ea6</li>",
    "</ul>",
    "<p><b>\u539f\u7406\uff1a</b>\u5047\u8bbe\u4e24\u6b21\u91c7\u6837\u4e4b\u95f4\u6d53\u5ea6\u6309\u65f6\u95f4\u7ebf\u6027\u53d8\u5316\u3002</p>",
    "<p><b>\u9002\u7528\u60c5\u51b5\uff1a</b>\u91c7\u6837\u8f83\u5bc6\u3001\u6d53\u5ea6\u53d8\u5316\u8f83\u5e73\u6ed1\uff1b\u4e0d\u9002\u5408\u4e8b\u4ef6\u578b\u5c16\u5cf0\uff08\u4f1a\u201c\u62b9\u5e73\u201d\u5cf0\u503c\uff09\u3002</p>",

    # ---- 2.2 矩形插值 ----
    "<h5>2.2 \u77e9\u5f62\u63d2\u503c (rectangularInterpolation\uff0c\u9636\u68af\u4fdd\u6301)</h5>",
    "\\[\\widehat{C}(t)=C_1,\\quad t\\in[t_1,t_2)\\]",
    "<ul>",
    "<li>\\(C_1\\)\uff1a\u533a\u95f4\u8d77\u70b9\u91c7\u6837\u7684\u6d53\u5ea6</li>",
    "<li>\\(\\widehat{C}(t)\\)\uff1a\u63d2\u503c\u5f97\u5230\u7684\u6d53\u5ea6</li>",
    "</ul>",
    "<p><b>\u539f\u7406\uff1a</b>\u5047\u8bbe\u5728\u4e24\u6b21\u91c7\u6837\u95f4\u6d53\u5ea6\u4fdd\u6301\u4e0d\u53d8\u3002</p>",
    "<p><b>\u9002\u7528\u60c5\u51b5\uff1a</b>\u6d53\u5ea6\u5448\u9636\u68af\u5f0f\u53d8\u5316\u6216\u91c7\u6837\u9891\u7e41\uff1b\u5bf9\u5feb\u901f\u53d8\u5316\u8fc7\u7a0b\u53ef\u80fd\u504f\u5dee\u66f4\u5927\u3002</p>",

    # ---- 2.3 三角核插值 ----
    "<h5>2.3 \u4e09\u89d2\u6838\u63d2\u503c (triangularInterpolation)</h5>",
    "\\[\\widehat{C}(t)=\\frac{\\sum_{i=1}^{n} w_i(t)\\,C_i}{\\sum_{i=1}^{n} w_i(t)},",
    "\\qquad w_i(t)=\\max\\!\\left(0,\\,1-\\frac{|t-t_i|}{h}\\right)\\]",
    "<ul>",
    "<li>\\(t_i\\)\uff1a\u7b2c \\(i\\) \u4e2a\u91c7\u6837\u65f6\u523b</li>",
    "<li>\\(C_i\\)\uff1a\u7b2c \\(i\\) \u4e2a\u6d53\u5ea6\u89c2\u6d4b</li>",
    "<li>\\(h\\)\uff1a\u534a\u7a97\u5bbd</li>",
    "<li>\\(w_i(t)\\)\uff1a\u4e09\u89d2\u6838\u6743\u91cd</li>",
    "</ul>",
    "<p><b>\u539f\u7406\uff1a</b>\u8d8a\u63a5\u8fd1\u76ee\u6807\u65f6\u523b\u7684\u89c2\u6d4b\u6743\u91cd\u8d8a\u5927\uff0c\u8d85\u8fc7\u534a\u7a97\u5bbd\u5219\u6743\u91cd\u4e3a 0\u3002</p>",
    "<p><b>\u9002\u7528\u60c5\u51b5\uff1a</b>\u5e0c\u671b\u201c\u5c40\u90e8\u5e73\u6ed1\u201d\u800c\u4e0d\u662f\u786c\u7ebf\u6027\u8fde\u63a5\uff1b\u4e0d\u9002\u5408\u6570\u636e\u6781\u7a00\u758f\u4e14\u53d8\u5316\u5f88\u5feb\u3002</p>",

    # ---- 2.4 距离加权插值 ----
    "<h5>2.4 \u8ddd\u79bb\u52a0\u6743\u63d2\u503c (distanceWeightedInterpolation)</h5>",
    "\\[\\widehat{C}(t)=\\frac{\\sum_{i=1}^{n} w_i(t)\\,C_i}{\\sum_{i=1}^{n} w_i(t)},",
    "\\qquad w_i(t)=\\frac{1}{|t-t_i|^{p}}\\]",
    "<ul>",
    "<li>\\(\\widehat{C}(t)\\)\uff1a\u63d2\u503c\u5f97\u5230\u7684\u6d53\u5ea6</li>",
    "<li>\\(C_i\\)\uff1a\u7b2c \\(i\\) \u4e2a\u6d53\u5ea6\u89c2\u6d4b</li>",
    "<li>\\(p\\)\uff1a\u8ddd\u79bb\u8870\u51cf\u5e42\u6b21</li>",
    "</ul>",
    "<p><b>\u539f\u7406\uff1a</b>\u8ddd\u79bb\u8d8a\u8fd1\u6743\u91cd\u8d8a\u5927\uff1b\u901a\u8fc7 \\(p\\) \u63a7\u5236\u201c\u66f4\u4f9d\u8d56\u8fd1\u90bb\u201d\u8fd8\u662f\u201c\u66f4\u5e73\u6ed1\u201d\u3002</p>",
    "<p><b>\u9002\u7528\u60c5\u51b5\uff1a</b>\u91c7\u6837\u4e0d\u5747\u5300\u65f6\u5e38\u6bd4\u7ebf\u6027\u66f4\u7a33\uff1b\u4f46\u82e5\u76ee\u6807\u70b9\u9644\u8fd1\u6ca1\u6709\u6837\u672c\uff0c",
    "\u4f1a\u8fc7\u5ea6\u4f9d\u8d56\u8fdc\u5904\u6837\u672c\u5e76\u653e\u5927\u8bef\u5dee\u3002</p>",

    # ---- 2.5 通量 ----
    "<h5>2.5 \u4ece\u63d2\u503c\u6d53\u5ea6\u5230\u901a\u91cf</h5>",
    "\\[\\widehat{L}(t)=k\\;Q(t)\\;\\widehat{C}(t)\\]",
    "<ul>",
    "<li>\\(\\widehat{L}(t)\\)\uff1a\u9884\u6d4b\u901a\u91cf</li>",
    "<li>\\(Q(t)\\)\uff1a\u8be5\u65f6\u95f4\u6b65\u6d41\u91cf</li>",
    "<li>\\(\\widehat{C}(t)\\)\uff1a\u63d2\u503c\u5f97\u5230\u7684\u6d53\u5ea6</li>",
    "<li>\\(k\\)\uff1a\u5355\u4f4d\u6362\u7b97\u56e0\u5b50\uff0886.4\uff0c\u5c06 m\u00b3/s \u00d7 mg/L \u8f6c\u4e3a kg/d\uff09</li>",
    "</ul>",
    "<p><b>\u9002\u7528\u60c5\u51b5\uff1a</b>\u8fde\u7eed/\u9ad8\u9891 \\(Q(t)\\)\uff0c\u628a\u7a00\u758f \\(C\\) \u63d2\u503c\u540e\u53d8\u6210\u8fde\u7eed\u901a\u91cf\u5e8f\u5217\u3002</p>",

    "<hr>",
    "<p><i>RiverLoad Method 6 \u603b\u8d1f\u8377\u7531 RiverLoad \u5305\u539f\u51fd\u6570\u8ba1\u7b97\uff0c",
    "loadflex \u63d2\u503c\u6a21\u578b\u4f18\u5148\u8c03\u7528 loadflex::loadInterp\uff0c",
    "\u4e0d\u53ef\u7528\u65f6\u81ea\u52a8\u9000\u5316\u4e3a\u624b\u52a8\u5b9e\u73b0\u3002</i></p>"
  )
}

# ================================================================
# UI 参数块
# ================================================================

fw_interp_left_extra_ui <- function(ns, key = "interp") {
  shiny::tagList(
    shiny::helpText("\u63d2\u503c\u65b9\u6cd5\u8c03\u7528\u7b2c\u4e00\u6b65 QF/WQ \u6570\u636e + RiverLoad/loadflex"),
    shiny::selectInput(ns(paste0("qf_sheet_", key)), "QF\u6570\u636e\u8868", choices = NULL),
    shiny::selectInput(ns(paste0("wq_sheet_", key)), "WQ\u6570\u636e\u8868", choices = NULL),
    shiny::selectInput(ns(paste0("constituent_", key)), "\u6c34\u8d28\u6307\u6807(j)", choices = NULL),
    shiny::selectInput(
      ns(paste0("sub_method_", key)),
      "\u63d2\u503c\u5b50\u65b9\u6cd5",
      choices = c(
        "\u6d53\u5ea6\u7ebf\u6027\u63d2\u503c / RiverLoad Method 6" = "method6",
        "\u7ebf\u6027\u63d2\u503c (linearInterpolation)" = "linearInterp",
        "\u77e9\u5f62\u63d2\u503c (rectangularInterpolation)" = "rectangularInterp",
        "\u4e09\u89d2\u6838\u63d2\u503c (triangularInterpolation)" = "triangularInterp",
        "\u8ddd\u79bb\u52a0\u6743\u63d2\u503c (distanceWeightedInterpolation)" = "distanceWeightedInterp"
      ),
      selected = "method6"
    )
  )
}

# ================================================================
# Server：QF/WQ 数据源 reactive
# ================================================================

fw_interp_step1_reactive <- function(rv) {
  shiny::reactive({
    fw_get_step1_qf_wq(rv)
  })
}

# ================================================================
# Server：QF/WQ 表名与指标联动 observers
# ================================================================

fw_interp_init_observers <- function(input, session, step1_qf_wq) {

  shiny::observe({
    s1 <- step1_qf_wq()
    if (is.null(s1)) {
      shiny::updateSelectInput(session, "qf_sheet_interp", choices = character(0))
      shiny::updateSelectInput(session, "wq_sheet_interp", choices = character(0))
      return()
    }

    qf_list <- fw_as_named_table_list(s1$QF, "QF")
    wq_list <- fw_as_named_table_list(s1$WQ, "WQ")
    if (length(qf_list) == 0 || length(wq_list) == 0) {
      shiny::updateSelectInput(session, "qf_sheet_interp", choices = character(0))
      shiny::updateSelectInput(session, "wq_sheet_interp", choices = character(0))
      return()
    }

    qf_nm <- names(qf_list); wq_nm <- names(wq_list)

    cur_qf <- shiny::isolate(input$qf_sheet_interp)
    cur_wq <- shiny::isolate(input$wq_sheet_interp)
    if (is.null(cur_qf) || !(cur_qf %in% qf_nm)) cur_qf <- qf_nm[1]
    if (is.null(cur_wq) || !(cur_wq %in% wq_nm)) cur_wq <- wq_nm[1]

    shiny::updateSelectInput(session, "qf_sheet_interp", choices = qf_nm, selected = cur_qf)
    shiny::updateSelectInput(session, "wq_sheet_interp", choices = wq_nm, selected = cur_wq)
  })

  shiny::observe({
    s1 <- step1_qf_wq()
    if (is.null(s1)) {
      shiny::updateSelectInput(session, "constituent_interp", choices = character(0))
      return()
    }

    wq_list <- fw_as_named_table_list(s1$WQ, "WQ")
    if (length(wq_list) == 0) {
      shiny::updateSelectInput(session, "constituent_interp", choices = character(0))
      return()
    }

    ws <- input$wq_sheet_interp
    if (is.null(ws) || !(ws %in% names(wq_list))) ws <- names(wq_list)[1]

    cands <- fw_get_wq_constituents(wq_list[[ws]])
    if (length(cands) == 0) cands <- names(wq_list[[ws]])

    cur <- shiny::isolate(input$constituent_interp)
    if (is.null(cur) || !(cur %in% cands)) cur <- cands[1]

    shiny::updateSelectInput(session, "constituent_interp", choices = cands, selected = cur)
  })
}

# ================================================================
# Server：数据获取（daily_all）
# ================================================================

fw_interp_get_daily_all <- function(input, step1_qf_wq) {
  s1 <- step1_qf_wq()
  if (is.null(s1)) return(NULL)

  prep <- tryCatch(
    fw_prepare_interp_input(
      step1_data  = s1,
      qf_sheet    = input$qf_sheet_interp,
      wq_sheet    = input$wq_sheet_interp,
      constituent = input$constituent_interp,
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

fw_interp_run_calc <- function(input, step1_qf_wq, key = "interp") {
  s1 <- step1_qf_wq()
  if (is.null(s1)) {
    shiny::showNotification("\u672a\u627e\u5230\u7b2c\u4e00\u6b65 QF/WQ \u6570\u636e\u3002", type = "error")
    return(NULL)
  }

  res <- tryCatch(
    fw_run_flux_interp(
      step1_data  = s1,
      qf_sheet    = input[[paste0("qf_sheet_", key)]],
      wq_sheet    = input[[paste0("wq_sheet_", key)]],
      constituent = input[[paste0("constituent_", key)]],
      date_range  = input[[paste0("daterange_", key)]],
      sub_method  = input[[paste0("sub_method_", key)]] %||% "method6",
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

fw_interp_settle_extra <- function(res) {
  if (is.null(res)) return("")
  diag <- res$diag %||% list()
  paste0(
    "\nQF\u8868: ",   res$params$qf_sheet    %||% "",
    "\nWQ\u8868: ",   res$params$wq_sheet    %||% "",
    "\n\u6307\u6807: ", res$params$constituent %||% "",
    "\n\u5b50\u65b9\u6cd5: ", diag$sub_label  %||% res$params$sub_method %||% "",
    "\n\u8ba1\u7b97\u5f15\u64ce: ", diag$source_label %||% "",
    "\n\u76d1\u6d4b\u5929\u6570: ", diag$n_obs %||% "",
    "\n\u63d2\u503c\u5929\u6570: ", diag$n_interpolated %||% "",
    "\n\u8ba1\u7b97\u5929\u6570: ", diag$n_total_days %||% "",
    "\n\u9010\u65e5\u6c47\u603b(kg): ", diag$daily_sum_kg %||% "",
    "\nRiverLoad M6 \u603b\u8d1f\u8377(kg): ", diag$rl_this_kg %||% "NA"
  )
}

# ================================================================
# Server：插值方法专属诊断图
# ================================================================

fw_interp_render_diag <- function(res) {
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

  # 1) 浓度时序：观测 vs 插值
  ylim <- range(c(c_obs, c_est), na.rm = TRUE, finite = TRUE)
  if (all(!is.finite(ylim))) ylim <- c(0, 1)
  graphics::plot(dt, c_est, type = "l", col = "#2c7fb8", lwd = 1.5,
                 xlab = "\u65e5\u671f", ylab = "\u6d53\u5ea6 (mg/L)",
                 main = "\u6d53\u5ea6\uff1a\u89c2\u6d4b vs \u63d2\u503c", ylim = ylim)
  obs_ok <- which(!is.na(c_obs))
  if (length(obs_ok) > 0)
    graphics::points(dt[obs_ok], c_obs[obs_ok],
                     pch = 16, col = grDevices::rgb(0.85, 0.2, 0.2, 0.7), cex = 1.2)
  graphics::legend("topleft", legend = c("C_est (\u63d2\u503c)", "C_obs (\u5b9e\u6d4b)"),
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
  if (length(obs_ok) > 0)
    graphics::points(q[obs_ok], c_obs[obs_ok], pch = 16,
                     col = grDevices::rgb(0.85, 0.2, 0.2, 0.6))

  # 4) RiverLoad method1-6 总负荷对比
  mc <- res$diag$model_compare
  if (!is.null(mc) && is.data.frame(mc) && nrow(mc) > 0 && any(is.finite(mc$total_load_kg))) {
    vals <- mc$total_load_kg
    nms  <- mc$method
    # 当前子方法高亮 method6
    cur_rl <- if (res$diag$sub_method == "method6") "method6" else "method6"
    cols <- ifelse(nms == cur_rl, "#e41a1c", "#4daf4a")
    bp <- graphics::barplot(vals, names.arg = nms, col = cols,
                            main = "RiverLoad \u603b\u8d1f\u8377\u5bf9\u6bd4 (kg)",
                            ylab = "\u603b\u8d1f\u8377 (kg)", las = 2, cex.names = 0.75)
    graphics::text(bp, vals, labels = round(vals, 1), pos = 3, cex = 0.7)

    # 在柱状图上叠加当前模块逐日汇总线
    dsk <- res$diag$daily_sum_kg
    if (is.finite(dsk %||% NA_real_)) {
      graphics::abline(h = dsk, lty = 2, col = "#ff7f00", lwd = 1.5)
      graphics::legend("topright",
                       legend = c("Method 6 (\u9ad8\u4eae)", "\u5176\u4ed6 Method",
                                  paste0("\u5f53\u524d\u9010\u65e5\u6c47\u603b: ", round(dsk, 1))),
                       fill = c("#e41a1c", "#4daf4a", NA),
                       border = c("black", "black", NA),
                       lty = c(NA, NA, 2),
                       col = c(NA, NA, "#ff7f00"),
                       bty = "n", cex = 0.7)
    } else {
      graphics::legend("topright", legend = c("Method 6", "\u5176\u4ed6"),
                       fill = c("#e41a1c", "#4daf4a"), bty = "n", cex = 0.8)
    }
  } else {
    graphics::plot.new()
    graphics::text(0.5, 0.5, "RiverLoad \u603b\u8d1f\u8377\u5bf9\u6bd4\u4e0d\u53ef\u7528", cex = 0.9)
  }

  invisible(NULL)
}
