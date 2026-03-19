# =====================================================================
# mod_flux_composite.R
# 复合法 —— 专属 UI 参数 + Server 初始化 + 数据获取 + 计算调度
# 数据获取结构与 mod_flux_regression.R 一致（QF/WQ 表名 + 水质指标联动）
# =====================================================================

if (!exists("%||%", mode = "function")) {
  `%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x
}

# ---------- UI 参数块（嵌入通用 mod_flux_method_page_ui 的 left_extra）----------

#' 复合法专属的左侧参数控件
#' @param ns  namespace 函数
#' @param key 方法键名（固定为 "composite"）
#' @return tagList
fw_composite_left_extra_ui <- function(ns, key = "composite") {
  shiny::tagList(

    # --- 数据选择：QF 表 / WQ 表 / 水质指标（与回归法一致）---
    shiny::selectInput(
      ns(paste0("qf_sheet_", key)),
      "\u6d41\u91cf\u8868\uff08QF\uff09",
      choices = character(0)
    ),
    shiny::selectInput(
      ns(paste0("wq_sheet_", key)),
      "\u6c34\u8d28\u8868\uff08WQ\uff09",
      choices = character(0)
    ),
    shiny::selectInput(
      ns(paste0("constituent_", key)),
      "\u6c34\u8d28\u6307\u6807",
      choices = character(0)
    ),

    shiny::tags$hr(),

    # --- 复合子方法 ---
    shiny::selectInput(
      ns(paste0("comp_sub_method_", key)),
      "\u590d\u5408\u5b50\u65b9\u6cd5",
      choices  = fw_composite_sub_choices(),
      selected = "abs_linear"
    ),

    # --- loadComp 参数（非 wrtds_kalman 时显示）---
    shiny::conditionalPanel(
      condition = paste0(
        "input['", ns(paste0("comp_sub_method_", key)), "'] != 'wrtds_kalman'"),
      shiny::selectInput(
        ns(paste0("comp_reg_model_", key)),
        "\u56de\u5f52\u7ed3\u6784\u6a21\u578b",
        choices  = c("\u5b63\u8282\u56de\u5f52" = "lm_season",
                     "\u7ebf\u6027\u56de\u5f52" = "lm_simple"),
        selected = "lm_season"
      ),
      shiny::selectInput(
        ns(paste0("comp_interp_method_", key)),
        "\u6b8b\u5dee\u63d2\u503c\u65b9\u6cd5",
        choices  = c("\u7ebf\u6027\u63d2\u503c" = "linearInterpolation",
                     "\u77e9\u5f62\u63d2\u503c" = "rectangularInterpolation"),
        selected = "linearInterpolation"
      )
    ),

    # --- WRTDS-Kalman 参数 ---
    shiny::conditionalPanel(
      condition = paste0(
        "input['", ns(paste0("comp_sub_method_", key)), "'] == 'wrtds_kalman'"),
      shiny::numericInput(ns(paste0("wrtds_windowY_", key)),
                          "WRTDS windowY (\u5e74)", value = 7, min = 1, max = 30, step = 1),
      shiny::numericInput(ns(paste0("wrtds_windowQ_", key)),
                          "WRTDS windowQ (logQ)", value = 2, min = 0.5, max = 10, step = 0.5),
      shiny::numericInput(ns(paste0("wrtds_windowS_", key)),
                          "WRTDS windowS (\u5b63\u8282)", value = 0.5, min = 0.1, max = 2, step = 0.1),
      shiny::numericInput(ns(paste0("kalman_rho_", key)),
                          "\u03c1 (Kalman AR\u7cfb\u6570)", value = 0.90, min = 0.01, max = 0.999, step = 0.05),
      shiny::numericInput(ns(paste0("kalman_niter_", key)),
                          "MC \u8fed\u4ee3\u6b21\u6570", value = 200, min = 50, max = 2000, step = 50)
    )
  )
}

# ---------- Server：QF/WQ 表名与指标联动 observers（与回归法一致）----------

#' 注册复合法的 QF 表、WQ 表、水质指标联动 observers
#' @param input, session  Shiny 标准
#' @param step1_qf_wq  reactive（fw_regression_step1_reactive 返回值）
#' @param key           方法键名，默认 "composite"
fw_composite_init_observers <- function(input, session, step1_qf_wq,
                                        key = "composite") {

  qf_id  <- paste0("qf_sheet_", key)
  wq_id  <- paste0("wq_sheet_", key)
  con_id <- paste0("constituent_", key)

  # ---- QF / WQ 表名同步 ----
  shiny::observe({
    s1 <- step1_qf_wq()
    if (is.null(s1)) {
      shiny::updateSelectInput(session, qf_id, choices = character(0))
      shiny::updateSelectInput(session, wq_id, choices = character(0))
      return()
    }

    qf_list <- fw_as_named_table_list(s1$QF, "QF")
    wq_list <- fw_as_named_table_list(s1$WQ, "WQ")
    if (length(qf_list) == 0 || length(wq_list) == 0) {
      shiny::updateSelectInput(session, qf_id, choices = character(0))
      shiny::updateSelectInput(session, wq_id, choices = character(0))
      return()
    }

    qf_nm <- names(qf_list)
    wq_nm <- names(wq_list)

    cur_qf <- shiny::isolate(input[[qf_id]])
    cur_wq <- shiny::isolate(input[[wq_id]])
    if (is.null(cur_qf) || !(cur_qf %in% qf_nm)) cur_qf <- qf_nm[1]
    if (is.null(cur_wq) || !(cur_wq %in% wq_nm)) cur_wq <- wq_nm[1]

    shiny::updateSelectInput(session, qf_id, choices = qf_nm, selected = cur_qf)
    shiny::updateSelectInput(session, wq_id, choices = wq_nm, selected = cur_wq)
  })

  # ---- 水质指标联动 ----
  shiny::observe({
    s1 <- step1_qf_wq()
    if (is.null(s1)) {
      shiny::updateSelectInput(session, con_id, choices = character(0))
      return()
    }

    wq_list <- fw_as_named_table_list(s1$WQ, "WQ")
    if (length(wq_list) == 0) {
      shiny::updateSelectInput(session, con_id, choices = character(0))
      return()
    }

    ws <- input[[wq_id]]
    if (is.null(ws) || !(ws %in% names(wq_list))) ws <- names(wq_list)[1]

    cands <- fw_get_wq_constituents(wq_list[[ws]])
    if (length(cands) == 0) cands <- names(wq_list[[ws]])

    cur <- shiny::isolate(input[[con_id]])
    if (is.null(cur) || !(cur %in% cands)) cur <- cands[1]

    shiny::updateSelectInput(session, con_id, choices = cands, selected = cur)
  })
}

# ---------- Server：复合法数据获取（与回归法一致）----------

#' 复合法的 daily_all 数据获取
#' 根据 QF 表 + WQ 表 + 水质指标合并后调用 fw_prepare_flux_data
#' @param input        Shiny input
#' @param key          方法键名 "composite"
#' @param step1_qf_wq  reactive（返回 list(QF=..., WQ=...)）
#' @return data.frame 或 NULL
fw_composite_get_daily_all <- function(input, key, step1_qf_wq) {
  s1 <- step1_qf_wq()
  if (is.null(s1)) return(NULL)

  qf_list <- fw_as_named_table_list(s1$QF, "QF")
  wq_list <- fw_as_named_table_list(s1$WQ, "WQ")
  if (length(qf_list) == 0 || length(wq_list) == 0) return(NULL)

  qf_sel <- input[[paste0("qf_sheet_", key)]]
  wq_sel <- input[[paste0("wq_sheet_", key)]]
  con    <- input[[paste0("constituent_", key)]]

  if (is.null(qf_sel) || is.null(wq_sel) || is.null(con)) return(NULL)
  if (!(qf_sel %in% names(qf_list)) || !(wq_sel %in% names(wq_list))) return(NULL)

  qf_df <- qf_list[[qf_sel]]
  wq_df <- wq_list[[wq_sel]]

  tryCatch(
    fw_prepare_flux_data(qf_df = qf_df, wq_df = wq_df, constituent = con),
    error = function(e) NULL
  )
}

# ---------- Server：复合法方法原理文本（完整版，全部一次性返回）----------

#' 复合法原理完整文本（全部子方法 + WRTDS-Kalman）
#' 当前选中的子方法用高亮背景标记
#' @param active_sub  当前选中的子方法键名
#' @return character (HTML + MathJax)
fw_composite_principle_text <- function(active_sub = "abs_linear") {

  # ---- 高亮辅助 ----
  hl_start <- function(sub) {
    if (identical(active_sub, sub))
      '<div style="background:#fffbe6; border-left:4px solid #f5a623; padding:8px 12px; margin:6px 0; border-radius:4px;">'
    else
      '<div style="padding:4px 0;">'
  }
  hl_end <- "</div>"

  paste0(
    # ================================================================
    # 大标题 + 概述
    # ================================================================
    "<h4>\u590d\u5408\u65b9\u6cd5\uff08Composite Method\uff09</h4>",

    "<h4>1. loadflex::loadComp &mdash; \u56de\u5f52&ldquo;\u7ed3\u6784\u9879&rdquo; + \u63d2\u503c&ldquo;\u6821\u6b63\u9879&rdquo;</h4>",
    "<p>\u5148\u7528\u56de\u5f52\u6a21\u578b\u5f97\u5230\u5e73\u6ed1\u7ed3\u6784\u9884\u6d4b\uff0c\u518d\u628a&ldquo;\u6b8b\u5dee&rdquo;\u4f5c\u4e3a\u6821\u6b63\u9879\u63d2\u503c\u5230\u8fde\u7eed\u65f6\u95f4\u6b65\uff0c\u6700\u540e\u5408\u6210\u3002",
    "\u56de\u5f52\u9884\u6d4b\uff08\u7ed3\u6784\u9879\uff09\u4e3a \\(\\widehat{L}_{\\text{reg}}(t)\\)\uff08\u6216 \\(\\widehat{C}_{\\text{reg}}(t)\\)\uff09\uff0c",
    "\u6b8b\u5dee\u63d2\u503c\u5f97\u5230\u7684\u6821\u6b63\u9879\u4e3a \\(\\widehat{R}(t)\\)\u3002",
    "loadComp \u5728\u5904\u7406\u6b8b\u5dee\u9879\u65f6\u4f1a\u4f7f\u7528\u63d2\u503c\u65b9\u6cd5\u3002</p>",

    # ================================================================
    # 1.1 绝对残差 + 线性空间
    # ================================================================
    hl_start("abs_linear"),
    "<h5>1.1 absolute residuals + linear space\uff1a\u7edd\u5bf9\u6b8b\u5dee + \u7ebf\u6027\u7a7a\u95f4</h5>",
    "\\[\\widehat{L}(t)=\\widehat{L}_{\\text{reg}}(t)+\\widehat{R}(t)\\]",
    "<ul>",
    "<li>\\(\\widehat{L}(t)\\)\uff1a\u6700\u7ec8\u590d\u5408\u9884\u6d4b\u901a\u91cf</li>",
    "<li>\\(\\widehat{L}_{\\text{reg}}(t)\\)\uff1a\u56de\u5f52\u6a21\u578b\u7ed9\u51fa\u7684\u901a\u91cf\u9884\u6d4b</li>",
    "<li>\\(\\widehat{R}(t)\\)\uff1a\u5bf9&ldquo;\u7edd\u5bf9\u6b8b\u5dee&rdquo;\u63d2\u503c\u5f97\u5230\u7684\u6821\u6b63\u91cf\uff08\u4e0e \\(\\widehat{L}\\) \u540c\u5355\u4f4d\uff09</li>",
    "</ul>",
    "<p><b>\u539f\u7406\uff1a</b>\u628a\u6b8b\u5dee\u5f53\u6210&ldquo;\u53ef\u968f\u65f6\u95f4\u5e73\u6ed1\u53d8\u5316\u7684\u504f\u5dee&rdquo;\uff0c\u76f4\u63a5\u52a0\u56de\u7ed3\u6784\u9879\u3002</p>",
    "<p><b>\u9002\u7528\u60c5\u51b5\uff1a</b>\u504f\u5dee\u66f4\u50cf&ldquo;\u52a0\u6027\u504f\u79fb&rdquo;\uff08\u4f8b\u5982\u957f\u671f\u7cfb\u7edf\u504f\u9ad8/\u504f\u4f4e\uff0c\u4ee5\u540c\u5355\u4f4d\u589e\u51cf\u4f53\u73b0\uff09\u3002</p>",
    hl_end,

    # ================================================================
    # 1.2 相对残差 + 线性空间
    # ================================================================
    hl_start("rel_linear"),
    "<h5>1.2 relative residuals + linear space\uff1a\u76f8\u5bf9\u504f\u5dee + \u7ebf\u6027\u7a7a\u95f4</h5>",
    "\\[\\widehat{L}(t)=\\widehat{L}_{\\text{reg}}(t)\\bigl(1+\\widehat{R}(t)\\bigr)\\]",
    "<ul>",
    "<li>\\(\\widehat{L}(t)\\)\uff1a\u6700\u7ec8\u590d\u5408\u9884\u6d4b\u901a\u91cf</li>",
    "<li>\\(\\widehat{L}_{\\text{reg}}(t)\\)\uff1a\u56de\u5f52\u6a21\u578b\u7ed9\u51fa\u7684\u901a\u91cf\u9884\u6d4b</li>",
    "<li>\\(\\widehat{R}(t)\\)\uff1a\u5bf9&ldquo;\u76f8\u5bf9\u6b8b\u5dee&rdquo;\u63d2\u503c\u5f97\u5230\u7684\u6821\u6b63\u91cf\uff08\u65e0\u91cf\u7eb2\uff09\uff0c",
    "\u5178\u578b\u5b9a\u4e49 \\(R=\\dfrac{L_{\\text{obs}}-L_{\\text{reg}}}{L_{\\text{reg}}}\\)</li>",
    "</ul>",
    "<p><b>\u539f\u7406\uff1a</b>\u628a\u504f\u5dee\u89c6\u4e3a&ldquo;\u4e58\u6027\u6bd4\u4f8b\u8bef\u5dee&rdquo;\uff0c\u7528\u6bd4\u4f8b\u4fee\u6b63\u7ed3\u6784\u9879\u3002</p>",
    "<p><b>\u9002\u7528\u60c5\u51b5\uff1a</b>\u504f\u5dee\u4e0e\u91cf\u7ea7\u76f8\u5173\uff08\u9ad8\u8d1f\u8377\u65f6\u504f\u5dee\u66f4\u5927\uff09\uff0c\u4e58\u6027\u4fee\u6b63\u66f4\u81ea\u7136\u3002</p>",
    hl_end,

    # ================================================================
    # 1.3 绝对残差 + log 空间
    # ================================================================
    hl_start("abs_log"),
    "<h5>1.3 absolute residuals + log space\uff1a\u7edd\u5bf9\u6b8b\u5dee + log \u7a7a\u95f4</h5>",
    "\\[\\log\\widehat{L}(t)=\\log\\widehat{L}_{\\text{reg}}(t)+\\widehat{R}(t)\\]",
    "<ul>",
    "<li>\\(\\widehat{L}(t)\\)\uff1a\u6700\u7ec8\u590d\u5408\u9884\u6d4b\u901a\u91cf</li>",
    "<li>\\(\\widehat{L}_{\\text{reg}}(t)\\)\uff1a\u56de\u5f52\u6a21\u578b\u7ed9\u51fa\u7684\u901a\u91cf\u9884\u6d4b</li>",
    "<li>\\(\\widehat{R}(t)\\)\uff1a log \u7a7a\u95f4\u7684\u7edd\u5bf9\u6b8b\u5dee\u6821\u6b63\u9879</li>",
    "</ul>",
    "<p><b>\u539f\u7406\uff1a</b>log \u7a7a\u95f4\u7684\u52a0\u6027\u7b49\u4e8e\u7ebf\u6027\u7a7a\u95f4\u7684\u4e58\u6027\uff1b\u5e38\u7528\u4e8e\u53f3\u504f\u4e14\u4e58\u6027\u566a\u58f0\u660e\u663e\u7684\u901a\u91cf\u5e8f\u5217\u3002</p>",
    "<p><b>\u9002\u7528\u60c5\u51b5\uff1a</b>\u901a\u91cf\u5448\u5bf9\u6570\u6b63\u6001\u7279\u5f81\u3001\u8bef\u5dee\u66f4\u50cf\u500d\u6570\u8bef\u5dee\u65f6\uff1b\u56de\u53d8\u6362\u65f6\u901a\u5e38\u8fd8\u9700\u8981\u8003\u8651\u504f\u5dee\u4fee\u6b63\u3002</p>",
    hl_end,

    # ================================================================
    # 1.4 相对残差 + log 空间
    # ================================================================
    hl_start("rel_log"),
    "<h5>1.4 relative residuals + log space\uff1a\u76f8\u5bf9\u6b8b\u5dee + log \u7a7a\u95f4</h5>",
    "\\[\\log\\widehat{L}(t)=\\log\\widehat{L}_{\\text{reg}}(t)",
    "+\\log\\!\\bigl(1+\\widehat{R}(t)\\bigr)\\]",
    "<ul>",
    "<li>\\(\\widehat{L}(t)\\)\uff1a\u6700\u7ec8\u590d\u5408\u9884\u6d4b\u901a\u91cf</li>",
    "<li>\\(\\widehat{L}_{\\text{reg}}(t)\\)\uff1a\u56de\u5f52\u6a21\u578b\u7ed9\u51fa\u7684\u901a\u91cf\u9884\u6d4b</li>",
    "<li>\\(\\widehat{R}(t)\\)\uff1a\u7ebf\u6027\u5b9a\u4e49\u4e0b\u7684\u76f8\u5bf9\u6b8b\u5dee\uff08\u65e0\u91cf\u7eb2\uff09</li>",
    "<li>\\(\\log(1+\\widehat{R}(t))\\)\uff1a\u628a\u76f8\u5bf9\u6b8b\u5dee\u8f6c\u4e3a log \u7a7a\u95f4\u7684\u53ef\u52a0\u4fee\u6b63\u9879</li>",
    "</ul>",
    "<p><b>\u539f\u7406\uff1a</b>\u5148\u5728&ldquo;\u76f8\u5bf9\u8bef\u5dee&rdquo;\u610f\u4e49\u4e0b\u4fee\u6b63\uff0c\u518d\u8f6c\u5230 log \u7a7a\u95f4\u5408\u6210\uff0c\u517c\u987e\u6bd4\u4f8b\u8bef\u5dee\u4e0e log \u7a33\u5065\u6027\u3002</p>",
    "<p><b>\u9002\u7528\u60c5\u51b5\uff1a</b>\u65e2\u5e0c\u671b\u76f8\u5bf9\u4fee\u6b63\u3001\u53c8\u5e0c\u671b\u5728 log \u7a7a\u95f4\u5904\u7406\uff08\u4f8b\u5982\u6781\u7aef\u503c\u591a\u3001\u5c3a\u5ea6\u8de8\u5ea6\u5927\uff09\u3002</p>",
    hl_end,

    # ================================================================
    # 分割线
    # ================================================================
    "<hr>",

    # ================================================================
    # 2. WRTDS-Kalman（完整原理 2.1–2.7）
    # ================================================================
    hl_start("wrtds_kalman"),

    "<h4>2. EGRET \u4e2d WRTDS&ndash;Kalman\uff08WRTDS_K\uff09\u7684\u8d1f\u8377\u8ba1\u7b97</h4>",
    "<p>WRTDS&ndash;Kalman\uff08WRTDS_K\uff09\u662f\u5728 WRTDS \u57fa\u7840\u4e0a\u52a0\u5165\u65f6\u95f4\u76f8\u5173\u6b8b\u5dee\u4fee\u6b63\u7684\u65b9\u6cd5\u3002",
    "\u5176\u6838\u5fc3\u601d\u60f3\u4e0d\u662f\u76f4\u63a5\u5bf9\u6d53\u5ea6\u518d\u505a\u4e00\u6b21\u666e\u901a\u56de\u5f52\uff0c\u800c\u662f\u628a WRTDS \u5728\u5bf9\u6570\u7a7a\u95f4\u4e2d\u7684\u6807\u51c6\u5316\u6b8b\u5dee\u4f5c\u4e3a\u4e00\u4e2a\u968f\u65f6\u95f4\u6f14\u5316\u7684\u8fc7\u7a0b\uff1a",
    "\u5728\u6709\u89c2\u6d4b\u7684\u65e5\u671f\uff0c\u76f4\u63a5\u4f7f\u7528\u89c2\u6d4b\u4fe1\u606f\uff1b",
    "\u5728\u65e0\u89c2\u6d4b\u7684\u65e5\u671f\uff0c\u6839\u636e\u76f8\u90bb\u91c7\u6837\u65e5\u4e4b\u95f4\u6b8b\u5dee\u7684\u65f6\u95f4\u76f8\u5173\u6027\uff0c",
    "\u7528\u6761\u4ef6 AR(1) \u8fc7\u7a0b\u548c Monte Carlo \u65b9\u6cd5\u8865\u5168\u6b8b\u5dee\u5e8f\u5217\uff0c",
    "\u8fdb\u800c\u5f97\u5230\u9010\u65e5\u6d53\u5ea6\u548c\u9010\u65e5\u8d1f\u8377\u4f30\u8ba1\u3002</p>",

    # ---- 2.1 WRTDS 对数空间拟合 ----
    "<h5>2.1 WRTDS \u7684\u5bf9\u6570\u7a7a\u95f4\u62df\u5408\u8868\u8fbe\u5f0f</h5>",
    "\\[\\ln(c_t)=\\beta_{0t}+\\beta_{1t}\\ln(Q_t)+\\beta_{2t}T_t",
    "+\\beta_{3t}\\sin(2\\pi T_t)+\\beta_{4t}\\cos(2\\pi T_t)+\\sigma_t z_t\\]",
    "<ul>",
    "<li>\\(\\ln(c_t)\\)\uff1a\u7b2c \\(t\\) \u65e5\u6d53\u5ea6\u7684\u81ea\u7136\u5bf9\u6570</li>",
    "<li>\\(c_t\\)\uff1a\u7b2c \\(t\\) \u65e5\u6d53\u5ea6</li>",
    "<li>\\(Q_t\\)\uff1a\u7b2c \\(t\\) \u65e5\u5e73\u5747\u6d41\u91cf</li>",
    "<li>\\(T_t\\)\uff1a\u7b2c \\(t\\) \u65e5\u7684\u5c0f\u6570\u5e74\u4efd\u65f6\u95f4</li>",
    "<li>\\(\\beta_{0t},\\beta_{1t},\\beta_{2t},\\beta_{3t},\\beta_{4t}\\)\uff1aWRTDS \u5728\u7b2c \\(t\\) \u65e5\u5bf9\u5e94\u7684\u5c40\u90e8\u52a0\u6743\u56de\u5f52\u7cfb\u6570</li>",
    "<li>\\(\\sigma_t\\)\uff1a\u7b2c \\(t\\) \u65e5\u6761\u4ef6\u8bef\u5dee\u6807\u51c6\u5dee</li>",
    "<li>\\(z_t\\)\uff1a\u7b2c \\(t\\) \u65e5\u6807\u51c6\u5316\u6b8b\u5dee</li>",
    "</ul>",
    "<p><b>\u539f\u7406\uff1a</b>WRTDS \u8ba4\u4e3a\u5bf9\u6570\u6d53\u5ea6\u53ef\u4ee5\u8868\u793a\u4e3a\u6d41\u91cf\u3001\u65f6\u95f4\u548c\u5b63\u8282\u9879\u7684\u5e73\u6ed1\u51fd\u6570\uff0c",
    "\u518d\u52a0\u4e0a\u4e00\u4e2a\u8bef\u5dee\u9879\uff1b\u5176\u4e2d\u8bef\u5dee\u9879\u88ab\u5199\u4e3a \\(\\sigma_t z_t\\)\uff0c\u4fbf\u4e8e\u540e\u7eed\u5c06\u6b8b\u5dee\u6807\u51c6\u5316\u5904\u7406\u3002</p>",
    "<p><b>\u9002\u7528\u60c5\u51b5\uff1a</b>\u9002\u7528\u4e8e\u5df2\u6709 WRTDS \u57fa\u7840\u6a21\u578b\u3001\u5e76\u5e0c\u671b\u5728\u9010\u65e5\u5c3a\u5ea6\u4e0a\u8fdb\u4e00\u6b65\u4fee\u6b63\u7cfb\u7edf\u504f\u5dee\u7684\u60c5\u5f62\u3002</p>",

    # ---- 2.2 观测日残差与标准化残差 ----
    "<h5>2.2 \u89c2\u6d4b\u65e5\u6b8b\u5dee\u4e0e\u6807\u51c6\u5316\u6b8b\u5dee</h5>",
    "<p>\u5148\u5b9a\u4e49 WRTDS \u7684\u5bf9\u6570\u7a7a\u95f4\u62df\u5408\u5747\u503c\uff1a</p>",
    "\\[\\hat{y}_t=\\beta_{0t}+\\beta_{1t}\\ln(Q_t)+\\beta_{2t}T_t",
    "+\\beta_{3t}\\sin(2\\pi T_t)+\\beta_{4t}\\cos(2\\pi T_t)\\]",
    "<p>\u5219\u89c2\u6d4b\u65e5\u7684\u539f\u59cb\u5bf9\u6570\u6b8b\u5dee\u4e3a\uff1a</p>",
    "\\[r_t=\\ln\\!\\left(C_t^{\\text{obs}}\\right)-\\hat{y}_t\\]",
    "<p>\u8fdb\u4e00\u6b65\u5b9a\u4e49\u6807\u51c6\u5316\u6b8b\u5dee\uff1a</p>",
    "\\[z_t=\\frac{r_t}{\\sigma_t}\\]",
    "<ul>",
    "<li>\\(\\hat{y}_t\\)\uff1a\u7b2c \\(t\\) \u65e5 WRTDS \u5bf9\u6570\u6d53\u5ea6\u7684\u62df\u5408\u5747\u503c</li>",
    "<li>\\(C_t^{\\text{obs}}\\)\uff1a\u7b2c \\(t\\) \u65e5\u89c2\u6d4b\u6d53\u5ea6</li>",
    "<li>\\(r_t\\)\uff1a\u7b2c \\(t\\) \u65e5\u539f\u59cb\u5bf9\u6570\u6b8b\u5dee</li>",
    "<li>\\(z_t\\)\uff1a\u7b2c \\(t\\) \u65e5\u6807\u51c6\u5316\u6b8b\u5dee</li>",
    "<li>\\(\\sigma_t\\)\uff1a\u7b2c \\(t\\) \u65e5\u6761\u4ef6\u8bef\u5dee\u6807\u51c6\u5dee</li>",
    "</ul>",
    "<p>WRTDS&ndash;Kalman \u5b9e\u9645\u4fee\u6b63\u7684\u4e0d\u662f\u539f\u59cb\u6b8b\u5dee\uff0c\u800c\u662f\u6807\u51c6\u5316\u6b8b\u5dee\u3002",
    "\u8fd9\u6837\u505a\u53ef\u4ee5\u6d88\u9664\u4e0d\u540c\u65e5\u671f\u8bef\u5dee\u5c3a\u5ea6\u4e0d\u4e00\u81f4\u7684\u95ee\u9898\uff0c",
    "\u4f7f\u6b8b\u5dee\u8fc7\u7a0b\u66f4\u9002\u5408\u7528\u7edf\u4e00\u7684\u65f6\u95f4\u76f8\u5173\u7ed3\u6784\u6765\u5efa\u6a21\u3002</p>",

    # ---- 2.3 无观测日的条件 AR(1) 残差生成 ----
    "<h5>2.3 \u65e0\u89c2\u6d4b\u65e5\u7684\u6761\u4ef6 AR(1) \u6b8b\u5dee\u751f\u6210</h5>",
    "<p>\u5bf9\u4e8e\u4e24\u4e2a\u76f8\u90bb\u91c7\u6837\u65e5\u4e4b\u95f4\u7684\u65e0\u89c2\u6d4b\u533a\u95f4\uff0c",
    "\u8bbe\u533a\u95f4\u8d77\u70b9\u548c\u7ec8\u70b9\u7684\u6807\u51c6\u5316\u6b8b\u5dee\u5206\u522b\u4e3a \\(z_1\\) \u548c \\(z_n\\)\uff0c",
    "\u5219\u4e2d\u95f4\u65e5\u671f\u7684\u6807\u51c6\u5316\u6b8b\u5dee\u6309\u6761\u4ef6 AR(1) \u8fc7\u7a0b\u751f\u6210\uff1a</p>",
    "\\[z_{k+1}=\\rho\\, z_k+\\sqrt{1-\\rho^2}\\,e_k,\\qquad e_k\\sim N(0,1),\\quad 1<k<n\\]",
    "<ul>",
    "<li>\\(z_k\\)\uff1a\u65e0\u89c2\u6d4b\u533a\u95f4\u5185\u7b2c \\(k\\) \u4e2a\u4f4d\u7f6e\u7684\u6807\u51c6\u5316\u6b8b\u5dee</li>",
    "<li>\\(\\rho\\)\uff1aAR(1) \u6ede\u540e 1 \u9636\u76f8\u5173\u7cfb\u6570</li>",
    "<li>\\(e_k\\)\uff1a\u72ec\u7acb\u6807\u51c6\u6b63\u6001\u968f\u673a\u53d8\u91cf</li>",
    "<li>\\(z_1, z_n\\)\uff1a\u65e0\u89c2\u6d4b\u533a\u95f4\u4e24\u7aef\u3001\u7531\u5b9e\u9645\u91c7\u6837\u65e5\u786e\u5b9a\u7684\u6807\u51c6\u5316\u6b8b\u5dee</li>",
    "</ul>",
    "<p>WRTDS&ndash;Kalman \u5047\u5b9a\u77ed\u65f6\u6ede\u4e0b\u7684\u6807\u51c6\u5316\u6b8b\u5dee\u5177\u6709\u81ea\u76f8\u5173\u6027\uff0c",
    "\u56e0\u6b64\u5728\u76f8\u90bb\u91c7\u6837\u65e5\u4e4b\u95f4\uff0c\u4e0d\u662f\u7b80\u5355\u7ebf\u6027\u63d2\u503c\uff0c",
    "\u800c\u662f\u6309\u7167 AR(1) \u76f8\u5173\u7ed3\u6784\u751f\u6210\u6b8b\u5dee\u5e8f\u5217\uff1b",
    "\u540c\u65f6\u8be5\u751f\u6210\u8fc7\u7a0b\u53d7\u533a\u95f4\u4e24\u7aef\u5df2\u77e5\u6b8b\u5dee\u7ea6\u675f\uff0c",
    "\u56e0\u6b64\u5c5e\u4e8e&ldquo;\u6761\u4ef6\u751f\u6210&rdquo;\uff0c\u800c\u4e0d\u662f\u5355\u7eaf\u5411\u524d\u9012\u63a8\u3002</p>",

    # ---- 2.4 由标准化残差生成逐日浓度 ----
    "<h5>2.4 \u7531\u6807\u51c6\u5316\u6b8b\u5dee\u751f\u6210\u9010\u65e5\u6d53\u5ea6</h5>",
    "<p>\u5bf9\u4efb\u610f\u4e00\u6b21 Monte Carlo \u91cd\u590d\u6a21\u62df\uff0c\u7b2c \\(t\\) \u65e5\u751f\u6210\u6d53\u5ea6\u4e3a\uff1a</p>",
    "\\[C_t^{(m)}=\\exp\\!\\left(\\hat{y}_t+\\sigma_t\\, z_t^{(m)}\\right)\\]",
    "<ul>",
    "<li>\\(C_t^{(m)}\\)\uff1a\u7b2c \\(m\\) \u6b21\u6a21\u62df\u4e0b\u7b2c \\(t\\) \u65e5\u751f\u6210\u6d53\u5ea6</li>",
    "<li>\\(z_t^{(m)}\\)\uff1a\u7b2c \\(m\\) \u6b21\u6a21\u62df\u4e0b\u7b2c \\(t\\) \u65e5\u7684\u6807\u51c6\u5316\u6b8b\u5dee</li>",
    "<li>\\(\\hat{y}_t\\)\uff1a\u7b2c \\(t\\) \u65e5 WRTDS \u7684\u5bf9\u6570\u7a7a\u95f4\u62df\u5408\u5747\u503c</li>",
    "<li>\\(\\sigma_t\\)\uff1a\u7b2c \\(t\\) \u65e5\u6761\u4ef6\u8bef\u5dee\u6807\u51c6\u5dee</li>",
    "</ul>",
    "<p>\u5148\u5728\u5bf9\u6570\u7a7a\u95f4\u4e2d\u628a&ldquo;WRTDS \u5e73\u6ed1\u7ed3\u6784&rdquo;\u4e0e&ldquo;\u52a8\u6001\u6b8b\u5dee\u4fee\u6b63&rdquo;\u76f8\u52a0\uff0c",
    "\u518d\u6307\u6570\u53d8\u6362\u56de\u539f\u59cb\u6d53\u5ea6\u7a7a\u95f4\u3002",
    "\u8fd9\u91cc\u4f30\u8ba1\u7684\u662f\u67d0\u4e00\u6b21\u5b9e\u73b0\u503c\uff0c\u56e0\u6b64\u4e0d\u518d\u52a0\u5165\u6807\u51c6 WRTDS \u671f\u671b\u503c\u516c\u5f0f\u4e2d\u7684 \\(\\sigma_t^2/2\\) \u504f\u5dee\u4fee\u6b63\u9879\u3002</p>",

    "<p>\u5bf9 \\(M\\) \u6b21\u91cd\u590d\u6a21\u62df\u53d6\u5747\u503c\uff0c\u53ef\u5f97\u5230 WRTDS&ndash;Kalman \u7684\u9010\u65e5\u6821\u6b63\u6d53\u5ea6\uff1a</p>",
    "\\[\\widehat{C}_t^{\\,wk}=\\frac{1}{M}\\sum_{m=1}^{M} C_t^{(m)}\\]",
    "<ul>",
    "<li>\\(\\widehat{C}_t^{\\,wk}\\)\uff1a\u7b2c \\(t\\) \u65e5\u6700\u7ec8\u7684 WRTDS&ndash;Kalman \u6821\u6b63\u6d53\u5ea6</li>",
    "<li>\\(M\\)\uff1aMonte Carlo \u91cd\u590d\u6b21\u6570</li>",
    "</ul>",
    "<p>\u5355\u6b21\u6a21\u62df\u5305\u542b\u968f\u673a\u6027\uff0c\u56e0\u6b64 EGRET \u901a\u8fc7\u591a\u6b21\u91cd\u590d\u751f\u6210\u5e76\u53d6\u5747\u503c\uff0c\u5f97\u5230\u7a33\u5b9a\u7684\u9010\u65e5\u6d53\u5ea6\u4f30\u8ba1\u3002</p>",

    # ---- 2.5 由校正浓度生成逐日负荷 ----
    "<h5>2.5 \u7531\u6821\u6b63\u6d53\u5ea6\u751f\u6210\u9010\u65e5\u8d1f\u8377</h5>",
    "<p>\u5bf9\u4efb\u610f\u4e00\u6b21 Monte Carlo \u91cd\u590d\u6a21\u62df\uff0c\u7b2c \\(t\\) \u65e5\u8d1f\u8377\u4e3a\uff1a</p>",
    "\\[F_t^{(m)} = 86.4\\,Q_t\\,C_t^{(m)}\\]",
    "<ul>",
    "<li>\\(F_t^{(m)}\\)\uff1a\u7b2c \\(m\\) \u6b21\u6a21\u62df\u4e0b\u7b2c \\(t\\) \u65e5\u751f\u6210\u8d1f\u8377</li>",
    "<li>\\(Q_t\\)\uff1a\u7b2c \\(t\\) \u65e5\u6d41\u91cf\uff0c\u5355\u4f4d\u901a\u5e38\u4e3a m\\(^3\\) s\\(^{-1}\\)</li>",
    "<li>\\(C_t^{(m)}\\)\uff1a\u7b2c \\(m\\) \u6b21\u6a21\u62df\u4e0b\u7b2c \\(t\\) \u65e5\u751f\u6210\u6d53\u5ea6\uff0c\u5355\u4f4d\u901a\u5e38\u4e3a mg L\\(^{-1}\\)</li>",
    "<li>\\(86.4\\)\uff1a\u5355\u4f4d\u6362\u7b97\u7cfb\u6570\uff0c\u7528\u4e8e\u5c06 m\\(^3\\) s\\(^{-1}\\) \u4e0e mg L\\(^{-1}\\) \u8f6c\u6362\u4e3a kg day\\(^{-1}\\)</li>",
    "</ul>",
    "<p>\u9010\u65e5\u8d1f\u8377\u672c\u8d28\u4e0a\u4ecd\u7136\u662f&ldquo;\u6d41\u91cf \u00d7 \u6d53\u5ea6&rdquo;\uff0c",
    "WRTDS&ndash;Kalman \u7684\u4f5c\u7528\u5728\u4e8e\u5148\u7ed9\u51fa\u52a8\u6001\u4fee\u6b63\u540e\u7684\u9010\u65e5\u6d53\u5ea6\uff0c",
    "\u518d\u4e0e\u9010\u65e5\u6d41\u91cf\u76f8\u4e58\u5f97\u5230\u9010\u65e5\u8d1f\u8377\u3002</p>",

    "<p>\u5bf9 \\(M\\) \u6b21\u91cd\u590d\u6a21\u62df\u53d6\u5747\u503c\uff0c\u53ef\u5f97\u5230\u6700\u7ec8\u9010\u65e5\u6821\u6b63\u8d1f\u8377\uff1a</p>",
    "\\[\\widehat{F}_t^{\\,wk}=\\frac{1}{M}\\sum_{m=1}^{M} F_t^{(m)}\\]",
    "<ul>",
    "<li>\\(\\widehat{F}_t^{\\,wk}\\)\uff1a\u7b2c \\(t\\) \u65e5\u6700\u7ec8\u7684 WRTDS&ndash;Kalman \u6821\u6b63\u8d1f\u8377</li>",
    "</ul>",
    "<p>\u4e0e\u9010\u65e5\u6d53\u5ea6\u76f8\u540c\uff0c\u9010\u65e5\u8d1f\u8377\u4e5f\u662f\u901a\u8fc7\u591a\u6b21\u751f\u6210\u540e\u53d6\u5747\u503c\u5f97\u5230\u7a33\u5b9a\u4f30\u8ba1\u3002</p>",

    # ---- 2.6 年尺度负荷聚合 ----
    "<h5>2.6 \u5e74\u5c3a\u5ea6\u8d1f\u8377\u805a\u5408</h5>",
    "<p>\u82e5\u5c06\u4e00\u5e74\u5185\u9010\u65e5\u6821\u6b63\u8d1f\u8377\u7d2f\u52a0\uff0c\u5219\u53ef\u5f97\u5230\u5e74\u5ea6\u805a\u5408\u8d1f\u8377\uff1a</p>",
    "\\[L_{\\text{year}}=\\sum_{j=1}^{365}\\widehat{F}_j^{\\,wk}",
    "=\\sum_{j=1}^{365} 86.4\\,Q_j\\,\\widehat{C}_j^{\\,wk}\\]",
    "<ul>",
    "<li>\\(L_{\\text{year}}\\)\uff1a\u5e74\u5c3a\u5ea6\u805a\u5408\u8d1f\u8377</li>",
    "<li>\\(\\widehat{F}_j^{\\,wk}\\)\uff1a\u7b2c \\(j\\) \u65e5 WRTDS&ndash;Kalman \u6821\u6b63\u8d1f\u8377</li>",
    "<li>\\(\\widehat{C}_j^{\\,wk}\\)\uff1a\u7b2c \\(j\\) \u65e5 WRTDS&ndash;Kalman \u6821\u6b63\u6d53\u5ea6</li>",
    "<li>\\(Q_j\\)\uff1a\u7b2c \\(j\\) \u65e5\u6d41\u91cf</li>",
    "</ul>",
    "<p>\u5e74\u8d1f\u8377\u7531\u5168\u5e74\u9010\u65e5\u8d1f\u8377\u7d2f\u52a0\u5f97\u5230\u3002",
    "WRTDS&ndash;Kalman \u7684\u6539\u8fdb\u4e0d\u5728\u4e8e\u6539\u53d8\u8d1f\u8377\u5b9a\u4e49\u672c\u8eab\uff0c",
    "\u800c\u5728\u4e8e\u63d0\u9ad8\u6bcf\u4e00\u65e5\u6d53\u5ea6\u548c\u8d1f\u8377\u4f30\u8ba1\u7684\u5408\u7406\u6027\uff0c",
    "\u8fdb\u800c\u6539\u5584\u6708\u3001\u5b63\u3001\u5e74\u5c3a\u5ea6\u805a\u5408\u7ed3\u679c\u3002</p>",

    # ---- 2.7 适用情况 ----
    "<h5>2.7 \u9002\u7528\u60c5\u51b5</h5>",
    "<p><b>\u9002\u5408\uff1a</b>\u5f53\u7814\u7a76\u76ee\u6807\u662f\u83b7\u5f97\u67d0\u4e00\u65e5\u3001\u67d0\u4e00\u6708\u3001\u67d0\u4e00\u5b63\u6216\u67d0\u4e00\u5e74\u7684\u6700\u4f73\u6d53\u5ea6/\u8d1f\u8377\u4f30\u8ba1\uff0c",
    "\u5e76\u4e14\u89c2\u6d4b\u6b8b\u5dee\u5b58\u5728\u77ed\u65f6\u95f4\u5c3a\u5ea6\u7684\u6301\u7eed\u504f\u9ad8\u6216\u504f\u4f4e\u73b0\u8c61\u65f6\uff0c",
    "WRTDS&ndash;Kalman \u5f80\u5f80\u6bd4\u666e\u901a WRTDS \u66f4\u5408\u9002\u3002</p>",
    "<p><b>\u4e0d\u9002\u5408\uff1a</b>\u82e5\u7814\u7a76\u76ee\u6807\u662f\u957f\u671f\u8d8b\u52bf\u8bc6\u522b\uff0c\u5219\u66f4\u9002\u5408\u4f7f\u7528\u6d41\u91cf\u5f52\u4e00\u5316\u7ed3\u679c\uff0c",
    "\u800c\u4e0d\u662f WRTDS&ndash;Kalman \u7684\u9010\u65e5\u751f\u6210\u503c\uff1b",
    "\u53e6\u5916\uff0c\u5f53\u91c7\u6837\u6781\u5176\u7a00\u758f\u3001\u6b8b\u5dee\u77ed\u671f\u81ea\u76f8\u5173\u5f88\u5f31\u65f6\uff0c",
    "WRTDS&ndash;Kalman \u4e0e\u666e\u901a WRTDS \u7684\u5dee\u5f02\u901a\u5e38\u4e0d\u4f1a\u5f88\u5927\u3002</p>",

    hl_end
  )
}

# ---------- Server：复合法原理渲染 ----------

fw_composite_render_principle <- function(input, key = "composite") {
  sub <- input[[paste0("comp_sub_method_", key)]] %||% "abs_linear"
  shiny::withMathJax(
    shiny::tags$div(
      style = "font-size:14px; line-height:1.8; max-height:600px; overflow-y:auto;",
      shiny::HTML(fw_composite_principle_text(sub))
    )
  )
}

# ---------- Server：复合法计算调度 ----------

#' 复合法的"开始计算"逻辑
#' @param input          Shiny input
#' @param daily_station  reactive (返回 data.frame)
#' @param key            方法键名 "composite"
#' @param conv_factor    换算系数
#' @return res list 或 NULL
fw_composite_run_calc <- function(input, daily_station, key = "composite",
                                  conv_factor = 86.4) {
  d <- daily_station()
  if (is.null(d) || nrow(d) == 0) {
    shiny::showNotification("\u5f53\u524d\u6570\u636e\u9009\u62e9\u4e0d\u53ef\u7528\u4e8e\u901a\u91cf\u8ba1\u7b97\u3002", type = "error")
    return(NULL)
  }

  sub <- input[[paste0("comp_sub_method_", key)]] %||% "abs_linear"

  res <- tryCatch(
    fw_run_composite_method(
      dat_daily     = d,
      sub_method    = sub,
      reg_model     = input[[paste0("comp_reg_model_", key)]]     %||% "lm_season",
      interp_method = input[[paste0("comp_interp_method_", key)]] %||% "linearInterpolation",
      conv_factor   = conv_factor,
      param1        = input[[paste0("param1_", key)]],
      param2        = input[[paste0("param2_", key)]],
      wrtds_windowY = input[[paste0("wrtds_windowY_", key)]] %||% 7,
      wrtds_windowQ = input[[paste0("wrtds_windowQ_", key)]] %||% 2,
      wrtds_windowS = input[[paste0("wrtds_windowS_", key)]] %||% 0.5,
      kalman_rho    = input[[paste0("kalman_rho_", key)]]    %||% 0.90,
      kalman_niter  = input[[paste0("kalman_niter_", key)]]  %||% 200,
      date_range    = input[[paste0("daterange_", key)]]
    ),
    error = function(e) {
      shiny::showNotification(e$message, type = "error")
      NULL
    }
  )

  res
}

# ---------- Server：复合法结算文本额外信息 ----------

#' 复合法结算文本中的额外行
#' @param res  计算结果 list
#' @return character
fw_composite_settle_extra <- function(res) {
  if (is.null(res)) return("")
  p <- res$params
  sub <- p$sub_method %||% "unknown"
  sub_lbl <- fw_composite_sub_label(sub)

  if (identical(sub, "wrtds_kalman")) {
    paste0(
      "\n\u5b50\u65b9\u6cd5: ", sub_lbl,
      "\nBackend: EGRET + EGRETci",
      "\nwindowY=", p$wrtds_windowY,
      "  windowQ=", p$wrtds_windowQ,
      "  windowS=", p$wrtds_windowS,
      "\nrho=", p$kalman_rho, "  niter=", p$kalman_niter)
  } else {
    paste0(
      "\n\u5b50\u65b9\u6cd5: ", sub_lbl,
      "\nBackend: loadflex (loadLm + loadComp/loadInterp)",
      "\n\u56de\u5f52\u6a21\u578b: ", p$reg_model,
      "\n\u63d2\u503c\u65b9\u6cd5: ", p$interp_method)
  }
}

# ---------- Server：复合法诊断图覆盖 ----------

#' 复合法的诊断图渲染（覆盖通用 fw_plot_flux_diag）
#' 在 mod_flux.R 中当 key == "composite" 时调用此函数替代默认诊断图
fw_composite_render_diag <- function(res) {
  if (is.null(res)) {
    graphics::plot.new()
    graphics::text(0.5, 0.5, "No diagnostic result")
  } else {
    fw_plot_composite_diag(res)
  }
}
