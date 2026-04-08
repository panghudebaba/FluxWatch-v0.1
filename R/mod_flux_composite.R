# =====================================================================
# mod_flux_composite.R
# 复合法 —— 专属 UI 参数 + Server 初始化 + 数据获取 + 计算调度
# 数据获取结构与 mod_flux_regression.R 一致（QF/WQ 表名 + 水质指标联动）
# =====================================================================

if (!exists("%||%", mode = "function")) {

  `%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x
}

# =====================================================================
# 1. UI 参数块（嵌入通用 mod_flux_method_page_ui 的 left_extra）
# =====================================================================

#' 复合法专属的左侧参数控件
#' @param ns  namespace 函数
#' @param key 方法键名（固定为 "composite"）
#' @return tagList
fw_composite_left_extra_ui <- function(ns, key = "composite") {
  shiny::tagList(

    # --- 数据选择 ---
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

    # ★ 新增：站点选择（多选）
    shiny::selectizeInput(
      ns(paste0("stations_", key)),
      "\u7ad9\u70b9\u9009\u62e9\uff08\u53ef\u591a\u9009\uff09",
      choices  = character(0),
      multiple = TRUE,
      options  = list(
        placeholder = "\u52a0\u8f7d\u6570\u636e\u540e\u81ea\u52a8\u586b\u5145...",
        plugins     = list("remove_button")
      )
    ),
    # 全选按钮
    shiny::actionLink(
      ns(paste0("select_all_stations_", key)),
      "\u5168\u9009\u6240\u6709\u7ad9\u70b9",
      icon = shiny::icon("check-double")
    ),

    shiny::tags$hr(),

    # --- 复合子方法（原有不变）---
    shiny::selectInput(
      ns(paste0("comp_sub_method_", key)),
      "\u590d\u5408\u5b50\u65b9\u6cd5",
      choices  = fw_composite_sub_choices(),
      selected = "abs_linear"
    ),

    # --- loadComp 参数（原有不变）---
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

    # --- WRTDS-Kalman 参数（原有不变）---
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


# =====================================================================
# 2. Server：QF/WQ 表名与指标联动 observers
# =====================================================================

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


# =====================================================================
# 3. 数据合并工具：fw_composite_merge_qf_wq
# =====================================================================

#' 从 QF（流量）和 WQ（水质）表合并为 data.frame(date, Q, C_obs, station, WYBM)
#' @param qf_df       流量表（需含日期列 + Q 列）
#' @param wq_df       水质表（需含日期列 + 指标列）
#' @param constituent 水质指标列名
#' @return data.frame 或 NULL
fw_composite_merge_qf_wq <- function(qf_df, wq_df, constituent) {

  if (is.null(qf_df) || is.null(wq_df)) return(NULL)
  qf_df <- as.data.frame(qf_df, stringsAsFactors = FALSE)
  wq_df <- as.data.frame(wq_df, stringsAsFactors = FALSE)

  # ── 识别日期列 ──
  find_date_col <- function(df) {
    cands <- c("date", "Date", "DATE", "TM", "tm", "datetime", "Datetime",
               "sample_date", "SampleDate", "\u65e5\u671f")
    hit <- intersect(cands, names(df))
    if (length(hit) > 0) return(hit[1])
    for (nm in names(df)) {
      if (inherits(df[[nm]], "Date") || inherits(df[[nm]], "POSIXct")) return(nm)
    }
    NULL
  }

  # ── 识别流量列 ──
  find_q_col <- function(df) {
    cands <- c("Q", "q", "flow", "Flow", "FLOW", "discharge", "Discharge",
               "\u6d41\u91cf", "mean_flow", "daily_flow")
    hit <- intersect(cands, names(df))
    if (length(hit) > 0) return(hit[1])
    NULL
  }

  # ── 识别站点列 ──
  find_station_col <- function(df) {
    cands <- c("station", "Station", "STATION", "site", "Site",
               "\u7ad9\u70b9", "\u7ad9\u540d", "site_no")
    hit <- intersect(cands, names(df))
    if (length(hit) > 0) return(hit[1])
    NULL
  }

  # ── QF 表解析 ──
  qf_date_col <- find_date_col(qf_df)
  qf_q_col    <- find_q_col(qf_df)
  if (is.null(qf_date_col)) stop("\u6d41\u91cf\u8868\u4e2d\u672a\u627e\u5230\u65e5\u671f\u5217\u3002")
  if (is.null(qf_q_col))    stop("\u6d41\u91cf\u8868\u4e2d\u672a\u627e\u5230\u6d41\u91cf\u5217 (Q)\u3002")

  qf <- data.frame(
    date = fw_as_date(qf_df[[qf_date_col]]),
    Q    = fw_as_num(qf_df[[qf_q_col]]),
    stringsAsFactors = FALSE
  )

  # 站点
  qf_st_col <- find_station_col(qf_df)
  if (!is.null(qf_st_col)) {
    qf$station <- as.character(qf_df[[qf_st_col]])
  } else {
    qf$station <- "ALL"
  }

  # WYBM
  wybm_cands <- c("WYBM", "wybm", "\u6c34\u6e90\u6807\u7801")
  wybm_hit   <- intersect(wybm_cands, names(qf_df))
  if (length(wybm_hit) > 0) {
    qf$WYBM <- as.character(qf_df[[wybm_hit[1]]])
  } else {
    qf$WYBM <- NA_character_
  }

  qf <- qf[!is.na(qf$date), , drop = FALSE]

  # ── WQ 表解析 ──
  wq_date_col <- find_date_col(wq_df)
  if (is.null(wq_date_col)) stop("\u6c34\u8d28\u8868\u4e2d\u672a\u627e\u5230\u65e5\u671f\u5217\u3002")
  if (!(constituent %in% names(wq_df)))
    stop(paste0("\u6c34\u8d28\u8868\u4e2d\u672a\u627e\u5230\u6307\u6807\u5217: ", constituent))

  wq <- data.frame(
    date  = fw_as_date(wq_df[[wq_date_col]]),
    C_obs = fw_as_num(wq_df[[constituent]]),
    stringsAsFactors = FALSE
  )
  wq <- wq[!is.na(wq$date), , drop = FALSE]

  # ── 左连接：QF left join WQ ──
  merged <- merge(qf, wq, by = "date", all.x = TRUE)
  merged <- merged[order(merged$date), , drop = FALSE]
  rownames(merged) <- NULL

  if (nrow(merged) == 0) stop("\u5408\u5e76\u540e\u65e0\u53ef\u7528\u6570\u636e\uff0c\u8bf7\u68c0\u67e5\u65e5\u671f\u662f\u5426\u5339\u914d\u3002")

  n_obs <- sum(is.finite(merged$C_obs) & is.finite(merged$Q) & merged$Q > 0)
  if (n_obs < 3)
    warning(paste0("\u6709\u6548\u89c2\u6d4b\u4ec5 ", n_obs, " \u6761\uff0c\u53ef\u80fd\u4e0d\u8db3\u4ee5\u652f\u6491\u6a21\u578b\u62df\u5408\u3002"))

  merged
}


# =====================================================================
# 4. Server：复合法数据获取（reactive 版，供 daily_all 使用）
# =====================================================================

#' 复合法的 daily_all 数据获取（直接合并 QF/WQ）
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

  tryCatch(
    fw_composite_merge_qf_wq(
      qf_df       = qf_list[[qf_sel]],
      wq_df       = wq_list[[wq_sel]],
      constituent = con
    ),
    error = function(e) NULL
  )
}


# =====================================================================
# 5. Server：复合法计算调度（★ 关键函数）
# =====================================================================

#' 复合法的"开始计算"逻辑
#' 与 weighted / ratio 等方法一致：直接从 step1_qf_wq 获取数据
#' @param input          Shiny input
#' @param step1_qf_wq   reactive（返回 list(QF=..., WQ=...)）
#' @param key            方法键名 "composite"
#' @return res list 或 NULL
fw_composite_run_calc <- function(input, step1_qf_wq, key = "composite") {

  conv_factor <- 86.4

  # ── 1) 从 step1_qf_wq 获取原始表 ──
  s1 <- step1_qf_wq()
  if (is.null(s1)) {
    shiny::showNotification(
      "\u6570\u636e\u672a\u5c31\u7eea\uff0c\u8bf7\u5148\u4e0a\u4f20\u5e76\u52a0\u8f7d\u6570\u636e\u3002",
      type = "error")
    return(NULL)
  }

  qf_list <- fw_as_named_table_list(s1$QF, "QF")
  wq_list <- fw_as_named_table_list(s1$WQ, "WQ")
  if (length(qf_list) == 0 || length(wq_list) == 0) {
    shiny::showNotification(
      "\u672a\u627e\u5230\u53ef\u7528\u7684\u6d41\u91cf/\u6c34\u8d28\u6570\u636e\u8868\u3002",
      type = "error")
    return(NULL)
  }

  # ── 2) 读取用户选择 ──
  qf_sel <- input[[paste0("qf_sheet_", key)]]
  wq_sel <- input[[paste0("wq_sheet_", key)]]
  con    <- input[[paste0("constituent_", key)]]

  if (is.null(qf_sel) || is.null(wq_sel) || is.null(con)) {
    shiny::showNotification(
      "\u8bf7\u9009\u62e9\u6d41\u91cf\u8868\u3001\u6c34\u8d28\u8868\u548c\u6c34\u8d28\u6307\u6807\u3002",
      type = "error")
    return(NULL)
  }
  if (!(qf_sel %in% names(qf_list)) || !(wq_sel %in% names(wq_list))) {
    shiny::showNotification(
      "\u9009\u62e9\u7684\u5de5\u4f5c\u8868\u4e0d\u5b58\u5728\u3002",
      type = "error")
    return(NULL)
  }

  qf_df <- qf_list[[qf_sel]]
  wq_df <- wq_list[[wq_sel]]

  # ── 3) 合并 QF + WQ（★ 使用 fw_composite_merge_qf_wq）──
  d <- tryCatch(
    fw_composite_merge_qf_wq(
      qf_df       = qf_df,
      wq_df       = wq_df,
      constituent = con
    ),
    error = function(e) {
      shiny::showNotification(
        paste0("\u6570\u636e\u51c6\u5907\u5931\u8d25: ", e$message),
        type = "error")
      NULL
    }
  )
  if (is.null(d) || nrow(d) == 0) {
    shiny::showNotification(
      "\u5f53\u524d\u6570\u636e\u9009\u62e9\u4e0d\u53ef\u7528\u4e8e\u901a\u91cf\u8ba1\u7b97\uff0c\u8bf7\u66f4\u6362\u6570\u636e\u6e90\u3002",
      type = "error")
    return(NULL)
  }

  # ── 4) 单站筛选 ──
  if ("station" %in% names(d)) {
    st <- unique(stats::na.omit(as.character(d$station)))
    if (length(st) > 0) d <- d[d$station == st[1], , drop = FALSE]
  }

  if (nrow(d) == 0) {
    shiny::showNotification(
      "\u7b5b\u9009\u540e\u65e0\u53ef\u7528\u6570\u636e\u3002", type = "error")
    return(NULL)
  }

  # ── 5) 调用核心计算 ──
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
      shiny::showNotification(
        paste0("\u8ba1\u7b97\u5931\u8d25: ", e$message),
        type = "error")
      NULL
    }
  )

  res
}


# =====================================================================
# 6. Server：方法原理文本（完整版，全部子方法 + WRTDS-Kalman）
# =====================================================================

#' 复合法原理完整文本
#' 当前选中的子方法用高亮背景标记
#' @param active_sub  当前选中的子方法键名
#' @return character (HTML + MathJax)
fw_composite_principle_text <- function(active_sub = "abs_linear") {

  hl_start <- function(sub) {
    if (identical(active_sub, sub))
      '<div style="background:#fffbe6; border-left:4px solid #f5a623; padding:8px 12px; margin:6px 0; border-radius:4px;">'
    else
      '<div style="padding:4px 0;">'
  }
  hl_end <- "</div>"

  paste0(
    "<h4>\u590d\u5408\u65b9\u6cd5\uff08Composite Method\uff09</h4>",

    "<h4>1. loadflex::loadComp &mdash; \u56de\u5f52&ldquo;\u7ed3\u6784\u9879&rdquo; + \u63d2\u503c&ldquo;\u6821\u6b63\u9879&rdquo;</h4>",
    "<p>\u5148\u7528\u56de\u5f52\u6a21\u578b\u5f97\u5230\u5e73\u6ed1\u7ed3\u6784\u9884\u6d4b\uff0c\u518d\u628a&ldquo;\u6b8b\u5dee&rdquo;\u4f5c\u4e3a\u6821\u6b63\u9879\u63d2\u503c\u5230\u8fde\u7eed\u65f6\u95f4\u6b65\uff0c\u6700\u540e\u5408\u6210\u3002",
    "\u56de\u5f52\u9884\u6d4b\uff08\u7ed3\u6784\u9879\uff09\u4e3a \\(\\widehat{L}_{\\text{reg}}(t)\\)\uff08\u6216 \\(\\widehat{C}_{\\text{reg}}(t)\\)\uff09\uff0c",
    "\u6b8b\u5dee\u63d2\u503c\u5f97\u5230\u7684\u6821\u6b63\u9879\u4e3a \\(\\widehat{R}(t)\\)\u3002",
    "loadComp \u5728\u5904\u7406\u6b8b\u5dee\u9879\u65f6\u4f1a\u4f7f\u7528\u63d2\u503c\u65b9\u6cd5\u3002</p>",

    # 1.1
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

    # 1.2
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

    # 1.3
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

    # 1.4
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

    "<hr>",

    # 2. WRTDS-Kalman
    hl_start("wrtds_kalman"),

    "<h4>2. EGRET \u4e2d WRTDS&ndash;Kalman\uff08WRTDS_K\uff09\u7684\u8d1f\u8377\u8ba1\u7b97</h4>",
    "<p>WRTDS&ndash;Kalman\uff08WRTDS_K\uff09\u662f\u5728 WRTDS \u57fa\u7840\u4e0a\u52a0\u5165\u65f6\u95f4\u76f8\u5173\u6b8b\u5dee\u4fee\u6b63\u7684\u65b9\u6cd5\u3002",
    "\u5176\u6838\u5fc3\u601d\u60f3\u4e0d\u662f\u76f4\u63a5\u5bf9\u6d53\u5ea6\u518d\u505a\u4e00\u6b21\u666e\u901a\u56de\u5f52\uff0c\u800c\u662f\u628a WRTDS \u5728\u5bf9\u6570\u7a7a\u95f4\u4e2d\u7684\u6807\u51c6\u5316\u6b8b\u5dee\u4f5c\u4e3a\u4e00\u4e2a\u968f\u65f6\u95f4\u6f14\u5316\u7684\u8fc7\u7a0b\uff1a",
    "\u5728\u6709\u89c2\u6d4b\u7684\u65e5\u671f\uff0c\u76f4\u63a5\u4f7f\u7528\u89c2\u6d4b\u4fe1\u606f\uff1b",
    "\u5728\u65e0\u89c2\u6d4b\u7684\u65e5\u671f\uff0c\u6839\u636e\u76f8\u90bb\u91c7\u6837\u65e5\u4e4b\u95f4\u6b8b\u5dee\u7684\u65f6\u95f4\u76f8\u5173\u6027\uff0c",
    "\u7528\u6761\u4ef6 AR(1) \u8fc7\u7a0b\u548c Monte Carlo \u65b9\u6cd5\u8865\u5168\u6b8b\u5dee\u5e8f\u5217\uff0c",
    "\u8fdb\u800c\u5f97\u5230\u9010\u65e5\u6d53\u5ea6\u548c\u9010\u65e5\u8d1f\u8377\u4f30\u8ba1\u3002</p>",

    "<h5>2.1 WRTDS \u7684\u5bf9\u6570\u7a7a\u95f4\u62df\u5408\u8868\u8fbe\u5f0f</h5>",
    "\\[\\ln(c_t)=\\beta_{0t}+\\beta_{1t}\\ln(Q_t)+\\beta_{2t}T_t",
    "+\\beta_{3t}\\sin(2\\pi T_t)+\\beta_{4t}\\cos(2\\pi T_t)+\\sigma_t z_t\\]",
    "<ul>",
    "<li>\\(\\ln(c_t)\\)\uff1a\u7b2c \\(t\\) \u65e5\u6d53\u5ea6\u7684\u81ea\u7136\u5bf9\u6570</li>",
    "<li>\\(c_t\\)\uff1a\u7b2c \\(t\\) \u65e5\u6d53\u5ea6</li>",
    "<li>\\(Q_t\\)\uff1a\u7b2c \\(t\\) \u65e5\u5e73\u5747\u6d41\u91cf</li>",
    "<li>\\(T_t\\)\uff1a\u7b2c \\(t\\) \u65e5\u7684\u5c0f\u6570\u5e74\u4efd\u65f6\u95f4</li>",
    "<li>\\(\\beta_{0t},\\dots,\\beta_{4t}\\)\uff1aWRTDS \u5c40\u90e8\u52a0\u6743\u56de\u5f52\u7cfb\u6570</li>",
    "<li>\\(\\sigma_t\\)\uff1a\u6761\u4ef6\u8bef\u5dee\u6807\u51c6\u5dee</li>",
    "<li>\\(z_t\\)\uff1a\u6807\u51c6\u5316\u6b8b\u5dee</li>",
    "</ul>",

    "<h5>2.2 \u89c2\u6d4b\u65e5\u6b8b\u5dee\u4e0e\u6807\u51c6\u5316\u6b8b\u5dee</h5>",
    "\\[\\hat{y}_t=\\beta_{0t}+\\beta_{1t}\\ln(Q_t)+\\beta_{2t}T_t",
    "+\\beta_{3t}\\sin(2\\pi T_t)+\\beta_{4t}\\cos(2\\pi T_t)\\]",
    "\\[r_t=\\ln\\!\\left(C_t^{\\text{obs}}\\right)-\\hat{y}_t\\]",
    "\\[z_t=\\frac{r_t}{\\sigma_t}\\]",

    "<h5>2.3 \u65e0\u89c2\u6d4b\u65e5\u7684\u6761\u4ef6 AR(1) \u6b8b\u5dee\u751f\u6210</h5>",
    "\\[z_{k+1}=\\rho\\, z_k+\\sqrt{1-\\rho^2}\\,e_k,\\qquad e_k\\sim N(0,1),\\quad 1<k<n\\]",

    "<h5>2.4 \u7531\u6807\u51c6\u5316\u6b8b\u5dee\u751f\u6210\u9010\u65e5\u6d53\u5ea6</h5>",
    "\\[C_t^{(m)}=\\exp\\!\\left(\\hat{y}_t+\\sigma_t\\, z_t^{(m)}\\right)\\]",
    "\\[\\widehat{C}_t^{\\,wk}=\\frac{1}{M}\\sum_{m=1}^{M} C_t^{(m)}\\]",

    "<h5>2.5 \u7531\u6821\u6b63\u6d53\u5ea6\u751f\u6210\u9010\u65e5\u8d1f\u8377</h5>",
    "\\[F_t^{(m)} = 86.4\\,Q_t\\,C_t^{(m)}\\]",
    "\\[\\widehat{F}_t^{\\,wk}=\\frac{1}{M}\\sum_{m=1}^{M} F_t^{(m)}\\]",

    "<h5>2.6 \u5e74\u5c3a\u5ea6\u8d1f\u8377\u805a\u5408</h5>",
    "\\[L_{\\text{year}}=\\sum_{j=1}^{365}\\widehat{F}_j^{\\,wk}",
    "=\\sum_{j=1}^{365} 86.4\\,Q_j\\,\\widehat{C}_j^{\\,wk}\\]",

    "<h5>2.7 \u9002\u7528\u60c5\u51b5</h5>",
    "<p><b>\u9002\u5408\uff1a</b>\u89c2\u6d4b\u6b8b\u5dee\u5b58\u5728\u77ed\u65f6\u95f4\u5c3a\u5ea6\u6301\u7eed\u504f\u9ad8/\u504f\u4f4e\u65f6\uff0cWRTDS-K \u6bd4\u666e\u901a WRTDS \u66f4\u5408\u9002\u3002</p>",
    "<p><b>\u4e0d\u9002\u5408\uff1a</b>\u91c7\u6837\u6781\u5176\u7a00\u758f\u3001\u6b8b\u5dee\u77ed\u671f\u81ea\u76f8\u5173\u5f88\u5f31\u65f6\uff0c\u4e24\u8005\u5dee\u5f02\u4e0d\u5927\u3002</p>",

    hl_end
  )
}


# =====================================================================
# 7. Server：复合法原理渲染
# =====================================================================

fw_composite_render_principle <- function(input, key = "composite") {
  sub <- input[[paste0("comp_sub_method_", key)]] %||% "abs_linear"
  shiny::withMathJax(
    shiny::tags$div(
      style = "font-size:14px; line-height:1.8; max-height:600px; overflow-y:auto;",
      shiny::HTML(fw_composite_principle_text(sub))
    )
  )
}


# =====================================================================
# 8. Server：复合法结算文本额外信息
# =====================================================================

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


# =====================================================================
# 9. Server：复合法诊断图覆盖
# =====================================================================

#' 复合法的诊断图渲染
fw_composite_render_diag <- function(res) {
  if (is.null(res)) {
    graphics::plot.new()
    graphics::text(0.5, 0.5, "No diagnostic result")
  } else {
    fw_plot_composite_diag(res)
  }
}
