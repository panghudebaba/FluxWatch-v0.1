# =====================================================================
# mod_flux_regression.R
# 回归法 —— 专属 UI 参数 + Server 初始化 + 数据获取 + 计算调度
# =====================================================================

if (!exists("%||%", mode = "function")) {
  `%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x
}

# ---------- UI 参数块（嵌入通用 mod_flux_method_page_ui 的 left_extra）----------

#' 回归法专属的左侧参数控件
#' @param ns  namespace 函数
#' @param key 方法键名（固定为 "regression"）
#' @return tagList
fw_regression_left_extra_ui <- function(ns, key = "regression") {
  shiny::tagList(
    shiny::helpText("回归方法固定调用第一步处理后的 QF/WQ 数据"),
    shiny::selectInput(ns(paste0("qf_sheet_", key)), "QF数据表", choices = NULL),
    shiny::selectInput(ns(paste0("wq_sheet_", key)), "WQ数据表", choices = NULL),
    shiny::selectInput(ns(paste0("constituent_", key)), "水质指标(j)", choices = NULL),
    shiny::selectInput(
      ns(paste0("reg_model_", key)),
      "回归模型",
      choices = c("季节回归" = "loadLm_season", "线性回归" = "loadLm_simple"),
      selected = "loadLm_season"
    )
  )
}

# ---------- Server：QF/WQ 数据源 reactive ----------

#' 创建 step1_qf_wq reactive（从 rv 中提取第一步 QF/WQ 数据）
#' @param rv reactiveValues
#' @return reactive 返回 list(QF, WQ, source) 或 NULL
fw_regression_step1_reactive <- function(rv) {
  shiny::reactive({
    fw_get_step1_qf_wq(rv)
  })
}

# ---------- Server：QF/WQ 表名与指标联动 observers ----------

#' 注册回归法的 QF 表、WQ 表、水质指标联动 observers
#' @param input, session  Shiny 标准
#' @param step1_qf_wq  reactive（fw_regression_step1_reactive 返回值）
fw_regression_init_observers <- function(input, session, step1_qf_wq) {

  # ---- QF / WQ 表名同步 ----
  shiny::observe({
    s1 <- step1_qf_wq()
    if (is.null(s1)) {
      shiny::updateSelectInput(session, "qf_sheet_regression", choices = character(0))
      shiny::updateSelectInput(session, "wq_sheet_regression", choices = character(0))
      return()
    }

    qf_list <- fw_as_named_table_list(s1$QF, "QF")
    wq_list <- fw_as_named_table_list(s1$WQ, "WQ")
    if (length(qf_list) == 0 || length(wq_list) == 0) {
      shiny::updateSelectInput(session, "qf_sheet_regression", choices = character(0))
      shiny::updateSelectInput(session, "wq_sheet_regression", choices = character(0))
      return()
    }

    qf_nm <- names(qf_list)
    wq_nm <- names(wq_list)

    cur_qf <- shiny::isolate(input$qf_sheet_regression)
    cur_wq <- shiny::isolate(input$wq_sheet_regression)
    if (is.null(cur_qf) || !(cur_qf %in% qf_nm)) cur_qf <- qf_nm[1]
    if (is.null(cur_wq) || !(cur_wq %in% wq_nm)) cur_wq <- wq_nm[1]

    shiny::updateSelectInput(session, "qf_sheet_regression", choices = qf_nm, selected = cur_qf)
    shiny::updateSelectInput(session, "wq_sheet_regression", choices = wq_nm, selected = cur_wq)
  })

  # ---- 水质指标联动 ----
  shiny::observe({
    s1 <- step1_qf_wq()
    if (is.null(s1)) {
      shiny::updateSelectInput(session, "constituent_regression", choices = character(0))
      return()
    }

    wq_list <- fw_as_named_table_list(s1$WQ, "WQ")
    if (length(wq_list) == 0) {
      shiny::updateSelectInput(session, "constituent_regression", choices = character(0))
      return()
    }

    ws <- input$wq_sheet_regression
    if (is.null(ws) || !(ws %in% names(wq_list))) ws <- names(wq_list)[1]

    cands <- fw_get_wq_constituents(wq_list[[ws]])
    if (length(cands) == 0) cands <- names(wq_list[[ws]])

    cur <- shiny::isolate(input$constituent_regression)
    if (is.null(cur) || !(cur %in% cands)) cur <- cands[1]

    shiny::updateSelectInput(session, "constituent_regression", choices = cands, selected = cur)
  })
}

# ---------- Server：回归法数据获取 ----------

#' 回归法的 daily_all 数据获取
#' @param input       Shiny input
#' @param step1_qf_wq reactive（同上）
#' @return data.frame(station, WYBM, date, Q, C_obs) 或 NULL
fw_regression_get_daily_all <- function(input, step1_qf_wq) {
  s1 <- step1_qf_wq()
  if (is.null(s1)) return(NULL)

  prep <- tryCatch(
    fw_prepare_regression_input(
      step1_data  = s1,
      qf_sheet    = input$qf_sheet_regression,
      wq_sheet    = input$wq_sheet_regression,
      constituent = input$constituent_regression,
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

# ---------- Server：回归法计算调度 ----------

#' 回归法的"开始计算"逻辑
#' @param input       Shiny input
#' @param step1_qf_wq reactive
#' @param key         方法键名 "regression"
#' @return res list（与 fw_run_flux_with_config 返回结构一致）或 NULL
fw_regression_run_calc <- function(input, step1_qf_wq, key = "regression") {
  s1 <- step1_qf_wq()
  if (is.null(s1)) {
    shiny::showNotification("未找到第一步 QF/WQ 数据。", type = "error")
    return(NULL)
  }

  res <- tryCatch(
    fw_run_flux_with_config(
      method     = "regression",
      date_range = input[[paste0("daterange_", key)]],
      step1_data = s1,
      regression_cfg = list(
        qf_sheet     = input[[paste0("qf_sheet_", key)]],
        wq_sheet     = input[[paste0("wq_sheet_", key)]],
        constituent  = input[[paste0("constituent_", key)]],
        model_choice = input[[paste0("reg_model_", key)]]
      )
    ),
    error = function(e) {
      shiny::showNotification(e$message, type = "error")
      NULL
    }
  )

  res
}

# ---------- Server：回归法结算文本额外信息 ----------

#' 回归法结算文本中的额外行
#' @param res  计算结果 list
#' @return character
fw_regression_settle_extra <- function(res) {
  if (is.null(res)) return("")
  paste0(
    "\nQF表: ", res$params$qf_sheet %||% "",
    "\nWQ表: ", res$params$wq_sheet %||% "",
    "\n指标: ", res$params$constituent %||% "",
    "\n模型: ", res$params$model_choice %||% ""
  )
}
