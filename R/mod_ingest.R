# =========================================================
# mod_ingest.R
# Shiny ingest module (fixed-range workbook layout)
# 示例数据常驻：未上传时自动加载 csv/ 下的示例文件
# =========================================================

mod_ingest_ui <- function(id) {
  ns <- shiny::NS(id)

  shinydashboard::tabItem(
    tabName = "ingest",

    shiny::fluidRow(

      shinydashboard::box(
        width = 4,
        status = "primary",
        solidHeader = TRUE,
        title = shiny::tagList(
          shiny::icon("upload"),
          " 上传文件与校验 Upload Workbook & Validate"
        ),

        shiny::fileInput(
          ns("file"),
          "选择模板文件 / Select workbook",
          accept = c(".xlsx", ".xls"),
          buttonLabel = "浏览 Browse",
          placeholder = "未选择文件 No file selected",
          width = "100%"
        ),

        shiny::checkboxInput(
          ns("auto_impute_flow"),
          "流量缺失自动插补 Auto-impute flow gaps",
          value = TRUE
        ),

        shiny::tags$small(
          style = "color:#666;"
        ),

        shiny::tags$hr(),

        shiny::fluidRow(
          shiny::column(
            4,
            shiny::actionButton(
              ns("load"),
              "加载校验",
              icon = shiny::icon("check"),
              class = "btn-primary btn-block"
            )
          ),
          shiny::column(
            4,
            shiny::actionButton(
              ns("load_example"),
              "示例数据",
              icon = shiny::icon("flask"),
              class = "btn-info btn-block"
            )
          ),
          shiny::column(
            4,
            shiny::actionButton(
              ns("reset"),
              "清空状态",
              icon = shiny::icon("undo"),
              class = "btn-default btn-block"
            )
          )
        ),

        shiny::tags$hr(),

        # 示例数据下载
        shiny::h5(
          shiny::icon("download"),
          " 下载示例模板 Download Example"
        ),
        shiny::uiOutput(ns("example_data_ui")),

        shiny::tags$hr(),

        # 当前数据来源提示
        shiny::uiOutput(ns("data_source_badge")),

        shiny::h5(
          shiny::icon("clipboard-check"),
          " 校验日志 Validation Log"
        ),
        shiny::verbatimTextOutput(ns("log"))
      ),

      shinydashboard::box(
        width = 8,
        status = "info",
        solidHeader = TRUE,
        title = shiny::tagList(
          shiny::icon("table"),
          " 工作表预览 Preview"
        ),

        shiny::selectInput(
          ns("sheet"),
          "选择工作表 / Select sheet",
          choices = NULL,
          width = "100%"
        ),

        DT::dataTableOutput(ns("table"))
      )
    )
  )
}


# =========================================================
# helper: 数值统一保留三位小数
# =========================================================
fw_round_numeric_df <- function(df, digits = 3) {
  if (is.null(df) || !is.data.frame(df)) return(df)
  num_cols <- vapply(df, is.numeric, logical(1))
  if (any(num_cols)) {
    df[num_cols] <- lapply(df[num_cols], function(x) round(x, digits))
  }
  df
}

fw_round_numeric_object <- function(x, digits = 3) {
  if (is.null(x)) return(NULL)
  if (is.data.frame(x)) return(fw_round_numeric_df(x, digits = digits))
  if (is.matrix(x) && is.numeric(x)) return(round(x, digits))
  if (is.list(x)) return(lapply(x, fw_round_numeric_object, digits = digits))
  if (is.numeric(x)) return(round(x, digits))
  x
}


# =========================================================
# helper: 查找示例文件
# =========================================================
fw_find_example_files <- function() {
  # 1) 优先：工作目录下的 csv/
  data_dir <- file.path(getwd(), "csv")

  # 2) 回退：尝试从包的 inst/app/csv 查找（打包后才生效）
  if (!dir.exists(data_dir)) {
    pkg <- tryCatch(utils::packageName(), error = function(e) NULL)
    if (!is.null(pkg) && nzchar(pkg)) {
      app_dir <- system.file("app", package = pkg)
      if (nzchar(app_dir)) data_dir <- file.path(app_dir, "csv")
    }
  }

  # 3) 回退：开发阶段 inst/app/csv
  if (!dir.exists(data_dir)) {
    candidate <- file.path("inst", "app", "csv")
    if (dir.exists(candidate)) data_dir <- candidate
  }

  if (!dir.exists(data_dir)) return(character(0))

  list.files(data_dir, pattern = "\\.(xlsx|xls|csv)$",
             full.names = TRUE, ignore.case = TRUE)
}



mod_ingest_server <- function(id, rv) {
  shiny::moduleServer(id, function(input, output, session) {

    keep_digits <- 3L
    ns <- session$ns

    # 内部标记：当前数据来源
    data_source <- shiny::reactiveVal("none")  # "none" / "example" / "user"

    # =========================================================
    # 通用加载函数（示例 / 用户上传 共用）
    # =========================================================
    do_load <- function(filepath, source_label = "user") {
      tryCatch({
        res <- fw_read_validate_workbook(
          path = filepath,
          auto_impute_flow = isTRUE(input$auto_impute_flow)
        )

        res$clean_list <- fw_round_numeric_object(res$clean_list, digits = keep_digits)
        res$raw_list   <- fw_round_numeric_object(res$raw_list,   digits = keep_digits)
        res$flow_info  <- fw_round_numeric_object(res$flow_info,  digits = keep_digits)

        src_prefix <- if (identical(source_label, "example")) {
          paste0("\u2705 \u5df2\u52a0\u8f7d\u793a\u4f8b\u6570\u636e: ", basename(filepath), "\n")
        } else {
          paste0("\u2705 \u5df2\u52a0\u8f7d\u7528\u6237\u6570\u636e: ", basename(filepath), "\n")
        }

        rv$ingest_log     <- paste0(src_prefix, res$log)
        rv$clean_list     <- res$clean_list
        rv$raw_list       <- res$raw_list
        rv$ingest_issues  <- res$issues
        rv$flow_info      <- res$flow_info
        rv$info_meta      <- res$info_meta
        rv$flux_input     <- NULL
        rv$topology       <- NULL

        data_source(source_label)

        sheet_choices <- names(res$clean_list)
        shiny::updateSelectInput(
          session, "sheet",
          choices = sheet_choices,
          selected = if (length(sheet_choices) > 0) sheet_choices[[1]] else NULL
        )

        if (identical(source_label, "example")) {
          shiny::showNotification(
            paste0("\u793a\u4f8b\u6570\u636e\u5df2\u52a0\u8f7d: ", basename(filepath)),
            type = "message", duration = 4)
        } else {
          shiny::showNotification(
            "\u6570\u636e\u52a0\u8f7d\u6210\u529f\u3002", type = "message", duration = 3)
        }

        TRUE
      }, error = function(e) {
        rv$ingest_log     <- paste("\u9519\u8bef ERROR:", e$message)
        rv$clean_list     <- NULL
        rv$raw_list       <- NULL
        rv$ingest_issues  <- NULL
        rv$flow_info      <- NULL
        rv$info_meta      <- NULL
        rv$flux_input     <- NULL
        rv$topology       <- NULL
        data_source("none")

        shiny::updateSelectInput(session, "sheet", choices = character(0), selected = NULL)
        FALSE
      })
    }

    # =========================================================
    # 示例文件列表 reactive
    # =========================================================
    example_files <- shiny::reactive({
      fw_find_example_files()
    })

    # =========================================================
    # ★ App 启动时自动加载示例数据（仅首次、且用户未上传时）
    # =========================================================
    observeEvent(TRUE, {
      files <- fw_find_example_files()
      if (length(files) > 0 && is.null(rv$clean_list)) {
        # 优先选 xlsx 文件
        xlsx_files <- files[grepl("\\.xlsx$", files, ignore.case = TRUE)]
        target <- if (length(xlsx_files) > 0) xlsx_files[1] else files[1]
        do_load(target, source_label = "example")
      }
    }, once = TRUE)

    # =========================================================
    # 按钮：加载用户文件
    # =========================================================
    observeEvent(input$load, {
      shiny::req(input$file)
      do_load(input$file$datapath, source_label = "user")
    })

    # =========================================================
    # 按钮：加载示例数据
    # =========================================================
    observeEvent(input$load_example, {
      files <- example_files()
      if (length(files) == 0) {
        shiny::showNotification(
          "csv/ \u76ee\u5f55\u4e0b\u672a\u627e\u5230\u793a\u4f8b\u6587\u4ef6\u3002",
          type = "warning")
        return()
      }

      # 如果有多个文件且用户已选择，用选中的；否则用第一个 xlsx
      sel <- input$example_file_select
      if (!is.null(sel) && file.exists(sel)) {
        do_load(sel, source_label = "example")
      } else {
        xlsx_files <- files[grepl("\\.xlsx$", files, ignore.case = TRUE)]
        target <- if (length(xlsx_files) > 0) xlsx_files[1] else files[1]
        do_load(target, source_label = "example")
      }
    })

    # =========================================================
    # 按钮：清空状态
    # =========================================================
    observeEvent(input$reset, {
      rv$ingest_log     <- "\u5df2\u6e05\u7a7a\u72b6\u6001 State cleared."
      rv$clean_list     <- NULL
      rv$raw_list       <- NULL
      rv$ingest_issues  <- NULL
      rv$flow_info      <- NULL
      rv$info_meta      <- NULL
      rv$flux_input     <- NULL
      rv$topology       <- NULL
      data_source("none")

      shiny::updateSelectInput(session, "sheet", choices = character(0), selected = NULL)
    })

    # =========================================================
    # 当前数据来源标记
    # =========================================================
    output$data_source_badge <- shiny::renderUI({
      src <- data_source()
      if (identical(src, "example")) {
        shiny::tags$div(
          style = "margin-bottom:8px;",
          shiny::tags$span(
            class = "label label-info",
            style = "font-size:13px; padding:4px 10px;",
            shiny::icon("flask"),
            " \u5f53\u524d: \u793a\u4f8b\u6570\u636e (Example)"
          ),
          shiny::tags$br(),
          shiny::tags$small(
            style = "color:#888;",
            "\u4e0a\u4f20\u81ea\u5df1\u7684\u6587\u4ef6\u53ef\u8986\u76d6\u793a\u4f8b\u6570\u636e\u3002"
          )
        )
      } else if (identical(src, "user")) {
        shiny::tags$div(
          style = "margin-bottom:8px;",
          shiny::tags$span(
            class = "label label-success",
            style = "font-size:13px; padding:4px 10px;",
            shiny::icon("user"),
            " \u5f53\u524d: \u7528\u6237\u4e0a\u4f20\u6570\u636e"
          )
        )
      } else {
        shiny::tags$div(
          style = "margin-bottom:8px;",
          shiny::tags$span(
            class = "label label-default",
            style = "font-size:13px; padding:4px 10px;",
            "\u672a\u52a0\u8f7d\u6570\u636e"
          )
        )
      }
    })

    # =========================================================
    # 示例文件下载 UI + Handler
    # =========================================================
    output$example_data_ui <- shiny::renderUI({
      files <- example_files()
      if (length(files) == 0) {
        return(shiny::tags$p(
          style = "color:#999; font-size:12px;",
          "csv/ \u76ee\u5f55\u4e0b\u672a\u627e\u5230\u793a\u4f8b\u6587\u4ef6\u3002"
        ))
      }

      if (length(files) == 1) {
        shiny::tagList(
          shiny::downloadButton(
            ns("download_example"),
            paste0("\u4e0b\u8f7d: ", basename(files[1])),
            class = "btn-default btn-sm",
            icon = shiny::icon("file-download")
          )
        )
      } else {
        file_choices <- stats::setNames(files, basename(files))
        shiny::tagList(
          shiny::selectInput(
            ns("example_file_select"),
            "\u9009\u62e9\u793a\u4f8b\u6587\u4ef6",
            choices = file_choices,
            width = "100%"
          ),
          shiny::downloadButton(
            ns("download_example"),
            "\u4e0b\u8f7d\u793a\u4f8b\u6570\u636e",
            class = "btn-default btn-sm",
            icon = shiny::icon("file-download")
          )
        )
      }
    })

    output$download_example <- shiny::downloadHandler(
      filename = function() {
        files <- example_files()
        if (length(files) == 1) return(basename(files[1]))
        sel <- input$example_file_select
        if (!is.null(sel) && file.exists(sel)) return(basename(sel))
        "example_data.xlsx"
      },
      content = function(file) {
        files <- example_files()
        src <- NULL
        if (length(files) == 1) {
          src <- files[1]
        } else {
          sel <- input$example_file_select
          if (!is.null(sel) && file.exists(sel)) src <- sel
        }
        if (!is.null(src) && file.exists(src)) {
          file.copy(src, file)
        } else {
          writeLines("No example data found.", file)
        }
      }
    )

    # =========================================================
    # 校验日志
    # =========================================================
    output$log <- shiny::renderText({
      rv$ingest_log %||% "\u5c1a\u672a\u52a0\u8f7d\u6587\u4ef6 No file loaded."
    })

    # =========================================================
    # 工作表预览
    # =========================================================
    output$table <- DT::renderDataTable({
      shiny::req(input$sheet, rv$clean_list)
      dat <- rv$clean_list[[input$sheet]]
      shiny::req(dat)

      dt <- DT::datatable(
        dat,
        rownames = FALSE,
        filter = "top",
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          language = list(
            search = "\u641c\u7d22 Search:",
            lengthMenu = "\u6bcf\u9875\u663e\u793a _MENU_ \u6761 Show _MENU_ rows per page",
            info = "\u663e\u793a\u7b2c _START_ \u81f3 _END_ \u6761\uff0c\u5171 _TOTAL_ \u6761 Showing _START_ to _END_ of _TOTAL_ rows",
            infoEmpty = "\u65e0\u6570\u636e No data available",
            zeroRecords = "\u672a\u627e\u5230\u5339\u914d\u8bb0\u5f55 No matching records found",
            paginate = list(
              previous = "\u4e0a\u4e00\u9875 Previous",
              `next` = "\u4e0b\u4e00\u9875 Next"
            )
          )
        )
      )

      numeric_cols <- names(dat)[vapply(dat, is.numeric, logical(1))]
      if (length(numeric_cols) > 0) {
        dt <- DT::formatRound(dt, columns = numeric_cols, digits = keep_digits)
      }

      if (identical(input$sheet, "Flow") && "is_imputed" %in% names(dat)) {
        dt <- DT::formatStyle(
          dt,
          columns = names(dat),
          target = "row",
          backgroundColor = DT::styleEqual(
            c(TRUE, FALSE),
            c("#fff3cd", NA)
          ),
          valueColumns = "is_imputed"
        )
      }

      dt
    })
  })
}
