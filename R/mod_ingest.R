# =========================================================
# mod_ingest.R
# Shiny ingest module (fixed-range workbook layout)
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
            6,
            shiny::actionButton(
              ns("load"),
              "加载并校验 Load & Validate",
              icon = shiny::icon("check"),
              class = "btn-primary btn-block"
            )
          ),
          shiny::column(
            6,
            shiny::actionButton(
              ns("reset"),
              "清空状态 Reset",
              icon = shiny::icon("undo"),
              class = "btn-default btn-block"
            )
          )
        ),

        shiny::tags$hr(),

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



mod_ingest_server <- function(id, rv) {
  shiny::moduleServer(id, function(input, output, session) {

    observeEvent(input$load, {
      shiny::req(input$file)

      tryCatch({
        res <- fw_read_validate_workbook(
          path = input$file$datapath,
          auto_impute_flow = isTRUE(input$auto_impute_flow)
        )

        rv$ingest_log <- res$log
        rv$clean_list <- res$clean_list
        rv$raw_list <- res$raw_list
        rv$ingest_issues <- res$issues
        rv$flow_info <- res$flow_info
        rv$info_meta <- res$info_meta

        # 不再使用
        rv$flux_input <- NULL
        rv$topology <- NULL

        sheet_choices <- names(res$clean_list)

        shiny::updateSelectInput(
          session,
          "sheet",
          choices = sheet_choices,
          selected = if (length(sheet_choices) > 0) sheet_choices[[1]] else NULL
        )

      }, error = function(e) {
        rv$ingest_log <- paste("错误 ERROR:", e$message)
        rv$clean_list <- NULL
        rv$raw_list <- NULL
        rv$ingest_issues <- NULL
        rv$flow_info <- NULL
        rv$info_meta <- NULL
        rv$flux_input <- NULL
        rv$topology <- NULL

        shiny::updateSelectInput(
          session,
          "sheet",
          choices = character(0),
          selected = NULL
        )
      })
    })

    observeEvent(input$reset, {
      rv$ingest_log <- "已清空状态 State cleared."
      rv$clean_list <- NULL
      rv$raw_list <- NULL
      rv$ingest_issues <- NULL
      rv$flow_info <- NULL
      rv$info_meta <- NULL
      rv$flux_input <- NULL
      rv$topology <- NULL

      shiny::updateSelectInput(
        session,
        "sheet",
        choices = character(0),
        selected = NULL
      )
    })

    output$log <- shiny::renderText({
      rv$ingest_log %||% "尚未加载文件 No file loaded."
    })

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
            search = "搜索 Search:",
            lengthMenu = "每页显示 _MENU_ 条 Show _MENU_ rows per page",
            info = "显示第 _START_ 至 _END_ 条，共 _TOTAL_ 条 Showing _START_ to _END_ of _TOTAL_ rows",
            infoEmpty = "无数据 No data available",
            zeroRecords = "未找到匹配记录 No matching records found",
            paginate = list(
              previous = "上一页 Previous",
              `next` = "下一页 Next"
            )
          )
        )
      )

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

