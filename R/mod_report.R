mod_report_ui <- function(id) {
  ns <- NS(id)

  shinydashboard::tabItem(
    tabName = "report",

    shiny::fluidRow(
      shinydashboard::box(
        width = 6,
        status = "primary",
        solidHeader = TRUE,
        title = shiny::tagList(shiny::icon("file-download"), " 数据导出 Data Export"),

        shiny::h5("选择需要导出的内容 Select items to export:"),
        shiny::tags$hr(),
        shiny::downloadButton(ns("download_clean_data"), "清洗后数据 CSV", class = "btn-info btn-block"),
        shiny::tags$br(),
        shiny::downloadButton(ns("download_flux_result"), "通量结果 CSV", class = "btn-info btn-block"),
        shiny::tags$br(),
        shiny::downloadButton(ns("download_design_result"), "方案结果 CSV", class = "btn-info btn-block"),
        shiny::tags$br(),
        shiny::downloadButton(ns("download_report_plot"), "图表汇总 PNG", class = "btn-info btn-block")
      ),

      shinydashboard::box(
        width = 6,
        status = "success",
        solidHeader = TRUE,
        title = shiny::tagList(shiny::icon("file-alt"), " 研究报告 Research Report"),

        shiny::tags$div(
          class = "bilingual-block",
          shiny::tags$p(class = "zh", "基于当前分析结果自动生成标准化研究报告。"),
          shiny::tags$p(class = "en", "Automatically generate a standardized research report.")
        ),

        shiny::tags$hr(),

        shiny::selectInput(
          ns("report_format"),
          "报告格式 Report Format",
          choices = c("HTML" = "html", "Word" = "docx", "PDF" = "pdf")
        ),

        shiny::downloadButton(ns("download_report"), "生成报告 Generate Report", class = "btn-success btn-block"),
        shiny::tags$hr(),
        shiny::verbatimTextOutput(ns("txt_report_status"))
      )
    )
  )
}

mod_report_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {

    output$download_clean_data <- shiny::downloadHandler(
      filename = function() paste0("FluxWatch_clean_", Sys.Date(), ".csv"),
      content = function(file) {
        utils::write.csv(
          if (!is.null(rv$clean_df)) rv$clean_df else data.frame(note = "No data"),
          file,
          row.names = FALSE
        )
      }
    )

    output$download_flux_result <- shiny::downloadHandler(
      filename = function() paste0("FluxWatch_flux_result_", Sys.Date(), ".csv"),
      content = function(file) {
        utils::write.csv(
          if (!is.null(rv$flux_df)) rv$flux_df else data.frame(note = "No data"),
          file,
          row.names = FALSE
        )
      }
    )

    output$download_design_result <- shiny::downloadHandler(
      filename = function() paste0("FluxWatch_design_result_", Sys.Date(), ".csv"),
      content = function(file) {
        utils::write.csv(
          if (!is.null(rv$design_df)) rv$design_df else data.frame(note = "No data"),
          file,
          row.names = FALSE
        )
      }
    )

    output$download_report_plot <- shiny::downloadHandler(
      filename = function() paste0("FluxWatch_summary_", Sys.Date(), ".png"),
      content = function(file) {
        fw_export_summary_plot(
          file = file,
          clean_df = rv$clean_df,
          flux_df = rv$flux_df,
          design_df = rv$design_df
        )
      }
    )

    output$download_report <- shiny::downloadHandler(
      filename = function() {
        ext <- switch(input$report_format,
                      html = ".html",
                      docx = ".docx",
                      pdf  = ".pdf",
                      ".html")
        paste0("FluxWatch_report_", Sys.Date(), ext)
      },
      content = function(file) {
        fw_write_simple_report(
          file = file,
          report_format = input$report_format,
          clean_df = rv$clean_df,
          flux_df = rv$flux_df,
          design_df = rv$design_df
        )
      }
    )

    output$txt_report_status <- shiny::renderPrint({
      cat("Clean data:", ifelse(!is.null(rv$clean_df), "Ready", "Not loaded"), "\n")
      cat("Flux result:", ifelse(!is.null(rv$flux_df), "Computed", "Not computed"), "\n")
      cat("Design:", ifelse(!is.null(rv$design_df), "Done", "Not run"), "\n")
    })
  })
}
