mod_design_ui <- function(id) {
  ns <- NS(id)

  shinydashboard::tabItem(
    tabName = "design",

    shiny::fluidRow(
      shinydashboard::box(
        width = 4,
        status = "primary",
        solidHeader = TRUE,
        title = shiny::tagList(shiny::icon("drafting-compass"), " 设计参数 Design Parameters"),

        shiny::numericInput(
          ns("design_n_mc"),
          "蒙特卡洛迭代次数 MC Iterations",
          value = 1000,
          min = 100,
          max = 50000,
          step = 100
        ),

        shiny::selectInput(
          ns("design_intervals"),
          "候选采样间隔 Candidate Intervals",
          choices = c("1h","2h","4h","6h","8h","12h","24h","48h","7d","14d","30d"),
          multiple = TRUE,
          selected = c("6h","12h","24h","7d")
        ),

        shiny::numericInput(
          ns("design_target_re"),
          "目标相对误差(%) Target RE(%)",
          value = 15,
          min = 1,
          max = 100,
          step = 1
        ),

        shiny::actionButton(
          ns("btn_run_design"),
          "运行采样设计 Run Design",
          icon = shiny::icon("cogs"),
          class = "btn-primary btn-block"
        ),

        shiny::tags$hr(),
        shiny::downloadButton(ns("download_design_table"), "下载方案表 CSV", class = "btn-success btn-block"),

        shiny::tags$hr(),
        shiny::h5(shiny::icon("trophy"), " 最佳方案 Best Scheme"),
        shiny::verbatimTextOutput(ns("txt_design_best"))
      ),

      shinydashboard::box(
        width = 8,
        status = "info",
        solidHeader = TRUE,
        title = shiny::tagList(shiny::icon("table"), " 方案对比 Scheme Comparison"),
        DT::dataTableOutput(ns("tbl_design")),
        shiny::tags$hr(),
        shiny::h5(shiny::icon("chart-bar"), " 相对误差 vs 采样间隔 RE vs Interval"),
        shiny::plotOutput(ns("plot_design"), height = 300)
      )
    )
  )
}

mod_design_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {

    observeEvent(input$btn_run_design, {
      req(rv$clean_df, input$design_intervals)

      rv$design_df <- fw_run_design(
        intervals = input$design_intervals,
        target_re_pct = input$design_target_re,
        n_mc = input$design_n_mc
      )
    })

    output$txt_design_best <- shiny::renderPrint({
      if (is.null(rv$design_df)) {
        cat("Run design first.")
      } else {
        best <- rv$design_df[which.min(rv$design_df$Mean_RE_pct), ]
        cat(
          "Best:", best$Interval,
          "| Samples/yr:", best$Samples_yr,
          "| RE:", best$Mean_RE_pct, "%\n"
        )
      }
    })

    output$tbl_design <- DT::renderDataTable({
      req(rv$design_df)
      DT::datatable(rv$design_df, rownames = FALSE, options = list(pageLength = 15))
    })

    output$plot_design <- shiny::renderPlot({
      req(rv$design_df)
      fw_plot_design(rv$design_df, target_re = input$design_target_re)
    })

    output$download_design_table <- shiny::downloadHandler(
      filename = function() paste0("FluxWatch_design_", Sys.Date(), ".csv"),
      content = function(file) {
        utils::write.csv(
          if (!is.null(rv$design_df)) rv$design_df else data.frame(note = "No data"),
          file,
          row.names = FALSE
        )
      }
    )
  })
}
