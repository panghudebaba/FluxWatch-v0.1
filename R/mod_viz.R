# =========================================================
# mod_viz.R
# Visualization module
# =========================================================

# =========================================================
# mod_viz.R  ——  仅 UI 部分重构，Server 不变
# =========================================================

mod_viz_ui <- function(id) {
  ns <- shiny::NS(id)

  # ── 卡片式分节辅助函数 ─────────────────────────────────
  section <- function(icon_name, title_zh, title_en, bar_colour = "#3c8dbc", ...) {
    shiny::tags$div(
      style = paste(
        "border: 1px solid #d2d6de;",
        "border-radius: 5px;",
        "margin-bottom: 18px;"
      ),
      # 标题栏：仅上方圆角
      shiny::tags$div(
        style = paste0(
          "background-color: ", bar_colour, ";",
          "color: #ffffff;",
          "padding: 9px 14px;",
          "font-size: 15px;",
          "font-weight: bold;",
          "letter-spacing: 0.4px;",
          "border-radius: 4px 4px 0 0;"
        ),
        shiny::icon(icon_name, style = "margin-right:7px; font-size:16px;"),
        title_zh,
        shiny::tags$span(
          style = paste(
            "font-weight: normal;",
            "font-size: 12px;",
            "margin-left: 8px;",
            "opacity: 0.88;"
          ),
          title_en
        )
      ),
      # 内容区：仅下方圆角，允许下拉菜单溢出
      shiny::tags$div(
        style = paste(
          "padding: 13px 14px 11px 14px;",
          "background: #fff;",
          "border-radius: 0 0 4px 4px;",
          "overflow: visible;"
        ),
        ...
      )
    )
  }


  shinydashboard::tabItem(
    tabName = "viz",

    shiny::fluidRow(

      # ════════════════════════════════════════════════════
      # 左侧设置面板
      # ════════════════════════════════════════════════════
      shinydashboard::box(
        width       = 4,
        status      = "primary",
        solidHeader = TRUE,
        title       = shiny::tagList(
          shiny::icon("sliders-h"), " 图形设置 Plot Settings"
        ),

        # ══════════════════════════════════════════════════
        # ① 数据选择
        # ══════════════════════════════════════════════════
        section(
          "database", "① 数据选择", "Data Selection",
          bar_colour = "#3c8dbc",

          shiny::radioButtons(
            ns("data_source"),
            "数据来源 Data source",
            choices = c(
              "使用第一步清洗结果 Use ingest result" = "ingest",
              "上传新文件 Upload new file"           = "upload"
            ),
            selected = "ingest"
          ),

          shiny::conditionalPanel(
            condition = sprintf("input['%s'] == 'upload'", ns("data_source")),
            ns = ns,
            shiny::fileInput(
              ns("viz_file"),
              "上传文件 Upload file",
              accept      = c(".xlsx", ".xls", ".rds", ".csv"),
              buttonLabel = "浏览 Browse",
              placeholder = "未选择文件 No file selected",
              width       = "100%"
            )
          ),

          shiny::selectizeInput(
            ns("rain_sites"),
            "降雨站点 Rain sites",
            choices  = NULL,
            multiple = TRUE
          ),

          shiny::selectizeInput(
            ns("flow_sites"),
            "流量站点 Flow sites",
            choices  = NULL,
            multiple = TRUE
          ),

          shiny::selectizeInput(
            ns("wq_sites"),
            "水质站点 WQ sites",
            choices  = NULL,
            multiple = TRUE
          ),

          shiny::checkboxInput(
            ns("use_topology_match"),
            "同站点联动时使用 Topology 映射 Use Topology mapping for linked mode",
            value = TRUE
          ),

          shiny::dateRangeInput(
            ns("date_range"),
            "时间范围 Date range",
            start    = Sys.Date() - 30,
            end      = Sys.Date(),
            format   = "yyyy-mm-dd",
            language = "zh-CN"
          )
        ),

        # ══════════════════════════════════════════════════
        # ② 绘图模式与指标选择
        # ══════════════════════════════════════════════════
        section(
          "chart-bar", "② 绘图模式与指标", "Plot Mode & Indicators",
          bar_colour = "#00a65a",

          shiny::selectInput(
            ns("plot_mode"),
            "绘图模式 Plot mode",
            choices = c(
              "降雨 + 流量 + 水质 PP + QF + WQ"             = "rain_flow_wq",
              "流量 + 水质 QF + WQ"                          = "flow_wq",
              "同站点水质 + 流量 Linked WQ + QF"             = "linked_flow_wq",
              "降雨 + 流量 + 浓度标记 PP + QF + Conc Label"  = "rain_flow_conclabel",
              "logC-logQ 浓度-流量对数关系"      = "logc_logq",
              "单个要素 Single variable"                      = "single"
            ),
            selected = "flow_wq"
          ),

          shiny::conditionalPanel(
            condition = sprintf("input['%s'] == 'single'", ns("plot_mode")),
            ns = ns,
            shiny::selectInput(
              ns("single_type"),
              "单图类型 Single plot type",
              choices = c(
                "降雨 Rain"          = "rain",
                "流量 Flow"          = "flow",
                "水质 Water quality" = "wq"
              ),
              selected = "flow"
            )
          ),

          shiny::selectInput(
            ns("rain_panel_mode"),
            "降雨面板 Rain panels",
            choices = c(
              "叠加 Overlay"             = "overlay",
              "按站点分面 Facet by site" = "facet_site"
            ),
            selected = "overlay"
          ),

          shiny::selectInput(
            ns("flow_panel_mode"),
            "流量面板 Flow panels",
            choices = c(
              "叠加 Overlay"             = "overlay",
              "按站点分面 Facet by site" = "facet_site"
            ),
            selected = "overlay"
          ),

          shiny::selectInput(
            ns("wq_panel_mode"),
            "水质面板 WQ panels",
            choices = c(
              "叠加 Overlay"                          = "overlay",
              "按站点分面 Facet by site"               = "facet_site",
              "按指标分面 Facet by indicator"          = "facet_var",
              "站点 × 指标 Facet by site × indicator" = "facet_site_var"
            ),
            selected = "facet_site_var"
          ),

          shiny::uiOutput(ns("ui_wq_vars"))
        ),

        # ══════════════════════════════════════════════════
        # ③ 图形参数设置
        # ══════════════════════════════════════════════════
        section(
          "cog", "③ 图形参数", "Chart Parameters",
          bar_colour = "#f39c12",

          shiny::fluidRow(
            shiny::column(6,
                          shiny::textInput(
                            ns("date_breaks"),
                            "时间刻度 Date breaks",
                            value = "1 month"
                          )
            ),
            shiny::column(6,
                          shiny::textInput(
                            ns("date_labels"),
                            "标签格式 Date labels",
                            value = "%Y-%m-%d"
                          )
            )
          ),

          shiny::fluidRow(
            shiny::column(6,
                          shiny::numericInput(
                            ns("base_size"),
                            "字号 Base size",
                            value = 13, min = 8, max = 28, step = 1
                          )
            ),
            shiny::column(6,
                          shiny::numericInput(
                            ns("rel_size"),
                            "相对字号 Rel size",
                            value = 1.05, min = 0.5, max = 3, step = 0.05
                          )
            )
          ),

          shiny::checkboxInput(
            ns("show_wq_smooth"),
            "显示水质趋势线 Show WQ smooth",
            value = TRUE
          ),

          shiny::checkboxInput(
            ns("show_imputed_points"),
            "显示流量插补点 Show imputed flow points",
            value = TRUE
          ),

          shiny::checkboxInput(
            ns("add_standard_lines"),
            "显示水质标准线 Show WQ standard lines",
            value = FALSE
          ),

          shiny::tags$hr(style = "margin: 10px 0;"),

          shiny::selectInput(
            ns("download_format"),
            "下载格式 Download format",
            choices  = c("PNG" = "png", "JPG" = "jpg", "PDF" = "pdf"),
            selected = "png"
          ),

          shiny::downloadButton(
            ns("download_plot"),
            "下载图片 Download plot",
            class = "btn-info btn-block"
          )
        )
      ),

      # ════════════════════════════════════════════════════
      # 右侧图形展示面板
      # ════════════════════════════════════════════════════
      shinydashboard::box(
        width       = 8,
        status      = "info",
        solidHeader = TRUE,
        title       = shiny::tagList(
          shiny::icon("chart-area"), " 图形展示 Plot Display"
        ),

        shiny::verbatimTextOutput(ns("viz_log")),
        shiny::plotOutput(ns("plot_main"), height = "1000px")
      )
    )
  )
}



mod_viz_server <- function(id, rv) {
  shiny::moduleServer(id, function(input, output, session) {

    `%||%` <- function(a, b) if (!is.null(a)) a else b

    # -----------------------------------------------------
    # 数据源 reactive
    # -----------------------------------------------------
    viz_clean_list <- shiny::reactive({
      if (identical(input$data_source, "ingest")) {
        shiny::req(rv$clean_list)
        return(fw_viz_normalize_clean_list(rv$clean_list))
      }

      shiny::req(input$viz_file)
      fw_viz_read_uploaded_data(input$viz_file$datapath)
    })

    # -----------------------------------------------------
    # 日志
    # -----------------------------------------------------
    output$viz_log <- shiny::renderText({
      cl <- viz_clean_list()
      nm <- names(cl)

      dd <- fw_viz_get_plot_data(cl)

      lines <- c(
        "Visualization ready / 图形模块已就绪",
        paste0("Data source / 数据来源: ", if (identical(input$data_source, "ingest")) "ingest" else "upload"),
        paste0("Sheets / 工作表: ", paste(nm, collapse = ", "))
      )

      if (!is.null(dd$rain)) lines <- c(lines, paste0("Rain rows / 降雨行数: ", nrow(dd$rain)))
      if (!is.null(dd$flow)) lines <- c(lines, paste0("Flow rows / 流量行数: ", nrow(dd$flow)))
      if (!is.null(dd$wq)) lines <- c(lines, paste0("WaterQuality rows / 水质行数: ", nrow(dd$wq)))
      if (!is.null(dd$topo)) lines <- c(lines, paste0("Topology rows / 拓扑行数: ", nrow(dd$topo)))

      paste(lines, collapse = "\n")
    })

    # -----------------------------------------------------
    # 动态 choices
    # -----------------------------------------------------
    shiny::observe({
      cl <- viz_clean_list()
      dd <- fw_viz_get_plot_data(cl)

      rain_choices <- fw_viz_get_site_choices(dd$rain)
      flow_choices <- fw_viz_get_site_choices(dd$flow)
      wq_choices   <- fw_viz_get_site_choices(dd$wq)
      wq_vars      <- fw_viz_get_wq_vars(dd$wq)

      shiny::updateSelectizeInput(
        session, "rain_sites",
        choices = rain_choices,
        selected = if (length(rain_choices) > 0) utils::head(unname(rain_choices), min(2, length(rain_choices))) else NULL,
        server = TRUE
      )

      shiny::updateSelectizeInput(
        session, "flow_sites",
        choices = flow_choices,
        selected = if (length(flow_choices) > 0) utils::head(unname(flow_choices), min(2, length(flow_choices))) else NULL,
        server = TRUE
      )

      shiny::updateSelectizeInput(
        session, "wq_sites",
        choices = wq_choices,
        selected = if (length(wq_choices) > 0) utils::head(unname(wq_choices), min(2, length(wq_choices))) else NULL,
        server = TRUE
      )

      current_wq_sel <- isolate(input$wq_vars)
      keep_sel <- intersect(current_wq_sel %||% character(0), wq_vars)
      if (length(keep_sel) == 0 && length(wq_vars) > 0) {
        keep_sel <- if ("TN" %in% wq_vars) "TN" else utils::head(wq_vars, min(2, length(wq_vars)))
      }

      output$ui_wq_vars <- shiny::renderUI({
        shiny::selectizeInput(
          session$ns("wq_vars"),
          "水质指标 WQ indicators",
          choices = wq_vars,
          selected = keep_sel,
          multiple = TRUE
        )
      })

      # 自动更新日期范围
      tm_all <- c()
      if (!is.null(dd$rain) && "TM" %in% names(dd$rain)) tm_all <- c(tm_all, dd$rain$TM)
      if (!is.null(dd$flow) && "TM" %in% names(dd$flow)) tm_all <- c(tm_all, dd$flow$TM)
      if (!is.null(dd$wq) && "TM" %in% names(dd$wq)) tm_all <- c(tm_all, dd$wq$TM)

      tm_all <- tm_all[!is.na(tm_all)]
      if (length(tm_all) > 0) {
        shiny::updateDateRangeInput(
          session, "date_range",
          start = as.Date(min(tm_all)),
          end = as.Date(max(tm_all))
        )
      }
    })

    # -----------------------------------------------------
    # 当前图
    # -----------------------------------------------------
    current_plot <- shiny::reactive({
      cl <- viz_clean_list()
      shiny::req(input$date_range)

      start_time <- as.POSIXct(paste(input$date_range[1], "00:00:00"), tz = "Asia/Shanghai")
      end_time   <- as.POSIXct(paste(input$date_range[2], "23:59:59"), tz = "Asia/Shanghai")

      std_lines <- NULL

      fw_viz_build_plot(
        clean_list = cl,
        plot_mode = input$plot_mode,
        single_type = input$single_type %||% "flow",
        rain_sites = input$rain_sites,
        flow_sites = input$flow_sites,
        wq_sites = input$wq_sites,
        wq_vars = input$wq_vars,
        rain_panel_mode = input$rain_panel_mode,
        flow_panel_mode = input$flow_panel_mode,
        wq_panel_mode = input$wq_panel_mode,
        start_time = start_time,
        end_time = end_time,
        use_topology_match = isTRUE(input$use_topology_match),
        show_wq_smooth = isTRUE(input$show_wq_smooth),
        show_imputed_points = isTRUE(input$show_imputed_points),
        add_standard_lines = isTRUE(input$add_standard_lines),
        std_lines = std_lines,
        date_breaks = input$date_breaks,
        date_labels = input$date_labels,
        base_size = input$base_size,
        rel_size = input$rel_size
      )
    })

    output$plot_main <- shiny::renderPlot({
      current_plot()
    }, res = 120)

    # -----------------------------------------------------
    # 下载
    # -----------------------------------------------------
    output$download_plot <- shiny::downloadHandler(
      filename = function() {
        ext <- input$download_format %||% "png"
        paste0(
          "Viz_",
          input$plot_mode, "_",
          format(Sys.time(), "%Y%m%d_%H%M%S"),
          ".",
          ext
        )
      },
      content = function(file) {
        ext <- input$download_format %||% "png"
        p <- current_plot()

        if (ext == "png") {
          ggplot2::ggsave(
            filename = file,
            plot = p,
            width = 14,
            height = 10,
            dpi = 300,
            units = "in",
            device = "png",
            bg = "white"
          )
        } else if (ext == "jpg") {
          ggplot2::ggsave(
            filename = file,
            plot = p,
            width = 14,
            height = 10,
            dpi = 300,
            units = "in",
            device = "jpeg",
            bg = "white"
          )
        } else if (ext == "pdf") {
          ggplot2::ggsave(
            filename = file,
            plot = p,
            width = 14,
            height = 10,
            units = "in",
            device = grDevices::pdf,
            bg = "white"
          )
        }
      }
    )
  })
}
