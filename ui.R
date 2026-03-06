# ui.R  —— 完整 UI 定义，不包含 library/source/shinyApp

ui <- shinydashboard::dashboardPage(
  skin = "blue",

  # ----------------- Header -----------------
  shinydashboard::dashboardHeader(
    title = "FluxWatch",
    titleWidth = 260,

    # ✅ 这里必须用 *Output 占位符*，不能写 output$xxx
    shinydashboard::dropdownMenuOutput("messages"),
    shinydashboard::dropdownMenuOutput("notifications"),
    shinydashboard::dropdownMenuOutput("tasks")
  ),

  # ----------------- Sidebar -----------------
  shinydashboard::dashboardSidebar(
    width = 260,
    shiny::uiOutput("ui_sidebar")   # 动态生成菜单（basic/pro）
  ),

  # ----------------- Body -----------------
  shinydashboard::dashboardBody(
    shinydashboard::tabItems(

      # 1 数据导入
      shinydashboard::tabItem(
        tabName = "ingest",
        shiny::fluidRow(
          shinydashboard::box(
            width = 5, status = "primary", solidHeader = TRUE,
            title = "上传与校验",
            shiny::fileInput("data_csv", "上传 CSV", accept = c(".csv")),
            shiny::checkboxInput("guess_types", "自动猜测列类型（更宽松）", value = TRUE),
            shiny::actionButton("btn_load", "加载并校验", class = "btn-primary"),
            shiny::actionButton("btn_reset", "清空状态", class = "btn-default"),
            shiny::tags$hr(),
            shiny::verbatimTextOutput("txt_ingest_status")
          ),
          shinydashboard::box(
            width = 7, status = "info", solidHeader = TRUE,
            title = "数据预览",
            shiny::uiOutput("ui_schema_hint"),
            shiny::tableOutput("tbl_preview")
          )
        )
      ),

      # 2 通量计算
      shinydashboard::tabItem(
        tabName = "flux",
        shiny::fluidRow(
          shinydashboard::box(
            width = 4, status = "primary", solidHeader = TRUE,
            title = "计算设置",
            shiny::selectInput(
              "method", "通量方法",
              choices = c(
                "日积分 (daily_integral)" = "daily_integral",
                "期间平均 (period_mean)"  = "period_mean"
              )
            ),
            shiny::numericInput("conv_factor", "单位换算系数（可选）", value = 86.4, min = 0),
            shiny::actionButton("btn_compute", "计算通量", class = "btn-primary"),
            shiny::tags$hr(),
            shiny::verbatimTextOutput("txt_flux_summary")
          ),
          shinydashboard::box(
            width = 8, status = "info", solidHeader = TRUE,
            title = "通量结果",
            shiny::tableOutput("tbl_flux_daily")
          )
        )
      ),

      # 3 可视化
      shinydashboard::tabItem(
        tabName = "viz",
        shiny::fluidRow(
          shinydashboard::box(
            width = 4, status = "primary", solidHeader = TRUE,
            title = "图形选项",
            shiny::selectInput(
              "viz_type", "图形类型",
              choices = c("日通量时序" = "daily_ts", "累计通量" = "cumulative")
            ),
            shiny::checkboxInput("viz_show_points", "显示点", value = TRUE)
          ),
          shinydashboard::box(
            width = 8, status = "info", solidHeader = TRUE,
            title = "图形",
            shiny::plotOutput("plot_flux", height = 320)
          )
        )
      ),

      # 4 不确定性（付费功能：basic 会被 server 拦截，或显示提示）
      shinydashboard::tabItem(
        tabName = "unc",
        shiny::uiOutput("ui_unc_body")
      ),

      # 5 方案设计（付费功能）
      shinydashboard::tabItem(
        tabName = "design",
        shiny::uiOutput("ui_design_body")
      ),

      # 6 导出
      shinydashboard::tabItem(
        tabName = "export",
        shiny::fluidRow(
          shinydashboard::box(
            width = 6, status = "primary", solidHeader = TRUE,
            title = "导出结果",
            shiny::downloadButton("download_flux_daily", "下载：日通量结果 CSV"),
            shiny::tags$br(), shiny::tags$br(),
            shiny::downloadButton("download_mc_summary", "下载：MC 汇总 CSV"),
            shiny::tags$br(), shiny::tags$br(),
            shiny::downloadButton("download_design_table", "下载：方案推荐表 CSV")
          ),
          shinydashboard::box(
            width = 6, status = "info", solidHeader = TRUE,
            title = "导出说明",
            shiny::verbatimTextOutput("txt_export_note")
          )
        )
      ),

      # 7 状态页
      shinydashboard::tabItem(
        tabName = "status",
        shiny::fluidRow(
          shinydashboard::box(
            width = 12, status = "info", solidHeader = TRUE,
            title = "当前状态",
            shiny::verbatimTextOutput("txt_status")
          )
        )
      )
    )
  )
)
