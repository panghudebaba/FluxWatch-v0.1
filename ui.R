# ui.R  —— 只定义 ui，不要 library()/source()/shinyApp()

ui <- shinydashboard::dashboardPage(
  skin = "blue",

  shinydashboard::dashboardHeader(
    title = "FluxWatch",
    titleWidth = 260
  ),

  shinydashboard::dashboardSidebar(
    width = 260,

    shinydashboard::sidebarMenu(
      id = "nav",

      shinydashboard::menuItem("1 数据导入", tabName = "ingest",  icon = shiny::icon("upload")),
      shinydashboard::menuItem("2 通量计算", tabName = "flux",    icon = shiny::icon("calculator")),
      shinydashboard::menuItem("3 可视化",   tabName = "viz",     icon = shiny::icon("chart-line")),
      shinydashboard::menuItem("4 不确定性", tabName = "unc",     icon = shiny::icon("random")),
      shinydashboard::menuItem("5 方案设计", tabName = "design",  icon = shiny::icon("sliders-h")),
      shinydashboard::menuItem("6 结果导出", tabName = "export",  icon = shiny::icon("download")),

      shinydashboard::menuItem(" ", tabName = "sep", icon = shiny::icon("minus")), # 轻量分隔（替代 hr）

      shinydashboard::menuItem("项目状态", tabName = "status", icon = shiny::icon("info-circle"))
    )
  ),

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
              choices = c(
                "日通量时序" = "daily_ts",
                "累计通量"   = "cumulative"
              )
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

      # 4 不确定性（Monte Carlo）
      shinydashboard::tabItem(
        tabName = "unc",
        shiny::fluidRow(
          shinydashboard::box(
            width = 4, status = "primary", solidHeader = TRUE,
            title = "Monte Carlo 设置",
            shiny::numericInput("mc_n", "模拟次数 n", value = 2000, min = 100, step = 100),
            shiny::numericInput("default_cv_Q", "默认 Q 变异系数 CV（无 Q_sd 时）", value = 0.05, min = 0),
            shiny::numericInput("default_cv_C", "默认 C 变异系数 CV（无 C_sd 时）", value = 0.10, min = 0),
            shiny::numericInput("mc_seed", "随机种子", value = 1, min = 1),
            shiny::actionButton("btn_mc", "运行不确定性分析", class = "btn-primary"),
            shiny::tags$hr(),
            shiny::verbatimTextOutput("txt_mc_summary")
          ),
          shinydashboard::box(
            width = 8, status = "info", solidHeader = TRUE,
            title = "MC 分布",
            shiny::plotOutput("plot_mc_dist", height = 320),
            shiny::tableOutput("tbl_mc_ci")
          )
        )
      ),

      # 5 方案设计（预算-精度）
      shinydashboard::tabItem(
        tabName = "design",
        shiny::fluidRow(
          shinydashboard::box(
            width = 4, status = "primary", solidHeader = TRUE,
            title = "设计参数",
            shiny::numericInput("budget", "预算（总额）", value = 10000, min = 0),
            shiny::numericInput("cost_sample", "单次采样成本", value = 200, min = 0),
            shiny::numericInput("target_re", "目标相对误差（例如 0.10）", value = 0.10, min = 0, max = 1, step = 0.01),
            shiny::textInput("candidates", "候选频次（每年，逗号分隔）", value = "12,24,48"),
            shiny::numericInput("design_reps", "模拟重复次数", value = 300, min = 50, step = 50),
            shiny::actionButton("btn_design", "推荐方案", class = "btn-primary"),
            shiny::tags$hr(),
            shiny::verbatimTextOutput("txt_design_best")
          ),
          shinydashboard::box(
            width = 8, status = "info", solidHeader = TRUE,
            title = "方案对比表",
            shiny::tableOutput("tbl_design")
          )
        )
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
