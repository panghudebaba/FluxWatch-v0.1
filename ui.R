# 定义UI界面

# Define UI for application
ui <- dashboardPage(
  skin = "blue",

  dashboardHeader(
    title = "FluxWatch",
    titleWidth = 260
  ),

  dashboardSidebar(
    width = 260,

    sidebarMenu(
      id = "nav",

      menuItem("1 数据导入", tabName = "ingest", icon = icon("upload")),
      menuItem("2 通量计算", tabName = "flux", icon = icon("calculator")),
      menuItem("3 可视化",   tabName = "viz", icon = icon("chart-line")),
      menuItem("4 不确定性", tabName = "unc", icon = icon("random")),
      menuItem("5 方案设计", tabName = "design", icon = icon("sliders-h")),
      menuItem("6 结果导出",     tabName = "export", icon = icon("download")),

      hr(),

      menuItem("项目状态", tabName = "status", icon = icon("info-circle"))
    )
  ),

  dashboardBody(
    tabItems(
      # 1 数据导入
      tabItem(
        tabName = "ingest",
        fluidRow(
          box(
            width = 5, status = "primary", solidHeader = TRUE,
            title = "上传与校验",
            fileInput(
              "data_csv", "上传 CSV",
              accept = c(".csv")
            ),
            checkboxInput("guess_types", "自动猜测列类型（更宽松）", value = TRUE),
            actionButton("btn_load", "加载并校验", class = "btn-primary"),
            actionButton("btn_reset", "清空状态", class = "btn-default"),
            tags$hr(),
            verbatimTextOutput("txt_ingest_status")
          ),
          box(
            width = 7, status = "info", solidHeader = TRUE,
            title = "数据预览",
            uiOutput("ui_schema_hint"),
            tableOutput("tbl_preview")
          )
        )
      ),

      # 2 通量计算
      tabItem(
        tabName = "flux",
        fluidRow(
          box(
            width = 4, status = "primary", solidHeader = TRUE,
            title = "计算设置",
            selectInput(
              "method", "通量方法",
              choices = c(
                "日积分 (daily_integral)" = "daily_integral",
                "期间平均 (period_mean)"  = "period_mean"
              )
            ),
            numericInput("conv_factor", "单位换算系数（可选）", value = 86.4, min = 0),
            actionButton("btn_compute", "计算通量", class = "btn-primary"),
            tags$hr(),
            verbatimTextOutput("txt_flux_summary")
          ),
          box(
            width = 8, status = "info", solidHeader = TRUE,
            title = "通量结果",
            tableOutput("tbl_flux_daily")
          )
        )
      ),

      # 3 可视化
      tabItem(
        tabName = "viz",
        fluidRow(
          box(
            width = 4, status = "primary", solidHeader = TRUE,
            title = "图形选项",
            selectInput(
              "viz_type", "图形类型",
              choices = c(
                "日通量时序" = "daily_ts",
                "累计通量"   = "cumulative"
              )
            ),
            checkboxInput("viz_show_points", "显示点", value = TRUE)
          ),
          box(
            width = 8, status = "info", solidHeader = TRUE,
            title = "图形",
            plotOutput("plot_flux", height = 320)
          )
        )
      ),

      # 4 不确定性（Monte Carlo）
      tabItem(
        tabName = "unc",
        fluidRow(
          box(
            width = 4, status = "primary", solidHeader = TRUE,
            title = "Monte Carlo 设置",
            numericInput("mc_n", "模拟次数 n", value = 2000, min = 100, step = 100),
            numericInput("default_cv_Q", "默认 Q 变异系数 CV（无 Q_sd 时）", value = 0.05, min = 0),
            numericInput("default_cv_C", "默认 C 变异系数 CV（无 C_sd 时）", value = 0.10, min = 0),
            numericInput("mc_seed", "随机种子", value = 1, min = 1),
            actionButton("btn_mc", "运行不确定性分析", class = "btn-primary"),
            tags$hr(),
            verbatimTextOutput("txt_mc_summary")
          ),
          box(
            width = 8, status = "info", solidHeader = TRUE,
            title = "MC 分布",
            plotOutput("plot_mc_dist", height = 320),
            tableOutput("tbl_mc_ci")
          )
        )
      ),

      # 5 方案设计（预算-精度）
      tabItem(
        tabName = "design",
        fluidRow(
          box(
            width = 4, status = "primary", solidHeader = TRUE,
            title = "设计参数",
            numericInput("budget", "预算（总额）", value = 10000, min = 0),
            numericInput("cost_sample", "单次采样成本", value = 200, min = 0),
            numericInput("target_re", "目标相对误差（例如 0.10）", value = 0.10, min = 0, max = 1, step = 0.01),
            textInput("candidates", "候选频次（每年，逗号分隔）", value = "12,24,48"),
            numericInput("design_reps", "模拟重复次数", value = 300, min = 50, step = 50),
            actionButton("btn_design", "推荐方案", class = "btn-primary"),
            tags$hr(),
            verbatimTextOutput("txt_design_best")
          ),
          box(
            width = 8, status = "info", solidHeader = TRUE,
            title = "方案对比表",
            tableOutput("tbl_design")
          )
        )
      ),

      # 6 导出
      tabItem(
        tabName = "export",
        fluidRow(
          box(
            width = 6, status = "primary", solidHeader = TRUE,
            title = "导出结果",
            downloadButton("download_flux_daily", "下载：日通量结果 CSV"),
            tags$br(), tags$br(),
            downloadButton("download_mc_summary", "下载：MC 汇总 CSV"),
            tags$br(), tags$br(),
            downloadButton("download_design_table", "下载：方案推荐表 CSV")
          ),
          box(
            width = 6, status = "info", solidHeader = TRUE,
            title = "导出说明",
            verbatimTextOutput("txt_export_note")
          )
        )
      ),

      # 7 状态页（可选）
      tabItem(
        tabName = "status",
        fluidRow(
          box(
            width = 12, status = "info", solidHeader = TRUE,
            title = "当前状态",
            verbatimTextOutput("txt_status")
          )
        )
      )
    )
  )
)


