
ui <- shinydashboard::dashboardPage(
  skin = "blue",

  # 1：Header — 简化 title，用 CSS 修复 overflow
  shinydashboard::dashboardHeader(
    title = shiny::tags$span(
      shiny::tags$span(
        "通量宝 FluxWatch",
        style = "font-weight:700; font-size:17px; vertical-align:middle;"
      )
    ),
    titleWidth = 260,

    shiny::tags$li(
      class = "dropdown",
      style = "padding:8px 16px; color:#fff; font-size:12px; max-width:600px; line-height:1.4;",
      shiny::tags$span(
        "流域水文水环境数据交互式分析平台",
        shiny::tags$br(),
        "Interactive Analysis Platform for Watershed Hydrology & Water Environment"
      )
    ),

    shiny::tags$li(
      class = "dropdown",
      style = "padding:14px 20px; color:rgba(255,255,255,0.7); font-size:12px; font-weight:600;",
      "V 0.01"
    )
  ),

  # ----------------- 侧边栏 Sidebar -----------------
  shinydashboard::dashboardSidebar(
    width = 260,
    shinydashboard::sidebarMenu(

      id = "nav",

      shinydashboard::menuItem(
        shiny::tags$span("平台简介", shiny::tags$br(),
                         shiny::tags$small("Introduction", style = "color:#aaa;")),
        tabName = "welcome",
        icon    = shiny::icon("home"),
        selected = TRUE
      ),

      shiny::tags$hr(style = "border-color:#444; margin:4px 16px;"),

      shinydashboard::menuItem(
        shiny::tags$span("1 数据上传与整理清洗", shiny::tags$br(),
                         shiny::tags$small("Data Upload, Sorting & Cleaning",
                                           style = "color:#aaa;")),
        tabName = "ingest",
        icon    = shiny::icon("upload")
      ),

      shinydashboard::menuItem(
        shiny::tags$span("2 可视分析与统计处理", shiny::tags$br(),
                         shiny::tags$small("Visualization & Statistical Analysis",
                                           style = "color:#aaa;")),
        tabName = "viz",
        icon    = shiny::icon("chart-line")
      ),

      shinydashboard::menuItem(
        shiny::tags$span("3 通量计算和结果展示", shiny::tags$br(),
                         shiny::tags$small("Flux Calculation & Result Presentation",
                                           style = "color:#aaa;")),
        tabName = "flux",
        icon    = shiny::icon("calculator")
      ),

      # ★ 已移除 badgeLabel = "PRO" 和 badgeColor = "yellow" ★
      shinydashboard::menuItem(
        shiny::tags$span("4 监测方案设计与优化", shiny::tags$br(),
                         shiny::tags$small("Monitoring Scheme Design & Optimization",
                                           style = "color:#aaa;")),
        tabName = "design",
        icon    = shiny::icon("drafting-compass")
      ),

      shinydashboard::menuItem(
        shiny::tags$span("5 报告编制与成果输出", shiny::tags$br(),
                         shiny::tags$small("Report Compilation & Result Output",
                                           style = "color:#aaa;")),
        tabName = "report",
        icon    = shiny::icon("file-alt")
      ),

      shiny::tags$hr(style = "border-color:#444; margin:4px 16px;"),

      shinydashboard::menuItem(
        shiny::tags$span("关于我们", shiny::tags$br(),
                         shiny::tags$small("About Us", style = "color:#aaa;")),
        tabName = "about",
        icon    = shiny::icon("users")
      ),

      shinydashboard::menuItem(
        shiny::tags$span("免责声明", shiny::tags$br(),
                         shiny::tags$small("Disclaimer", style = "color:#aaa;")),
        tabName = "disclaimer",
        icon    = shiny::icon("exclamation-triangle")
      )
    )
  ),

  # ----------------- 主体 Body -----------------
  shinydashboard::dashboardBody(

    shiny::tags$head(
      shiny::tags$style(shiny::HTML("
        /* ===== 关键修复：Header Logo 区域 ===== */
        .main-header .logo {
          height: 50px !important;
          line-height: 50px !important;
          padding: 0 12px !important;
          overflow: visible !important;
        }
        .main-header .logo img {
          max-height: 34px !important;
          vertical-align: middle !important;
          display: inline !important;
        }
        .main-header .navbar {
          min-height: 50px !important;
        }
        .left-side, .main-sidebar {
          padding-top: 50px !important;
        }
        .sidebar .sidebar-menu > li > a > img {
          display: inline-block !important;
        }

        .content-wrapper { background-color: #f4f6f9; }
        .box-header .box-title { font-weight: 600; }

        .brand-hero {
          text-align: center;
          padding: 40px 20px;
          background: linear-gradient(135deg, #1a3a5c 0%, #2980b9 100%);
          border-radius: 8px;
          color: #fff;
          margin-bottom: 20px;
        }
        .brand-hero .brand-logo {
          height: 120px;
          margin-bottom: 16px;
          display: inline-block !important;
        }
        .brand-hero .brand-seal {
          font-family: 'STKaiti', 'KaiTi', 'FangSong', serif;
          font-size: 52px;
          font-weight: 700;
          letter-spacing: 12px;
          margin-bottom: 4px;
        }
        .brand-hero .brand-en {
          font-size: 28px;
          font-weight: 300;
          letter-spacing: 4px;
          opacity: 0.85;
        }
        .brand-hero .brand-tagline {
          font-size: 14px;
          margin-top: 16px;
          opacity: 0.8;
          line-height: 1.6;
        }

        .workflow-step {
          padding: 12px 16px;
          margin-bottom: 8px;
          border-left: 4px solid #3c8dbc;
          background: #eef5fb;
          border-radius: 4px;
        }
        .workflow-step .step-num {
          display: inline-block;
          width: 28px; height: 28px;
          line-height: 28px;
          text-align: center;
          border-radius: 50%;
          background: #3c8dbc;
          color: #fff;
          font-weight: 700;
          margin-right: 10px;
        }

        .bilingual-block { margin-bottom: 18px; line-height: 1.8; }
        .bilingual-block .zh { font-size: 14px; color: #333; }
        .bilingual-block .en { font-size: 13px; color: #666; font-style: italic; }

        .disclaimer-text { color: #888; font-size: 13px; line-height: 1.8; }

        .sidebar-menu > li > a {
          padding: 14px 18px !important;
          line-height: 1.6 !important;
          font-size: 15px !important;
        }
        .sidebar-menu > li > a small {
          font-size: 12px !important;
          line-height: 1.5 !important;
        }
        .sidebar-menu > li {
          margin-bottom: 4px !important;
        }

      "))
    ),

    shinydashboard::tabItems(

      # ====================== 平台简介 ======================
      shinydashboard::tabItem(
        tabName = "welcome",

        shiny::fluidRow(
          shiny::column(
            width = 12,
            shiny::tags$div(
              class = "brand-hero",
              shiny::tags$img(src = "logo.png", class = "brand-logo"),
              shiny::tags$div(class = "brand-seal", "通量宝"),
              shiny::tags$div(class = "brand-en",   "FluxWatch"),
              shiny::tags$div(
                class = "brand-tagline",
                "流域水文水环境数据交互式分析平台",
                shiny::tags$br(),
                "Interactive Analysis Platform for Watershed Hydrology & Water Environment"
              )
            )
          )
        ),

        shiny::fluidRow(
          shinydashboard::box(
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            title = shiny::tagList(
              shiny::icon("info-circle"),
              " 平台介绍 Platform Introduction"
            ),

            shiny::tags$div(
              class = "bilingual-block",
              shiny::tags$p(
                class = "zh",
                shiny::tags$strong("通量宝"),
                "是水利部交通运输部国家能源局南京水利科学研究院",
                "生态水文与水资源保护研究团队研发的面向流域水文水环境领域的",
                "数据交互式分析平台软件。该平台可实现流域水文水质数据上传、",
                "清洗、统计与可视化分析，监测断面物质通量计算、不确定性分析、",
                "断面通量监测方案制定和优化，数据分析结果导出和研究报告编制。"
              ),
              shiny::tags$p(
                class = "en",
                shiny::tags$strong("FluxWatch"),
                "is an interactive data analysis platform software for watershed ",
                "hydrology and water environment, developed by the Ecohydrology ",
                "and Water Resources Protection Research Team from Nanjing ",
                "Hydraulic Research Institute (NHRI). This platform enables the ",
                "uploading, cleaning, statistical analysis, and visualization of ",
                "watershed hydrological and water quality data; material flux ",
                "calculating at flux monitoring stations and the uncertainty analysis; ",
                "flux monitoring schemes formulation and optimization; data analysis ",
                "results downloading; compilation of research reports ", shiny::tags$em("etc"), "."
              )
            )
          )
        ),

        shiny::fluidRow(
          shinydashboard::box(
            width = 6,
            status = "info",
            solidHeader = TRUE,
            title = shiny::tagList(
              shiny::icon("user-graduate"),
              " 服务对象 Target Users"
            ),
            shiny::tags$div(
              class = "bilingual-block",
              shiny::tags$p(class = "zh",
                            "从事水文水资源和水生态水环境研究的研究生、学者和研究人员。"
              ),
              shiny::tags$p(class = "en",
                            "Graduate students, scholars, and researchers engaged in ",
                            "hydrology, water resources, aquatic ecology, and water environment studies."
              )
            )
          ),

          shinydashboard::box(
            width = 6,
            status = "success",
            solidHeader = TRUE,
            title = shiny::tagList(
              shiny::icon("star"),
              " 主要功能 Key Features"
            ),
            shiny::tags$div(
              class = "workflow-step",
              shiny::tags$span(class = "step-num", "1"),
              "数据上传与整理清洗 Data Upload, Sorting & Cleaning"
            ),
            shiny::tags$div(
              class = "workflow-step",
              shiny::tags$span(class = "step-num", "2"),
              "可视分析与统计处理 Visualization & Statistical Analysis"
            ),
            shiny::tags$div(
              class = "workflow-step",
              shiny::tags$span(class = "step-num", "3"),
              "通量计算和结果展示 Flux Calculation & Result Presentation"
            ),
            shiny::tags$div(
              class = "workflow-step",
              shiny::tags$span(class = "step-num", "4"),
              "监测方案设计与优化 Monitoring Scheme Design & Optimization"
            ),
            shiny::tags$div(
              class = "workflow-step",
              shiny::tags$span(class = "step-num", "5"),
              "报告编制与成果输出 Report Compilation & Result Output"
            )
          )
        )
      ),

      # ====================== 1 数据上传与整理清洗 ======================
      shinydashboard::tabItem(
        tabName = "ingest",

        shiny::fluidRow(
          shinydashboard::box(
            width = 4,
            status = "primary",
            solidHeader = TRUE,
            title = shiny::tagList(
              shiny::icon("upload"),
              " 上传与校验 Upload & Validation"
            ),

            shiny::fileInput(
              "data_csv",
              "上传 CSV 文件 / Upload CSV",
              accept      = c(".csv"),
              width       = "100%",
              placeholder = "未选择文件 No file selected",
              buttonLabel = "浏览 Browse"
            ),

            shiny::tags$hr(),

            shiny::fluidRow(
              shiny::column(
                6,
                shiny::actionButton(
                  "btn_load",
                  "加载并校验",
                  icon  = shiny::icon("check"),
                  class = "btn-primary btn-block"
                )
              ),
              shiny::column(
                6,
                shiny::actionButton(
                  "btn_reset",
                  "清空状态",
                  icon  = shiny::icon("undo"),
                  class = "btn-default btn-block"
                )
              )
            ),

            shiny::tags$hr(),

            shiny::h5(shiny::icon("clipboard-check"), " 校验日志 Validation Log"),
            shiny::verbatimTextOutput("txt_ingest_status")
          ),

          shinydashboard::box(
            width = 8,
            status = "info",
            solidHeader = TRUE,
            title = shiny::tagList(
              shiny::icon("table"),
              " 数据预览与编辑 Data Preview & Editing"
            ),

            shiny::uiOutput("ui_schema_hint"),
            shiny::tags$br(),
            DT::dataTableOutput("tbl_preview")
          )
        )
      ),

      # ====================== 2 可视分析与统计处理 ======================
      shinydashboard::tabItem(
        tabName = "viz",

        shiny::fluidRow(
          shinydashboard::box(
            width = 4,
            status = "primary",
            solidHeader = TRUE,
            title = shiny::tagList(
              shiny::icon("sliders-h"),
              " 图形选项 Plot Options"
            ),

            shiny::selectInput(
              "viz_var",
              "绘图变量 Variable",
              choices = c(
                "浓度 Concentration" = "concentration",
                "通量 Flux"           = "flux",
                "温度 Temperature"    = "temperature"
              )
            ),

            shiny::selectInput(
              "viz_type",
              "图形类型 Plot Type",
              choices = c(
                "日时序图 Daily Time Series" = "daily_ts",
                "累计通量图 Cumulative Flux" = "cumulative",
                "月箱线图 Monthly Box Plot"      = "boxplot_month"
              )
            ),

            shiny::checkboxInput("viz_show_points", "显示数据点 Show data points", value = TRUE),
            shiny::checkboxInput("viz_show_smooth", "添加趋势线 Add trend line", value = FALSE),

            shiny::tags$hr(),

            shiny::downloadButton("download_plot", "下载图片 Download (PNG)", class = "btn-info btn-block")
          ),

          shinydashboard::box(
            width = 8,
            status = "info",
            solidHeader = TRUE,
            title = shiny::tagList(shiny::icon("chart-area"), " 图形展示 Plot Display"),
            shiny::plotOutput("plot_flux", height = 400)
          )
        )
      ),

      # ====================== 3 通量计算和结果展示 ======================
      shinydashboard::tabItem(
        tabName = "flux",

        shiny::fluidRow(
          shinydashboard::box(
            width = 4,
            status = "primary",
            solidHeader = TRUE,
            title = shiny::tagList(shiny::icon("calculator"), " 计算设置 Calculation Settings"),

            shiny::selectInput(
              "method", "通量方法 Flux Method",
              choices = c(
                "日积分（梯形法） Daily Integral (trapezoid)" = "daily_integral",
                "期间平均 Period Mean" = "period_mean"
              )
            ),

            shiny::numericInput("conv_factor", "单位换算系数 Conversion Factor", value = 86.4, min = 0, step = 0.1),

            shiny::actionButton("btn_compute", "计算通量 Calculate Flux", icon = shiny::icon("play"), class = "btn-primary btn-block"),

            shiny::tags$hr(),
            shiny::h5(shiny::icon("file-export"), " 导出 Export"),
            shiny::downloadButton("download_flux_daily", "下载通量结果 CSV", class = "btn-success btn-block"),

            shiny::tags$hr(),
            shiny::h5(shiny::icon("info-circle"), " 计算摘要 Summary"),
            shiny::verbatimTextOutput("txt_flux_summary")
          ),

          shinydashboard::box(
            width = 8,
            status = "info",
            solidHeader = TRUE,
            title = shiny::tagList(shiny::icon("table"), " 通量结果 Flux Results"),
            DT::dataTableOutput("tbl_flux_daily")
          )
        )
      ),

      # ====================== 4 监测方案设计与优化 ======================
      shinydashboard::tabItem(
        tabName = "design",

        shiny::fluidRow(
          shinydashboard::box(
            width = 4,
            status = "primary",
            solidHeader = TRUE,
            title = shiny::tagList(shiny::icon("drafting-compass"), " 设计参数 Design Parameters"),

            shiny::numericInput("design_n_mc", "蒙特卡洛迭代次数 MC Iterations", value = 1000, min = 100, max = 50000, step = 100),

            shiny::selectInput("design_intervals", "候选采样间隔 Candidate Intervals",
                               choices  = c("1h","2h","4h","6h","8h","12h","24h","48h","7d","14d","30d"),
                               multiple = TRUE,
                               selected = c("6h","12h","24h","7d")
            ),

            shiny::numericInput("design_target_re", "目标相对误差(%) Target RE(%)", value = 15, min = 1, max = 100, step = 1),

            shiny::actionButton("btn_run_design", "运行采样设计 Run Design", icon = shiny::icon("cogs"), class = "btn-primary btn-block"),

            shiny::tags$hr(),
            shiny::downloadButton("download_design_table", "下载方案表 CSV", class = "btn-success btn-block"),

            shiny::tags$hr(),
            shiny::h5(shiny::icon("trophy"), " 最佳方案 Best Scheme"),
            shiny::verbatimTextOutput("txt_design_best")
          ),

          shinydashboard::box(
            width = 8,
            status = "info",
            solidHeader = TRUE,
            title = shiny::tagList(shiny::icon("table"), " 方案对比 Scheme Comparison"),
            DT::dataTableOutput("tbl_design"),
            shiny::tags$hr(),
            shiny::h5(shiny::icon("chart-bar"), " 相对误差 vs 采样间隔 RE vs Interval"),
            shiny::plotOutput("plot_design", height = 300)
          )
        )
      ),

      # ====================== 5 报告编制与成果输出 ======================
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
            shiny::downloadButton("download_clean_data", "清洗后数据 CSV", class = "btn-info btn-block"),
            shiny::tags$br(),
            shiny::downloadButton("download_flux_result", "通量结果 CSV", class = "btn-info btn-block"),
            shiny::tags$br(),
            shiny::downloadButton("download_design_result", "方案结果 CSV", class = "btn-info btn-block"),
            shiny::tags$br(),
            shiny::downloadButton("download_report_plot", "图表汇总 PNG", class = "btn-info btn-block")
          ),

          shinydashboard::box(
            width = 6,
            status = "success",
            solidHeader = TRUE,
            title = shiny::tagList(shiny::icon("file-alt"), " 研究报告 Research Report"),

            shiny::tags$div(
              class = "bilingual-block",
              shiny::tags$p(class = "zh",
                            "基于当前分析结果自动生成标准化研究报告。"
              ),
              shiny::tags$p(class = "en",
                            "Automatically generate a standardized research report."
              )
            ),

            shiny::tags$hr(),

            shiny::selectInput("report_format", "报告格式 Report Format",
                               choices = c("HTML" = "html", "Word" = "docx", "PDF" = "pdf")
            ),

            shiny::downloadButton("download_report", "生成报告 Generate Report", class = "btn-success btn-block"),
            shiny::tags$hr(),
            shiny::verbatimTextOutput("txt_report_status")
          )
        )
      ),

      # ====================== 关于我们 ======================
      shinydashboard::tabItem(
        tabName = "about",

        shiny::fluidRow(
          shinydashboard::box(
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            title = shiny::tagList(shiny::icon("building"), " 研发团队 Development Team"),

            shiny::tags$div(
              class = "bilingual-block",
              shiny::tags$p(class = "zh",
                            shiny::tags$strong("通量宝"),
                            "由水利部交通运输部国家能源局南京水利科学研究院",
                            shiny::tags$strong("生态水文与水资源保护研究团队"),
                            "研发。"
              ),
              shiny::tags$p(class = "en",
                            shiny::tags$strong("FluxWatch"),
                            " is developed by the ",
                            shiny::tags$strong("Ecohydrology and Water Resources Protection Research Team"),
                            " from Nanjing Hydraulic Research Institute (NHRI)."
              )
            ),

            shiny::tags$hr(),

            shiny::fluidRow(
              shiny::column(6,
                            shiny::h5(shiny::icon("envelope"), " 联系方式 Contact"),
                            shiny::p("邮箱 Email: your_email@nhri.cn"),
                            shiny::p("地址 Address: 南京市广州路 223 号"),
                            shiny::p("网站: ", shiny::tags$a(href = "http://www.nhri.cn", target = "_blank", "www.nhri.cn"))
              ),
              shiny::column(6,
                            shiny::h5(shiny::icon("github"), " 开源地址 Repository"),
                            shiny::p(shiny::tags$a(href = "https://github.com/yourrepo/FluxWatch", target = "_blank", shiny::icon("github"), " GitHub")),
                            shiny::h5(shiny::icon("bug"), " 问题反馈 Bug Report"),
                            shiny::p(shiny::tags$a(href = "https://github.com/yourrepo/FluxWatch/issues", target = "_blank", "Submit Issue"))
              )
            ),

            shiny::tags$hr(),
            shiny::h5(shiny::icon("book"), " 参考文献 References"),
            shiny::tags$ol(
              shiny::tags$li("作者 (年份). 文章标题. 期刊名. DOI: ..."),
              shiny::tags$li("Author (Year). Title. Journal. DOI: ...")
            )
          )
        )
      ),

      # ====================== 免责声明 ======================
      shinydashboard::tabItem(
        tabName = "disclaimer",

        shiny::fluidRow(
          shinydashboard::box(
            width = 12,
            status = "warning",
            solidHeader = TRUE,
            title = shiny::tagList(shiny::icon("exclamation-triangle"), " 免责声明 Disclaimer"),

            shiny::tags$div(
              class = "disclaimer-text",
              shiny::h5("中文"),
              shiny::tags$p(
                "通量宝（FluxWatch）仅供科研与教学用途。",
                "开发者不对使用用户自有数据所产生的结果的准确性、",
                "完整性或适用性作任何保证。"
              ),
              shiny::tags$hr(),
              shiny::h5("English"),
              shiny::tags$p(
                "FluxWatch is provided for research and educational purposes only. ",
                "The developers make no warranty regarding the accuracy or fitness ",
                "of results produced using user-supplied data."
              ),
              shiny::tags$hr(),
              shiny::tags$p(
                shiny::tags$strong("版本 Version: "), "V 0.01",
                shiny::tags$br(),
                shiny::tags$strong("许可证 License: "), "MIT",
                shiny::tags$br(),
                shiny::tags$strong("发布日期 Release: "), format(Sys.Date(), "%Y-%m-%d")
              )
            )
          )
        )
      )

    ) # end tabItems
  )   # end dashboardBody
)
