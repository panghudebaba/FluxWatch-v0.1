app_ui <- function() {
  shinydashboard::dashboardPage(
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
        mod_welcome_ui("welcome"),
        mod_ingest_ui("ingest"),
        mod_viz_ui("viz"),
        mod_flux_ui("flux"),
        mod_design_ui("design"),
        mod_report_ui("report"),
        mod_about_ui("about"),
        mod_disclaimer_ui("disclaimer")
      )
    )
  )
}
