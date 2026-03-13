mod_welcome_ui <- function(id) {
  ns <- shiny::NS(id)

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
          shiny::tags$p(
            class = "zh",
            "从事水文水资源和水生态水环境研究的研究生、学者和研究人员。"
          ),
          shiny::tags$p(
            class = "en",
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
  )
}

mod_welcome_server <- function(id, rv) {
  shiny::moduleServer(id, function(input, output, session) {
  })
}
