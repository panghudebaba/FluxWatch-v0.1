mod_about_ui <- function(id) {
  ns <- NS(id)

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
          shiny::tags$p(
            class = "zh",
            shiny::tags$strong("通量宝"),
            "由水利部交通运输部国家能源局南京水利科学研究院",
            shiny::tags$strong("生态水文与水资源保护研究团队"),
            "研发。"
          ),
          shiny::tags$p(
            class = "en",
            shiny::tags$strong("FluxWatch"),
            " is developed by the ",
            shiny::tags$strong("Ecohydrology and Water Resources Protection Research Team"),
            " from Nanjing Hydraulic Research Institute (NHRI)."
          )
        ),

        shiny::tags$hr(),

        shiny::fluidRow(
          shiny::column(
            6,
            shiny::h5(shiny::icon("envelope"), " 联系方式 Contact"),
            shiny::p("邮箱 Email: your_email@nhri.cn"),
            shiny::p("地址 Address: 南京市广州路 223 号"),
            shiny::p("网站: ", shiny::tags$a(href = "http://www.nhri.cn", target = "_blank", "www.nhri.cn"))
          ),
          shiny::column(
            6,
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
  )
}

mod_about_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {})
}
