mod_disclaimer_ui <- function(id) {
  ns <- NS(id)

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
}

mod_disclaimer_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {})
}
