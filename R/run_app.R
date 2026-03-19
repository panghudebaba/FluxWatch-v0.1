run_app <- function(...) {
  options(shiny.maxRequestSize = 100 * 1024^2)

  shiny::shinyApp(
    ui = app_ui(),
    server = app_server
  )
}
