# library(shiny)
# library(shinydashboard)
#
# # 直接加载 ui.R 和 server.R，而不需要再 source ui.R
# source("global.R", local = TRUE)
# source("server.R", local = TRUE)
# source("ui.R", local = TRUE)
#
# # 设置为 Pro 版
#
# # Sys.setenv(FLUXWATCH_PLAN = "pro")
#
# # 然后启动 Shiny 应用
# shiny::shinyApp(ui = ui, server = server)
#
#
# # ui 应该在此引用 ui.R 中的对象
# # shinyApp(ui = ui, server = server)
#
#
# # shinyApp(ui,server)
# shiny::runApp("E:/shinyapp/FluxWatch", launch.browser = TRUE)
# #
#
#
# library(shiny)
# library(shinydashboard)
# library(DT)
#
# # 自动加载 R/ 下所有脚本
r_files <- list.files("R", pattern = "\\.R$", full.names = TRUE)
invisible(lapply(r_files, source, encoding = "UTF-8"))

shinyApp(
  ui = app_ui(),
  server = app_server
)
