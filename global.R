# 调用包以及函数


library(shiny)
library(shinydashboard)


# 直接 source 所有需要的文件，只加载一次
source("global.R", local = TRUE)
source("ui.R", local = TRUE)
source("server.R", local = TRUE)


# 调用 shinyApp，只一次
shiny::shinyApp(ui = ui, server = server)

