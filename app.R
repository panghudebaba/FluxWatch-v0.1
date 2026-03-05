library(shiny)
library(shinydashboard)

# 直接加载 ui.R 和 server.R，而不需要再 source ui.R
source("global.R", local = TRUE)
source("server.R", local = TRUE)

# ui 应该在此引用 ui.R 中的对象
shinyApp(ui = ui, server = server)


