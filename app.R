library(shiny)
library(shinydashboard)

source("global.R", local = TRUE)
source("ui.R", local = TRUE)
source("server.R", local = TRUE)

shinyApp(ui, server)
