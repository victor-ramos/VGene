library(shinyBS)
library(shinydashboard)
library(shinyjs)
library(V8)

jscode <- "shinyjs.reload = function() { window.location.reload(true); }"

ui = shiny::fluidPage(
    
    shinyjs::useShinyjs(),
    extendShinyjs(text = jscode, functions = c("reload")),
    theme = shinythemes::shinytheme("flatly"),
    
    shiny::navbarPage( title = "V Gene Usage",
                       
                       source("ui-tab-start.R",local=TRUE)$value,
                       source("ui-tab-execution.R",local=TRUE)$value
                       
    )
)