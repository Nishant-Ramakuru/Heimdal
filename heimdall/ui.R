library(shiny)
library(shinydashboard)

shinyUI( 
  dashboardPage(
    skin = "blue",
    dashboardHeader(
      disable = T,
      title = "Heimdall",
      dropdownMenu(type = "messages"),
      dropdownMenu(type = "notifications"),
      dropdownMenu(type = "tasks")
      ),
      dashboardSidebar(uiOutput("side"),disable=T),
      dashboardBody(
        shinyjs::useShinyjs(),
        uiOutput("page")),
      singleton(
        tags$head(tags$script('Shiny.addCustomMessageHandler("msg",
                              function(message) {
                              alert(message.text);
                              }
        );'))
        )
      
    )
  )
  
