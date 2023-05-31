library(shiny)
library(shinydashboard)
library(ggplot2)

source("selection_box.R")

ui <- dashboardPage(
  header = dashboardHeader(title = "CCS Knowledge Map Report"),
  sidebar = dashboardSidebar(disable = TRUE),
  body = dashboardBody( survey_box_ui("survey_selection"), box(plotOutput("data")) ),
  skin = "purple"
)

server <- function(input, output){
  g1 = ggplot(mtcars, aes(x=cyl)) + 
    geom_histogram()
  output$data = renderPlot({ g1 })
}

shinyApp(ui = ui, server = server)