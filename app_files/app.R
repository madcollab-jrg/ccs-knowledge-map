library(shinydashboard)
library(ggplot2)

source("selection_box.R")
source("data_description_box.R")
#source("graphics.R")

get_dashboard_body = function(){
  dashboard_body = tabItem(tabName = "rep",
                            column(width = 2, survey_box_ui()),
                            column(width = 4, get_data_description_ui()),
                            column(width = 6, box(title = "", plotOutput("data"), width = 12) )
    )
  return(dashboard_body)
}

ui <- dashboardPage(
  header = dashboardHeader(title = "CCS Knowledge Map Report", titleWidth = "calc(100% - 44px)"),
  sidebar = dashboardSidebar(sidebarMenu(id = "tabs",
                                         menuItem(
                                           "Representative Summary",
                                           tabName = "rep"),
                                         menuItem(
                                           "Result Summary",
                                           tabName = "result_summary"
                                         ) )),
  body = dashboardBody( tabItems( get_dashboard_body(), tabItem(tabName = "result_summary") ) ),
  skin = "purple"
)

server <- function(input, output){
  
  get_data_description_reaction(input, output)
  
  observeEvent(input$run_report, 
               {
                 g1 = ggplot(mtcars, aes(x=cyl)) + 
                   geom_histogram()
                 output$data = renderPlot({ g1 })
               })
}

shinyApp(ui = ui, server = server)