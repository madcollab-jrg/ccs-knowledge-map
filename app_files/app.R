library(shinydashboard)
library(ggplot2)

source("selection_box.R")
source("data_description_box.R")
source("graphics_representative.R")

surveys = c("Air Quality Survey", "Community Ideas Survey", 
            "Story From the Community Survey", "Environmental Justice Survey", 
            "Tree Canopy Survey", "Urban Heat Survey")
surveyInputId = c("Air Quality Survey" = "air_quality_qs", 
                  "Community Ideas Survey"="ej_report_qs", 
                  "Story From the Community Survey"="ej_storytile_qs",
                  "Environmental Justice Survey"="ej_survey_qs", 
                  "Tree Canopy Survey"="tree_canopy_qs", 
                  "Urban Heat Survey"="urban_head_qs")

get_dashboard_body = function(){
  dashboard_body = tabItem(tabName = "rep",
                            column(width = 2, survey_box_ui(surveys)),
                            column(width = 4, get_data_description_ui()),
                            column(width = 6, representative_ui() )
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
  skin = "blue"
)

server <- function(input, output){
  
  # middle panel data description
  get_data_description_reaction(input, output, surveys, surveyInputId)
  
  observeEvent(input$run_report, 
               {
                 g1 = ggplot(mtcars, aes(x=cyl)) + 
                   geom_histogram()
                 output$data = renderPlot({ g1 })
               })
}

shinyApp(ui = ui, server = server)