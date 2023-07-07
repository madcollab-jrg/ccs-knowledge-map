library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(tidyr)

source("selection_box.R")
source("data_description_box.R")
source("graphics_representative.R")

surveys = c("Air Quality Survey", "Community Ideas Survey", 
            "Story From the Community Survey", "Environmental Justice Survey", 
            "Tree Canopy Survey", "Urban Heat Survey", 
            "Urban Heat Map", "Air Quality Map","Tree Canopy Map")
surveyInputId = c("Air Quality Survey" = "air_quality_qs", 
                  "Community Ideas Survey"="ej_report_qs", 
                  "Story From the Community Survey"="ej_storytile_qs",
                  "Environmental Justice Survey"="ej_survey_qs", 
                  "Tree Canopy Survey"="tree_canopy_qs", 
                  "Urban Heat Survey"="urban_heat_qs",
                  "Air Quality Map" = "air_quality_map_qs",
                  "Tree Canopy Map" = "tree_canopy_map_qs",
                  "Urban Heat Map" = "urban_heat_map_qs")
input_to_data_demo = c("Air Quality Survey" = "air-quality-survey/",
                   "Community Ideas Survey" = "ej-report/",
                   "Story From the Community Survey" = "ej-storytile/",
                   "Environmental Justice Survey" = "ej-survey/", 
                   "Tree Canopy Survey" = "tree-canopy-survey/",
                   "Urban Heat Survey" = "urban-heat-survey/", 
                   "Urban Heat Map" = "urban-heat-map/",
                   "Air Quality Map" = "air-quality-map/",
                   "Tree Canopy Map" = "tree-canopy-map/") # map for survey to where the data is located for the file
input_to_data_survey = c("Air Quality Survey" = "air-quality/air_survey.csv",
                        "Community Ideas Survey" = "ej-report/ej_report.csv",
                        "Story From the Community Survey" = "ej-storytile/ej_story.csv",
                        "Environmental Justice Survey" = "ej-survey/ej_survey.csv", 
                        "Tree Canopy Survey" = "tree-canopy/tree_survey.csv",
                        "Urban Heat Survey" = "urban-heat/heat_survey.csv", 
                        "Urban Heat Map" = "urban-heat/heat_map.csv",
                        "Air Quality Map" = "air-quality/air_map.csv",
                        "Tree Canopy Map" = "tree-canopy/tree_map.csv")

get_dashboard_body = function(){
  dashboard_body = tabItem(tabName = "rep",
                            column(width = 2, survey_box_ui(surveys)),
                            column(width = 4, get_data_description_ui()),
                            column(width = 6, representative_ui() ),
                            column(width = 12, box(title = "Survey Information Place Holder", width = 12) )
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
  demographic_data_loc = "/Volumes/cbjackson2/ccs-knowledge/ccs-data-demographic_unprocessed/"
  survey_data_loc = "/Volumes/cbjackson2/ccs-knowledge/ccs-data/"
  
  # place holder for survey data loading - might need to change this to reactive to run report
  survey_data = eventReactive(
      list(input$run_report),
    { 
    req(input$survey)
    # import data here - reactive to input$survey
    name = input$survey
    read.csv(paste(survey_data_loc,input_to_data_survey[[input$survey]],sep=''))
    })
  
  census_data = reactive({
    input$run_report
    req(input$rep_comp)
    req(input$census_item)
    # import census data
  })
  
  # middle panel data description
  get_data_description_reaction(input, output, surveyInputId, survey_data )
  
  # Representation 
  
}

shinyApp(ui = ui, server = server)