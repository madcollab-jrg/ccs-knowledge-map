library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
library(yaml)

source("selection_box.R")
source("data_description_box.R")
source("graphics_representative.R")
source("survey_results.R")
source("data_util.R")

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

has_results = c("Air Quality Survey" = T, 
                "Community Ideas Survey"=F, 
                "Story From the Community Survey"=F,
                "Environmental Justice Survey"=T, 
                "Tree Canopy Survey"=T, 
                "Urban Heat Survey"=T,
                "Air Quality Map" = F,
                "Tree Canopy Map" = F,
                "Urban Heat Map" = F)

censusInputId = c("Census Tract"="census_tract_items", 
                  "Census State"="census_state_items", 
                  "Census County"="census_county_items")
input_to_data_demo = c("Air Quality Survey" = "air-quality-survey",
                   "Community Ideas Survey" = "ej-report",
                   "Story From the Community Survey" = "ej-storytile",
                   "Environmental Justice Survey" = "ej-survey", 
                   "Tree Canopy Survey" = "tree-canopy-survey",
                   "Urban Heat Survey" = "urban-heat-survey", 
                   "Urban Heat Map" = "urban-heat-map",
                   "Air Quality Map" = "air-quality-map",
                   "Tree Canopy Map" = "tree-canopy-map") 
census_input_to_data = c("Census Tract"="tract", 
                       "Census State"="state", 
                       "Census County"="county")
census_level_input_to_data = read_yaml("census_items/census_level_to_results.yaml")
input_to_data_survey = c("Air Quality Survey" = "air-quality/air_survey.csv",
                        "Community Ideas Survey" = "ej-report/ej_report.csv",
                        "Story From the Community Survey" = "ej-storytile/ej_story.csv",
                        "Environmental Justice Survey" = "ej-survey/ej_survey.csv", 
                        "Tree Canopy Survey" = "tree-canopy/tree_survey.csv",
                        "Urban Heat Survey" = "urban-heat/heat_survey.csv", 
                        "Urban Heat Map" = "urban-heat/heat_map.csv",
                        "Air Quality Map" = "air-quality/air_map.csv",
                        "Tree Canopy Map" = "tree-canopy/tree_map.csv")
question_type_map = c()

get_dashboard_body = function(){
  # reactivly display results box
  dashboard_body = tabItem(tabName = "rep",
                            column(width = 2, survey_box_ui(surveys)),
                            column(width = 5, get_data_description_ui()),
                            column(width = 5, representative_ui() ),
                            column(width = 12, survey_results_ui(),tags$head(tags$style(HTML('.content-wrapper { overflow: auto; }'))))
                            #column(width = 12, uiOutput("results"),tags$head(tags$style(HTML('.content-wrapper { overflow: auto; }'))))
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
  
  # import survey data
  survey_data = eventReactive(
      list(input$run_report),
    { 
    req(input$survey)
    # import data here - reactive to input$survey
    name = input$survey
    survey_data = read.csv(paste(survey_data_loc,input_to_data_survey[[name]],sep=''))
    survey_data[,-1]
    })
  
  # import census data
  census_data = reactive({
    census_level = census_input_to_data[[input$census_level]]
    census_id = censusInputId[input$census_level]
  })
  
  # file for representativeness scores
  file_to_get = reactive({
    input$run_report
    if(input$survey != "" & input$census_level != ""){
      census_level = census_input_to_data[[input$census_level]]
      census_id = censusInputId[input$census_level]
      
      print(census_id)
      print(input[[census_id]])
      
      key = input[[census_id]]
      
      file = paste( input_to_data_demo[[input$survey]], census_level, census_level_input_to_data[["data"]][[census_level]][[ key ]], sep = "-")
      file_loc = paste(input_to_data_demo[[input$survey]],"/",file,".RData",sep="")
    }else{
      file = ""
    }
  })
  
  # get question column number + question type for survey results
  question_number = eventReactive(
    list(input$run_report),
    { 
      req(input$survey)
      surveyQid = surveyInputId[[input$survey]]
      question = input[[surveyQid]]
      as.integer(str_extract(question, regex("[0-9]+")))#+3
    })
  
  # get question type for graphics display
  question_type = eventReactive(
    list(input$run_report),
    { 
      req(input$survey)
      q_num = question_number()
      get_question_type(input$survey, q_num)
    })
  
  # question subtype - needed for matrix type questions
  question_subtype = eventReactive(
    list(input$run_report),
    { 
      req(input$survey)
      q_num = question_number()
      get_question_subtype(input$survey, q_num)
    })
  
  # is something a survey - For non-survey they haven't decided on graphical displays
  is_survey = eventReactive(list(input$run_report),
                            {
                              req(input$survey)
                              has_results[[input$survey]]
                            })
  
  # middle panel data description
  get_data_description_reaction(input, output, surveyInputId, survey_data, census_data, file_loc = file_to_get)
  
  # Representation 
  get_representative_reactive(input, output, file_to_get)

  # results graphics
  resulting_graphics(input, output, survey_data, is_survey, question_number, question_type, question_subtype)
  
}

# run app
shinyApp(ui = ui, server = server)


