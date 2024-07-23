library(bs4Dash)
library(shinydashboard)

surveys <- c(
  "Air Quality Survey",
  "Environmental Justice Survey",
  "Tree Canopy Survey", "Urban Heat Survey",
  "Urban Heat Map", "Air Quality Map", "Tree Canopy Map",
  "Environmental Justice Story", "Environmental Justice Report",
  "Tree Knowledge", "Carbon Concerns", "Energy Concerns",
  "General Survey", "Health Impacts"
)

reporting_tool_body <- function(
    survey, demographic, geography,
    demo, demog, surveyQues) {
  dashboard_body <- fluidRow(
    useShinyjs(),
    class = "container-row",
    style = "margin-top: 24px;",
    column(
      class = "survey-box-wrapper",
      4,
      survey_box_ui(surveys)
    ),
    column(
      8,
      get_data_description_ui(survey, demographic, geography, demo),
      survey_results_ui(demog, surveyQues),
      uiOutput("results")
    ),
  )
  return(dashboard_body)
}
