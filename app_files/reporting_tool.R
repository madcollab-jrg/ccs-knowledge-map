library(bs4Dash)
library(shinydashboard)

# surveys <- c(
#   "Air Quality Survey", "Community Ideas Survey",
#   "Story From the Community Survey", "Environmental Justice Survey",
#   "Tree Canopy Survey", "Urban Heat Survey",
#   "Urban Heat Map", "Air Quality Map", "Tree Canopy Map"
# )

surveys <- c(
  "Air Quality Survey",
  "Environmental Justice Survey",
  "Tree Canopy Survey", "Urban Heat Survey",
  "Urban Heat Map", "Air Quality Map", "Tree Canopy Map",
  "Environmental Justice Story", "Environmental Justice Report",
  "Tree Knowledge", "Carbon Concerns", "Energy Concerns",
  "General Survey", "Health Impacts"
)

reporting_tool_body <- function() {
  # browser()
  # print(demographic_data)
  # reactively display results box
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
      get_data_description_ui(),
      survey_results_ui(),
      uiOutput("results")
    ),
  )
  return(dashboard_body)
}
