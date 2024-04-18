library(bs4Dash)
library(shinydashboard)


source("pages/components/selection_box.R")
# source("pages/components/representative_table.R")
source("pages/components/data_desc.R")

surveys <- c(
  "Air Quality Survey",
  "Environmental Justice Survey"
)

reporting_tool_body <- function() {
  # reactively display results box
  dashboard_body <- fluidRow(
    class = "container-row",
    style = "margin-top: 24px;",
    column(
      class = "survey-box-wrapper",
      4,
      survey_box_ui(surveys)
    ),
    column(
      8,
      get_data_description_ui()
    ),
  )
  return(dashboard_body)
}
