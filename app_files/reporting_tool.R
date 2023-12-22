library(shinydashboard)
library(bs4Dash)

surveys <- c(
    "Air Quality Survey", "Community Ideas Survey",
    "Story From the Community Survey", "Environmental Justice Survey",
    "Tree Canopy Survey", "Urban Heat Survey",
    "Urban Heat Map", "Air Quality Map", "Tree Canopy Map"
)

reporting_tool_body <- function() {
    # reactively display results box
    dashboard_body <- fluidRow(
        class = "container-row",
        style = "margin-top: 24px;",
        column(
            4,
            survey_box_ui(surveys)
        ),
        column(
            8,
            get_data_description_ui(),
            representative_ui(),
            survey_results_ui(),
            uiOutput("results")
        ),
    )
    return(dashboard_body)
}
