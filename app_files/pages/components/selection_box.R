library(bs4Dash)
library(shinydashboard)

# Load survey questions from YAML file
survey_questions <-
  yaml::read_yaml("pages/components/data/survey_questions.yaml")

census_items <-
  yaml::read_yaml("pages/components/data/census_items.yaml")

survey_box_ui <- function(surveys) {
  surveyui <- box(
    class = "survey-box",
    collapsible = FALSE,
    width = 12,
    actionButton("helpBtn",
      "Help",
      class = "button-common",
      style = "margin-bottom: 0.5rem;"
    ),
    actionButton("definitionsBtn",
      "Definitions",
      class = "button-common",
      style = "margin-bottom: 0.5rem;"
    ),
    p("STEP 1: Select a survey",
      class = "label",
      style = "margin-top: 0.5rem;"
    ),
    span("You can review the list of surveys here:", class = "label-desc"),
    actionLink("datasetEle", "Dataset", class = "label-link font-sm"),
    selectInput("surveyName", "Select Survey",
      choices = names(survey_questions$surveys)
    ),
    p("STEP 2: Select a question from the survey", class = "label"),
    uiOutput("surveyQuestion"),
    p("STEP 3: Choose a geography to examine the data", class = "label"),
    span("Check the list of geographies here: ", class = "label-desc"),
    actionLink("howWeAnalEle",
      "How we aggregate the data based on geography unit",
      class = "label-link font-sm"
    ),
    selectInput("censusLevel", "Select Census Level",
      choices = names(census_items$census_level)
    ),
    uiOutput("demographyLevel"),
    p("STEP 4: Choose which demography you want to examine",
      class = "label"
    ),
    HTML("<div class='label-desc'>
            Check the list of available demographic descriptors and groupings
            </div>"),
    selectInput("demographic", "Select Demography",
      choices = c("Age", "Gender", "Education", "Income")
    ),
    actionButton(
      inputId = "run_report", label = "Run Report",
      class = "button-common"
    )
  )

  return(surveyui)
}

render_selection_boxes <- function(input, output, session) {
  # Dynamically generate survey question dropdown based on selected survey name
  output$surveyQuestion <- renderUI({
    survey_name <- input$surveyName
    survey_questions <- survey_questions$surveys[[survey_name]]$questions
    selectInput("surveyQuestion", "Select Question", choices = survey_questions)
  })

  output$demographyLevel <- renderUI({
    census_label <- input$censusLevel
    census_items <- census_items$census_level[[census_label]]
    selectInput("demographyLevel", "Select Level", choices = census_items)
  })
}
