library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
library(yaml)
library(shinyjs)

source("selection_box.R")
source("graphics_representative.R")
source("solving-data-box.R")
source("survey_results.R")
source("data_util.R")
source("reporting_tool.R")

source("pages/home_page.R")
source("pages/avail_data.R")
source("pages/about.R")
source("pages/info_page.R")
source("pages/strategies_page.R")

# source("pages/components/selection_box.R")

surveyInputId <- c(
  "Air Quality Survey" = "air_quality_qs",
  "Environmental Justice Survey" = "ej_survey_qs",
  "Tree Canopy Survey" = "tree_canopy_qs",
  "Urban Heat Survey" = "urban_heat_qs",
  "Air Quality Map" = "air_quality_map_qs",
  "Tree Canopy Map" = "tree_canopy_map_qs",
  "Urban Heat Map" = "urban_heat_map_qs",
  "Carbon Concerns" = "carbon_survey_qs",
  "Carbon Survey" = "carbon_survey_qs",
  "Energy Survey" = "energy_survey_qs",
  "General Survey" = "general_survey_qs",
  "Heat Health Survey" = "heat_health_survey_qs",
  "Trees Greenery Survey" = "trees_greenery_survey_qs",
  "Tree Knowledge" = "tree_knowledge_qs",
  "Energy Concerns" = "energy_concerns_qs",
  "General Survey" = "general_survey_qs",
  "Health Impacts" = "health_impacts_qs"
)

has_results <- c(
  "Air Quality Survey" = TRUE,
  "Environmental Justice Survey" = TRUE, "Tree Canopy Survey" = TRUE,
  "Urban Heat Survey" = TRUE,
  "Air Quality Map" = TRUE, "Tree Canopy Map" = TRUE, "Urban Heat Map" = FALSE,
  "Carbon Survey" = TRUE,
  "Carbon Concerns" = TRUE,
  "Tree Knowledge" = TRUE,
  "Energy Concerns" = TRUE,
  "Health Impacts" = TRUE,
  "Energy Survey" = TRUE, "General Survey" = TRUE,
  "Heat Health Survey" = TRUE,
  "Trees Greenery Survey" = TRUE
)

censusInputId <- c(
  "Census Tract" = "census_tract_items",
  "Census State" = "census_state_items",
  "Census County" = "census_county_items",
  "Zipcode" = "census_zipcode_items",
  "State Lower" = "census_state_lower_items",
  "State Upper" = "census_state_upper_items",
  "Congress" = "census_congress_items"
)
input_to_data_demo <- c(
  "Air Quality Survey" = "air-quality-survey",
  "Environmental Justice Survey" = "ej-survey",
  "Tree Canopy Survey" = "tree-canopy-survey",
  "Urban Heat Survey" = "urban-heat-survey",
  "Urban Heat Map" = "urban-heat-map",
  "Air Quality Map" = "air-quality-map",
  "Tree Canopy Map" = "tree-canopy-map",
  "Carbon Concerns" = "carbon-concerns",
  "Tree Knowledge" = "tree-knowledge",
  "Energy Concerns" = "energy-concerns",
  "General Survey" = "general-survey",
  "Health Impacts" = "health-impacts",
  # "Carbon Survey" = "carbon_survey",
  # "Energy Survey" = "energy_survey",
  # "General Survey" = "general_survey",
  # "Heat Health Survey" = "heat_health_survey",
  "Trees Greenery Survey" = "trees_greenery_survey"
)
census_input_to_data <- c(
  "Census Tract" = "tract",
  "Census State" = "state",
  "Census County" = "county",
  "Zipcode" = "zipcode",
  "State Lower" = "state_lower",
  "State Upper" = "state_upper",
  "Congress" = "congress"
)
census_level_input_to_data <-
  read_yaml("census_items/census_level_to_results.yaml")

input_to_data_survey_desc <- c(
  "Air Quality Map" = "air_map",
  "Air Quality Survey" = "air_survey",
  "Carbon Concerns" = "carbon_concerns",
  "Environmental Justice Report" = "ej_report",
  "Environmental Justice Story" = "ej_story",
  "Environmental Justice Survey" = "ej_survey",
  "Energy Concerns" = "energy_concerns",
  "General Survey" = "general_survey",
  "Health Impacts" = "health_impacts",
  "Urban Heat Survey" = "heat_survey",
  "Urban Heat Map" = "heat_map",
  "Heat Map" = "heat_map",
  "Heat Survey" = "heat_survey",
  "Tree Knowledge" = "tree_knowledge",
  "Tree Map" = "tree_map",
  "Tree Survey" = "tree_survey",
  "Tree Canopy Survey" = "tree_survey"
)

question_type_map <- c()

demographic_desc_to_data <- c(
  "Age" = "age",
  "Gender" = "gender",
  "Income" = "income",
  "Education" = "education",
  "Race" = "race"
)

ui <- dashboardPage(
  header = dashboardHeader(
    title = tags$a(href = "/", tags$img(
      src = "assets/logo.png", alt = "Logo", height = "50px",
      style = "margin-right: 5px"
    )),
    navbarMenu(
      id = "navmenu",
      navbarTab(tabName = "home_page", text = "Home"),
      navbarTab(tabName = "avail_data", text = "Available Data"),
      navbarTab(tabName = "reporting_tool", text = "Reporting Tool"),
      navbarTab(tabName = "about", text = "About"),
      navbarTab(tabName = "info_page", text = "Info"),
      navbarTab(tabName = "strategies_page", text = "Strategies")
    )
  ),
  sidebar = dashboardSidebar(
    disable = TRUE
  ),
  body = dashboardBody(
    navbarMenu(
      id = "navmenu",
      navbarTab(tabName = "home_page", text = "Home"),
      navbarTab(tabName = "avail_data", text = "Available Data"),
      navbarTab(tabName = "reporting_tool", text = "Reporting Tool"),
      navbarTab(tabName = "about", text = "About"),
      navbarTab(tabName = "info_page", text = "Info"),
      navbarTab(tabName = "strategies_page", text = "Info 3")
    ),
    useShinyjs(), # Initialize shinyjs
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "home.css"),
      tags$link(rel = "stylesheet", type = "text/css", href = "common.css"),
      tags$link(rel = "stylesheet", type = "text/css", href = "overrides.css"),
      tags$style(
        HTML(
          "
            @import url('https://fonts.googleapis.com/css2?family=Inter:wght@100;200;300;400;500;600;700;800;900&display=swap');
            body {
              font-family: 'Inter', sans-serif;
            }
          "
        )
      ),
      tags$script(defer = TRUE, type = "text/javascript", src = "main.js")
    ),
    tabItems(
      tabItem(tabName = "home_page", home_tab_body()),
      tabItem(tabName = "avail_data", avail_data_tab_body()),
      tabItem(
        tabName = "reporting_tool",
        reporting_tool_body(
          textOutput("survey"), textOutput("demographic"),
          textOutput("census_level"), textOutput("demo"),
          textOutput("demog"), textOutput("surveyQues")
        )
      ),
      tabItem(tabName = "about", about_tab_body()),
      tabItem(tabName = "info_page", info_tab_body()),
      tabItem(tabName = "strategies_page", strategies_data_tab_body())
    ),
    # Footer content
    tags$footer(
      class = "footer",
      "© 2024 CCS Knowledge Map. All rights reserved."
    )
  ),
  skin = "blue"
)

server <- function(input, output, session) {
  # Function to update the selected survey, demo, level
  output$survey <- renderText({
    paste(input$survey)
  })

  output$surveyQues <- renderText({
    paste(input$survey)
  })

  output$demographic <- renderText({
    paste(input$demographic)
  })

  output$demo <- renderText({
    paste(input$demographic)
  })

  output$demog <- renderText({
    paste(input$demographic)
  })

  output$census_level <- renderText({
    paste(input$census_level)
  })

  demographic_data_loc <-
    "/Volumes/cbjackson2/ccs-knowledge/ccs-data-demographic_unprocessed/"
  # survey_data_loc <- "/Volumes/cbjackson2/ccs-knowledge/ccs-data/"

  survey_data_loc <- "/Volumes/cbjackson2/ccs-knowledge/ccs-data-updated/"

  # import survey data
  survey_data <- eventReactive(
    list(input$run_report),
    {
      req(input$survey)
      req(input$demographic)
      # import data here - reactive to input$survey
      name <- input$survey
      # survey_data <-
      #   read.csv(paste(survey_data_loc,
      # input_to_data_survey[[name]], sep = ""))
      survey_data <-
        read.csv(paste(survey_data_loc, input_to_data_survey_desc[[name]], "/",
          input_to_data_survey_desc[[name]], ".csv",
          sep = ""
        ))
      # print(paste(survey_data_loc, input_to_data_survey_desc[[name]], "/",
      #   input_to_data_survey_desc[[name]], ".csv",
      #   sep = ""
      # ))
      survey_data[, -1]
    }
  )

  # import census data
  census_data <- reactive({
    census_level <- census_input_to_data[[input$census_level]]
    census_id <- censusInputId[input$census_level]
  })

  # import demographic data
  demographic_data <- reactive({
    demographic_id <- demographic_desc_to_data[[input$demographic]]
    return(demographic_id)
  })

  # file for representativeness scores
  file_to_get <- reactive({
    input$run_report
    if (input$survey != "" & input$census_level != "" &
      input$demographic != "") {
      census_level <- census_input_to_data[[input$census_level]]
      census_id <- censusInputId[input$census_level]

      # print(census_id)
      # print(input[[census_id]])

      key <- input[[census_id]]

      file <- paste(input_to_data_demo[[input$survey]], census_level,
        census_level_input_to_data[["data"]][[census_level]][[key]],
        sep = "-"
      )
      file_loc <- paste(input_to_data_demo[[input$survey]],
        "/", file, ".RData",
        sep = ""
      )
    } else {
      file <- ""
    }
  })

  file_to_get_sum <- reactive({
    req(input$run_report)
    if (input$survey != "" & input$census_level != "" &
      input$demographic != "") {
      census_level <- census_input_to_data[[input$census_level]]
      census_id <- censusInputId[input$census_level]

      key <- input[[census_id]]

      file <- paste(input_to_data_survey_desc[[input$survey]], census_level,
        census_level_input_to_data[["data"]][[census_level]][[key]],
        sep = "-"
      )
      file_sum <- paste(input_to_data_survey_desc[[input$survey]],
        "/", file, ".RData",
        sep = ""
      )
    } else {
      file <- ""
    }
  })


  # get question column number + question type for survey results
  question_number <- eventReactive(
    list(input$run_report),
    {
      req(input$survey)
      req(input$demographic)
      surveyQid <- surveyInputId[[input$survey]]
      question <- input[[surveyQid]]
      as.integer(str_extract(question, regex("[0-9]+"))) #+3
    }
  )

  # get question type for graphics display
  question_type <- eventReactive(
    list(input$run_report),
    {
      req(input$survey)
      req(input$demographic)
      q_num <- question_number()
      print(q_num)
      get_question_type(input$survey, q_num)
    }
  )

  # question subtype - needed for matrix type questions
  question_subtype <- eventReactive(
    list(input$run_report),
    {
      req(input$survey)
      req(input$demographic)
      q_num <- question_number()
      get_question_subtype(input$survey, q_num)
    }
  )

  # is something a survey - For non-survey they haven't
  #  decided on graphical displays
  is_survey <- eventReactive(list(input$run_report), {
    req(input$survey)
    req(input$demographic)
    has_results[[input$survey]]
  })

  # # middle panel data description
  # get_data_description_reaction(input, output, surveyInputId,
  #   survey_data, census_data,
  #   file_loc = file_to_get
  # )

  get_data_desc_rep_reaction(input, output, surveyInputId,
    survey_data, census_data,
    file_loc = file_to_get,
    file_sum = file_to_get_sum,
    demographic_desc = demographic_data()
  )

  # Representation
  # get_representative_reactive(input, output, file_to_get)

  # results graphics

  # print(is_survey)

  resulting_graphics(
    input, output, survey_data, is_survey,
    question_number, question_type, question_subtype,
    demographic_desc = demographic_data()
  )

  # all button and action link interaction on UI
  observeEvent(input$availDataBtn, {
    updateTabItems(session, "navmenu", "avail_data")
  })
  observeEvent(input$helpBtn, {
    updateTabItems(session, "navmenu", "about")
  })
  observeEvent(input$definitionsBtn, {
    updateTabItems(session, "navmenu", "avail_data")
  })
  observeEvent(input$datasetEle, {
    updateTabItems(session, "navmenu", "avail_data")
  })
  observeEvent(input$howWeAnalEle, {
    updateTabItems(session, "navmenu", "info_page")
  })
  observeEvent(input$curatedData, {
    updateTabItems(session, "navmenu", "avail_data")
  })
  observeEvent(input$strategies, {
    updateTabItems(session, "navmenu", "strategies_page")
  })
  shinyjs::enable("run_report")
}

# run app
shinyApp(ui = ui, server = server)
