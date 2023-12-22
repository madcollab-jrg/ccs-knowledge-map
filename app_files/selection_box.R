# Functionality for the selection box section of the knowledge graph
# which gives option to select survey, questions, representative metrics,
# and census.

# Given a survey name, return a list of questions related to that survey
# Args:
#   - survey: type string, the survey that is selected
# Returns: List of survey questions
get_survey_questions <- function(survey) {
    # if (survey == "Air Quality Survey"){
    #     air_quality = readLines(paste(getwd(),"/survey_questions/air_quality.txt", sep = ""))
    #     return( air_quality )
    # }else if (survey == "Environmental Justice Survey"){
    #     # environmental justice survey
    #     ej_survey = readLines(paste(getwd(),"/survey_questions/ej_survey.txt", sep = ""))
    #     return( ej_survey )
    # }else if (survey == "Tree Canopy Survey"){
    #     # tree canopy survey
    #     tree_canopy = readLines(paste(getwd(),"/survey_questions/tree_canopy.txt", sep = ""))
    #     return( tree_canopy )
    # }else if (survey == "Urban Heat Survey"){
    #     # urban heat survey
    #     urban_heat = readLines(paste(getwd(),"/survey_questions/urban_heat.txt", sep = ""))
    #     return( urban_heat )
    # } else if (survey == "Carbon Survey") {
    #     carbon = readLines(paste0(getwd(),"/survey_questions/carbon.txt"))
    #     return( carbon )
    # }

    if (grepl("survey", survey, ignore.case = T)) {
        file_name <- gsub(" ", "_", gsub(" survey", "", tolower(survey)))
        file_path <- paste0(getwd(), "/survey_questions/", file_name, ".txt")
        return(readLines(file_path))
    }
    return(c(""))
}

get_census_items <- function(census_item) {
    # "'Census State'", "'Census County'", "'Census Block'"
    survey_questions <- c() # place holder

    # TODO: probably save this locally or within app files
    # file_loc = "/Volumes/cbjackson2/ccs-knowledge/ccs-data/filter_inputs/census_values.csv"
    file_loc <- paste(getwd(), "/census_items/census_items.yaml", sep = "")
    possible_values <- read_yaml(file_loc)
    if (census_item == "'Census Tract'") {
        survey_questions <- possible_values$tract
    } else if (census_item == "'Census State'") {
        survey_questions <- c("Wisconsin")
    } else if (census_item == "'Census County'") {
        survey_questions <- possible_values$county
    } else if (census_item == "'Zipcode'") {
        survey_questions <- possible_values$zip
    }
    return(survey_questions)
}

make_conditional_panel_survey <- function(survey, id) {
    # make a conditional panel for the survey box ui that
    # gives provides different question for the different
    # survey responses
    #
    # Args:
    #   survey: the survey that is selected. expected to be "'name-of-survey'"
    #   all_surveys: list of all surveys
    #   id: id for the selectInput box created
    #
    # Returns:
    #   conditionalPanel that holds the questions for survey selected
    #   and contains a selectInput with inputId = id
    survey_conditional_panel <- conditionalPanel(
        condition = paste0("input.survey == '", survey, "'"),
        selectInput(
            inputId = id,
            label = div(style = "font-size:0.85rem;", "STEP 2: Select a question from the survey"),
            choices = get_survey_questions(survey),
            selectize = F
        )
    )

    return(survey_conditional_panel)
}

make_conditional_panel_census <- function(census_item, id) {
    census_conditional_panel <- conditionalPanel(
        condition = paste("input.census_level ==", census_item),
        selectInput(
            inputId = id,
            label = div(style = "font-size:0.85rem;", "STEP 4: Choose which data you would like to examine"),
            choices = get_census_items(census_item),
            selectize = F
        )
    )
}

survey_box_ui <- function(surveys) {
    # Construct and return survey box UI. This contains
    # Select Survey, Select Questions, Select Representativeness comparisons, select
    # Census, and an action button to run the survey.
    #
    # Returns:
    #   box with the required selectInput and conditionalPanels

    surveyui <- box(
        collapsible = FALSE,
        actionButton("Help", "Help", class = "button-common", style = "margin-bottom: 0.5rem;"),
        actionButton("Definitions", "Definitions", class = "button-common", style = "margin-bottom: 0.5rem;"),
        selectizeInput(
            inputId = "survey",
            label = div(
                style = "font-size: 0.85rem;",
                "STEP 1: Select a survey",
                HTML("<br/><span style='font-weight: 400; class='text-lighter''>You can review the list of surveys here:</span> <span style='font-weight: 700; text-decoration: underline;'>Dataset</span>")
            ),
            choices = surveys,
            options = list(
                placeholder = "Please select an option below",
                onInitialize = I('function() { this.setValue(""); }')
            )
        ),
        conditionalPanel(
            condition = "input.survey == ''",
            selectInput(
                inputId = "null",
                label = div(style = "font-size:0.85rem;", "STEP 2: Select a question from the survey"),
                choices = c("")
            )
        ),
        make_conditional_panel_survey("Air Quality Survey", "air_quality_qs"),
        make_conditional_panel_survey("Air Quality Map", "air_quality_map_qs"),
        make_conditional_panel_survey("Community Ideas Survey", "ej_report_qs"),
        make_conditional_panel_survey("Story From the Community Survey", "ej_storytile_qs"),
        make_conditional_panel_survey("Environmental Justice Survey", "ej_survey_qs"),
        make_conditional_panel_survey("Tree Canopy Survey", "tree_canopy_qs"),
        make_conditional_panel_survey("Tree Canopy Map", "tree_canopy_map_qs"),
        make_conditional_panel_survey("Urban Heat Survey", "urban_heat_qs"),
        make_conditional_panel_survey("Urban Heat Map", "urban_heat_map_qs"),
        make_conditional_panel_survey("Carbon Survey", "carbon_survey_qs"),
        make_conditional_panel_survey("Energy Survey", "energy_survey_qs"),
        make_conditional_panel_survey("General Survey", "general_survey_qs"),
        make_conditional_panel_survey("Heat Health Survey", "heat_health_survey_qs"),
        make_conditional_panel_survey("Trees Greenery Survey", "trees_greenery_survey_qs"),
        p(""),
        p(""),

        # use to compute representation
        selectizeInput(
            inputId = "census_level",
            # label = div(style = "font-size:0.85rem;", "STEP 3: Choose a geography to examine the data"),
            label = div(
                style = "font-size: 0.85rem;",
                "STEP 3: Choose a geography to examine the data",
                HTML("<br/><span style='font-weight: 400;' class='text-lighter'>Check the list of geographies here:</span> <span style='font-weight: 700; text-decoration: underline;'>How we analyse the data</span>")
            ),
            choices = c("Census Tract", "Census State", "Census County", "Zipcode"),
            options = list(
                placeholder = "Please select an option below",
                onInitialize = I('function() { this.setValue(""); }')
            )
        ),
        conditionalPanel(
            condition = "input.census_level == ''",
            selectInput(
                inputId = "null",
                # label = div(style = "font-size:0.85rem;", "STEP 4: Choose which data you would like to examine"),
                label = div(
                    style = "font-size: 0.85rem;",
                    "STEP 4: Choose which data you would like to examine",
                    HTML("<br/><span style='font-weight: 400;' class='text-lighter'>Check the list of available demographic descriptors and groupings</span>")
                ),
                choices = c("")
            )
        ),
        make_conditional_panel_census("'Census Tract'", "census_tract_items"),
        make_conditional_panel_census("'Census State'", "census_state_items"),
        make_conditional_panel_census("'Census County'", "census_county_items"),
        make_conditional_panel_census("'Zipcode'", "census_zipcode_items"),
        actionButton(inputId = "run_report", label = "Run Report", class = "button-common"),
        width = 12
    )
    return(surveyui)
}
