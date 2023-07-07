# Functionality for the selection box section of the knowledge graph
# which gives option to select survey, questions, representative metrics,
# and census.

get_survey_questions <- function(survey){
    # Given a survey name, return a list of questions related to that 
    # survey
    # Args:
    #   survey: type string, the survey that is selected
    #   
    # Returns:
    #   List of survey questions
    survey = gsub("'","",survey)
    if (survey == "Air Quality Survey"){
        air_quality = readLines(paste(getwd(),"/survey_questions/air_quality.txt", sep = ""))
        return( air_quality )
    }else if (survey == "Environmental Justice Survey"){
        # environmental justice survey
        ej_survey = readLines(paste(getwd(),"/survey_questions/ej_survey.txt", sep = ""))
        return( ej_survey )
    }else if (survey == "Tree Canopy Survey"){
        # tree canopy survey
        tree_canopy = readLines(paste(getwd(),"/survey_questions/tree_canopy.txt", sep = ""))
        return( tree_canopy )
    }else if (survey == "Urban Heat Survey"){
        # urban heat survey
        urban_head = readLines(paste(getwd(),"/survey_questions/urban_heat.txt", sep = ""))
        return( urban_head )
    }
    return(c(""))
}

get_census_items = function(census_item){
    # "'Census State'", "'Census County'", "'Census Block'"
    survey_questions = c() # place holder
    
    # TODO: probably save this locally or within app files
    file_loc = "/Volumes/cbjackson2/ccs-knowledge/ccs-data/filter_inputs/census_values.csv"
    possible_values = read.csv(file_loc)
    if(census_item == "'Census Track'"){
        values = unique(possible_values$Location)
        survey_questions = values[!is.na(values)]
    }else if(census_item == "'Census State'"){
        # TODO: is there anything else to put here
        survey_questions = c("Wisconsin")
    }else if(census_item == "'Census County'"){
        values = unique(possible_values$COUNTY)
        survey_questions = values[!is.na(values)]
    }else if(census_item == "'Census Block'"){
        # TODO: what to do here
        values = unique(possible_values$census_block_full_name)
        survey_questions = values[!is.na(values)]
    }
    return(survey_questions)
}

make_conditional_panel_survey <- function(survey, all_surveys, id){
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
    survey_conditional_panel = conditionalPanel(
        condition = paste("input.survey ==", survey),
        selectInput(inputId = id,
                    label = div(style = "font-size:10px", "Choose a question"),
                    choices = get_survey_questions(survey),
                    selectize = F)
    )
    
    return(survey_conditional_panel)
}

make_conditional_panel_census = function(census_item, id){
    census_conditional_panel = conditionalPanel(
        condition = paste("input.census_level ==", census_item),
        selectInput(inputId = id,
                    label = div(style = "font-size:10px", "Census Item"),
                    choices = get_census_items(census_item),
                    selectize = F)
    )
}

survey_box_ui <- function(surveys){
    # Construct and return survey box UI. This contains
    # Select Survey, Select Questions, Select Representativeness comparisons, select
    # Census, and an action button to run the survey. 
    #
    # Returns:
    #   box with the required selectInput and conditionalPanels
    
    surveyui <- box(
                selectizeInput(inputId = "survey",
                label = div(style = "font-size:10px", "Choose a survey"),
                choices = surveys,
                options = list(
                    placeholder = 'Please select an option below',
                    onInitialize = I('function() { this.setValue(""); }')
                )),
                conditionalPanel(
                    condition = "input.survey == ''",
                    selectInput(inputId = "null",
                                label = div(style = "font-size:10px", "Choose a question"),
                                choices = c(""))
                    ),
                make_conditional_panel_survey("'Air Quality Survey'", surveys, "air_quality_qs"),
                make_conditional_panel_survey("'Air Quality Map'", surveys, "air_quality_map_qs"),
                make_conditional_panel_survey("'Community Ideas Survey'", surveys, "ej_report_qs"),
                make_conditional_panel_survey("'Story From the Community Survey'", surveys, "ej_storytile_qs"),
                make_conditional_panel_survey("'Environmental Justice Survey'", surveys, "ej_survey_qs"),
                make_conditional_panel_survey("'Tree Canopy Survey'", surveys, "tree_canopy_qs"),
                make_conditional_panel_survey("'Tree Canopy Map'", surveys, "tree_canopy_map_qs"),
                make_conditional_panel_survey("'Urban Heat Survey'", surveys, "urban_heat_qs"),
                make_conditional_panel_survey("'Urban Heat Map'", surveys, "urban_heat_map_qs"),
                p(""),
                p(""),
                
                # use to compute representation
                selectizeInput(inputId = "census_level",
                               label = div(style = "font-size:10px", "Representativeness Comparison Level"),
                               choices = c("Census Track", "Census State", "Census County"),
                               options = list(
                                   placeholder = 'Please select an option below',
                                   onInitialize = I('function() { this.setValue(""); }')
                               )),
                conditionalPanel(
                    condition = "input.census_level == ''",
                    selectInput(inputId = "null",
                                label = div(style = "font-size:10px", "Census Item"),
                                choices = c(""))
                ),
                make_conditional_panel_census("'Census Track'", "census_track_items"),
                make_conditional_panel_census("'Census State'", "census_state_items"),
                make_conditional_panel_census("'Census County'", "census_county_items"),
                actionButton(inputId = "run_report", label = "Run Report"),
                width = 12
            )
    return(surveyui)
}