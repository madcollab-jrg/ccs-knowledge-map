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
    if (survey == "Survey1"){
        return(c("Survey 1 Q1", "Survey 1 Q2"))
    }else if (survey == "Survey2") {
       return(c("Survey 2 Q1", "Survey 2 Q2", "Survey 2 Q3"))
    }else if (survey == "Survey3") {
       return(c("Survey 3 Q1"))
    }
}

survey_box_ui <- function(){
    # Construct and return survey box UI. This contains
    # Select Survey, Select Questions, Select Representativeness comparisons, select
    # Census, and an action button to run the survey. 
    #
    # Returns:
    #   box with the required selectInput and conditionalPanels
    surveyui <- box(
                selectizeInput(inputId = "survey",
                label = div(style = "font-size:10px", "Choose a survey"),
                choices = c("", "Survey1", "Survey2", "Survey3"),
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
                conditionalPanel(
                    condition = "input.survey == 'Survey1'",
                    selectInput(inputId = "s1questions",
                    label = div(style = "font-size:10px", "Choose a question"),
                    choices = get_survey_questions("Survey1"))
                ),
                conditionalPanel(
                    condition = "input.survey == 'Survey2'",
                    selectInput(inputId = "s2questions",
                    label = div(style = "font-size:10px", "Choose a question"),
                    choices = get_survey_questions("Survey2"))
                ),
                conditionalPanel(
                    condition = "input.survey == 'Survey3'",
                    selectInput(inputId = "s3questions",
                    label = div(style = "font-size:10px", "Choose a question"),
                    choices = get_survey_questions("Survey3"))
                ),
                p(""),
                p(""),
                selectInput(inputId = "rep_comp",
                            label = div(style = "font-size:10px","Representativeness Comparison"),
                            choices = c("Census track")),
                selectInput(inputId = "census_item",
                            label = div(style = "font-size:12px","Census Item"),
                            choices = c("Census Tract ...")
                            ),
                actionButton(inputId = "run_report", label = "Run Report"),
                width = 12
            )
    return(surveyui)
}