library(shinydashboard)
library(shiny)

get_survey_questions <- function(survey){
    if (survey == "Survey1"){
        return(c("Survey 1 Q1", "Survey 1 Q2"))
    }else if (survey == "Survey2") {
       return(c("Survey 2 Q1", "Survey 2 Q2", "Survey 2 Q3"))
    }else if (survey == "Survey3") {
       return(c("Survey 3 Q1"))
    }
}

survey_box_ui <- function(id){
    surveyui <- box(
                selectizeInput(inputId = "survey",
                label = "Choose a survey",
                choices = c("", "Survey1", "Survey2", "Survey3"),
                options = list(
                    placeholder = 'Please select an option below',
                    onInitialize = I('function() { this.setValue(""); }')
                )),
                conditionalPanel(
                    condition = "input.survey == ''",
                    selectInput(inputId = "null",
                                label = "Choose a question",
                                choices = c(""))
                    ),
                conditionalPanel(
                    condition = "input.survey == 'Survey1'",
                    selectInput(inputId = "survey1questions",
                    label = "Choose a question",
                    choices = get_survey_questions("Survey1"))
                ),
                conditionalPanel(
                    condition = "input.survey == 'Survey2'",
                    selectInput(inputId = "survey2questions",
                    label = "Choose a question",
                    choices = get_survey_questions("Survey2"))
                ),
                conditionalPanel(
                    condition = "input.survey == 'Survey3'",
                    selectInput(inputId = "survey3questions",
                    label = "Choose a question",
                    choices = get_survey_questions("Survey3"))
                ),
                p(""),
                p(""),
                selectInput(inputId = "rep_comp",
                            label = "Representativeness Comparison",
                            choices = c("Census track")),
                selectInput(inputId = "census_item",
                            label = "Census Item",
                            choices = c("Census Tract ..."))
            )
    return(surveyui)
}