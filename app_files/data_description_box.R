get_data_description_ui = function(){
  #
  #
  #
  data_description = box( title = "Data Description", 
                          p( textOutput(outputId = "survey_selection") ),
                          width = 12 )
  
  return(data_description)
}

get_data_description_reaction = function(input, output, surveys, surveyIds){
  #
  #
  #
  #
  reaction = observeEvent(input$run_report,
               {
                 
                 survey.selected = input[["survey"]] # id of the survey selection 
                 survey.selected.Id = surveyIds[survey.selected]
                 survey.selected.question = input[[survey.selected.Id]]
                 survey.selected.question.cut = gsub("^((\\w+\\W+){9}\\w+).*$","\\1",survey.selected.question)
                 output$survey_selection = renderText( sprintf("In the %s, for %s... there was %d reponses", survey.selected, survey.selected.question.cut, 0) )
               })
  return(reaction)
}