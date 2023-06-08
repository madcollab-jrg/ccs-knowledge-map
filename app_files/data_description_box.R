get_data_description_ui = function(){
  # Data description UI. 
  #
  # Return:
  #   box containing the necessary components of 
  #   the data description UI. See get_data_description_reaction()
  #   for the components.
  data_description = box( title = "Data Description", 
                          p( textOutput(outputId = "survey_selection") ),
                          width = 12 )
  
  return(data_description)
}

get_data_description_reaction = function(input, output, surveyIds, survey_data = NA, census_data = NA){
  # Reaction event when the button Run Report is pressed to populate the data description
  # box. What is printed is the survey selected, question selected (if there is one), and
  # the number of people that responded to the survey. Also there will be table as a data summary
  #
  # Args:
  #   input: server input
  #   output: server output
  #   surveysIds: dictionary that maps survey -> surveyIds for conditionalPanel 
  #   survey_data: the data from the survey 
  #   census_data: the data from the census 
  #
  #
  # Returns:
  #   observeEvent object that reacts to the button push
  reaction = observeEvent(input$run_report,
               {
                 survey.selected = input[["survey"]] # id of the survey selection 
                 survey.selected.Id = surveyIds[survey.selected]
                 survey.selected.question = input[[survey.selected.Id]]
                 if(survey.selected.question == ""){
                   # if no questions to select to print out survey and number of responses
                   output$survey_selection = renderText( sprintf("In the %s, there was %d responses", survey.selected, 0 )  )
                 }else{
                   # format printing the question
                   size = length(gregexpr(" ", survey.selected.question)[[1]])
                   if(size > 10){
                     survey_q_to_print = gsub("^((\\w+\\W+){9}\\w+).*$","\\1",survey.selected.question)
                     if( substr(survey_q_to_print, (nchar(survey_q_to_print) + 1)-3, nchar(survey_q_to_print) ) != "..." ){
                       survey_q_to_print = paste(survey_q_to_print, "...")
                     }
                   }else{
                     survey_q_to_print = survey.selected.question
                   }
                   
                   #print output
                   output$survey_selection = renderText( sprintf("In the %s, for \"%s\" there was %d responses", 
                                                                 survey.selected, 
                                                                 survey_q_to_print, 
                                                                 0 ))
                 }
               })
  return(reaction)
}