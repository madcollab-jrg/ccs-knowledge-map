get_data_description_ui = function(){
  data_description = box( title = "Data Description", 
                          p( textOutput(outputId = "survey_selection") ),
                          width = 12 )
  
  return(data_description)
}

get_data_description_reaction = function(input, output){
  
  reaction = observeEvent(input$run_report,
               {
                 survey.selected = input$survey
                 if(survey.selected == "Survey1"){
                   survey.question = input$s1questions
                 }else if(survey.selected == "Survey2"){
                   survey.question = input$s2questions
                 }else{
                   survey.question = input$s3questions
                 }
                  
                 output$survey_selection = renderText( sprintf("In the %s, %s there was %d reponses", survey.selected, survey.question, 0) )
               })
  return(reaction)
}