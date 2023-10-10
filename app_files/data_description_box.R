source("table.R")
library(gt)

get_data_description_ui = function(){
  # Data description UI. 
  #
  # Return:
  #   box containing the necessary components of 
  #   the data description UI. See get_data_description_reaction()
  #   for the components.
  data_description = box( title = "Data Description", 
                          #div(style = "margin-top: -30px"),
                          #p( textOutput(outputId = "survey_selection") ),
                          gt_output("summary_table"),
                          width = 12 )
  
  return(data_description)
}

print_questions = function(survey.selected, survey.selected.question, output, n){
  if(survey.selected.question == ""){
    # if no questions to select to print out survey and number of responses
    output$survey_selection = renderText( sprintf("In the %s, there was %d responses", survey.selected, n )  )
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
    output$survey_selection = renderText( sprintf("In the %s, for \"%s\" there was %d total responses", 
                                                  survey.selected, 
                                                  survey_q_to_print, 
                                                  n  ))
  }
}

get_data_description_reaction = function(input, output, surveyIds, survey_data = NA, census_data = NA, file_loc = NA){
  # When run report is pressed populate the data description box with table.
  # Table should have counts of people who answered the survey within wisconsin,
  # and split up into sub categories.
  reaction = observeEvent(input$run_report,
               {
                 survey.selected = input[["survey"]] # survey selected
                 
                 # if the survey box is not empty - that is an option has been selected
                 if(survey.selected != ""){
                   survey.selected.Id = surveyIds[survey.selected] # id of the survey selected
                   survey.selected.question = input[[survey.selected.Id]]
                   n = 0
                   if(survey.selected.question != ""){
                     q_number = as.integer( str_match(survey.selected.question, "Q\\s*(.*?)\\s*:")[,2] )
                     message(survey.selected.question)
                     n = nrow(data.frame(survey_data()[[(4+q_number)]]) %>% drop_na() )
                   }else{
                     n = nrow(survey_data())
                   }
                   print_questions( survey.selected, survey.selected.question, output, n )
                   if(input$census_level != ""){
                     
                     data_loc = paste("/Volumes/cbjackson2/ccs-knowledge/results_summary/", file_loc(), sep = "") # change
                     tbl_data = get_table(data_loc)[[1]]
                     gt_tbl = gt(tbl_data, rownames_to_stub = T)
                     
                     gt_tbl = 
                       gt_tbl %>%
                       tab_row_group(label = "Gender", rows = 1:2)%>%
                       tab_row_group(label = "Age", rows = 3:8)%>%
                       tab_row_group(label = "Education", rows = 9:12)%>%
                       tab_row_group(label = "Income", rows = 13:20) %>%
                       tab_header( title = "Number of Survey Responses" ) %>%
                       tab_footnote(footnote = paste("Counts are from minority participates in ", input[[census_data()]]),
                                    location = cells_title())%>%
                       tab_style(
                         style = cell_text(size = pct(80)),
                         locations = list(cells_body(), cells_title(), cells_stub(), cells_column_labels(), cells_row_groups() )
                       ) %>%
                       tab_options(data_row.padding = px(2), footnotes.font.size = pct(65))
                     
                     output$summary_table = render_gt(gt_tbl)
                       
                   }
                 }
               })
  return(reaction)
}