representative_ui = function(){
  ui = box( title = "How representative are the responses?",
            box(
                  textOutput("gender_title"),
                   tableOutput("gender_rep_data_table"), 
                   textOutput("age_title"),
                   tableOutput("age_rep_data_table"), width = 12),
            box(textOutput("edu_title"),
                   tableOutput("edu_rep_data_table"),
                   textOutput("income_title"),
                   tableOutput("income_rep_data_table"), width = 12),
          
            width = 12)
    
  return(ui)
}

get_representative_reactive = function(input, output){
  reaction = observeEvent(input$run_report,
                          {
                            # code
                            
                            #data = get(load(paste(getwd(),"data_preprocessing",
                                        #"results","air-quality-survey",
                                        #"air-quality-survey-state-55.RData",
                                        #sep="/")))
                           
                            if(input[["survey"]] != "" && input$census_level != "" ){
                              output$gender_title = renderText( "Gender" )
                              output$age_title = renderText( "Age" )
                              output$edu_title = renderText( "Education" )
                              output$income_title = renderText( "Income" )
                              
                              
                              gender = data.frame(matrix(ncol=2, nrow = 2))
                              colnames(gender) = c("Black", "Hispanic")
                              rownames(gender) = c("Male", "Female")
                              output$gender_rep_data_table = renderTable(gender, rownames = T)
                              
                              age = data.frame(matrix(ncol=2, nrow = 6))
                              colnames(age) = c("Black", "Hispanic")
                              rownames(age) = c("18-24", "25-34", "35-44", "45-54", "55-64", "65 or over")
                              output$age_rep_data_table = renderTable(age, rownames = T)
                              
                              education = data.frame(matrix(ncol=2, nrow = 4))
                              colnames(education) = c("Black", "Hispanic")
                              rownames(education) = c("Less than high school", "High-school graduate",
                                                      "Some college/Technical school",
                                                      "Bachelor's and higher")
                              output$edu_rep_data_table = renderTable(education, rownames = T)
                              
                              income = data.frame(matrix(ncol=2, nrow = 8))
                              colnames(income) = c("Black", "Hispanic")
                              rownames(income) = c("Less than $25,000", "$25,000 to $34,999",
                                                   "$35,000 to $49,999", "$50,000 to $74,999",
                                                   "$75,000 to $99,999", "$100,000 to $149,999",
                                                   "$150,000 to $199,999", "$200,000 or more")
                              output$income_rep_data_table = renderTable(income, rownames = T)
                            }
                          })
  
  return(reaction)
}
