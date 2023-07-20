library(gt)

representative_ui = function(){
  ui = box( title = "How representative are the responses?",
            gt_output("rep_table"),
          
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
                              
                              tbl_data = data.frame(matrix(ncol=2, nrow = 20))
                              colnames(tbl_data) = c("Black", "Hispanic")
                              rownames(tbl_data) = c("Male", "Female", 
                                                     "18-24", "25-34", "35-44", "45-54", "55-64", "65 or over",
                                                     "Less than high school", "High-school graduate",
                                                     "Some college/Technical school",
                                                     "Bachelor's and higher",
                                                     "Less than $25,000", "$25,000 to $34,999",
                                                     "$35,000 to $49,999", "$50,000 to $74,999",
                                                     "$75,000 to $99,999", "$100,000 to $149,999",
                                                     "$150,000 to $199,999", "$200,000 or more"
                                                     )
                              gt_tbl = gt(tbl_data, rownames_to_stub = T)
                              
                              
                              gt_tbl = 
                                gt_tbl %>%
                                tab_row_group(label = "Gender", rows = 1:2)%>%
                                tab_row_group(label = "Age", rows = 3:8)%>%
                                tab_row_group(label = "Education", rows = 9:12)%>%
                                tab_row_group(label = "Income", rows = 13:20) %>%
                                tab_header( title = "Survey Representativness" )
                              output$rep_table = render_gt(expr = gt_tbl)
                              
                              
                              #output$gender_title = renderText( "Gender" )
                              #output$age_title = renderText( "Age" )
                              #output$edu_title = renderText( "Education" )
                              #output$income_title = renderText( "Income" )
                              
                              
                              #gender = data.frame(matrix(ncol=2, nrow = 2))
                              #colnames(gender) = c("Black", "Hispanic")
                              #rownames(gender) = c("Male", "Female")
                              
                              
                              #age = data.frame(matrix(ncol=2, nrow = 6))
                              #colnames(age) = c("Black", "Hispanic")
                              #rownames(age) = c("18-24", "25-34", "35-44", "45-54", "55-64", "65 or over")
                              #output$age_rep_data_table = renderTable(age, rownames = T)
                              
                              #education = data.frame(matrix(ncol=2, nrow = 4))
                              #colnames(education) = c("Black", "Hispanic")
                              #rownames(education) = c("Less than high school", "High-school graduate",
                                                      #"Some college/Technical school",
                                                      #"Bachelor's and higher")
                              
                              
                              #income = data.frame(matrix(ncol=2, nrow = 8))
                              #colnames(income) = c("Black", "Hispanic")
                              #rownames(income) = c("Less than $25,000", "$25,000 to $34,999",
                              #                     "$35,000 to $49,999", "$50,000 to $74,999",
                              #                     "$75,000 to $99,999", "$100,000 to $149,999",
                              #                     "$150,000 to $199,999", "$200,000 or more")
                              
                            }
                          })
  
  return(reaction)
}
