library(gt)

representative_ui = function(){
  ui = box( title = "How representative are the responses?",
            gt_output("rep_table"),
          
            width = 12)
    
  return(ui)
}

get_representative_reactive = function(input, output, file_loc = NA){
  reaction = observeEvent(input$run_report,
                          {
                            # code
                            
                            if(input[["survey"]] != "" && input$census_level != "" ){
                              data_loc = paste(getwd(),"/../data_preprocessing/results/", file_loc(), sep = "")
                              tbl_data = get(load(data_loc))
                              message(tbl_data)
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
                                tab_header( title = "Survey Representativness" ) %>%
                                data_color( method = "numeric", rows = where(~ is.numeric(.x) & max(.x, na.rm = T) <= 1E6), 
                                            columns = where(~ is.numeric( ) & max(.x, na.rm = T) <= 1E6), palette = c("red", "blue"))
                              output$rep_table = render_gt(expr = gt_tbl)
                            }
                          })
  
  return(reaction)
}
