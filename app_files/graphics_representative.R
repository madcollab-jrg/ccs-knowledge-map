library(gt)

representative_ui = function(){
  ui = box( title = "How representative are the responses?",
            gt_output("rep_table"),
          
            width = 12)
    
  return(ui)
}

get_pal = function(min_val,max_val){
  pal <- function(x) {
    if(is.na(x)){return("white")}
    f_neg <- scales::col_numeric(
      palette = c("#A60021FF", "#FFFFEBFF"),
      domain = c(min_val, 0)
    )
    f_pos <- scales::col_numeric(
      palette = c("#FFFFEBFF", "#2400D9FF"),
      domain = c(0, max_val)
    )
    ifelse(x < 0 | is.na(x), f_neg(x), f_pos(x))
  }
  return(Vectorize(pal))
}

get_representative_reactive = function(input, output, file_loc = NA){
  reaction = observeEvent(input$run_report,
                          {
                            # code
                            
                            if(input[["survey"]] != "" && input$census_level != "" ){
                              data_loc = paste(getwd(),"/../data_preprocessing/results/", file_loc(), sep = "")
                              tbl_data = get(load(data_loc))
                              colnames(tbl_data) = c("Black", "Hispanic", "Total")
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
                              colors = NULL
                              if(sum(is.na(tbl_data)) < 60){colors = get_pal(min(tbl_data, na.rm=T), max(tbl_data, na.rm=T))}
    
                              gt_tbl = 
                                gt_tbl %>%
                                tab_row_group(label = "Gender", rows = 1:2)%>%
                                tab_row_group(label = "Age", rows = 3:8)%>%
                                tab_row_group(label = "Education", rows = 9:12)%>%
                                tab_row_group(label = "Income", rows = 13:20) %>%
                                tab_header( title = "Survey Representativness" ) %>%
                                data_color( method = "numeric", 
                                            colors = colors ) %>%
                                tab_style(
                                  style = cell_text(size = pct(75)),
                                  locations = list(cells_body(), cells_title(), cells_stub(), cells_column_labels(), cells_row_groups() )
                                ) %>%
                                fmt_number(decimals = 2)
                              
                              output$rep_table = render_gt(expr = gt_tbl)
                            }
                          })
  
  return(reaction)
}
