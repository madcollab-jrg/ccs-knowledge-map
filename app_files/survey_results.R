library(readr)
library(reshape2)
library(dplyr)
library(tidygeocoder)
library(tidycensus)
library(tidyverse)
library(googleLanguageR)
library(cld2)
library(datasets)
library(textstem)
library(tidytext)
library(DT)
library(ggraph)
library(igraph)
library(RColorBrewer)
library(ggthemes)
library(tm)
library(tidytext)
library(textdata)
library(topicmodels)


survey_results_ui = function(input){
  ui = box(title = "Survey Results", 
           box(title = "Income", plotOutput("survey_results1"), width=12), 
           box(title = "Education", plotOutput("survey_results2"),width=12),
           box(title = "Age",plotOutput("survey_results3"),width=12), 
           box(title = "Gender", plotOutput("survey_results4"),width=12),
           width = 12
    )
  return(ui)
}

#Text Questions
#Matrix Questions
#Multi-Choice Questions
#Select Box Questions

make_color_mapping = function(column, options){
  col_pal = brewer.pal(length(options), "PuOr")
  color_mapping = tibble(!!column := options, col = col_pal)
  return(color_mapping)
}

text_questions = function(survey_data, demographic_variable){
  # Import stoplist 
  malletwords <- scan("/Volumes/cbjackson2/ccs-knowledge/ccs-data/report_data/mallet.txt", character(), quote = "")
  
  # Extract example question and demographic data
  print(names(survey_data))
  example_open <- survey_data
  names(example_open)[2] <- "response" 
  
  example_open$response_cleaned <- tolower(gsub('[[:punct:]]', ' ', example_open$response))
  example_open$response_cleaned <- removeWords(example_open$response_cleaned, c(stopwords("english"),malletwords))
  example_open$response_cleaned <- lemmatize_words(example_open$response_cleaned)
  
  ###### BIGRAMS
  bigram_response <- example_open %>% 
    unnest_tokens(unigram, 
                  response_cleaned, token = "ngrams", n = 2)
  
  bigram_response.m <- bigram_response %>%
    pivot_longer(!`Contribution.ID`:Gender, names_to = "tokens", values_to = "word")
  
  # Summarize data by demographic factors - Gender, Hipanic/Latino/Spanish Origin, Race / Ethnicity, Year of Birth, Annual Household Income level, Education Level
  bigram_summary <- bigram_response.m %>% 
    group_by(!!sym(demographic_variable),word) %>% 
    summarise(count = n()) %>%
    mutate(freq = round(count / sum(count),digits=2))
  
  bigram_graph <- bigram_summary %>%
    filter(freq >= .02) %>%
    graph_from_data_frame() %>%
    ggraph(layout = "fr") +
    geom_edge_link(aes(edge_alpha = count, edge_width = count), edge_colour = "darkred") +
    geom_node_point(size = 5) +
    geom_node_text(aes(label = name), repel = TRUE,
                   point.padding = unit(0.2, "lines")) +
    theme_void()
  
  return(bigram_graph)
}

matrix_questions = function(example_matrix, demographic_variable, q_type){
  names(example_matrix)[2] <- "response" 
  
  # Since the questions are in one column, we need to seperate them and then the question and respone variables. ONLY FOR CCS QUESTIONS. In the allquestions_types questions in the CCS column = 1
  example_matrix <- example_matrix %>% 
    separate_rows(response, sep = "; ") %>%
    separate(response, into = c("question", "answer"), sep = " - ")
  example_matrix$answer <- as.factor(example_matrix$answer)
  
  matrix_summary <- example_matrix %>% 
    group_by(!!sym(demographic_variable),question,answer) %>% 
    summarise(n = n()) %>%
    mutate(freq = n / sum(n)) 
  
  matrix_summary <- matrix_summary %>%
    filter(!is.na(!!sym(demographic_variable)))
  
  if(q_type == "frequency"){
    ############## FREQUENCY ##############
    color_set_frequency <- data.frame (answer  = c("Always","Often","Sometimes","Rarely","Never"),
                                       col = c("#E66101","#FDB863","#F7F7F7","#B2ABD2","#5E3C99")) 
    
    # Create high and lows tables with question, group, outcome (question), variable, value, and color
    numlevels<-length(color_set_frequency$answer)
    pal<-brewer.pal((numlevels),"PuOr")
    legend.pal<-pal
    
    matrix_summary <- merge(matrix_summary,color_set_frequency, by="answer")
    # merge colors with dataframe
    
    # TRANSFORM HIGH/LOWS
    highs <- matrix_summary[which(matrix_summary$answer %in% c("Always","Often")),]
    lows <- matrix_summary[which(matrix_summary$answer %in% c("Never", "Rarely")),]
    mylevels <- c("Always","Often","Sometimes","Rarely","Never")
  }else if(q_type == "agree"){
    color_set_agree <- data.frame (answer  = c("Agree Strongly","Agree Somewhat","Neutral","Disagree Somewhat","Disagree Strongly"), col = c("#E66101","#FDB863","#F7F7F7","#B2ABD2","#5E3C99")) 
    
    # Create high and lows tables with question, group, outcome (question), variable, value, and color
    numlevels<-length(color_set_agree$answer)
    pal<-brewer.pal((numlevels),"PuOr")
    legend.pal<-pal
    
    matrix_summary <- merge(matrix_summary,color_set_agree, by="answer")
    # merge colors with dataframe
    
    # TRANSFORM HIGH/LOWS
    highs <- matrix_summary[which(matrix_summary$answer %in% c("Agree Strongly","Agree Somewhat")),]
    lows <- matrix_summary[which(matrix_summary$answer %in% c("Disagree Somewhat","Disagree Strongly")),]
    mylevels <- c("Agree Strongly","Agree Somewhat","Neutral","Disagree Somewhat","Disagree Strongly")
  }else if(q_type == "agree 1"){
    color_set_agree1 <- data.frame (answer  = c("Strongly agree","Agree","Neutral","Disagree","Strongly disagree"), col = c("#E66101","#FDB863","#F7F7F7","#B2ABD2","#5E3C99")) 
    
    # Create high and lows tables with question, group, outcome (question), variable, value, and color
    numlevels<-length(color_set_agree1$answer)
    pal<-brewer.pal((numlevels),"PuOr")
    legend.pal<-pal
    
    matrix_summary <- merge(matrix_summary,color_set_agree1, by="answer")
    # merge colors with dataframe
    
    # TRANSFORM HIGH/LOWS
    highs <- matrix_summary[which(matrix_summary$answer %in% c("Strongly agree","Agree")),]
    lows <- matrix_summary[which(matrix_summary$answer %in% c("Disagree","Strongly disagree ")),]
    mylevels <- c("Strongly agree","Agree","Neutral","Disagree","Strongly disagree")
  }else if(q_type == "agree 2"){
    color_set_agree2 <- data.frame (answer  = c("Strongly agree","Agree","Neither Agree nor disagree","Disagree","Strongly disagree"), col = c("#E66101","#FDB863","#F7F7F7","#B2ABD2","#5E3C99")) 
    
    # Create high and lows tables with question, group, outcome (question), variable, value, and color
    numlevels<-length(color_set_agree2$answer)
    pal<-brewer.pal((numlevels),"PuOr")
    legend.pal<-pal
    
    matrix_summary <- merge(matrix_summary,color_set_agree2, by="answer")
    # merge colors with dataframe
    
    # TRANSFORM HIGH/LOWS
    highs <- matrix_summary[which(matrix_summary$answer %in% c("Strongly agree","Agree")),]
    lows <- matrix_summary[which(matrix_summary$answer %in% c("Disagree","Strongly disagree ")),]
    mylevels <- c("Strongly agree","Agree","Neither Agree nor disagree","Disagree","Strongly disagree")
  }
  else if(q_type == "responsible"){
    color_set_fchange <- data.frame (answer  = c("Highly responsible","Moderately responsible","Somewhat responsible","Not responsible","Unsure"),col = c("#E66101","#FDB863","#FAE9D6","#B2ABD2","#F7F7F7")) 
    
    # Create high and lows tables with question, group, outcome (question), variable, value, and color
    numlevels<-length(color_set_fchange$answer)
    pal<-brewer.pal((numlevels),"PuOr")
    legend.pal<-pal
    
    matrix_summary <- merge(matrix_summary,color_set_fchange, by="answer")
    # merge colors with dataframe
    
    # TRANSFORM HIGH/LOWS
    highs <- matrix_summary[which(matrix_summary$answer %in% c("Highly responsible","Moderately responsible")),]
    lows <- matrix_summary[which(matrix_summary$answer %in% c("Not responsible","Unsure")),]
    mylevels <- c("Highly responsible","Moderately responsible","Somewhat responsible","Not responsible","Unsure")
  }else if(q_type == "awareness"){
    color_set_awareness <- data.frame (answer  = c("Aware","Not Aware"),
                                       col = c("#E66101","#5E3C99"))
    
    # Create high and lows tables with question, group, outcome (question), variable, value, and color
    numlevels<-length(color_set_awareness$answer)
    pal<-brewer.pal((numlevels),"PuOr")
    legend.pal<-pal
    
    matrix_summary <- merge(matrix_summary,color_set_awareness, by="answer")
    # merge colors with dataframe
    
    # TRANSFORM HIGH/LOWS
    highs <- matrix_summary[which(matrix_summary$answer %in% c("Aware")),]
    lows <- matrix_summary[which(matrix_summary$answer %in% c("Not Aware")),]
    mylevels <- c("Aware","Not Aware")
  }else if(q_type == "informed"){
    color_set_informed <- data.frame (answer  = c("Very Informed","Informed","Moderately Informed","Slightly Informed","Not at all Informed"),
                                      col = c("#E66101","#FDB863","#F7F7F7","#B2ABD2","#5E3C99"))
    
    # Create high and lows tables with question, group, outcome (question), variable, value, and color
    numlevels<-length(color_set_informed$answer)
    pal<-brewer.pal((numlevels),"PuOr")
    legend.pal<-pal
    
    matrix_summary <- merge(matrix_summary,color_set_informed, by="answer")
    # merge colors with dataframe
    
    # TRANSFORM HIGH/LOWS
    highs <- matrix_summary[which(matrix_summary$answer %in% c("Very Informed","Informed")),]
    lows <- matrix_summary[which(matrix_summary$answer %in% c("Slightly Informed","Not at all Informed")),]
    mylevels <- c("Very Informed","Informed","Moderately Informed","Slightly Informed","Not at all Informed")
  }else if(q_type == "amount"){
    color_set_amount <- data.frame (answer  = c("A lot","A Good Amount","Some","A Little","Nothing"),
                                    col = c("#E66101","#FDB863","#F7F7F7","#B2ABD2","#5E3C99"))
    
    # Create high and lows tables with question, group, outcome (question), variable, value, and color
    numlevels<-length(color_set_amount$answer)
    pal<-brewer.pal((numlevels),"PuOr")
    legend.pal<-pal
    
    matrix_summary <- merge(matrix_summary,color_set_amount, by="answer")
    # merge colors with dataframe
    
    # TRANSFORM HIGH/LOWS
    highs <- matrix_summary[which(matrix_summary$answer %in% c("A lot","A Good Amount")),]
    lows <- matrix_summary[which(matrix_summary$answer %in% c("A Little","Nothing")),]
    mylevels <- c("A lot","A Good Amount","Some","A Little","Nothing")
  }else if(q_type == "binary"){
    color_set_binary <- data.frame (answer  = c("Yes","No"),
                                    col = c("#E66101","#5E3C99"))
    
    # Create high and lows tables with question, group, outcome (question), variable, value, and color
    numlevels<-length(color_set_binary$answer)
    pal<-brewer.pal((numlevels),"PuOr")
    legend.pal<-pal
    
    matrix_summary <- merge(matrix_summary,color_set_binary, by="answer")
    # merge colors with dataframe
    
    # TRANSFORM HIGH/LOWS
    highs <- matrix_summary[which(matrix_summary$answer %in% c("Yes")),]
    lows <- matrix_summary[which(matrix_summary$answer %in% c("No")),]
    mylevels <- c("Yes","No")
  }else if(q_type == "importance"){
    color_set_importance <- data.frame (answer  = c("Extremely Unimportant","Very Unimportant","Somewhat Unimportant","Neither Important nor Unimportant","Somewhat Important","Very Important","Extremely Important"),col = 
                                          c("#E66101","#FDB863","#FAE9D6","#F7F7F7","#B2ABD2","#6753D7","#3F009E")) 
    
    # Create high and lows tables with question, group, outcome (question), variable, value, and color
    numlevels<-length(color_set_importance$answer)
    pal<-brewer.pal((numlevels),"PuOr")
    legend.pal<-pal
    
    
    matrix_summary <- merge(matrix_summary,color_set_importance, by="answer")
    # merge colors with dataframe
    
    # TRANSFORM HIGH/LOWS
    highs <- matrix_summary[which(matrix_summary$answer %in% c("Extremely Unimportant","Very Unimportant","Somewhat Unimportant")),]
    lows <- matrix_summary[which(matrix_summary$answer %in% c("Somewhat Important","Very Important","Extremely Important")),]
    mylevels <- c("Extremely Unimportant","Very Unimportant","Somewhat Unimportant","Neither Important nor Unimportant","Somewhat Important","Very Important","Extremely Important")
  }else if(q_type == "change"){
    color_set_fchange <- data.frame (answer  = c("Increased","Decreased","Stayed the same","I don't Know"),col = c("#E66101","#FDB863","#3F009E","#F7F7F7")) 
    
    # Create high and lows tables with question, group, outcome (question), variable, value, and color
    numlevels<-length(color_set_fchange$answer)
    pal<-brewer.pal((numlevels),"PuOr")
    legend.pal<-pal
    
    matrix_summary <- merge(matrix_summary,color_set_fchange, by="answer")
    # merge colors with dataframe
    
    # TRANSFORM HIGH/LOWS
    highs <- matrix_summary[which(matrix_summary$answer %in% c("Increased")),]
    lows <- matrix_summary[which(matrix_summary$answer %in% c("Decreased")),]
    mylevels <- c("Increased","Decreased","Stayed the same","I don't Know")
  }
  matrix_visualization <- ggplot() + 
    geom_bar(data=highs, aes(x = question, y=freq, fill=col), position="stack", stat="identity") +
    geom_bar(data=lows, aes(x = question, y=-freq, fill=col), position="stack", stat="identity") +
    facet_grid(cols = vars(!!sym(demographic_variable)))+
    geom_hline(yintercept = 0, color =c("white")) +
    scale_fill_identity("Percent of Respondents", 
                        labels = mylevels, 
                        breaks=legend.pal, 
                        guide="legend") + 
    theme_classic() +
    coord_flip() +
    scale_x_discrete(labels = function(x) str_wrap(str_replace_all(x, "foo" , " "),width = 40)) + 
    labs(y="",x="") +
    theme(
      legend.position = "bottom"
    ) 
  scale_y_continuous(breaks=seq(-10,100,25))
  return(matrix_visualization)
}

multi_choice_questions = function(example_multi, demographic_variable, filter_input, coloring, options){
  #example_multi <- survey_data[,c(2,col_num,45:53,19,22)]
  names(example_multi)[2] <- "response" 
  
  example_multi <- example_multi %>% 
    separate_rows(response, sep = "; ")
  
  if(!is.na(filter_input)){
    example_multi = example_multi %>% filter(!!sym(demographic_variable) == !!filter_input)
  }
  
  # Also extract other and run topic modeling? 
  multi_summary <- example_multi %>% 
    group_by(!!sym(demographic_variable),response) %>% 
    summarise(count = n()) %>%
    mutate(freq = round(count / sum(count),digits=2))
  
  # Remove other responses
  multi_summary <- multi_summary %>%
    filter(!grepl("^Other \\(please specify\\)", response)) %>%
    filter(!is.na(!!sym(demographic_variable)))  
  
  if(demographic_variable == "Gender"){
    multi_summary = multi_summary %>% filter(!Gender == "Non-binary")
  }
  
  multi_summary <- merge(multi_summary, coloring, by = demographic_variable)
  
  # Visualization (HORIZONTAL BAR CHART)
  multi_visualization <-
    ggplot(data=multi_summary, aes(x=response, y=freq, fill=col)) +
    geom_bar(stat="identity", color="black", 
             position=position_dodge(width = 0.7, preserve = "single"))+
    labs(y="Percent of Respondents") +
    coord_flip() + 
    theme_classic() +
    scale_fill_identity("Counts", labels = coloring[[demographic_variable]], breaks=coloring$col, guide="legend") + 
    scale_x_discrete(labels = function(x) str_wrap(str_replace_all(x, "foo" , " "),width = 40)) + 
    theme(
      legend.position="bottom",
      axis.text = element_text(size = 8))
  return(multi_visualization)
}

select_box_questions = function(survey_data, demographic_variable, filter_input, coloring, options){
  example_select <- survey_data
  names(example_select)[2] <- "response" 
  
  select_summary <- example_select %>% 
    group_by(!!sym(demographic_variable),response) %>% 
    summarise(count = n()) %>%
    mutate(freq = round(count / sum(count),digits=2))
  
  # Remove other responses
  select_summary <- select_summary %>%
    filter(!grepl("^Other \\(please specify\\)", response)) %>%
    filter(!is.na(!!sym(demographic_variable))) 
    
  if(demographic_variable == "Gender"){
    select_summary = select_summary %>% filter(Gender != "Non-binary")
  }
  
  select_summary <- merge(select_summary, coloring, by = demographic_variable) # USE SAME COLORS FROM OTHER
  print(select_summary$response)
  
  select_visualization <-
    ggplot(data=select_summary, aes(x=response, y=freq, fill=col)) +
    geom_bar(stat="identity", color="black", 
             position=position_dodge(width = 0.7, preserve = "single"))+
    labs(y="Count") +
    coord_flip() + 
    scale_fill_identity("Counts", labels = options, breaks=coloring, guide="legend") + 
    scale_x_discrete(labels = function(x) str_wrap(str_replace_all(x, "foo" , " "),width = 40)) + 
    theme(
      legend.position="bottom",axis.title.y = element_blank(),
      axis.text = element_text(size = 8))
  
  #datatable(select_summary[, c(1:4), drop=FALSE]) %>%
    #formatStyle('Gender',
    #            backgroundColor = styleEqual(gender_color_mapping$Gender, gender_color_mapping$col)
    #)
  
  return(select_visualization)
}

resulting_graphics = function(input, output, survey_data, is_survey, question = NA, question_type = NA, question_subtype = NA){
  # Populate the survey results boxes with the required graphics
  reaction = observeEvent(input$run_report,
                          {
                            req(input$survey)
                            q_type = question_type()
                            survey_flag = is_survey()
                            message(q_type)
                            if(q_type!= "Ranking" & survey_flag){ # Unsure how rank type questions are suppose to be displayed
                              question_num = question()+3 # column number of question
                              print(question_num)
                              
                              # column names of categories
                              income_var = "income_recode"
                              edu_var = "edu_recode"
                              age_var = "Year.of.Birth"
                              gender_var = "Gender"
                              
                              # subcategories options + color mapping
                              income_options = c(NA, "Less than $25,000", "$35,000 to $49,999", 
                                                 "$50,000 to $74,999", "$75,000 to $99,999", 
                                                 "$100,000 to $149,999", "$150,000 to $199,999", "$200,000 or more")
                              income_color_mapping = make_color_mapping(income_var, income_options)
                              
                              edu_options = c(NA, "Less than High School Diploma", "High School Graduate (Includes Equivalency)", 
                                              "Some College or Associates Degree", "Bachelors Degree or Higher")
                              edu_color_mapping = make_color_mapping(edu_var, edu_options)
                              
                              age_options = c(NA, "18_to_24", "25_to_34", "35_to_44", "45_to_54", "55_to_64", "65_over")
                              age_color_mapping = make_color_mapping(age_var, age_options)
                              
                              gender_options <- c(NA,"Non-binary","Male","Female") # Called multi-options in previous
                              gender_color_mapping = make_color_mapping(gender_var,gender_options)
                              
                              # get data and change year of birth
                              data = survey_data()
                              
                              data=data %>% 
                                mutate(Year.of.Birth = 2023-Year.of.Birth)%>%
                                mutate(Year.of.Birth = case_when( 
                                  Year.of.Birth >= 18 & Year.of.Birth <= 24 ~ "18_to_24",
                                  Year.of.Birth >= 25 & Year.of.Birth <=34 ~ "25_to_34",
                                  Year.of.Birth >= 35 & Year.of.Birth <=44 ~ "35_to_44",
                                  Year.of.Birth >= 45 & Year.of.Birth <= 54 ~ "45_to_54",
                                  Year.of.Birth >= 55 & Year.of.Birth <= 64 ~ "55_to_64",
                                  Year.of.Birth >= 65 ~ "65_over"
                                ) )
                              
                    
                              # data needed to make graphics by survey
                              data_for_visualization = NA
                              if(input$survey == "Urban Heat Survey"){
                                data_for_visualization = data[,c(2,question_num,45:53,22,19)]
                              }else if(input$survey == "Tree Canopy Survey"){
                                data_for_visualization = data[,c(2,question_num,48:56,25,22)]
                              }else if(input$survey == "Air Quality Survey"){
                                data_for_visualization = data[,c(2,question_num,47:55,24,21)]
                              }else if(input$survey == "Environmental Justice Survey"){
                                data_for_visualization = data[,c(2,question_num,28:58,27,24)]
                              }
                              
                              # print based on question type
                              message(q_type)
                              if(q_type == "matrix"){
                                q_subtype = question_subtype()
                                message(q_subtype)
                                output$survey_results1 = renderPlot( matrix_questions(data_for_visualization, income_var, q_subtype )  )
                                output$survey_results2 = renderPlot( matrix_questions(data_for_visualization, edu_var, q_subtype)  )
                                output$survey_results3 = renderPlot( matrix_questions(data_for_visualization, age_var, q_subtype)  )
                                output$survey_results4 = renderPlot( matrix_questions(data_for_visualization, gender_var, q_subtype)  )
                              }else if(q_type == "open-ended"){
                                output$survey_results1 = renderPlot( text_questions(data_for_visualization, income_var) )
                                output$survey_results2 = renderPlot( text_questions(data_for_visualization, edu_var) )
                                output$survey_results3 = renderPlot( text_questions(data_for_visualization, age_var) )
                                output$survey_results4 = renderPlot( text_questions(data_for_visualization, gender_var) )
                              }else if(q_type == "multi-choice"){
                                output$survey_results1 = renderPlot( multi_choice_questions(data_for_visualization, income_var, NA, income_color_mapping, income_options)  )
                                output$survey_results2 = renderPlot( multi_choice_questions(data_for_visualization, edu_var, NA, edu_color_mapping, edu_options)  )
                                output$survey_results3 = renderPlot( multi_choice_questions(data_for_visualization, age_var, NA, age_color_mapping, age_options)  )
                                output$survey_results4 = renderPlot( multi_choice_questions(data_for_visualization, gender_var, NA, gender_color_mapping, gender_options)  )
                              }else if(q_type == "select box"){
                                output$survey_results1 = renderPlot( select_box_questions(data_for_visualization, income_var, NA, income_color_mapping, income_options)  )
                                output$survey_results2 = renderPlot( select_box_questions(data_for_visualization, edu_var, NA, edu_color_mapping, edu_options)  )
                                output$survey_results3 = renderPlot( select_box_questions(data_for_visualization, age_var, NA, age_color_mapping, age_options)  )
                                output$survey_results4 = renderPlot( select_box_questions(data_for_visualization, gender_var, NA, gender_color_mapping, gender_options)  )
                              }
                              
                            }
                            else{
                              message("no")
                            }
                          })
}





