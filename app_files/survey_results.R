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

# todo: text question and select box question graphic integration
make_color_mapping = function(column, options){
  col_pal = brewer.pal(length(options), "PuOr")
  color_mapping = tibble(!!column := options, col = col_pal)
  return(color_mapping)
}

text_questions = function(survey_data, question){
  # Import stoplist 
  malletwords <- scan("/Volumes/cbjackson2/ccs-knowledge/ccs-data/report_data/mallet.txt", character(), quote = "")
  
  # Extract example question and demographic data
  example_open <- heat_survey[,c(2,4,45:53,19)]
  names(example_open)[2] <- "response" 
  
  example_open$response_cleaned <- tolower(gsub('[[:punct:]]', ' ', example_open$response))
  example_open$response_cleaned <- removeWords(example_open$response_cleaned, c(stopwords("english"),malletwords))
  example_open$response_cleaned <- lemmatize_words(example_open$response_cleaned)
  
  # Tokenize response_cleaned to create word cloud and summary responses by demographic groups
  unigram_response <- example_open %>% 
    unnest_tokens(unigram, 
                  response_cleaned, token = "ngrams", n = 1)
  
  # Melt data
  unigram_response.m <- unigram_response %>%
    pivot_longer(!`Contribution ID`:Gender, names_to = "tokens", values_to = "word")
  
  # Summarize data by demographic factors - Gender, Hipanic/Latino/Spanish Origin, Race / Ethnicity, Year of Birth, Annual Household Income level, Education Level
  unigram_summary <- unigram_response.m %>% 
    group_by(Gender,word) %>% 
    summarise(count = n()) %>%
    mutate(freq = round(count / sum(count),digits=2))
  
  # Remove infrequent categories. IF category has smaller than +/- 2 SDs remove from response data 
  unigram_constraint <- unigram_summary %>% 
    group_by(Gender) %>% 
    summarise(count = n()) %>%
    mutate(freq = round(count / sum(count),digits=2))
  
  # REMOVE RESPONSES WITH LESS THAN 10 percent of total or NA
  unigram_summary <- unigram_summary %>%
    filter(!is.na(Gender))  %>%
    filter(!Gender %in% unigram_constraint$Gender[which(unigram_constraint$freq <= .10 | is.na(unigram_constraint$Gender))])
  
  top_unigram_words <- unigram_summary %>%
    group_by(Gender) %>%
    arrange(-freq) %>%
    slice_head(n=20) %>%
    ungroup()
  
  top_unigram_words <- merge(top_unigram_words, gender_color_mapping, by = "Gender")
  
  unigram_plot <- ggplot(top_unigram_words, aes(x = reorder(word, freq), y = freq, fill=col)) +
    geom_bar(stat="identity", color="black", 
             position=position_dodge(width = 0.7, preserve = "single")) +
    scale_fill_identity("Counts", labels = gender_options, breaks=gender_color_mapping, guide="legend") + 
    coord_flip() +
    facet_wrap(~Gender, scales = "free_y") +
    ggtitle(paste0("Word Frequency By ",names(unigram_summary)[1])) + 
    labs(x = "Unigram",y = "Frequency") 
  
  
  ###### BIGRAMS
  bigram_response <- example_open %>% 
    unnest_tokens(unigram, 
                  response_cleaned, token = "ngrams", n = 2)
  
  bigram_response.m <- bigram_response %>%
    pivot_longer(!`Contribution ID`:Gender, names_to = "tokens", values_to = "word")
  
  # Summarize data by demographic factors - Gender, Hipanic/Latino/Spanish Origin, Race / Ethnicity, Year of Birth, Annual Household Income level, Education Level
  bigram_summary <- bigram_response.m %>% 
    group_by(Gender,word) %>% 
    summarise(count = n()) %>%
    mutate(freq = round(count / sum(count),digits=2))
  
  # Remove infrequent categories. IF category has smaller than +/- 2 SDs remove from response data 
  bigram_constraint <- bigram_summary %>% 
    group_by(Gender) %>% 
    summarise(count = n()) %>%
    mutate(freq = round(count / sum(count),digits=2))
  
  # REMOVE RESPONSES WITH LESS THAN 10 percent of total or NA
  bigram_summary <- bigram_summary %>%
    filter(!is.na(Gender))  %>%
    filter(!Gender %in% bigram_constraint$Gender[which(bigram_constraint$freq <= .10 | is.na(bigram_constraint$Gender))])
  
  top_bigram_words <- bigram_summary %>%
    group_by(Gender) %>%
    arrange(-freq) %>%
    slice_head(n=20) %>%
    ungroup()
  
  top_bigram_words <- merge(top_bigram_words, gender_color_mapping, by = "Gender")
  
  bigram_plot <- ggplot(top_bigram_words, aes(x = reorder(word, freq), y = freq, fill=col)) +
    geom_bar(stat="identity", color="black", 
             position=position_dodge(width = 0.7, preserve = "single")) +
    scale_fill_identity("Counts", labels = gender_options, breaks=gender_color_mapping, guide="legend") + 
    coord_flip() +
    facet_wrap(~Gender, scales = "free_y") +
    ggtitle(paste0("Word Frequency By ",names(unigram_summary)[1])) + 
    labs(x = "Bi-gram",y = "Frequency") 
  
  ###################################
  # POTENTIAL ANALYSES OF RESPONSES #
  ###################################
  
  ## 1. GRAPH
  #### https://chryswoods.com/text_analysis_r/ngrams.html 
  #### https://www.data-imaginist.com/2017/ggraph-introduction-layouts/
  #### https://cran.r-project.org/web/packages/ggraph/vignettes/Layouts.html 
  #### CAN BE USED TO SHOW OVERLAPPING AND UNIQUE RESPONSES BY GROUP. HOW TO CREATE NARRATIVE AND HELP USERS READ AND INTERPRET CHART
  
  ### UNIGRAM
  unigram_graph <- unigram_summary %>%
    group_by(Gender) %>%
    arrange(-freq) %>%
    slice_head(n=50) %>%
    select(word) %>%
    graph_from_data_frame()
  
  ggraph(unigram_graph, layout = 'fr') +
    geom_edge_link() +
    geom_node_point() +
    geom_node_text(aes(label = name), vjust = 1, hjust = 1)
  
  ### BIGRAM
  bigram_graph <- bigram_summary %>%
    group_by(Gender) %>%
    arrange(-freq) %>%
    slice_head(n=50) %>%
    select(word) %>%
    graph_from_data_frame()
  
  ggraph(bigram_graph, layout = "fr") +
    geom_edge_link() +
    geom_node_point() +
    geom_node_text(aes(label = name), vjust = 1, hjust = 1)
  
  
  # 2. SENTIMENT ANALYSIS
  
  #### https://www.tidytextmining.com/sentiment
  #### THERE ARE THREE DICTIONARIES FOR SENTIMENT AFFIN, NRC, AND BING. (NRC REPRESENTS MANY MORE EMPOTIONAL CATEGORIES)
  
  nrc_S <- get_sentiments("nrc") %>% 
    filter(!sentiment %in% c("positive", "negative"))
  
  # UNIGRAM
  unigram_summary <- unigram_summary %>%
    inner_join(nrc_S)
  
  # PLOT SENTIMENT WITH PROPORTION OF WORDS
  unigram_sentiment_summary <- unigram_summary %>% 
    group_by(Gender,sentiment) %>% 
    summarise(total_words = sum(count)) %>%
    mutate(freq = round(total_words / sum(total_words),digits=2))
  
  color_sentiment <- data.frame (sentiment  = c("anger","anticipation","disgust",
                                                "fear","joy","sadness","surprise", "trust"),
                                 col = c("#E66101","#FDB863","#F7F7F7","#B2ABD2","#5E3C99",
                                                  "#E6A1A1","#8A75E6","#B89EE6") #https://color.adobe.com/create/color-wheel
  )
  unigram_sentiment_summary <- merge(unigram_sentiment_summary,color_sentiment, by="sentiment")
  
  # SENTIMENT FACETED BY GENDER
  unigram_sentiment_plot <- ggplot(unigram_sentiment_summary, aes(x = reorder(sentiment, freq), y = freq, fill=col)) +
    geom_bar(stat="identity", color="black", 
             position=position_dodge(width = 0.7, preserve = "single")) +
    scale_fill_identity("Counts", labels = gender_options, breaks=gender_color_mapping, guide="legend") + 
    coord_flip() +
    facet_wrap(~Gender, scales = "free_y") +
    ggtitle(paste0("Word Frequency By ",names(unigram_summary)[1])) + 
    labs(x = "Emotion",y = "Frequency") 
  
  # WORDS FACETED BY SENTIMENT 
  unigram_summary <- merge(unigram_summary, gender_color_mapping, by = "Gender")
  
  unigram_sentiment_plot2 <- unigram_summary %>%
    group_by(Gender,sentiment) %>%
    slice_max(count, n = 5) %>% 
    ungroup() %>%
    mutate(word = reorder(word, count)) %>%
    ggplot(aes(count, word, fill = col)) +
    geom_col(show.legend = FALSE,position=position_dodge(width = 0.7, preserve = "single")) +
    scale_fill_identity("Counts", labels = gender_options, breaks=gender_color_mapping, guide="legend") + 
    facet_wrap(~sentiment, scales = "free_y") +
    labs(x = "Contribution to sentiment",
         y = NULL) + 
    theme(
      axis.text = element_text(size = 8))
  
  # 3. TOPIC MODELING
  
  # EACH COMMENT IS A DOCUMENT. EXTRACT WORD AND CONTRIBUTION ID
  doc_summary <- unigram_response.m %>% 
    group_by(`Contribution ID`, word) %>% 
    #group_by(Gender,word) %>% 
    summarise(count = n()) 
  
  doc.dtm <- doc_summary %>%
    cast_dtm(`Contribution ID`, word, count)
  
  tm_lda <- LDA(doc.dtm, k = 3)
  tm_topics <- tidy(tm_lda, matrix = "beta")
  
  tm_top_terms <- tm_topics %>%
    group_by(topic) %>%
    slice_max(beta, n = 5) %>% 
    ungroup() %>%
    arrange(topic, -beta)
  
  topic_viz <- tm_top_terms %>%
    mutate(term = reorder_within(term, beta, topic)) %>%
    ggplot(aes(beta, term, fill = factor(topic))) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~ topic, scales = "free") +
    scale_y_reordered()
}

matrix_questions = function(example_matrix, demographic_variable, filter_input, coloring, options){
  
  #ONLY WORKS FOR HEAT SURVEY + AIR QUALITY SURVEY
  
  names(example_matrix)[2] <- "response" 
  # Since the questions are in one column, we need to seperate them and then the question and respone variables
  example_matrix <- example_matrix %>% 
    separate_rows(response, sep = "; ") %>%
    separate(response, into = c("question", "answer"), sep = " - ")
  example_matrix$answer <- as.factor(example_matrix$answer)
  if(!is.na(filter_input)){
    example_matrix = example_matrix %>% filter(!!sym(demographic_variable) == !!filter_input)
  }
  matrix_summary <- example_matrix %>% 
    group_by(!!sym(demographic_variable), question,answer) %>% 
    summarise(n = n()) %>%
    mutate(freq = n / sum(n)) 
  
  # Recode likert to three responses 
  example_matrix <- example_matrix %>%
    mutate(
      likert_category = case_when(
        answer %in% c("Never", "Rarely") ~ "Infrequent",
        answer == "Sometimes" ~ "Moderate",
        answer %in% c("Often", "Always") ~ "Frequent",
        TRUE ~ NA_character_  # If there's any unexpected value, set it as NA
      )
    )
  
  # Create high and lows tables with question, group, outcome (question), variable, value, and color
  #numlevels<-length(unique(example_matrix$answer))
  #pal<-brewer.pal((numlevels),"PuOr")
  #legend.pal<-pal
  
  matrix_summary <- matrix_summary %>%
    filter(!is.na(!!sym(demographic_variable)))
  if(demographic_variable == "Gender"){
    matrix_summary = matrix_summary %>% filter(Gender != "Non-binary")
  }
  
  color_set_frequency <- data.frame (answer  = c("Always","Often","Sometimes","Rarely","Never"),
                                     col = c("#E66101","#FDB863","#F7F7F7","#B2ABD2","#5E3C99")) # https://color.adobe.com/create/color-wheel 
  matrix_summary <- merge(matrix_summary,color_set_frequency, by="answer")
  # merge colors
  
  highs <- matrix_summary[which(matrix_summary$answer %in% c("Always","Often")),]
  lows <- matrix_summary[which(matrix_summary$answer %in% c("Never", "Rarely")),]
  mylevels <- c("Always","Often","Sometimes","Rarely","Never")
  pal<-brewer.pal((5),"PuOr")
  legend.pal<-pal
  
  if(nrow(matrix_summary) > 0){
    matrix_visualization <- ggplot() + 
      geom_bar(data=highs, aes(x = question, y=freq, fill=col), position="stack", stat="identity",width = .75) +
      geom_bar(data=lows, aes(x = question, y=-freq, fill=col), position="stack", stat="identity",width = .75) +
      facet_grid(cols = vars(!!sym(demographic_variable)))  +
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
    scale_y_continuous(breaks=seq(-10,100,25), limits=c(20,100))
    
    return(matrix_visualization)
  }else{
    matrix_visualization <- ggplot() + 
      geom_blank()
    message("here")
    return(matrix_visualization)
  }
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

select_box_questions = function(survey_data, question){
  example_select <- survey_data[,c(2,question,22,48:56)]
  names(example_select)[2] <- "response" 
  
  select_summary <- example_select %>% 
    group_by(Gender,response) %>% 
    summarise(count = n()) %>%
    mutate(freq = round(count / sum(count),digits=2))
  
  # Remove other responses
  select_summary <- select_summary %>%
    filter(!grepl("^Other \\(please specify\\)", response)) %>%
    filter(!is.na(Gender))  %>%
    filter(!Gender == "Non-binary")
  
  select_summary <- merge(select_summary, gender_color_mapping, by = "Gender") # USE SAME COLORS FROM OTHER
  
  select_visualization <-
    ggplot(data=select_summary, aes(x=response, y=freq, fill=col)) +
    geom_bar(stat="identity", color="black", 
             position=position_dodge(width = 0.7, preserve = "single"))+
    labs(y="Count") +
    coord_flip() + 
    scale_fill_identity("Counts", labels = gender_options, breaks=gender_color_mapping, guide="legend") + 
    #facet_grid(. ~Gender)  +
    scale_x_discrete(labels = function(x) str_wrap(str_replace_all(x, "foo" , " "),width = 40)) + 
    theme(
      legend.position="bottom",axis.title.y = element_blank(),
      axis.text = element_text(size = 8))
  return(select_visualization)
}

resulting_graphics = function(input, output, survey_data, is_survey, question = NA, question_type = NA){
  reaction = observeEvent(input$run_report,
                          {
                            req(input$survey)
                            q_type = question_type()
                            survey_flag = is_survey()
                            message(q_type)
                            if(q_type!= "Ranking" & survey_flag){
                              question_num = question()+3
                              income_var = "income_recode"
                              edu_var = "edu_recode"
                              age_var = "Year.of.Birth"
                              gender_var = "Gender"
                              
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
                              
                    
                              # survey_data[,c(2,col_num,45:53,19,22)]
                              data_for_visualization = NA
                              if(input$survey == "Urban Heat Survey"){
                                data_for_visualization = data[,c(2,question_num,45:53,19,22)]
                              }else if(input$survey == "Tree Canopy Survey"){
                                data_for_visualization = data[,c(2,question_num,22,48:56,25)]
                              }else if(input$survey == "Air Quality Survey"){
                                data_for_visualization = data[,c(2,question_num,47:55,21,24)]
                              }else if(input$survey == "Environmental Justice Survey"){
                                data_for_visualization = data[,c(2,question_num,51:59,25,28)]
                              }
                              
                              print(nrow(data_for_visualization))
                              
                              message("print graphs")
                              if(q_type == "matrix"){
                                output$survey_results1 = renderPlot( matrix_questions(data_for_visualization, income_var, NA)  )
                                output$survey_results2 = renderPlot( matrix_questions(data_for_visualization, edu_var, NA)  )
                                output$survey_results3 = renderPlot( matrix_questions(data_for_visualization, age_var, NA)  )
                                output$survey_results4 = renderPlot( matrix_questions(data_for_visualization, gender_var, NA)  )
                              }else if(q_type == "open-ended"){
                                display_func = text_questions
                              }else if(q_type == "multi-choice"){
                                output$survey_results1 = renderPlot( multi_choice_questions(data_for_visualization, income_var, NA, income_color_mapping, income_options)  )
                                output$survey_results2 = renderPlot( multi_choice_questions(data_for_visualization, edu_var, NA, edu_color_mapping, edu_options)  )
                                output$survey_results3 = renderPlot( multi_choice_questions(data_for_visualization, age_var, NA, age_color_mapping, age_options)  )
                                output$survey_results4 = renderPlot( multi_choice_questions(data_for_visualization, gender_var, NA, gender_color_mapping, gender_options)  )
                              }else if(q_type == "select box"){
                                display_func = text_questions
                              }
                              
                            }
                            else{
                              message("no")
                            }
                          })
}





