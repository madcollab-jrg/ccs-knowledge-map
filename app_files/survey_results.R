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
library(textdata)
library(topicmodels)
library(networkD3)
library(SnowballC)
library(plotly)
library(wordcloud)
library(wordcloud2)
library(stm)
library(topicmodels)
library(tm)
library(tokenizers)

survey_results_ui <- function(demog, surveyQues) {
  ui <- box(
    title = HTML(paste(
      "<div class='card-title'><h1 class='page-subtitle'>Response by ",
      demog, "</h1>",
      "<p class='text-lighter font-sm'>", surveyQues, "</p></div>"
    )),
    plotlyOutput("survey_results"), # for plotly
    width = 12,
    collapsible = FALSE,
    maximizable = TRUE,
    solidHeader = TRUE,
    elevation = NULL
  )
  return(ui)
}

# Text Questions
# Matrix Questions
# Multi-Choice Questions
# Select Box Questions

make_color_mapping <- function(column, options) {
  col_pal <- brewer.pal(length(options), "PuOr")
  color_mapping <- tibble(!!column := options, col = col_pal)
  return(color_mapping)
}

custom_dict <- c(
  "qualiti" = "quality", "problem" = "problem",
  "caus" = "cause", "neighborhood" = "neighborhood",
  "smoke" = "smoke", "pollut" = "pollute", "respiratori" = "respiratory",
  "peopl" = "people", "resourc" = "resource", "vulner" = "vulnerability",
  "experi" = "experience", "diseas" = "disease", "bodi" = "body",
  "communiti" = "community", "reduc" = "reduce", "improv" = "improve",
  "particip" = "participate", "injustic" = "injustice", "promot" = "promote",
  "togeth" = "together", "advoc" = "advocate", "bodi" = "body",
  "initi" = "initialize", "citi" = "city", "compani" = "company",
  "activ" = "active", "anoth" = "another", "resourc" = "resource",
  "agenc" = "agency", "voic" = "voice", "lifestyl" = "lifestyle",
  "natur" = "nature", "provid" = "provide", "environ" = "environment",
  "sens" = "sense", "activ" = "active", "valu" = "value",
  "properti" = "properties", "sunstrok" = "sunstroke",
  "experienc" = "experience", "basketbal" = "basketball",
  "temperatur" = "temperature", "condit" = "condition",
  "alreadi" = "already", "emerg" = "emergency",
  "immedi" = "immediately", "bodi" = "body", "extrem" = "extrem",
  "guidanc" = "guidance", "awar" = "aware", "energi" = "energy",
  "increas" = "increase", "encourag" = "encourage", "busi" = "busy",
  "consumpt" = "consumption", "abil" = "ability", "alreadi" = "already",
  "vehicl" = "vehicle", "mani" = "many", "pollution-rel" = "pollution-related",
  "negat" = "negate", "contribut" = "contribute", "resid" = "reside"
)

replace_word_lemma <- function(word) {
  if (word %in% names(custom_dict)) {
    return(custom_dict[[word]])
  } else {
    return(word)
  }
}

perform_topic_modeling <- function(
    survey_data, demographic_variable,
    num_topics = 4) {
  # Import stoplist
  malletwords <-
    scan("/Volumes/cbjackson2/ccs-knowledge/ccs-data/report_data/mallet.txt",
      character(),
      quote = ""
    )

  # Extract example question and demographic data
  example_open <- survey_data
  names(example_open)[2] <- "response"

  # example_open$response_cleaned <- tolower(gsub(
  #   "[[:punct:]]", " ",
  #   example_open$response
  # ))
  # example_open$response_cleaned <- removeWords(
  #   example_open$response_cleaned,
  #   c(stopwords("english"), malletwords)
  # )
  # example_open$response_cleaned <-
  #   lemmatize_words(example_open$response_cleaned)

  example_open$response_cleaned <- tolower(example_open$response)
  example_open$response_cleaned <-
    gsub("[[:punct:]0-9]", " ", example_open$response_cleaned)
  example_open$response_cleaned <-
    removeWords(example_open$response_cleaned, c(
      stopwords("english")
      # malletwords
    ))
  example_open$response_cleaned <-
    stripWhitespace(example_open$response_cleaned)
  example_open$response_cleaned <-
    wordStem(example_open$response_cleaned, language = "english")
  example_open$response_cleaned <-
    lemmatize_words(example_open$response_cleaned)

  survey_data <- example_open

  # Preprocess the text data
  processed_texts <- textProcessor(
    documents = survey_data$response,
    metadata = survey_data
  )
  out <- prepDocuments(
    processed_texts$documents, processed_texts$vocab,
    processed_texts$meta
  )
  docs <- out$documents
  vocab <- out$vocab
  meta <- out$meta

  # Fit the STM model
  num_topics <- 2 # Choose an appropriate number of topics
  topic_model <- stm(
    documents = docs, vocab = vocab,
    K = num_topics, data = meta, max.em.its = 150, init.type = "Spectral"
  )

  print("***********")
  top_words <- labelTopics(topic_model)
  print(top_words)
  topic_words <- top_words$prob

  human_readable_topics <- apply(topic_words, 1, function(topic) {
    words <- unlist(strsplit(topic, ", "))
    readable_words <- sapply(words, function(word) {
      # Replace with custom dictionary if needed
      replace_word_lemma(word)
    })
    paste(readable_words, collapse = ", ")
  })

  for (i in 1:num_topics) {
    topic <- paste("Topic", i, "-", human_readable_topics[i])
    print(topic)
  }

  topics1 <- topic_words[1, ]
  topics2 <- topic_words[2, ]

  # topics1 <- paste(topics1, collapse = ", ")
  # topics2 <- paste(topics2, collapse = ", ")

  # topics1 <- paste("Topic 1 - ", topics1)
  # topics2 <- paste("Topic 2 - ", topics2)

  # print(topics1)
  # print(topics2)

  print("***********")


  # Get document-topic matrix
  doc_topic_matrix <- topic_model$theta

  # Convert matrix to data frame
  doc_topic_df <- as.data.frame(doc_topic_matrix)
  doc_topic_df$Document <- rownames(doc_topic_df)

  # Melt the data frame for plotting
  doc_topic_melted <- reshape2::melt(doc_topic_df, id.vars = "Document")

  # print(doc_topic_melted)

  # Get top words associated with each topic
  # top_words <- terms(topic_model, 10)

  # print(top_words)
  print("---------------------------------------------")
  # print(doc_topic_matrix)

  column_sum1 <- sum(doc_topic_matrix[, 1])
  print(paste("column_sum1", column_sum1))

  column_sum2 <- sum(doc_topic_matrix[, 2])
  print(paste("column_sum2", column_sum2))

  topic_sums <- colSums(doc_topic_matrix)
  topic_descriptions <- paste0(
    "Topic ", 1:num_topics, " - ",
    human_readable_topics
  )

  trace1 <- list(
    type = "bar",
    # x = c(column_sum1 / 400, column_sum2 / 400),
    # y = c(topics1, topics2),
    x = topic_sums / 400,
    y = topic_descriptions,
    marker = list(
      line = list(
        color = "rgb(8,48,107)",
        width = 1.5
      ),
      color = "rgb(158,202,225)"
    ),
    opacity = 0.6,
    orientation = "h"
  )
  data <- list(trace1)
  layout <- list(
    title = "Topic Modelling",
    xaxis = list(domain = c(0.15, 1)),
    margin = list(
      b = 80,
      l = 120,
      r = 10,
      t = 140
    ),
    barmode = "stack",
    plot_bgcolor = "rgb(255, 255, 255)",
    paper_bgcolor = "rgb(255, 255, 255)"
  )
  p <- plot_ly()
  p <- add_trace(p,
    type = trace1$type, x = trace1$x,
    y = trace1$y, marker = trace1$marker,
    opacity = trace1$opacity, orientation = trace1$orientation
  )
  p <- layout(p,
    title = layout$title,
    xaxis = layout$xaxis, margin = layout$margin,
    barmode = layout$barmode, plot_bgcolor = layout$plot_bgcolor,
    paper_bgcolor = layout$paper_bgcolor
  )
}


matrix_questions <- function(example_matrix, demographic_variable, q_type) {
  names(example_matrix)[2] <- "response"

  # Since the questions are in one column, we need to separate them
  # and then the question and response variables. ONLY FOR CCS QUESTIONS.
  # In the allquestions_types questions in the CCS column = 1
  example_matrix <- example_matrix %>%
    separate_rows(response, sep = "; ") %>%
    separate(response, into = c("question", "answer"), sep = " - ")
  example_matrix$answer <- as.factor(example_matrix$answer)

  matrix_summary <- example_matrix %>%
    group_by(!!sym(demographic_variable), question, answer) %>%
    summarise(n = n()) %>%
    mutate(freq = n / sum(n))

  matrix_summary <- matrix_summary %>%
    filter(!is.na(!!sym(demographic_variable)))

  # print(matrix_summary)

  # Define color sets based on q_type
  color_sets <- list(
    frequency = c("#E66101", "#FDB863", "#F7F7F7", "#B2ABD2", "#5E3C99"),
    agree = c("#E66101", "#FDB863", "#F7F7F7", "#B2ABD2", "#5E3C99"),
    agree1 = c("#E66101", "#FDB863", "#F7F7F7", "#B2ABD2", "#5E3C99"),
    agree2 = c("#E66101", "#FDB863", "#F7F7F7", "#B2ABD2", "#5E3C99"),
    responsible = c("#E66101", "#FDB863", "#FAE9D6", "#B2ABD2", "#F7F7F7"),
    awareness = c("#E66101", "#5E3C99"),
    informed = c("#E66101", "#FDB863", "#F7F7F7", "#B2ABD2", "#5E3C99"),
    amount = c("#E66101", "#FDB863", "#F7F7F7", "#B2ABD2", "#5E3C99"),
    binary = c("#E66101", "#5E3C99"),
    importance = c(
      "#E66101", "#FDB863", "#FAE9D6", "#F7F7F7",
      "#B2ABD2", "#6753D7", "#3F009E"
    )
  )

  color_set <- color_sets[[q_type]]

  matrix_summary <- merge(matrix_summary,
    data.frame(answer = levels(matrix_summary$answer), col = color_set),
    by = "answer"
  )

  # Create high and lows tables
  highs <- matrix_summary %>%
    filter(answer %in% c(
      "Always", "Often", "Agree Strongly",
      "Agree", "Strongly agree"
    ))

  lows <- matrix_summary %>%
    filter(answer %in% c(
      "Never", "Rarely", "Disagree Somewhat",
      "Disagree", "Strongly disagree"
    ))

  mylevels <- levels(matrix_summary$answer)

  matrix_summary <- matrix_summary[, -ncol(matrix_summary)]

  # print(matrix_summary)

  switch(demographic_variable,
    "Gender" = {
      color_var <- ~Gender
      legendgroup <- ~Gender
      legendtext <- "Gender"
    },
    "income_recode" = {
      color_var <- ~income_recode
      legendgroup <- ~income_recode
      legendtext <- "Income Groups"
    },
    "edu_recode" = {
      color_var <- ~edu_recode
      legendgroup <- ~edu_recode
      legendtext <- "Education Levels"
    },
    "Year.of.Birth" = {
      color_var <- ~Year.of.Birth
      legendgroup <- ~Year.of.Birth
      legendtext <- "Age Group"
    },
    "race_recode" = {
      color_var <- ~race_recode
      legendgroup <- ~race_recode
      legendtext <- "Race"
    }
  )

  matrix_visualization <- plot_ly(
    data = matrix_summary,
    x = ~freq,
    y = ~question,
    text = ~ paste(
      "<b>Response</b>: ", response
    ),
    hoverinfo = "text",
    type = "bar",
    orientation = "h",
    color = color_var,
    width = 800,
    height = 400,
    legendgroup = ~answer,
    group = color_var
  ) %>%
    layout(
      xaxis = list(title = "Percentage of Respondents", family = "'Inter'"),
      yaxis = list(title = "Questions", family = "'Inter'"),
      barmode = "stack",
      legend = list(
        x = 0,
        y = -0.2,
        orientation = "h",
        font = list(size = 12, family = "'Inter'"),
        title = list(
          text = legendtext,
          font = list(size = 16, family = "'Inter'")
        ),
        itemsizing = "constant",
        showlegend = TRUE
      )
    )

  return(matrix_visualization)
}

category_color <- list(
  "Male" = "#B2ABD2",
  "age" = "#5E3C99",
  "default" = "#808080" # Add a default color for <NA>
)

# Multi-Choice Questions
multi_choice_questions <- function(
    example_multi, demographic_variable,
    filter_input, coloring, options) {
  print(head(example_multi))
  names(example_multi)[2] <- "response"

  example_multi <- example_multi %>%
    separate_rows(response, sep = "; ")

  if (!is.na(filter_input)) {
    example_multi <- example_multi %>%
      filter(!!sym(demographic_variable) == !!filter_input)
  }

  # Also extract other and run topic modeling?
  multi_summary <- example_multi %>%
    group_by(!!sym(demographic_variable), response) %>%
    summarise(count = n()) %>%
    mutate(freq = round(count / sum(count), digits = 2))

  # Remove other responses
  multi_summary <- multi_summary %>%
    filter(!grepl("^Other \\(please specify\\)", response)) %>%
    filter(!is.na(!!sym(demographic_variable)))

  if (demographic_variable == "Gender") {
    multi_summary <- multi_summary %>% filter(!Gender == "Non-binary")
  }

  multi_summary <- merge(multi_summary, coloring, by = demographic_variable)

  multi_summary <- multi_summary[, -ncol(multi_summary)]

  # Manually wrap labels
  wrap_labels <- function(labels, max_length = 20, ellipsis_length = 2) {
    ifelse(nchar(labels) > max_length, paste0(substr(
      labels, 1,
      max_length - ellipsis_length
    ), ".."), labels)
  }
  multi_summary$wrapped_labels <- wrap_labels(multi_summary$response)

  # print(multi_summary)

  print(demographic_variable)

  # based on demographics determine the variables
  switch(demographic_variable,
    "Gender" = {
      color_var <- ~Gender
      legendgroup <- ~Gender
      legendtext <- "Gender"
    },
    "income_recode" = {
      color_var <- ~income_recode
      legendgroup <- ~income_recode
      legendtext <- "Income Groups"
    },
    "edu_recode" = {
      color_var <- ~edu_recode
      legendgroup <- ~edu_recode
      legendtext <- "Education Levels"
    },
    "Year.of.Birth" = {
      color_var <- ~Year.of.Birth
      legendgroup <- ~Year.of.Birth
      legendtext <- "Age Group"
    },
    "race_recode" = {
      color_var <- ~race_recode
      legendgroup <- ~race_recode
      legendtext <- "Race"
    }
  )

  multi_visualization <- NA

  # Visualization (HORIZONTAL BAR CHART) using Plotly
  multi_visualization <- plot_ly(
    data = multi_summary,
    x = ~freq,
    y = ~wrapped_labels,
    text = ~ paste(
      "<b>Response</b>: ", response
    ),
    hoverinfo = "text",
    type = "bar",
    orientation = "h",
    color = color_var,
    showlegend = TRUE,
    width = 800,
    height = 400,
    legendgroup = legendgroup
    # hovertemplate = paste(
    #   "<b>Response</b>: %{y}"
    #   "<br><b>Frequency</b>: %{x}<br>"
    # )
  ) %>%
    layout(
      yaxis = list(
        tickangle = -45, title = "Responses", family = "'Inter'",
        ticktext = ~wrapped_labels
      ),
      xaxis = list(title = "Percent of Respondents", family = "'Inter'"),
      legend = list(
        x = 0,
        y = -0.2,
        orientation = "h",
        font = list(size = 12, family = "'Inter'"),
        title = list(
          text = legendtext,
          font = list(size = 16, family = "'Inter'")
        ),
        itemsizing = "constant", # Ensures legend items have constant size
        showlegend = FALSE
      )
    )

  print("---------")

  print(multi_visualization)
  print("---------")

  return(multi_visualization)
}

select_box_questions <- function(
    survey_data, demographic_variable,
    filter_input, coloring, options) {
  example_select <- survey_data
  names(example_select)[2] <- "response"

  select_summary <- example_select %>%
    group_by(!!sym(demographic_variable), response) %>%
    summarise(count = n()) %>%
    mutate(freq = round(count / sum(count), digits = 2))

  # Remove other responses
  select_summary <- select_summary %>%
    filter(!grepl("^Other \\(please specify\\)", response)) %>%
    filter(!is.na(!!sym(demographic_variable)))

  if (demographic_variable == "Gender") {
    select_summary <- select_summary %>% filter(Gender != "Non-binary")
  }

  select_summary <- merge(select_summary, coloring, by = demographic_variable)

  select_summary <- select_summary[, -ncol(select_summary)]

  # Manually wrap labels
  # wrap_labels <- function(labels, max_length = 60) {
  #   str_wrap(labels, width = max_length, indent = 0, exdent = 0)
  # }
  wrap_labels <- function(labels, max_length = 20, ellipsis_length = 2) {
    ifelse(nchar(labels) > max_length, paste0(substr(
      labels, 1,
      max_length - ellipsis_length
    ), ".."), labels)
  }

  select_summary$wrapped_labels <- wrap_labels(select_summary$response)

  # print(select_summary)

  switch(demographic_variable,
    "Gender" = {
      color_var <- ~Gender
      legendgroup <- ~Gender
      legendtext <- "Gender"
    },
    "income_recode" = {
      color_var <- ~income_recode
      legendgroup <- ~income_recode
      legendtext <- "Income Groups"
    },
    "edu_recode" = {
      color_var <- ~edu_recode
      legendgroup <- ~edu_recode
      legendtext <- "Education Levels"
    },
    "Year.of.Birth" = {
      color_var <- ~Year.of.Birth
      legendgroup <- ~Year.of.Birth
      legendtext <- "Age Group"
    },
    "race_recode" = {
      color_var <- ~race_recode
      legendgroup <- ~race_recode
      legendtext <- "Race"
    }
  )

  select_visualization <- NA

  select_visualization <- plot_ly(
    data = select_summary,
    x = ~wrapped_labels,
    y = ~count,
    type = "bar",
    text = ~ paste(
      "<b>Response</b>: ", response
    ),
    hoverinfo = "text",
    color = color_var,
    width = 800,
    height = 400,
    legendgroup = legendgroup
  ) %>%
    layout(
      xaxis = list(title = "Responses", family = "'Inter'"),
      yaxis = list(title = "Count", family = "'Inter'"),
      legend = list(
        x = 420,
        y = 50,
        orientation = "h",
        font = list(size = 12, family = "'Inter'"),
        title = list(
          text = legendtext,
          font = list(size = 16, family = "'Inter'")
        ),
        itemsizing = "constant", # Ensures legend items have constant size
        showlegend = TRUE
      )
    )

  return(select_visualization)
}


demographic_data_to_var <- c(
  "age" = "age_var",
  "gender" = "gender_var",
  "income" = "income_var",
  "education" = "edu_var",
  "race" = "race_var"
)

data_for_visualization <- NA

resulting_graphics <- function(
    input, output, survey_data, is_survey,
    question = NA, question_type = NA, question_subtype = NA,
    demographic_desc = NA) {
  # Populate the survey results boxes with the required graphics
  reaction <- observeEvent(input$run_report, {
    req(input$survey)
    # print(question_type())
    q_type <- question_type()
    survey_flag <- is_survey()

    # print(paste("is_survey", is_survey))
    # print(paste("q_type", q_type))


    # print(q_type)
    # message(q_type)
    # print(input$survey)
    # print(survey_flag)

    if (q_type != "Ranking" & survey_flag) {
      # Unsure how rank type questions are suppose to be displayed
      question_num <- question() # column number of question

      print(paste("question_num", question_num))

      # column names of categories
      income_var <- "income_recode"
      edu_var <- "edu_recode"
      age_var <- "Year.of.Birth"
      gender_var <- "Gender"
      race_var <- "race_recode"

      # subcategories options + color mapping
      income_options <- c(
        NA, "Less than $25,000", "$35,000 to $49,999",
        "$50,000 to $74,999", "$75,000 to $99,999",
        "$100,000 to $149,999", "$150,000 to $199,999", "$200,000 or more"
      )
      income_color_mapping <- make_color_mapping(income_var, income_options)

      edu_options <- c(
        NA, "Less than High School Diploma",
        "High School Graduate (Includes Equivalency)",
        "Some College or Associates Degree", "Bachelors Degree or Higher"
      )
      edu_color_mapping <- make_color_mapping(edu_var, edu_options)

      age_options <- c(
        NA, "18_to_24", "25_to_34", "35_to_44", "45_to_54",
        "55_to_64", "65_over"
      )
      age_color_mapping <- make_color_mapping(age_var, age_options)

      # Called multi-options in previous
      gender_options <- c(NA, "Non-binary", "Male", "Female")
      gender_color_mapping <- make_color_mapping(gender_var, gender_options)

      race_options <- c(
        NA, "Black or African American", "Hispanic", "White",
        "Asian", "Native Hawaiian Pacific Islander",
        "American Indian Alaskan Native", "Two or More Races"
      )
      race_color_mapping <- make_color_mapping(race_var, race_options)

      # get data and change year of birth
      data <- survey_data()

      if ("Year.of.Birth" %in% names(data)) {
        data <- data %>%
          mutate(Year.of.Birth = 2024 - Year.of.Birth) %>%
          mutate(Year.of.Birth = case_when(
            Year.of.Birth >= 18 & Year.of.Birth <= 24 ~ "18_to_24",
            Year.of.Birth >= 25 & Year.of.Birth <= 34 ~ "25_to_34",
            Year.of.Birth >= 35 & Year.of.Birth <= 44 ~ "35_to_44",
            Year.of.Birth >= 45 & Year.of.Birth <= 54 ~ "45_to_54",
            Year.of.Birth >= 55 & Year.of.Birth <= 64 ~ "55_to_64",
            Year.of.Birth >= 65 ~ "65_over"
          ))
      } else {
        data <- data %>%
          mutate(Year.of.Birth = "18_to_24")
      }

      # data needed to make graphics by survey
      data_for_visualization <- NA
      # print(input$survey)
      if (input$survey == "Urban Heat Survey") {
        data_for_visualization <- data[, c(2, question_num + 3, 45:53, 22, 19)]
      } else if (input$survey == "Tree Canopy Survey") {
        data_for_visualization <- data[, c(2, question_num + 3, 48:56, 25, 22)]
      } else if (input$survey == "Air Quality Survey") {
        data_for_visualization <- data[, c(2, question_num + 3, 47:55, 24, 21)]
      } else if (input$survey == "Environmental Justice Survey") {
        data_for_visualization <- data[, c(2, question_num + 3, 28:58, 27, 24)]
      } else if (input$survey == "General Survey") {
        data_for_visualization <- data[, c(1, question_num + 1, 25:28, 24)]
      } else if (input$survey == "Carbon Concerns") {
        data_for_visualization <- data[, c(1, question_num + 1, 2:5, 5)]
      } else if (input$survey == "Tree Knowledge") {
        data_for_visualization <- data[, c(1, question_num + 1, 7:10, 6)]
        # data_for_visualization <- data[, c(1, question_num + 1, 2:5, 5)]
      } else if (input$survey == "Energy Concerns") {
        data_for_visualization <- data[, c(1, question_num + 1, 5:8, 4)]
        # data_for_visualization <- data[, c(1, question_num + 1, 2:5, 5)]
      } else if (input$survey == "General Survey") {
        data_for_visualization <- data[, c(1, question_num + 1, 2:5, 5)]
      } else if (input$survey == "Health Impacts") {
        data_for_visualization <- data[, c(1, question_num + 1, 10:13, 9)]
        # data_for_visualization <- data[, c(1, question_num + 1, 2:5, 5)]
      } else if (input$survey == "Energy Concerns") {
        data_for_visualization <- data[, c(1, question_num + 1, 5:8, 4)]
      } else if (input$survey == "Heat Health Survey") {
        data_for_visualization <- data[, c(1, question_num + 1, 10:13, 9)]
      } else if (input$survey == "Trees Greenery Survey") {
        data_for_visualization <- data[, c(1, question_num + 1, 7:10, 6)]
      }

      # print based on question type
      # message(q_type)
      if (q_type == "matrix") {
        q_subtype <- question_subtype()
        # message(q_subtype)
        message(demographic_desc)
        if (demographic_desc == "income") {
          output$survey_results <- renderPlotly(matrix_questions(
            data_for_visualization,
            income_var, q_subtype
          ))
        } else if (demographic_desc == "education") {
          output$survey_results <- renderPlotly(matrix_questions(
            data_for_visualization,
            edu_var, q_subtype
          ))
        } else if (demographic_desc == "age") {
          output$survey_results <- renderPlotly(matrix_questions(
            data_for_visualization,
            age_var, q_subtype
          ))
        } else if (demographic_desc == "gender") {
          output$survey_results <- renderPlotly(matrix_questions(
            data_for_visualization,
            gender_var, q_subtype
          ))
        } else if (demographic_desc == "race") {
          output$survey_results <- renderPlotly(matrix_questions(
            data_for_visualization,
            race_var, q_subtype
          ))
        }
      } else if (q_type == "open-ended") {
        if (demographic_desc == "income") {
          output$survey_results <- renderPlotly(perform_topic_modeling(
            data_for_visualization,
            income_var
          ))
        } else if (demographic_desc == "education") {
          output$survey_results <- renderPlotly(perform_topic_modeling(
            data_for_visualization,
            edu_var
          ))
        } else if (demographic_desc == "age") {
          output$survey_results <- renderPlotly(perform_topic_modeling(
            data_for_visualization,
            age_var
          ))
        } else if (demographic_desc == "gender") {
          output$survey_results <- renderPlotly(perform_topic_modeling(
            data_for_visualization,
            gender_var
          ))
          # output$survey_results <- renderPlotly(perform_topic_modeling(
          #   data_for_visualization,
          #   gender_var
          # ))
        } else if (demographic_desc == "race") {
          # output$survey_results <- renderPlotly(text_questions(
          #   data_for_visualization,
          #   race_var
          # ))
          output$survey_results <- renderPlotly(perform_topic_modeling(
            data_for_visualization,
            race_var
          ))
        }
      } else if (q_type == "multi-choice") {
        if (demographic_desc == "income") {
          output$survey_results <- renderPlotly(multi_choice_questions(
            data_for_visualization,
            income_var, NA, income_color_mapping, income_options
          ))
        } else if (demographic_desc == "education") {
          output$survey_results <- renderPlotly(multi_choice_questions(
            data_for_visualization,
            edu_var, NA, edu_color_mapping, edu_options
          ))
        } else if (demographic_desc == "age") {
          output$survey_results <- renderPlotly(multi_choice_questions(
            data_for_visualization,
            age_var, NA, age_color_mapping, age_options
          ))
        } else if (demographic_desc == "gender") {
          output$survey_results <- renderPlotly(multi_choice_questions(
            data_for_visualization,
            gender_var, NA, gender_color_mapping, gender_options
          ))
        } else if (demographic_desc == "race") {
          output$survey_results <- renderPlotly(multi_choice_questions(
            data_for_visualization,
            race_var, NA, race_color_mapping, race_options
          ))
        }
      } else if (q_type == "select box") {
        if (demographic_desc == "income") {
          output$survey_results <- renderPlotly(select_box_questions(
            data_for_visualization,
            income_var, NA, income_color_mapping, income_options
          ))
        } else if (demographic_desc == "education") {
          output$survey_results <- renderPlotly(select_box_questions(
            data_for_visualization,
            edu_var, NA, edu_color_mapping, edu_options
          ))
        } else if (demographic_desc == "age") {
          output$survey_results <- renderPlotly(select_box_questions(
            data_for_visualization,
            age_var, NA, age_color_mapping, age_options
          ))
        } else if (demographic_desc == "gender") {
          output$survey_results <- renderPlotly(select_box_questions(
            data_for_visualization,
            gender_var, NA, gender_color_mapping, gender_options
          ))
        } else if (demographic_desc == "race") {
          output$survey_results <- renderPlotly(select_box_questions(
            data_for_visualization,
            race_var, NA, race_color_mapping, race_options
          ))
        }
      }
    } else {
      message("no plots")
    }
  })
}
