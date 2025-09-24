# get information about the data
library(dplyr)
get_question_type <- function(survey, number) {
  print(survey)
  q_to_type <- read.csv("survey_questions/allquestion_types_2.csv")
  survey_name <- NULL
  if (survey == "Carbon Concerns" || survey == "Tree Knowledge" ||
    survey == "General Survey" || survey == "Health Impacts" ||
    survey == "Energy Concerns") {
    survey_name <- survey
  } else {
    survey_name <- str_sub(survey, 1, -8)
  }
  q <- q_to_type %>% filter(
    Survey == survey_name &
      Number == number
  )
  print(q[1, "Type"])
  return(tolower(as.character(q[1, "Type"])))
}

get_question_subtype <- function(survey, number) {
  q_to_type <- read.csv("survey_questions/allquestion_types_2.csv")
  q <- q_to_type %>% filter(Survey == str_sub(survey, 1, -8) & Number == number)
  return(tolower(as.character(q[1, "SubType"])))
}
