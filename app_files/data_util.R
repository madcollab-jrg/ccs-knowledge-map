# get information about the data
library(dplyr)
get_question_type = function(survey, number){
  q_to_type = read.csv("survey_questions/allquestion_types_2.csv")
  q = q_to_type %>% filter(Survey == str_sub(survey, 1, -8) & Number == number)
  return( tolower(as.character(q[1,"Type"]) ))
}

get_question_subtype = function(survey, number){
  q_to_type = read.csv("survey_questions/allquestion_types_2.csv")
  q = q_to_type %>% filter(Survey == str_sub(survey, 1, -8) & Number == number)
  return( tolower(as.character(q[1,"SubType"]) ))
}

#print(get_question_type("Tree Canopy Survey", 1))