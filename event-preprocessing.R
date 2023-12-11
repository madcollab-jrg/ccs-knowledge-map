### OCT AND MAR DELIBERATION SURVEYS

### LOAD PACKAGES
library(dplyr)
library(readr)
library(tidygeocoder)
library(tidycensus)
library(googleLanguageR)
library(stringr)


gl_auth("/Volumes/cbjackson2/ccs-knowledge/translate-ccs-ac3757553e16.json")
gl_auto_auth()

### IMPORT DATA FROM OCTOBER DELIBERATION
oct_black_closed <- read_csv("/Volumes/cbjackson2/ccs-knowledge/deliberation-data/oct_black/oct_black_closed.csv") # move to Zoonvierse Datasets folder
oct_black_forum <- read_csv("/Volumes/cbjackson2/ccs-knowledge/deliberation-data/oct_black/oct_black_forum.csv") # move to Zoonvierse Datasets folder

oct_latino_closed <- read_csv("/Volumes/cbjackson2/ccs-knowledge/deliberation-data/oct_latino/oct_latino_closed.csv") # move to Zoonvierse Datasets folder
oct_latino_forum <- read_csv("/Volumes/cbjackson2/ccs-knowledge/deliberation-data/oct_latino/oct_latino_forum.csv") # move to Zoonvierse Datasets folder
latino_demographics <- read_csv("/Volumes/cbjackson2/ccs-knowledge/deliberation-data/oct_latino/oct_demographic-latino.csv")
latino_demographics <- latino_demographics[,c(1,4:10)]
latino_demographics$race_recode <- "Hispanic"

### IMPORT DATA FROM MARCH FOLLOW-UP 
march_latino_airquality <- read_csv("/Volumes/cbjackson2/ccs-knowledge/deliberation-data/march_latino/oct_latino_forum-airquality.csv") # move to Zoonvierse Datasets folder
march_latino_urbanheat <- read_csv("/Volumes/cbjackson2/ccs-knowledge/deliberation-data/march_latino/oct_latino_forum-urbanheat.csv") # move to Zoonvierse Datasets folder


##############################################
# RECODE RESPONSES WITH ACTUAL LIKERT VALUES #
##############################################

agree_recode <- function(x) {
  recode(x, `1` = "Disagree Strongly", `2` = "Disagree Somewhat", 
         `3` = "Neutral", `4` = "Agree Somewhat", `5` = "Agree Strongly")
}

importance_recode <- function(x) {
  recode(x, `1` = "Extremely Unimportant", `2` = "Very Unimportant", 
         `3` = "Somewhat Unimportant", `4` = "Neither Important nor Unimportant", 
         `5` = "Somewhat Important", `6` = "Very Important", 
         `7` = "Extremely Important")
}

awareness_recode <- function(x) {
  recode(x, `1` = "Aware", `2` = "Not Aware")
}

informed_recode <- function(x) {
  recode(x, `1` = "Not at all Informed", `2` = "Slightly Informed", 
         `3` = "Moderately Informed", `4` = "Informed", `5` = "Very Informed")
}

amount_recode <- function(x) {
  recode(x, `1` = "Nothing", `2` = "A Little", 
         `3` = "Some", `4` = "A Good Amount", `5` = "A lot")
}

responsible_recode <- function(x) {
  recode(x, `1` = "Public Officials", `2` = "Tech Companies", 
         `3` = "Oil and Gas Companies", `4` = "Scientists", `5` = "Non-profit Organizations")
}

yesno_recode <- function(x) {
  recode(x, `1` = "Yes", `2` = "No")
}

reachout_recode <- function(x) {
  recode(x, `1` = "In-person event like today", `2` = "In-person survey", `3` = "Online survey", `4` = "Use of a website to post your opinions",
         `5` = "Phone call", `6` = "Text messages", `7` = "Other")
}

gender_recode <- function(x) {
  recode(x, `1` = "Male", `2` = "Female", `3` = "Non-binary")
}

income_recode <- function(x) {
  recode(x, `1` = "Less than $25,000", `2` = "$25,000 to $34,999", 
         `3` = "$35,000 to $49,999", `4` = "$50,000 to $74,999", 
         `5` = "$75,000 to $99,999", `6` = "$100,000 to $149,999",
         `7` = "$125,000 to $149,999", `8` = "$150,000 to $199,999",
         `9` = "$150,000 to $199,999", `10` = "$200,000 or more",
         `11` = "$200,000 or more")
} # THESE ARE RECODED TO MATCH CENSUS DATA

education_recode <- function(x) {
  recode(x, `1` = 'Less than High School Diploma',`2` = 'High School Graduate (Includes Equivalency)',
         `3` ='Some College or Associates Degree',`4` = 'Some College or Associates Degree',
         `5` = 'Some College or Associates Degree',`6` = 'Bachelors Degree or Higher',
         `7` = 'Bachelors Degree or Higher', `8` = 'Bachelors Degree or Higher',`9` = 'Bachelors Degree or Higher')
} # THESE ARE RECODED TO MATCH CENSUS DATA


#######################################
# CLEANING FOR BLACK OCT DELIBERATION #
#######################################

### RECODE VALUES IN DATAFRAMES 

oct_black_closed <- oct_black_closed %>%
  mutate(
    across(c(5:7), importance_recode), 
    across(c(8:12,15:21,23:24,39:41,46,47), agree_recode),
    across(c(13), informed_recode),
    across(c(14), amount_recode),
    across(c(28:38), awareness_recode),
    across(c(45), income_recode),
    across(c(43), gender_recode)
  )

oct_black_closed <- oct_black_closed %>% 
  rename(
         "income_recode" = "Which of the following ranges includes your total annual household income from all sources?",
         "gender" = "What is your gender?"
  )

### RENAME COLUMNS

oct_black_forum <- oct_black_forum %>% 
  rename("What do you think are the most urgent environmental issues facting your community?" = "environmental_issues_open",
         "How important are the following environmental issues to your everyday life?" = "environmental_goals_open", # IMPORTANCE
         "Imagine the perfect set up in your life, where you have complete control over your heat and cooling needs: What does this include and look like for you?" = "energyhousingB_q1",
         "If someone invited you to design the perfect housing complex for families to live in Madison or Dane County, what are your ideas for the heating and cooling systems?" = "energyhousingB_q2",
         "If your utility bill decreases (e.g., from $300 to $100 a month), how would that contribute to your well-being?" = "energyhousingB_q3",
         "Have you ever been in a situation where you could not cool off and you did not have AC? Or have you ever lived in a place that did not have access to AC? If so, what did you do?" = "ExtremeHeatA_q1",               
         "Was your life ever in danger due to extreme heat? Did you have any physical illness (such as feeling faint, tired, no energy to do things, headache, thirsty, nauseous, etc.). What happened?" = "ExtremeHeatA_q2a",             
         "If so, what programs or initiatives would you have liked to see to support you during that time?" = "ExtremeHeatA_q2b", 
         "Right now, when you need to find resources for keeping cool, what do you do or where do you go?" = "ExtremeHeatB_q2a",
         "In a perfect world, how can we make accessing those resources and information easier for you?" = "ExtremeHeatB_q2b",
         "Imagine that government officials, including from the city and the county, responded to the needs of community members. What would this support look like for you on an extremely hot day?" = "ExtremeHeatB_q1" ,
         "Do you think trees are important to the design of communities? If so, why?" = "trees_q1",                    
         "Imagine if you get to develop your dream neighborhood filled with green spaces, how would that green space be designed and who would lead it?" = "trees_q2",
         "Imagine the City gave you money to plant and maintain trees on the block where you currently live. What are some challenges that might arise?" = "trees_q3",                   
         "If the City offered a new tree planting initiative in your neighborhood today, would you join? If not, why not?" = "trees_q4",
         "edu_recode" = "education",
         "income_recode" = "household_income",
          "race_recode" = "race"
         )

oct_black_forum <- oct_black_forum %>%
  mutate(
    race_recode = recode(race_recode,"A person known as Black" = "Black"),
    across(c(2), gender_recode),
    across(c(3), education_recode),
    across(c(4), income_recode)
  )
names(oct_black_forum)[1] <- "ID"

## EXTRACT DEMOGRAPHIC INFORMATION FOR BLACK 
black_demographics <- oct_black_forum %>%
  select(1:4,6,7)
names(black_demographics)[1] <- "ID"
oct_black_forum <- oct_black_forum %>%
  select(-c(2:7,16,17))
oct_black_closed <- oct_black_closed %>%
  select(-c(2:3,39:41,43:45,48))

## Clean columns removing everything between parentheses e.g., (agree = 1 to disagree = 3)
remove_parentheses <- function(column_names) {
  return(sub(" \\(.*\\)", "", column_names))
}

names(oct_black_closed) <- remove_parentheses(names(oct_black_closed))

### ADD CLEAN DEMOGRAPHIC DATA FOR MERGING WITH LATINO DATASET
oct_black_closed <- merge(oct_black_closed,black_demographics,by="ID",all.x=TRUE)
oct_black_forum <- merge(oct_black_forum,black_demographics,by="ID",all.x=TRUE)

########################################
# CLEANING FOR LATINO OCT DELIBERATION #
########################################

# REMOVE COLUMNS NOT NEEDED
oct_latino_forum <- oct_latino_forum[,-c(2)]
oct_latino_closed <- oct_latino_closed[,-c(2,3)]

### RECODE VALUES IN DATAFRAMES 
oct_latino_closed <- oct_latino_closed %>%
  mutate(
    across(c(3:5), importance_recode), 
    across(c(6:10,13:19,21:22,37:39), agree_recode),
    across(c(11), informed_recode),
    across(c(12), amount_recode),
    across(c(26:36), awareness_recode),
    across(c(40), reachout_recode)
  )


### TRANSLATE COLUMNS 2,20,25 in oct_hispanic_closed AND NONE IN oct_hispanic_forum
closed_translate <- oct_latino_closed %>% 
  select(c(2,20,25))%>%
  mutate(across(everything(), ~ gl_translate(.x, target = "en") )
  ) 


# TRANSLATIONS
oct_latino_closed <- oct_latino_closed %>%
  mutate(
    `What do you think are the most urgent environmental issues facting your community?` = NULL,
    `What do you think are the most urgent environmental issues facting your community?`=
      closed_translate[[1]]$translatedText,.after=1) %>%
  mutate(  
    `What are your goals to improve the quality of environment?` = NULL,
    `What are your goals to improve the quality of environment?`=
      closed_translate[[2]]$translatedText,.after=5) %>%
  mutate(
    `What are your concerns of deploying new technologies such as the carbon dioxide removal storage in your neighborhood? (please write below)` = NULL,
    `What are your concerns of deploying new technologies such as the carbon dioxide removal storage in your neighborhood? (please write below)`=
      closed_translate[[3]]$translatedText,.after=24
  )

# REPLACE THAT with NA 
oct_latino_closed <- oct_latino_closed %>%
  mutate_at(vars(2, 20, 25), 
            ~ifelse(str_detect(., fixed("THAT")), NA_character_, .))

# CLEAN COLUMN NAMES
names(oct_latino_closed) <- remove_parentheses(names(oct_latino_closed))
oct_latino_closed <- oct_latino_closed %>%
  select(-c(37:39))


###################################
# CLEANING FOR LATINO DEMOGRAPHIC #
###################################
latino_demographics <- latino_demographics %>%
  mutate(
    across(c(3), gender_recode), 
    across(c(5), education_recode),
    across(c(6), income_recode),
    across(c(7:8), agree_recode)
  )

# ADD DEMOGRAPHIC (gender, edu_recode, income_recode, own_rent, race_recode, zip) INFO TO oct_latino_closed
oct_latino_closed <- merge(oct_latino_closed,latino_demographics[,c(1,7,8,3,5,6,9,4)], by = "ID")

names(oct_latino_closed)[38:42] <- c("Do you agree or disagree with the statement that “climate change is happening”?",
                                    "Do you agree or disagree with the statement that “climate change is caused by human activities”?" ,
                                    "gender","edu_recode","income_recode")

###########################################
# MERGE BLACK AND LATINO OCT DELIBERATION #
###########################################

oct_closed <- rbind(oct_black_closed, oct_latino_closed)

###### EXTRACT CATEGORIES OF SURVEYS
###### TOPICS COVERED IN DELIBERATION: Energy, Heat and Health, Tree and Green Spaces, and Carbon Technology. EACH SURVEY SHOULD INCLUDE QUESTIONS AND DEMOGRAPHIC INFORMATION

oct_energy <- oct_closed[,c(1,3,7,40:44)]
oct_heathealth <- oct_closed[,c(1,4,6,26:30,40:44)]
oct_treegreen <- oct_closed[,c(1,5,8,32,33,40:44)]
oct_carbon <- oct_closed[,c(1,34:36,40:44)]
oct_general <- oct_closed[,c(1,2,9:25,31,37:39,40:44)]

write.csv(oct_energy,"/Volumes/cbjackson2/ccs-knowledge/ccs-data/deliberation/oct_energy.csv")
write.csv(oct_heathealth,"/Volumes/cbjackson2/ccs-knowledge/ccs-data/deliberation/oct_heathealth.csv")
write.csv(oct_treegreen,"/Volumes/cbjackson2/ccs-knowledge/ccs-data/deliberation/oct_treesgreenery.csv")
write.csv(oct_carbon,"/Volumes/cbjackson2/ccs-knowledge/ccs-data/deliberation/oct_carbon.csv")
write.csv(oct_general,"/Volumes/cbjackson2/ccs-knowledge/ccs-data/deliberation/oct_general.csv")




