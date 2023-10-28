# Run this script to identify whether participants have qualified for the stipend. You must be connected to VPN and then my research drive folder

library(readr)
library(reshape2)
library(dplyr)
library(tidygeocoder)
library(tidycensus)
library(tidyverse)
library(googleLanguageR)
library(cld2)
library(datasets)


gl_auth("/Volumes/cbjackson2/ccs-knowledge/translate-ccs-ac3757553e16.json")
gl_auto_auth()
###########################################################
####### IMPORT/COMBINE DATASETS FROM RESEARCH DRIVE #######
###########################################################

#### LOG
# REMOVE BAD RESPONSES FROM WISC ECOLATINO


########## URBAN HEAT ########## 

# Import file survey and map files from Research Drive. Ignore the first 7 lines that have metadata for the element
heat_survey_MAIN <- read_csv("/Volumes/cbjackson2/ccs-knowledge/ccs-data/urban-heat/Form-ID-33_wisconsin-community-.csv", skip=8) # move to Zoonvierse Datasets folder
heat_survey_SECONDARY <- read_csv("/Volumes/cbjackson2/ccs-knowledge/ccs-data/urban-heat/Form-ID-36_wisconsin-community-.csv",skip=8) # move to Zoonvierse Datasets folder
heat_map_MAIN <- read_csv("/Volumes/cbjackson2/ccs-knowledge/ccs-data/urban-heat/SocialMap-ID-28_wisconsin-community-.csv", skip=8) # move to Zoonvierse Datasets folder
heat_map_SECONDARY <- read_csv("/Volumes/cbjackson2/ccs-knowledge/ccs-data/urban-heat/SocialMap-ID-41_wisconsin-community-.csv",skip=8) # move to Zoonvierse Datasets folder

# Handle mismatch in survey columns
heat_survey_SECONDARY <- heat_survey_SECONDARY[,-11]
names(heat_survey_MAIN)[4] <- "Please rate how frequently you have done each of the following during summer months and during extreme heat."

# Merge _MAIN AND _SECIONDAY datasets and remove imported file
heat_survey <- rbind(heat_survey_MAIN,heat_survey_SECONDARY)
heat_map <- rbind(heat_map_MAIN,heat_map_SECONDARY)

heat_translate <- heat_survey %>% 
  select(c(3,7,10))%>%
  mutate(across(everything(), ~ gl_translate(.x, target = "en") )
  ) 

# TRANSLATIONS
heat_survey <- heat_survey %>%
  mutate(
    `Have you or anyone you know experienced health issues related to urban heat (e.g., heat exhaustion, heat stroke) in the past year? If so, please tell us a bit about the persons' experiences.` = NULL,
    `Have you or anyone you know experienced health issues related to urban heat (e.g., heat exhaustion, heat stroke) in the past year? If so, please tell us a bit about the persons' experiences.`=
      heat_translate[[1]]$translatedText,.after=2) %>%
  mutate(  
    `What can local government do to help mitigate urban heat in your community?` = NULL,
    `What can local government do to help mitigate urban heat in your community?`=
      heat_translate[[2]]$translatedText,.after=6) %>%
  mutate(
    `In order to become more informed about urban heat and mitigation strategies, what specific information would you require?` = NULL,
    `In order to become more informed about urban heat and mitigation strategies, what specific information would you require?`=
      heat_translate[[3]]$translatedText,.after=9
  )

remove(heat_survey_MAIN,heat_survey_SECONDARY,heat_map_MAIN,heat_map_SECONDARY,heat_translate)

########## Tree Canopy ########## 

tree_survey_MAIN <- read_csv("/Volumes/cbjackson2/ccs-knowledge/ccs-data/tree-canopy/Form-ID-34_wisconsin-community-.csv", skip=8) # move to Zoonvierse Datasets folder
tree_survey_SECONDARY <- read_csv("/Volumes/cbjackson2/ccs-knowledge/ccs-data/tree-canopy/Form-ID-40_wisconsin-community-.csv",skip=8) # move to Zoonvierse Datasets folder
tree_map_MAIN <- read_csv("/Volumes/cbjackson2/ccs-knowledge/ccs-data/tree-canopy/SocialMap-ID-27_wisconsin-community-.csv", skip=8) # move to Zoonvierse Datasets folder
tree_map_SECONDARY <- read_csv("/Volumes/cbjackson2/ccs-knowledge/ccs-data/tree-canopy/SocialMap-ID-43_wisconsin-community-.csv", skip=8)
tree_survey_MAIN <- tree_survey_MAIN[,-13]
names(tree_survey_MAIN)[12] <- "What tree related goals would you like to see prioritized? Select up to 3"
names(tree_survey_MAIN)[11] <- "What type of trees would you like to see planted? Check all that apply."


# Merge _MAIN AND _SECIONDAY datasets and remove imported file
tree_survey <- rbind(tree_survey_MAIN,tree_survey_SECONDARY)
tree_map <- rbind(tree_map_MAIN,tree_map_SECONDARY)

tree_translate <- tree_survey %>% 
  select(c(8))%>%
  mutate(across(everything(), ~ gl_translate(.x, target = "en") )
  ) 

# TRANSLATIONS
tree_survey <- tree_survey %>%
  mutate(
    `How do trees impact your quality of life and the quality of your community?` = NULL,
    `How do trees impact your quality of life and the quality of your community?`=
      tree_translate[[1]]$translatedText,.after=7)

remove(tree_survey_MAIN,tree_survey_SECONDARY,tree_map_MAIN,tree_map_SECONDARY,tree_translate)

# Import EJ items 
ej_survey <- read_csv("/Volumes/cbjackson2/ccs-knowledge/ccs-data/ej-survey/Form-ID-31_wisconsin-community-.csv", skip=9)
ej_report <- read_csv("/Volumes/cbjackson2/ccs-knowledge/ccs-data/ej-report/Visioner-ID-29_wisconsin-community-.csv",skip=9)
ej_story <- read_csv("/Volumes/cbjackson2/ccs-knowledge/ccs-data/ej-storytile/Gather-ID-6_wisconsin-community-.csv", skip=9)


########## AirQuality ########## 

air_survey_MAIN <- read_csv("/Volumes/cbjackson2/ccs-knowledge/ccs-data/air-quality/Form-ID-30_wisconsin-community-.csv", skip=9) # move to Zoonvierse Datasets folder
air_survey_SECONDARY <- read_csv("/Volumes/cbjackson2/ccs-knowledge/ccs-data/air-quality/Form-ID-39_wisconsin-community-.csv",skip=9) # move to Zoonvierse Datasets folder
air_map_MAIN <- read_csv("/Volumes/cbjackson2/ccs-knowledge/ccs-data/air-quality/SocialMap-ID-26_wisconsin-community-.csv", skip=9) # move to Zoonvierse Datasets folder
air_map_SECONDARY <- read_csv("/Volumes/cbjackson2/ccs-knowledge/ccs-data/air-quality/SocialMap-ID-42_wisconsin-community-.csv", skip=9)


# Merge _MAIN AND _SECIONDAY datasets and remove imported file
air_survey <- rbind(air_survey_MAIN,air_survey_SECONDARY)
air_map <- rbind(air_map_MAIN,air_map_SECONDARY)

air_translate <- air_survey %>% 
  select(c(3,7,10)) %>%
  mutate(across(everything(), ~ gl_translate(.x, target = "en") )
  ) 

# TRANSLATIONS
air_survey <- air_survey %>%
  mutate(
    `Do you feel you are experiencing air quality problems in your neighborhood? If so, what are the specific air quality problems, and how are they impacting you, your family, or your neighbors?` = NULL,
    `Do you feel you are experiencing air quality problems in your neighborhood? If so, what are the specific air quality problems, and how are they impacting you, your family, or your neighbors?`=
      air_translate[[1]]$translatedText,.after=2)  %>%
  mutate(  
    `Do you feel that vulnerable populations (e.g., low-income individuals, children, elderly individuals) are disproportionately affected by air pollution in your community?` = NULL,
    `Do you feel that vulnerable populations (e.g., low-income individuals, children, elderly individuals) are disproportionately affected by air pollution in your community?`=
      air_translate[[2]]$translatedText,.after=6) %>%
  mutate(
    `Have you done anything to improve the air quality in your home or neighborhood? If so, what did you do? If not, what prevents you from taking action?` = NULL,
    `Have you done anything to improve the air quality in your home or neighborhood? If so, what did you do? If not, what prevents you from taking action?`=
      air_translate[[3]]$translatedText,.after=9
  )

remove(air_survey_MAIN,air_survey_SECONDARY,air_map_MAIN,air_map_SECONDARY,air_translate)

names(heat_survey)[grep("Which organization recruited you to the site?", colnames(heat_survey))] <- "organization"
names(heat_map)[grep("Which organization recruited you to the site?", colnames(heat_map))] <- "organization"
names(tree_survey)[grep("Which organization recruited you to the site?", colnames(tree_survey))] <- "organization"
names(tree_map)[grep("Which organization recruited you to the site?", colnames(tree_map))] <- "organization"
names(air_survey)[grep("Which organization recruited you to the site?", colnames(air_survey))] <- "organization"
names(air_map)[grep("Which organization recruited you to the site?", colnames(air_map))] <- "organization"

names(ej_survey)[grep("Which organization recruited you to the site?", colnames(ej_survey))] <- "organization"
names(ej_report)[grep("Which organization recruited you to the site?", colnames(ej_report))] <- "organization"
names(ej_story)[grep("Which organization recruited you to the site?", colnames(ej_story))] <- "organization"

#######################################
####### CHECK REQUIREMENTS FILE #######
#######################################

# Identify all participants (Email, username, and id) for each entry object
participants_heat_survey <- data.frame(id = heat_survey$`Member ID`, username = heat_survey$`Member Username`,email  =heat_survey$`Member Email`, 
                                       org = heat_survey$organization, address = heat_survey$`Home address`, hisp = heat_survey$`Hipanic/Latino/Spanish Origin`,
                                       race = heat_survey$`Race / Ethnicity`, yob = heat_survey$`Year of Birth`,  income_level = heat_survey$`Annual Household Income level`,
                                       edu = heat_survey$`Education Level`)
participants_heat_survey$type <-"heat survey"

participants_heat_map <- data.frame(id = heat_map$`Member ID`, username = heat_map$`Member Username`,email  =heat_map$`Member Email`,  
                                    org = heat_map$organization, address = heat_map$`Home address`, hisp = heat_map$`Hipanic/Latino/Spanish Origin`,
                                    race = heat_map$`Race / Ethnicity`, yob = heat_map$`Year of Birth`,  income_level = heat_map$`Annual Household Income level`,
                                    edu = heat_map$`Education Level`)
participants_heat_map$type <-"heat map"

participants_tree_survey <- data.frame(id = tree_survey$`Member ID`, username = tree_survey$`Member Username`,email  =tree_survey$`Member Email`,  
                                       org = tree_survey$organization, address = tree_survey$`Home address`, hisp = tree_survey$`Hipanic/Latino/Spanish Origin`,
                                       race = tree_survey$`Race / Ethnicity`, yob = tree_survey$`Year of Birth`,  income_level = tree_survey$`Annual Household Income level`,
                                       edu = tree_survey$`Education Level`)
participants_tree_survey$type <-"tree survey"

participants_tree_map <- data.frame(id = tree_map$`Member ID`, username = tree_map$`Member Username`,email  =tree_map$`Member Email`,   
                                    org = tree_map$organization, address = tree_map$`Home address`, hisp = tree_map$`Hipanic/Latino/Spanish Origin`,
                                    race = tree_map$`Race / Ethnicity`, yob = tree_map$`Year of Birth`,  income_level = tree_map$`Annual Household Income level`,
                                    edu = tree_map$`Education Level`)
participants_tree_map$type <-"tree map"

participants_air_survey <- data.frame(id = air_survey$`Member ID`, username = air_survey$`Member Username`,email  =air_survey$`Member Email`,    
                                      org = air_survey$organization, address = air_survey$`Home address`, hisp = air_survey$`Hipanic/Latino/Spanish Origin`,
                                      race = air_survey$`Race / Ethnicity`, yob = air_survey$`Year of Birth`,  income_level = air_survey$`Annual Household Income level`,
                                      edu = air_survey$`Education Level`)
participants_air_survey$type <-"air survey"

participants_air_map <- data.frame(id = air_map$`Member ID`, username = air_map$`Member Username`,email  =air_map$`Member Email`,     
                                   org = air_map$organization, address = air_map$`Home address`, hisp = air_map$`Hipanic/Latino/Spanish Origin`,
                                   race = air_map$`Race / Ethnicity`, yob = air_map$`Year of Birth`,  income_level = air_map$`Annual Household Income level`,
                                   edu = air_map$`Education Level`)
participants_air_map$type <-"air map"

participants_ej_survey <- data.frame(id = ej_survey$`Member ID`, username = ej_survey$`Member Username`,email  =ej_survey$`Member Email`,      
                                     org = ej_survey$organization, address = ej_survey$`Home address`, hisp = ej_survey$`Hipanic/Latino/Spanish Origin`,
                                     race = ej_survey$`Race / Ethnicity`, yob = ej_survey$`Year of Birth`,  income_level = ej_survey$`Annual Household Income level`,
                                     edu = ej_survey$`Education Level`)
participants_ej_survey$type <-"ej survey"

participants_ej_report <- data.frame(id = ej_report$`Member ID`, username = ej_report$`Member Username`,email  =ej_report$`Member Email`,      
                                     org = ej_report$organization, address = ej_report$`Home address`, hisp = ej_report$`Hipanic/Latino/Spanish Origin`,
                                     race = ej_report$`Race / Ethnicity`, yob = ej_report$`Year of Birth`,  income_level = ej_report$`Annual Household Income level`,
                                     edu = ej_report$`Education Level`)
participants_ej_report$type <-"ej report"

participants_ej_story <- data.frame(id = ej_story$`Member ID`, username = ej_story$`Member Username`,email  =ej_story$`Member Email`,     
                                    org = ej_story$organization, address = ej_story$`Home address`, hisp = ej_story$`Hipanic/Latino/Spanish Origin`,
                                    race = ej_story$`Race / Ethnicity`, yob = ej_story$`Year of Birth`,  income_level = ej_story$`Annual Household Income level`,
                                    edu = ej_story$`Education Level`)
participants_ej_story$type <-"ej story"


### COMBINE ALL DATA
participant_actions <- rbind(participants_heat_survey,participants_heat_map,
                             participants_tree_survey,participants_tree_map,
                             participants_air_survey,participants_air_map,
                             participants_ej_survey,participants_ej_report,participants_ej_story)

ccs_participants <- participant_actions %>% 
  distinct(id, .keep_all=TRUE)
ccs_participants$type <- NULL

# Remove records from research team
ccs_participants <- ccs_participants[which(!ccs_participants$id %in% c(40,45,59,61)),]

# Remove records bad responses 
potential_fakes <- c(316, 317, 318, 319, 320, 321, 322, 323, 324, 325, 327, 329,
                     330, 331, 332, 333, 334, 335, 336, 337, 338, 339, 340, 342, 343, 344, 345, 
                     346, 348, 349, 350, 351, 352, 353, 354, 355, 356, 358, 359, 360, 361, 
                     362, 363, 364, 365, 366, 367, 369, 370, 371, 372, 373, 374, 376, 378, 379, 380,
                     381, 382, 383, 384, 385, 389, 390, 391, 392, 392, 393, 395, 396, 397, 398, 399)

remove(participants_heat_survey,participants_heat_map,
       participants_tree_survey,participants_tree_map,
       participants_air_survey,participants_air_map,
       participants_ej_survey,participants_ej_report,participants_ej_story)

# Make dataframe wide with actions across
ccs_actions <- participant_actions %>%
  dcast(id + username + email + org  ~ type)
ccs_actions[5:12][ccs_actions[5:12]>=1] = 1

# Remove records from research team
ccs_actions <- ccs_actions[which(!ccs_actions$id %in% c(40,45,59,61,3940349)),]

# Compute activities that have been completed
ccs_actions <- ccs_actions %>%
  mutate(local_gov_actions = rowSums(across(c("air map","air survey","heat map","heat survey","tree map","tree survey"))),
         ej_actions = rowSums(across(c("ej report","ej story","ej survey")))
         )

ccs_actions_ecolatino <- ccs_actions[which(ccs_actions$org =="Wisconsin EcoLatinos"),]
ccs_actions_ecolatino$pay_status <- ifelse(ccs_actions_ecolatino$local_gov_actions == 6, "Pay Required","Incomplete")
ccs_actions_ecolatino$potential_fakes <- ifelse(ccs_actions_ecolatino$id %in% potential_fakes, "Potential Fake","") # Add which might be fakes in the data


ccs_actions_urbantriage <- ccs_actions[which(ccs_actions$org =="Urban Triage"),]
ccs_actions_urbantriage$pay_status <- ifelse(ccs_actions_urbantriage$local_gov_actions == 6 & 
                                               ccs_actions_urbantriage$ej_actions >= 1, "Pay Required","Incomplete")

ccs_actions_NA <- ccs_actions[which(is.na(ccs_actions$org) | ccs_actions$org =="None"),]


#######################################
write.csv(ccs_actions_ecolatino,"/Volumes/cbjackson2/ccs-knowledge/participant_actions/ccs_actions_ecolatino.csv")
write.csv(ccs_actions_urbantriage,"/Volumes/cbjackson2/ccs-knowledge/participant_actions/ccs_actions_urbantriage.csv")
write.csv(ccs_actions_NA,"/Volumes/cbjackson2/ccs-knowledge/participant_actions/ccs_actions_NA.csv")

###########################################
####### CLEAN CCS DATA FOR CCS-KM #########
###########################################

# GET DEMOGRAPHICS FROM URBAN TRIAGE AND MERGE 
ut_demographics <- read_csv("/Volumes/cbjackson2/ccs-knowledge/ccs-data/ut_demo/demographicsUT.csv") # move to Zoonvierse Datasets folder
colnames(ut_demographics) <- gsub("Contact - ", "", colnames(ut_demographics))

# Concatenate Home address for lookup
# UT DATA NEEDS TO BE CLEANED TO MATCH CCS KM
ut_demographics$address <- paste(ut_demographics$`Primary Address - Address Line 1`, 
                                 ut_demographics$`Primary Address - City`, 
                                 ut_demographics$`Primary Address - State/Province`, 
                                 ut_demographics$`Primary Address - Postal/Zip`, sep = ", ")

ut_demographics$org <- "Urban Triage"

ut_demographics <- ut_demographics %>% 
  mutate(income_recode=recode(`How much money do you make in a year?`, 
                              'Between $36,001 and $42,000' ='$35,000 to $49,999',
                              'Over 62K' ='$50,000 to $74,999', # MAY NEED TO LOOK UP JOB ROLES AND ADD AVERAGE SALARY
                              'Between $58,001 and $62,000' ='$50,000 to $74,999',
                              'Between $50,001 and $58,000' ='$50,000 to $74,999',
                              'Between $18,001 and $26,000' ='Less than $25,000',# OFF
                              'Between $10,001 and $18,000' ='Less than $25,000',
                              'Under $10,000' = 'Less than $25,000',
                              'Between $42,001 and $50,000' = '$35,000 to $49,999',
                              'Between $10,001 and $18,000; Between $18,001 and $26,000; Under $10,000' = 'Less than $25,000',
                              'Between $10,001 and $18,000; Between $36,001 and $42,000' = 'Less than $25,000',
                              'Between $10,001 and $18,000; Under $10,000' = 'Less than $25,000'
                              
  ))

ut_demographics <- ut_demographics %>% 
  mutate(edu_recode=recode(`What's your education level?`,
                           'GED/HSED' = 'Some College or Associates Degree',
                           'High School' = 'High School Graduate (Includes Equivalency)',
                           'Bachelor Degree/Four Year' = 'Bachelors Degree or Higher',
                           'Associate Degree/Two Year' = 'Some College or Associates Degree', 
                           'Did Not Complete High School' = 'Less than High School Diploma',
                           'Graduate/Master\'s' ='Bachelors Degree or Higher',
                           'Doctorate/PHD' ='Bachelors Degree or Higher'
  ))

ut_demographics$hisp <- ifelse(ut_demographics$`What is your race?`=="Latinx","Yes","No")

ut_demographics <- ut_demographics %>% 
  mutate(race=recode(`What is your race?`,
                     'A person known as Black' = 'Black or African American',
                     'White' = 'White', 
                     'A person known as Black; White' = 'Black or African American',
                     'White; Asian' = 'White'
  ))

ut_demographics$yob <- ut_demographics$`What year were you born?`

ut_ccs_participants <- data.frame(email = ut_demographics$Email,ut_demographics$org,ut_demographics$address,ut_demographics$hisp,ut_demographics$race,
                                    ut_demographics$yob,income_level =ut_demographics$income_recode,edu =ut_demographics$edu_recode)
colnames(ut_ccs_participants) <- gsub("ut_demographics.", "", colnames(ut_ccs_participants))

ccs_participants <- ccs_participants %>%
  left_join(ut_ccs_participants, by = "email", suffix = c("", ".ut_ccs_participants")) %>%
  mutate(
    income_level = ifelse(is.na(income_level), income_level.ut_ccs_participants, income_level),
    yob = ifelse(is.na(yob), yob.ut_ccs_participants, yob),
    address = ifelse(is.na(address), address.ut_ccs_participants, address),
    hisp = ifelse(is.na(hisp), hisp.ut_ccs_participants, hisp),
    race = ifelse(is.na(race), race.ut_ccs_participants, race),
    edu = ifelse(is.na(edu), edu.ut_ccs_participants, edu)
  ) %>%
  select(-ends_with(".ut_ccs_participants"))

### GET DATA FROM IN-PERSON DELIBERATION



#### REMOVE POTENTIAL FAKES FROM DATA TO BE IMPORTED TO KNOWLEDGE MAP
ccs_participants <- ccs_participants[which(!ccs_participants$id %in% potential_fakes),]


# Create a dataframe of all users and their home addresses with census information
participant_geo <- ccs_participants %>% 
  geocode(
    address = address,
    method = "census", full_results = TRUE, api_options = list(census_return_type = 'geographies')
  )

#GET ZIP CODES
participant_geo$zip <- substr(participant_geo$matched_address, nchar(participant_geo$matched_address) - 4, nchar(participant_geo$matched_address))


########## CONVERT VARIABLES TO MAPPING WITH CENSUS DATA 

# There are fewer categories in census data, need to reduce 
participant_geo$hisp_code <- ifelse(is.na(participant_geo$hisp) | participant_geo$hisp == "No", "Not Hispanic","Hispanic")
  
participant_geo <- participant_geo %>% 
  mutate(race_recode = case_when(grepl(";", race, ignore.case = TRUE) ~ "Mixed Race", 
                                 is.na(race) ~ NA, 
                                 TRUE ~ paste0(race)))

# There are fewer categories in census data, need to reduce 
participant_geo <- participant_geo %>% 
  mutate(income_recode=recode(income_level, 
                              '$100,000 to $124,999' ='$100,000 to $149,999',
                              '$125,000 to $149,999' ='$100,000 to $149,999',
                              '$150,000 to $174,999' ='$150,000 to $199,999',
                              '$175,000 to $199,999' ='$150,000 to $199,999',
                              '$200,000 to $249,999' ='$200,000 or more',
                              '$250,000 or more' ='$200,000 or more'
                              ))
# There are fewer categories in census data, need to reduce 
participant_geo <- participant_geo %>% 
  mutate(edu_recode=recode(edu,
                              'Associate degree' = 'Some College or Associates Degree',
                              'Some college credit' = 'Some College or Associates Degree', 
                              'Bachelor’s degree' = 'Bachelors Degree or Higher',
                              'Master’s degree' ='Bachelors Degree or Higher',
                              'Professional degree' ='Bachelors Degree or Higher',
                              'Doctorate' ='Bachelors Degree or Higher',
                              'Diploma or the equivalent (for example GED)' = 'High School Graduate (Includes Equivalency)',
                              'High school graduate' = 'High School Graduate (Includes Equivalency)',
                              'Less than high school graduate' = 'Less than High School Diploma',
                              'No degree' = 'Less than High School Diploma',
                              'Trade/technical/vocational training' = 'Some College or Associates Degree'
                              
  ))
  

participant_geo$census_tract_full <- paste(participant_geo$state_fips, participant_geo$county_fips,participant_geo$census_tract, sep="")
participant_geo$census_block_full <- paste(participant_geo$state_fips, participant_geo$county_fips,participant_geo$census_tract,participant_geo$census_block, sep="")


tree_survey <- merge(tree_survey, participant_geo, by.x = "Member Username" , by.y = "username", all.x = TRUE)  
tree_map <- merge(tree_map, participant_geo, by.x = "Member Username" , by.y = "username", all.x = TRUE)  
heat_survey <- merge(heat_survey, participant_geo, by.x = "Member Username" , by.y = "username", all.x = TRUE)  
heat_map <- merge(heat_map, participant_geo, by.x = "Member Username" , by.y = "username", all.x = TRUE)  
air_survey <- merge(air_survey, participant_geo, by.x = "Member Username" , by.y = "username", all.x = TRUE)  
air_map <- merge(air_map, participant_geo, by.x = "Member Username" , by.y = "username", all.x = TRUE)  
ej_survey <- merge(ej_survey, participant_geo, by.x = "Member Username" , by.y = "username", all.x = TRUE)  
ej_report <- merge(ej_report, participant_geo, by.x = "Member Username" , by.y = "username", all.x = TRUE)  
ej_story <- merge(ej_story, participant_geo, by.x = "Member Username" , by.y = "username", all.x = TRUE)  

#######################
##### TRANSLATION ##### 
#######################



#######################
####### EXPORT ####### 
#######################

# Export each ob
write.csv(tree_survey,"/Volumes/cbjackson2/ccs-knowledge/ccs-data/tree-canopy/tree_survey.csv")
write.csv(tree_map,"/Volumes/cbjackson2/ccs-knowledge/ccs-data/tree-canopy/tree_map.csv")
write.csv(heat_survey,"/Volumes/cbjackson2/ccs-knowledge/ccs-data/urban-heat/heat_survey.csv")
write.csv(heat_map,"/Volumes/cbjackson2/ccs-knowledge/ccs-data/urban-heat/heat_map.csv")
write.csv(air_survey,"/Volumes/cbjackson2/ccs-knowledge/ccs-data/air-quality/air_survey.csv")
write.csv(air_map,"/Volumes/cbjackson2/ccs-knowledge/ccs-data/air-quality/air_map.csv")
write.csv(ej_survey,"/Volumes/cbjackson2/ccs-knowledge/ccs-data/ej-survey/ej_survey.csv")
write.csv(ej_report,"/Volumes/cbjackson2/ccs-knowledge/ccs-data/ej-report/ej_report.csv")
write.csv(ej_story,"/Volumes/cbjackson2/ccs-knowledge/ccs-data/ej-storytile/ej_story.csv")

# Extract available questions and objects
questions_tree_survey <- data.frame(names(tree_survey)[4:14])
questions_heat_survey <- data.frame(names(heat_survey)[4:11])
questions_air_survey <- data.frame(names(air_survey)[4:13])

# Add actual names of county, tract, and block to populate the filters
census_names <- read_csv("/Volumes/cbjackson2/ccs-knowledge/census-data/environmental/eji-wisconsin.csv") 

census_values <- participant_geo %>% 
  distinct(census_block_full, .keep_all=TRUE) %>%
  select(state_fips:census_block_full) 

census_values <- merge(census_values,census_names[,c("geoid","name","COUNTY","Location")], by.x= "census_tract_full", by.y = "geoid", all.x=TRUE)
census_values$census_block_full_name <- paste0("Block:"," ",census_values$census_block,",",census_values$Location)

write.csv(questions_tree_survey,"/Volumes/cbjackson2/ccs-knowledge/ccs-data/filter_inputs/tree_questions.csv")
write.csv(questions_heat_survey,"/Volumes/cbjackson2/ccs-knowledge/ccs-data/filter_inputs/heat_questions.csv")
write.csv(questions_air_survey,"/Volumes/cbjackson2/ccs-knowledge/ccs-data/filter_inputs/air_questions.csv")
write.csv(census_values,"/Volumes/cbjackson2/ccs-knowledge/ccs-data/filter_inputs/census_values.csv")



