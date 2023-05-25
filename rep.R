
library(readr)
library(tidygeocoder)
library(tidycensus)
library(tidyverse)
library(gtsummary)
library(dplyr)


#######################################################################  
#####################  GET CCS DATA FROM PLATFORM ####################
####################################################################### 

# Import dataframe from CCS platform
data <- read_csv("~/Library/CloudStorage/Box-Box/ccs-knowledge/ccs-data/Urban Heat Survey.csv")

### FOR APP
#### CCS DATA
##### ALLOW USERS TO CHOOSE ELEMENT 
##### REQUIRE USERS TO CHOOSE QUESTION [SINGLE CHOICE]

#### CENSUS DATA
#### REQUIRE USERS TO SELECT COUNTY, TRACT, BLOCK
#### POST LIST OF OPTION ABOVE AND ALLOW MULTUPLE SELECT OF OPTIONS (ONLY OPTIONS IN CCS)
#### BUTTON CREATE REPORT


# Extract name of object 
obj <- data[2, 2]
obj <- c(obj$...2)

rec_date <- data[6, 2]
rec_date <- c(rec_date$...2)

#MAKE QUESTION ROW COLNAMES
data <- data.frame(data)
names(data)[1:dim(data)[2]] <- data[9,]

#CLEAR ROWS FROM DATAFRAME
data <- data[-c(1:9),]

#EXTRACT ONLY DEMOGRAPHIC. 
rep_data <- data[,c(12:25)]

# DATA SHOULD BE UNIQUE USERNAME (SOME DATASETS ALLOW MULTUPLE USER ENTRIES)
# rep_data <- 

# ADD CENSUS rep_data, TRACT, CITY, BLOCK data
census_full1 <- rep_data %>% 
  geocode(
  address = 'Home address',
  method = "census", full_results = TRUE, api_options = list(census_return_type = 'geographies')
)

census_full1$census_tract_full <- paste(census_full1$state_fips, census_full1$county_fips,census_full1$census_tract, sep="")
census_full1$census_block_full <- paste(census_full1$state_fips, census_full1$county_fips,census_full1$census_tract,census_full1$census_block, sep="")

# SUMMARIZE DATA FROM SELECTION
survey_summary <- gather(census_full1,Object,census_block_full) %>%
  group_by(census_block_full) %>%
  mutate(n = n()) %>%
  unique() %>%
  filter(Object %in% c("Gender","Hipanic/Latino/Spanish Origin","Race / Ethnicity",
                       "Year of Birth","Annual Household Income level","Education Level","census_block_full"))


# CONVERSIONS NEEDED 

# There are fewer categories in census data, need to reduce 
survey_summary$raceeth_recode <- 
  
# There are fewer categories in census data, need to reduce 
survey_summary$income_recode <- 
  
# There are fewer categories in census data, need to reduce 
survey_summary$edu_recode <- 
  
#######################################################################  
##################  STATISTICAL Representativeness COMPARISON #########
#######################################################################  
# STARTING EXAMPLE: https://academic.oup.com/jamiaopen/article/4/3/ooab077/6374690 


