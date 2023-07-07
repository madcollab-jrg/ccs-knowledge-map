## EXTRACT CENSUS DATA 
library(readr)
library(tidygeocoder)
library(tidycensus)
library(tidyverse)
library(gtsummary)
library(dplyr)

options(tigris_use_cache = TRUE)
census_api_key("087b597eb48c2a5edee92ce40fb7b889f0aa69ac", overwrite=TRUE) 

# https://walker-data.com/tidycensus/articles/basic-usage.html
# https://fgeerolf.com/code/R/tidycensus.html

# https://www.socialexplorer.com/data/ACS2016_5yr/metadata/?ds=ACS16_5yr (ACS DATA USED)

########################################## 
############# GENDER and EDU #############
########################################## 

# WE CAN COMPUTE TOTALS FOR BOTH GENDER AND EDUCATION STATUS USING THESE DATASETS. The tables are taken from
# https://www.socialexplorer.com/data/ACS2016_5yr/metadata/?ds=ACS16_5yr

############# TRACT ############# 

censustract_genderedu_white = get_acs(
  geography = "tract",
  state = "WI",
  variables = c(male_white_total = "C15002A_002E", 
                male_white_less_hs = "C15002A_003E",
                male_white_hs_grad = "C15002A_004E",
                male_white_some_college = "C15002A_005E",
                male_white_bs = "C15002A_006E",
                female_white_total = "C15002A_007E",
                female_white_less_hs = "C15002A_008E",
                female_white_hs_grad = "C15002A_009E",
                female_white_some_college = "C15002A_010E",
                female_white_bs = "C15002A_011E"),
  #table = "C15002A", 
  cache_table = TRUE,
  output = 'wide'
)
censustract_genderedu_white <- censustract_genderedu_white %>% select(-contains("C15002A"))

censustract_genderedu_black = get_acs(
  geography = "tract",
  state = "WI",
  variables = c(male_black_total = "C15002B_002E", 
                male_black_less_hs = "C15002B_003E",
                male_black_hs_grad = "C15002B_004E",
                male_black_some_college = "C15002B_005E",
                male_black_bs = "C15002B_006E",
                female_black_total = "C15002B_007E",
                female_black_less_hs = "C15002B_008E",
                female_black_hs_grad = "C15002B_009E",
                female_black_some_college = "C15002B_010E",
                female_black_bs = "C15002B_011E"),
  #table = "C15002B", 
  cache_table = TRUE,
  output = 'wide'
)
censustract_genderedu_black <- censustract_genderedu_black %>% select(-contains("C15002B"))


censustract_genderedu_aian = get_acs(
  geography = "tract",
  state = "WI",
  variables = c(male_aian_total = "C15002C_002E", 
                male_aian_less_hs = "C15002C_003E",
                male_aian_hs_grad = "C15002C_004E",
                male_aian_some_college = "C15002C_005E",
                male_aian_bs = "C15002C_006E",
                female_aian_total = "C15002C_007E",
                female_aian_less_hs = "C15002C_008E",
                female_aian_hs_grad = "C15002C_009E",
                female_aian_some_college = "C15002C_010E",
                female_aian_bs = "C15002C_011E"),
  #table = "C15002C", 
  cache_table = TRUE,
  #variables = c(male =),  
  output = 'wide'
)
censustract_genderedu_aian <- censustract_genderedu_aian %>% select(-contains("C15002C"))


censustract_genderedu_asian = get_acs(
  geography = "tract",
  state = "WI",
  variables = c(male_asian_total = "C15002D_002E", 
                male_asian_less_hs = "C15002D_003E",
                male_asian_hs_grad = "C15002D_004E",
                male_asian_some_college = "C15002D_005E",
                male_asian_bs = "C15002D_006E",
                female_asian_total = "C15002D_007E",
                female_asian_less_hs = "C15002D_008E",
                female_asian_hs_grad = "C15002D_009E",
                female_asian_some_college = "C15002D_010E",
                female_asian_bs = "C15002D_011E"),
  #table = "C15002D", 
  cache_table = TRUE,
  #variables = c(male =),  
  output = 'wide'
)
censustract_genderedu_asian <- censustract_genderedu_asian %>% select(-contains("C15002D"))


censustract_genderedu_nhpi = get_acs(
  geography = "tract",
  state = "WI",
  variables = c(male_nhpi_total = "C15002E_002E", 
                male_nhpi_less_hs = "C15002E_003E",
                male_nhpi_hs_grad = "C15002E_004E",
                male_nhpi_some_college = "C15002E_005E",
                male_nhpi_bs = "C15002E_006E",
                female_nhpi_total = "C15002E_007E",
                female_nhpi_less_hs = "C15002E_008E",
                female_nhpi_hs_grad = "C15002E_009E",
                female_nhpi_some_college = "C15002E_010E",
                female_nhpi_bs = "C15002E_011E"),
  #table = "C15002E", 
  cache_table = TRUE,
  #variables = c(male =),  
  output = 'wide'
)
censustract_genderedu_nhpi <- censustract_genderedu_nhpi %>% select(-contains("C15002E"))


censustract_genderedu_other = get_acs(
  geography = "tract",
  state = "WI",
  variables = c(male_other_total = "C15002F_002E", 
                male_other_less_hs = "C15002F_003E",
                male_other_hs_grad = "C15002F_004E",
                male_other_some_college = "C15002F_005E",
                male_other_bs = "C15002F_006E",
                female_other_total = "C15002F_007E",
                female_other_less_hs = "C15002F_008E",
                female_other_hs_grad = "C15002F_009E",
                female_other_some_college = "C15002F_010E",
                female_other_bs = "C15002F_011E"),
  #table = "C15002F", 
  cache_table = TRUE,
  #variables = c(male =),  
  output = 'wide'
)
censustract_genderedu_other <- censustract_genderedu_other %>% select(-contains("C15002F"))


censustract_genderedu_mixed = get_acs(
  geography = "tract",
  state = "WI",
  variables = c(male_mixed_total = "C15002G_002E", 
                male_mixed_less_hs = "C15002G_003E",
                male_mixed_hs_grad = "C15002G_004E",
                male_mixed_some_college = "C15002G_005E",
                male_mixed_bs = "C15002G_006E",
                female_mixed_total = "C15002G_007E",
                female_mixed_less_hs = "C15002G_008E",
                female_mixed_hs_grad = "C15002G_009E",
                female_mixed_some_college = "C15002G_010E",
                female_mixed_bs = "C15002G_011E"),
  #table = "C15002G", 
  cache_table = TRUE,
  #variables = c(male =),  
  output = 'wide'
)
censustract_genderedu_mixed <- censustract_genderedu_mixed %>% select(-contains("C15002G"))


censustract_genderedu_white_nothispanic = get_acs(
  geography = "tract",
  state = "WI",
  variables = c(male_w_nothisp_total = "C15002H_002E", 
                male_w_nothisp_less_hs = "C15002H_003E",
                male_w_nothisp_hs_grad = "C15002H_004E",
                male_w_nothisp_some_college = "C15002H_005E",
                male_w_nothisp_bs = "C15002H_006E",
                female_w_nothisp_total = "C15002H_007E",
                female_w_nothisp_less_hs = "C15002H_008E",
                female_w_nothisp_hs_grad = "C15002H_009E",
                female_w_nothisp_some_college = "C15002H_010E",
                female_w_nothisp_bs = "C15002H_011E"),
  #table = "C15002H", 
  cache_table = TRUE,
  #variables = c(male =),  
  output = 'wide'
)
censustract_genderedu_white_nothispanic <- censustract_genderedu_white_nothispanic %>% select(-contains("C15002H"))


censustract_genderedu_hispanic = get_acs(
  geography = "tract",
  state = "WI",
  variables = c(male_hisp_total = "C15002I_002E", 
                male_hisp_less_hs = "C15002I_003E",
                male_hisp_hs_grad = "C15002I_004E",
                male_hisp_some_college = "C15002I_005E",
                male_hisp_bs = "C15002I_006E",
                female_hisp_total = "C15002I_007E",
                female_hisp_less_hs = "C15002I_008E",
                female_hisp_hs_grad = "C15002I_009E",
                female_hisp_some_college = "C15002I_010E",
                female_hisp_bs = "C15002I_011E"),
  #table = "C15002I", 
  cache_table = TRUE,
  #variables = c(male =),  
  output = 'wide'
)
censustract_genderedu_hispanic <- censustract_genderedu_hispanic %>% select(-contains("C15002I"))


# Merge all datasets above
censustract_genderedu <- censustract_genderedu_white %>%
  left_join(censustract_genderedu_black, by='GEOID') %>%
  left_join(censustract_genderedu_aian, by='GEOID') %>%
  left_join(censustract_genderedu_asian, by='GEOID') %>%
  left_join(censustract_genderedu_nhpi, by='GEOID') %>%
  left_join(censustract_genderedu_other, by='GEOID') %>%
  left_join(censustract_genderedu_mixed, by='GEOID') %>%
  left_join(censustract_genderedu_white_nothispanic, by='GEOID') %>%
  left_join(censustract_genderedu_hispanic, by='GEOID')

names(censustract_genderedu)[2] <- "full_geoid"  

censustract_genderedu <- censustract_genderedu %>% select(-contains("NAME"))

censustract_genderedu <- censustract_genderedu %>%
  group_by(GEOID,full_geoid) %>%
  mutate(
         BLACK_TOTAL = sum(male_black_total+female_black_total),
         BLACK_FEMALE = sum(female_black_total), 
         BLACK_MALE = sum(male_black_total),
         BLACK_LESS_HS_TOTAL = sum(male_black_less_hs+female_black_less_hs),
         BLACK_HS_TOTAL = sum(male_black_hs_grad+female_black_hs_grad),
         BLACK_SOME_COLLEGE_TOTAL = sum(male_black_some_college+female_black_some_college),
         BLACK_COLLEGE_TOTAL = sum(male_black_bs+female_black_bs),
         HISP_TOTAL = sum(male_hisp_total+female_hisp_total),
         HISP_FEMALE = sum(female_hisp_total), 
         HISP_MALE = sum(male_hisp_total),
         HISP_LESS_HS_TOTAL = sum(male_hisp_less_hs+female_hisp_less_hs),
         HISP_HS_TOTAL = sum(male_hisp_hs_grad+female_hisp_hs_grad),
         HISP_SOME_COLLEGE_TOTAL = sum(male_hisp_some_college+female_hisp_some_college),
         HISP_COLLEGE_TOTAL = sum(male_hisp_bs+female_hisp_bs),
         
         UNDERREP_EDU_TOTAL = sum(BLACK_TOTAL+HISP_TOTAL),
         UNDERREP_FEMALE_TOTAL = sum(BLACK_FEMALE+HISP_FEMALE),
         UNDERREP_MALE = sum(BLACK_MALE+HISP_MALE),
         UNDERREP_LESS_HS_TOTAL = sum(BLACK_LESS_HS_TOTAL+HISP_LESS_HS_TOTAL),
         UNDERREP_HS_TOTAL = sum(BLACK_HS_TOTAL+HISP_HS_TOTAL),
         UNDERREP_SOME_COLLEGE_TOTAL = sum(BLACK_SOME_COLLEGE_TOTAL+HISP_SOME_COLLEGE_TOTAL),
         UNDERREP_COLLEGE_TOTAL = sum(BLACK_COLLEGE_TOTAL+HISP_COLLEGE_TOTAL),
         
)
# export
write.csv(censustract_genderedu,"/Volumes/cbjackson2/ccs-knowledge/census-data/census/tract/censustract_genderedu.csv")

############# State ############# 

censusstate_genderedu_white = get_acs(
  geography = "state",
  state = "WI",
  #table = "C15002A", 
  variables = c(male_white_total = "C15002A_002E", 
                male_white_less_hs = "C15002A_003E",
                male_white_hs_grad = "C15002A_004E",
                male_white_some_college = "C15002A_005E",
                male_white_bs = "C15002A_006E",
                female_white_total = "C15002A_007E",
                female_white_less_hs = "C15002A_008E",
                female_white_hs_grad = "C15002A_009E",
                female_white_some_college = "C15002A_010E",
                female_white_bs = "C15002A_011E"),
  cache_table = TRUE,
  #variables = c(male =),  
  output = 'wide'
)
censusstate_genderedu_white <- censusstate_genderedu_white %>% select(-contains("C15002A"))

censusstate_genderedu_black = get_acs(
  geography = "state",
  state = "WI",
  variables = c(male_black_total = "C15002B_002E", 
                male_black_less_hs = "C15002B_003E",
                male_black_hs_grad = "C15002B_004E",
                male_black_some_college = "C15002B_005E",
                male_black_bs = "C15002B_006E",
                female_black_total = "C15002B_007E",
                female_black_less_hs = "C15002B_008E",
                female_black_hs_grad = "C15002B_009E",
                female_black_some_college = "C15002B_010E",
                female_black_bs = "C15002B_011E"),
  #table = "C15002B", 
  cache_table = TRUE,
  output = 'wide'
)
censusstate_genderedu_black <- censusstate_genderedu_black %>% select(-contains("C15002B"))

censusstate_genderedu_aian = get_acs(
  geography = "state",
  state = "WI",
  variables = c(male_aian_total = "C15002C_002E", 
                male_aian_less_hs = "C15002C_003E",
                male_aian_hs_grad = "C15002C_004E",
                male_aian_some_college = "C15002C_005E",
                male_aian_bs = "C15002C_006E",
                female_aian_total = "C15002C_007E",
                female_aian_less_hs = "C15002C_008E",
                female_aian_hs_grad = "C15002C_009E",
                female_aian_some_college = "C15002C_010E",
                female_aian_bs = "C15002C_011E"),
  #table = "C15002C", 
  cache_table = TRUE,
  #variables = c(male =),  
  output = 'wide'
)
censusstate_genderedu_aian <- censusstate_genderedu_aian %>% select(-contains("C15002C"))


censusstate_genderedu_asian = get_acs(
  geography = "state",
  state = "WI",
  variables = c(male_asian_total = "C15002D_002E", 
                male_asian_less_hs = "C15002D_003E",
                male_asian_hs_grad = "C15002D_004E",
                male_asian_some_college = "C15002D_005E",
                male_asian_bs = "C15002D_006E",
                female_asian_total = "C15002D_007E",
                female_asian_less_hs = "C15002D_008E",
                female_asian_hs_grad = "C15002D_009E",
                female_asian_some_college = "C15002D_010E",
                female_asian_bs = "C15002D_011E"),
  #table = "C15002D", 
  cache_table = TRUE,
  #variables = c(male =),  
  output = 'wide'
)
censusstate_genderedu_asian <- censusstate_genderedu_asian %>% select(-contains("C15002D"))


censusstate_genderedu_nhpi = get_acs(
  geography = "state",
  state = "WI",
  variables = c(male_nhpi_total = "C15002E_002E", 
                male_nhpi_less_hs = "C15002E_003E",
                male_nhpi_hs_grad = "C15002E_004E",
                male_nhpi_some_college = "C15002E_005E",
                male_nhpi_bs = "C15002E_006E",
                female_nhpi_total = "C15002E_007E",
                female_nhpi_less_hs = "C15002E_008E",
                female_nhpi_hs_grad = "C15002E_009E",
                female_nhpi_some_college = "C15002E_010E",
                female_nhpi_bs = "C15002E_011E"),
  #table = "C15002E", 
  cache_table = TRUE,
  #variables = c(male =),  
  output = 'wide'
)
censusstate_genderedu_nhpi <- censusstate_genderedu_nhpi %>% select(-contains("C15002E"))


censusstate_genderedu_other = get_acs(
  geography = "state",
  state = "WI",
  variables = c(male_other_total = "C15002F_002E", 
                male_other_less_hs = "C15002F_003E",
                male_other_hs_grad = "C15002F_004E",
                male_other_some_college = "C15002F_005E",
                male_other_bs = "C15002F_006E",
                female_other_total = "C15002F_007E",
                female_other_less_hs = "C15002F_008E",
                female_other_hs_grad = "C15002F_009E",
                female_other_some_college = "C15002F_010E",
                female_other_bs = "C15002F_011E"),
  #table = "C15002F", 
  cache_table = TRUE,
  #variables = c(male =),  
  output = 'wide'
)
censusstate_genderedu_other <- censusstate_genderedu_other %>% select(-contains("C15002F"))


censusstate_genderedu_mixed = get_acs(
  geography = "state",
  state = "WI",
  variables = c(male_mixed_total = "C15002G_002E", 
                male_mixed_less_hs = "C15002G_003E",
                male_mixed_hs_grad = "C15002G_004E",
                male_mixed_some_college = "C15002G_005E",
                male_mixed_bs = "C15002G_006E",
                female_mixed_total = "C15002G_007E",
                female_mixed_less_hs = "C15002G_008E",
                female_mixed_hs_grad = "C15002G_009E",
                female_mixed_some_college = "C15002G_010E",
                female_mixed_bs = "C15002G_011E"),
  #table = "C15002G", 
  cache_table = TRUE,
  #variables = c(male =),  
  output = 'wide'
)
censusstate_genderedu_mixed <- censusstate_genderedu_mixed %>% select(-contains("C15002G"))


censusstate_genderedu_white_nothispanic = get_acs(
  geography = "state",
  state = "WI",
  variables = c(male_w_nothisp_total = "C15002H_002E", 
                male_w_nothisp_less_hs = "C15002H_003E",
                male_w_nothisp_hs_grad = "C15002H_004E",
                male_w_nothisp_some_college = "C15002H_005E",
                male_w_nothisp_bs = "C15002H_006E",
                female_w_nothisp_total = "C15002H_007E",
                female_w_nothisp_less_hs = "C15002H_008E",
                female_w_nothisp_hs_grad = "C15002H_009E",
                female_w_nothisp_some_college = "C15002H_010E",
                female_w_nothisp_bs = "C15002H_011E"),
  #table = "C15002H", 
  cache_table = TRUE,
  #variables = c(male =),  
  output = 'wide'
)
censusstate_genderedu_white_nothispanic <- censusstate_genderedu_white_nothispanic %>% select(-contains("C15002H"))


censusstate_genderedu_hispanic = get_acs(
  geography = "state",
  state = "WI",
  variables = c(male_hisp_total = "C15002I_002E", 
                male_hisp_less_hs = "C15002I_003E",
                male_hisp_hs_grad = "C15002I_004E",
                male_hisp_some_college = "C15002I_005E",
                male_hisp_bs = "C15002I_006E",
                female_hisp_total = "C15002I_007E",
                female_hisp_less_hs = "C15002I_008E",
                female_hisp_hs_grad = "C15002I_009E",
                female_hisp_some_college = "C15002I_010E",
                female_hisp_bs = "C15002I_011E"),
  #table = "C15002I", 
  cache_table = TRUE,
  #variables = c(male =),  
  output = 'wide'
)
censusstate_genderedu_hispanic <- censusstate_genderedu_hispanic %>% select(-contains("C15002I"))


# Merge all datasets above
censusstate_genderedu <- censusstate_genderedu_white %>%
  left_join(censusstate_genderedu_black, by='GEOID') %>%
  left_join(censusstate_genderedu_aian, by='GEOID') %>%
  left_join(censusstate_genderedu_asian, by='GEOID') %>%
  left_join(censusstate_genderedu_nhpi, by='GEOID') %>%
  left_join(censusstate_genderedu_other, by='GEOID') %>%
  left_join(censusstate_genderedu_mixed, by='GEOID') %>%
  left_join(censusstate_genderedu_white_nothispanic, by='GEOID') %>%
  left_join(censusstate_genderedu_hispanic, by='GEOID')

names(censusstate_genderedu)[2] <- "full_geoid"  

censusstate_genderedu <- censusstate_genderedu %>% select(-contains("NAME"))

censusstate_genderedu <- censusstate_genderedu %>%
  group_by(GEOID,full_geoid) %>%
  mutate(
    BLACK_TOTAL = sum(male_black_total+female_black_total),
    BLACK_FEMALE = sum(female_black_total), 
    BLACK_MALE = sum(male_black_total),
    BLACK_LESS_HS_TOTAL = sum(male_black_less_hs+female_black_less_hs),
    BLACK_HS_TOTAL = sum(male_black_hs_grad+female_black_hs_grad),
    BLACK_SOME_COLLEGE_TOTAL = sum(male_black_some_college+female_black_some_college),
    BLACK_COLLEGE_TOTAL = sum(male_black_bs+female_black_bs),
    
    HISP_TOTAL = sum(male_hisp_total+female_hisp_total),
    HISP_FEMALE = sum(female_hisp_total), 
    HISP_MALE = sum(male_hisp_total),
    HISP_LESS_HS_TOTAL = sum(male_hisp_less_hs+female_hisp_less_hs),
    HISP_HS_TOTAL = sum(male_hisp_hs_grad+female_hisp_hs_grad),
    HISP_SOME_COLLEGE_TOTAL = sum(male_hisp_some_college+female_hisp_some_college),
    HISP_COLLEGE_TOTAL = sum(male_hisp_bs+female_hisp_bs),
    
    UNDERREP_EDU_TOTAL = sum(BLACK_TOTAL+HISP_TOTAL),
    UNDERREP_FEMALE_TOTAL = sum(BLACK_FEMALE+HISP_FEMALE),
    UNDERREP_MALE = sum(BLACK_MALE+HISP_MALE),
    UNDERREP_LESS_HS_TOTAL = sum(BLACK_LESS_HS_TOTAL+HISP_LESS_HS_TOTAL),
    UNDERREP_HS_TOTAL = sum(BLACK_HS_TOTAL+HISP_HS_TOTAL),
    UNDERREP_SOME_COLLEGE_TOTAL = sum(BLACK_SOME_COLLEGE_TOTAL+HISP_SOME_COLLEGE_TOTAL),
    UNDERREP_COLLEGE_TOTAL = sum(BLACK_COLLEGE_TOTAL+HISP_COLLEGE_TOTAL),
    
  )

# export
write.csv(censusstate_genderedu,"/Volumes/cbjackson2/ccs-knowledge/census-data/census/state/censusstate_genderedu.csv")


############# County ############# 

censuscounty_genderedu_white = get_acs(
  geography = "county",
  state = "WI",
  variables = c(male_white_total = "C15002A_002E", 
                male_white_less_hs = "C15002A_003E",
                male_white_hs_grad = "C15002A_004E",
                male_white_some_college = "C15002A_005E",
                male_white_bs = "C15002A_006E",
                female_white_total = "C15002A_007E",
                female_white_less_hs = "C15002A_008E",
                female_white_hs_grad = "C15002A_009E",
                female_white_some_college = "C15002A_010E",
                female_white_bs = "C15002A_011E"),
  #table = "C15002A", 
  cache_table = TRUE,
  output = 'wide'
)
censuscounty_genderedu_white <- censuscounty_genderedu_white %>% select(-contains("C15002A"))

censuscounty_genderedu_black = get_acs(
  geography = "county",
  state = "WI",
  variables = c(male_black_total = "C15002B_002E", 
                male_black_less_hs = "C15002B_003E",
                male_black_hs_grad = "C15002B_004E",
                male_black_some_college = "C15002B_005E",
                male_black_bs = "C15002B_006E",
                female_black_total = "C15002B_007E",
                female_black_less_hs = "C15002B_008E",
                female_black_hs_grad = "C15002B_009E",
                female_black_some_college = "C15002B_010E",
                female_black_bs = "C15002B_011E"),
  #table = "C15002B", 
  cache_table = TRUE,
  output = 'wide'
)
censuscounty_genderedu_black <- censuscounty_genderedu_black %>% select(-contains("C15002B"))


censuscounty_genderedu_aian = get_acs(
  geography = "county",
  state = "WI",
  variables = c(male_aian_total = "C15002C_002E", 
                male_aian_less_hs = "C15002C_003E",
                male_aian_hs_grad = "C15002C_004E",
                male_aian_some_college = "C15002C_005E",
                male_aian_bs = "C15002C_006E",
                female_aian_total = "C15002C_007E",
                female_aian_less_hs = "C15002C_008E",
                female_aian_hs_grad = "C15002C_009E",
                female_aian_some_college = "C15002C_010E",
                female_aian_bs = "C15002C_011E"),
  #table = "C15002C", 
  cache_table = TRUE,
  #variables = c(male =),  
  output = 'wide'
)
censuscounty_genderedu_aian <- censuscounty_genderedu_aian %>% select(-contains("C15002C"))


censuscounty_genderedu_asian = get_acs(
  geography = "county",
  state = "WI",
  variables = c(male_asian_total = "C15002D_002E", 
                male_asian_less_hs = "C15002D_003E",
                male_asian_hs_grad = "C15002D_004E",
                male_asian_some_college = "C15002D_005E",
                male_asian_bs = "C15002D_006E",
                female_asian_total = "C15002D_007E",
                female_asian_less_hs = "C15002D_008E",
                female_asian_hs_grad = "C15002D_009E",
                female_asian_some_college = "C15002D_010E",
                female_asian_bs = "C15002D_011E"),
  #table = "C15002D", 
  cache_table = TRUE,
  #variables = c(male =),  
  output = 'wide'
)
censuscounty_genderedu_asian <- censuscounty_genderedu_asian %>% select(-contains("C15002D"))


censuscounty_genderedu_nhpi = get_acs(
  geography = "county",
  state = "WI",
  variables = c(male_nhpi_total = "C15002E_002E", 
                male_nhpi_less_hs = "C15002E_003E",
                male_nhpi_hs_grad = "C15002E_004E",
                male_nhpi_some_college = "C15002E_005E",
                male_nhpi_bs = "C15002E_006E",
                female_nhpi_total = "C15002E_007E",
                female_nhpi_less_hs = "C15002E_008E",
                female_nhpi_hs_grad = "C15002E_009E",
                female_nhpi_some_college = "C15002E_010E",
                female_nhpi_bs = "C15002E_011E"),
  #table = "C15002E", 
  cache_table = TRUE,
  #variables = c(male =),  
  output = 'wide'
)
censuscounty_genderedu_nhpi <- censuscounty_genderedu_nhpi %>% select(-contains("C15002E"))


censuscounty_genderedu_other = get_acs(
  geography = "county",
  state = "WI",
  variables = c(male_other_total = "C15002F_002E", 
                male_other_less_hs = "C15002F_003E",
                male_other_hs_grad = "C15002F_004E",
                male_other_some_college = "C15002F_005E",
                male_other_bs = "C15002F_006E",
                female_other_total = "C15002F_007E",
                female_other_less_hs = "C15002F_008E",
                female_other_hs_grad = "C15002F_009E",
                female_other_some_college = "C15002F_010E",
                female_other_bs = "C15002F_011E"),
  #table = "C15002F", 
  cache_table = TRUE,
  #variables = c(male =),  
  output = 'wide'
)
censuscounty_genderedu_other <- censuscounty_genderedu_other %>% select(-contains("C15002F"))


censuscounty_genderedu_mixed = get_acs(
  geography = "county",
  state = "WI",
  variables = c(male_mixed_total = "C15002G_002E", 
                male_mixed_less_hs = "C15002G_003E",
                male_mixed_hs_grad = "C15002G_004E",
                male_mixed_some_college = "C15002G_005E",
                male_mixed_bs = "C15002G_006E",
                female_mixed_total = "C15002G_007E",
                female_mixed_less_hs = "C15002G_008E",
                female_mixed_hs_grad = "C15002G_009E",
                female_mixed_some_college = "C15002G_010E",
                female_mixed_bs = "C15002G_011E"),
  #table = "C15002G", 
  cache_table = TRUE,
  #variables = c(male =),  
  output = 'wide'
)
censuscounty_genderedu_mixed <- censuscounty_genderedu_mixed %>% select(-contains("C15002G"))


censuscounty_genderedu_white_nothispanic = get_acs(
  geography = "county",
  state = "WI",
  variables = c(male_w_nothisp_total = "C15002H_002E", 
                male_w_nothisp_less_hs = "C15002H_003E",
                male_w_nothisp_hs_grad = "C15002H_004E",
                male_w_nothisp_some_college = "C15002H_005E",
                male_w_nothisp_bs = "C15002H_006E",
                female_w_nothisp_total = "C15002H_007E",
                female_w_nothisp_less_hs = "C15002H_008E",
                female_w_nothisp_hs_grad = "C15002H_009E",
                female_w_nothisp_some_college = "C15002H_010E",
                female_w_nothisp_bs = "C15002H_011E"),
  #table = "C15002H", 
  cache_table = TRUE,
  #variables = c(male =),  
  output = 'wide'
)
censuscounty_genderedu_white_nothispanic <- censuscounty_genderedu_white_nothispanic %>% select(-contains("C15002H"))


censuscounty_genderedu_hispanic = get_acs(
  geography = "county",
  state = "WI",
  variables = c(male_hisp_total = "C15002I_002E", 
                male_hisp_less_hs = "C15002I_003E",
                male_hisp_hs_grad = "C15002I_004E",
                male_hisp_some_college = "C15002I_005E",
                male_hisp_bs = "C15002I_006E",
                female_hisp_total = "C15002I_007E",
                female_hisp_less_hs = "C15002I_008E",
                female_hisp_hs_grad = "C15002I_009E",
                female_hisp_some_college = "C15002I_010E",
                female_hisp_bs = "C15002I_011E"),
  #table = "C15002I", 
  cache_table = TRUE,
  #variables = c(male =),  
  output = 'wide'
)
censuscounty_genderedu_hispanic <- censuscounty_genderedu_hispanic %>% select(-contains("C15002I"))


# Merge all datasets above
censuscounty_genderedu <- censuscounty_genderedu_white %>%
  left_join(censuscounty_genderedu_black, by='GEOID') %>%
  left_join(censuscounty_genderedu_aian, by='GEOID') %>%
  left_join(censuscounty_genderedu_asian, by='GEOID') %>%
  left_join(censuscounty_genderedu_nhpi, by='GEOID') %>%
  left_join(censuscounty_genderedu_other, by='GEOID') %>%
  left_join(censuscounty_genderedu_mixed, by='GEOID') %>%
  left_join(censuscounty_genderedu_white_nothispanic, by='GEOID') %>%
  left_join(censuscounty_genderedu_hispanic, by='GEOID')

names(censuscounty_genderedu)[2] <- "full_geoid"  

censuscounty_genderedu <- censuscounty_genderedu %>% select(-contains("NAME"))

censuscounty_genderedu <- censuscounty_genderedu %>%
  group_by(GEOID,full_geoid) %>%
  mutate(
    BLACK_TOTAL = sum(male_black_total+female_black_total),
    BLACK_FEMALE = sum(female_black_total), 
    BLACK_MALE = sum(male_black_total),
    BLACK_LESS_HS_TOTAL = sum(male_black_less_hs+female_black_less_hs),
    BLACK_HS_TOTAL = sum(male_black_hs_grad+female_black_hs_grad),
    BLACK_SOME_COLLEGE_TOTAL = sum(male_black_some_college+female_black_some_college),
    BLACK_COLLEGE_TOTAL = sum(male_black_bs+female_black_bs),
    HISP_TOTAL = sum(male_hisp_total+female_hisp_total),
    HISP_FEMALE = sum(female_hisp_total), 
    HISP_MALE = sum(male_hisp_total),
    HISP_LESS_HS_TOTAL = sum(male_hisp_less_hs+female_hisp_less_hs),
    HISP_HS_TOTAL = sum(male_hisp_hs_grad+female_hisp_hs_grad),
    HISP_SOME_COLLEGE_TOTAL = sum(male_hisp_some_college+female_hisp_some_college),
    HISP_COLLEGE_TOTAL = sum(male_hisp_bs+female_hisp_bs),
    
    UNDERREP_EDU_TOTAL = sum(BLACK_TOTAL+HISP_TOTAL),
    UNDERREP_FEMALE_TOTAL = sum(BLACK_FEMALE+HISP_FEMALE),
    UNDERREP_MALE = sum(BLACK_MALE+HISP_MALE),
    UNDERREP_LESS_HS_TOTAL = sum(BLACK_LESS_HS_TOTAL+HISP_LESS_HS_TOTAL),
    UNDERREP_HS_TOTAL = sum(BLACK_HS_TOTAL+HISP_HS_TOTAL),
    UNDERREP_SOME_COLLEGE_TOTAL = sum(BLACK_SOME_COLLEGE_TOTAL+HISP_SOME_COLLEGE_TOTAL),
    UNDERREP_COLLEGE_TOTAL = sum(BLACK_COLLEGE_TOTAL+HISP_COLLEGE_TOTAL),
    
  )

# export
write.csv(censuscounty_genderedu,"/Volumes/cbjackson2/ccs-knowledge/census-data/census/county/censuscounty_genderedu.csv")

################################## 
############### AGE ##############
##################################

############# TRACT ############# 
#https://www.socialexplorer.com/data/ACS2016_5yr/metadata/?ds=ACS16_5yr&table=B01001A 
censustract_age_white = get_acs(
  geography = "tract",
  state = "WI",
  variables = c(
                male_white_age18to19 = "B01001A_007E", 
                male_white_age20to24 = "B01001A_008E",
                male_white_age25to29 = "B01001A_009E",
                male_white_age30to34 = "B01001A_010E",
                male_white_age35to44 = "B01001A_011E",
                male_white_age45to54 = "B01001A_012E",
                male_white_age55to64 = "B01001A_013E",
                male_white_age65to74 = "B01001A_014E",
                male_white_age75to84 = "B01001A_015E",
                male_white_age85over = "B01001A_016E",
                
                
                female_white_age18to19 = "B01001A_022E", 
                female_white_age20to24 = "B01001A_023E",
                female_white_age25to29 = "B01001A_024E",
                female_white_age30to34 = "B01001A_025E",
                female_white_age35to44 = "B01001A_026E",
                female_white_age45to54 = "B01001A_027E",
                female_white_age55to64 = "B01001A_028E",
                female_white_age65to74 = "B01001A_029E",
                female_white_age75to84 = "B01001A_030E",
                female_white_age85over = "B01001A_031E"),
  #table = "B01001A", 
  cache_table = TRUE,
  #variables = c(male =),  
  output = 'wide'
)
censustract_age_white <- censustract_age_white %>% select(-contains("B01001"))


censustract_age_black = get_acs(
  geography = "tract",
  state = "WI",
  variables = c(male_black_age18to19 = "B01001B_007E", 
                male_black_age20to24 = "B01001B_008E",
                male_black_age25to29 = "B01001B_009E",
                male_black_age30to34 = "B01001B_010E",
                male_black_age35to44 = "B01001B_011E",
                male_black_age45to54 = "B01001B_012E",
                male_black_age55to64 = "B01001B_013E",
                male_black_age65to74 = "B01001B_014E",
                male_black_age75to84 = "B01001B_015E",
                male_black_age85over = "B01001B_016E",
                
                female_black_age18to19 = "B01001B_022E", 
                female_black_age20to24 = "B01001B_023E",
                female_black_age25to29 = "B01001B_024E",
                female_black_age30to34 = "B01001B_025E",
                female_black_age35to44 = "B01001B_026E",
                female_black_age45to54 = "B01001B_027E",
                female_black_age55to64 = "B01001B_028E",
                female_black_age65to74 = "B01001B_029E",
                female_black_age75to84 = "B01001B_030E",
                female_black_age85over = "B01001B_031E"),
  #table = "B01001B", 
  cache_table = TRUE,
  #variables = c(male =),  
  output = 'wide'
)
censustract_age_black <- censustract_age_black %>% select(-contains("B01001"))

censustract_age_aian = get_acs(
  geography = "tract",
  state = "WI",
  variables = c(male_aian_age18to19 = "B01001B_007E", 
                male_aian_age20to24 = "B01001B_008E",
                male_aian_age25to29 = "B01001B_009E",
                male_aian_age30to34 = "B01001B_010E",
                male_aian_age35to44 = "B01001B_011E",
                male_aian_age45to54 = "B01001B_012E",
                male_aian_age55to64 = "B01001B_013E",
                male_aian_age65to74 = "B01001B_014E",
                male_aian_age75to84 = "B01001B_015E",
                male_aian_age85over = "B01001B_016E",
                
                female_aian_age18to19 = "B01001B_022E", 
                female_aian_age20to24 = "B01001B_023E",
                female_aian_age25to29 = "B01001B_024E",
                female_aian_age30to34 = "B01001B_025E",
                female_aian_age35to44 = "B01001B_026E",
                female_aian_age45to54 = "B01001B_027E",
                female_aian_age55to64 = "B01001B_028E",
                female_aian_age65to74 = "B01001B_029E",
                female_aian_age75to84 = "B01001B_030E",
                female_aian_age85over = "B01001B_031E"),
  #table = "B01001C", 
  cache_table = TRUE,
  #variables = c(male =),  
  output = 'wide'
)
censustract_age_aian <- censustract_age_aian %>% select(-contains("B01001"))

censustract_age_asian = get_acs(
  geography = "tract",
  state = "WI",
  variables = c(male_asian_age18to19 = "B01001D_007E", 
                male_asian_age20to24 = "B01001D_008E",
                male_asian_age25to29 = "B01001D_009E",
                male_asian_age30to34 = "B01001D_010E",
                male_asian_age35to44 = "B01001D_011E",
                male_asian_age45to54 = "B01001D_012E",
                male_asian_age55to64 = "B01001D_013E",
                male_asian_age65to74 = "B01001D_014E",
                male_asian_age75to84 = "B01001D_015E",
                male_asian_age85over = "B01001D_016E",
                
                female_asian_age18to19 = "B01001D_022E", 
                female_asian_age20to24 = "B01001D_023E",
                female_asian_age25to29 = "B01001D_024E",
                female_asian_age30to34 = "B01001D_025E",
                female_asian_age35to44 = "B01001D_026E",
                female_asian_age45to54 = "B01001D_027E",
                female_asian_age55to64 = "B01001D_028E",
                female_asian_age65to74 = "B01001D_029E",
                female_asian_age75to84 = "B01001D_030E",
                female_asian_age85over = "B01001D_031E"),
  #table = "B01001D", 
  cache_table = TRUE,
  #variables = c(male =),  
  output = 'wide'
)
censustract_age_asian <- censustract_age_asian %>% select(-contains("B01001"))

censustract_age_nhpi = get_acs(
  geography = "tract",
  state = "WI",
  variables = c(
                male_nhpi_age18to19 = "B01001E_007E", 
                male_nhpi_age20to24 = "B01001E_008E",
                male_nhpi_age25to29 = "B01001E_009E",
                male_nhpi_age30to34 = "B01001E_010E",
                male_nhpi_age35to44 = "B01001E_011E",
                male_nhpi_age45to54 = "B01001E_012E",
                male_nhpi_age55to64 = "B01001E_013E",
                male_nhpi_age65to74 = "B01001E_014E",
                male_nhpi_age75to84 = "B01001E_015E",
                male_nhpi_age85over = "B01001E_016E",
                
                female_nhpi_age18to19 = "B01001E_022E", 
                female_nhpi_age20to24 = "B01001E_023E",
                female_nhpi_age25to29 = "B01001E_024E",
                female_nhpi_age30to34 = "B01001E_025E",
                female_nhpi_age35to44 = "B01001E_026E",
                female_nhpi_age45to54 = "B01001E_027E",
                female_nhpi_age55to64 = "B01001E_028E",
                female_nhpi_age65to74 = "B01001E_029E",
                female_nhpi_age75to84 = "B01001E_030E",
                female_nhpi_age85over = "B01001E_031E"),
  #table = "B01001E", 
  cache_table = TRUE,
  #variables = c(male =),  
  output = 'wide'
)
censustract_age_nhpi <- censustract_age_nhpi %>% select(-contains("B01001"))

censustract_age_other = get_acs(
  geography = "tract",
  state = "WI",
  variables = c(male_other_age18to19 = "B01001F_007E", 
                male_other_age20to24 = "B01001F_008E",
                male_other_age25to29 = "B01001F_009E",
                male_other_age30to34 = "B01001F_010E",
                male_other_age35to44 = "B01001F_011E",
                male_other_age45to54 = "B01001F_012E",
                male_other_age55to64 = "B01001F_013E",
                male_other_age65to74 = "B01001F_014E",
                male_other_age75to84 = "B01001F_015E",
                male_other_age85over = "B01001F_016E",
                
                female_other_age18to19 = "B01001F_022E", 
                female_other_age20to24 = "B01001F_023E",
                female_other_age25to29 = "B01001F_024E",
                female_other_age30to34 = "B01001F_025E",
                female_other_age35to44 = "B01001F_026E",
                female_other_age45to54 = "B01001F_027E",
                female_other_age55to64 = "B01001F_028E",
                female_other_age65to74 = "B01001F_029E",
                female_other_age75to84 = "B01001F_030E",
                female_other_age85over = "B01001F_031E"),
  #table = "B01001F", 
  cache_table = TRUE,
  #variables = c(male =),  
  output = 'wide'
)
censustract_age_other <- censustract_age_other %>% select(-contains("B01001"))

censustract_age_mixed = get_acs(
  geography = "tract",
  state = "WI",
  variables = c(male_mixed_age18to19 = "B01001G_007E", 
                male_mixed_age20to24 = "B01001G_008E",
                male_mixed_age25to29 = "B01001G_009E",
                male_mixed_age30to34 = "B01001G_010E",
                male_mixed_age35to44 = "B01001G_011E",
                male_mixed_age45to54 = "B01001G_012E",
                male_mixed_age55to64 = "B01001G_013E",
                male_mixed_age65to74 = "B01001G_014E",
                male_mixed_age75to84 = "B01001G_015E",
                male_mixed_age85over = "B01001G_016E",
                
                female_mixed_age18to19 = "B01001G_022E", 
                female_mixed_age20to24 = "B01001G_023E",
                female_mixed_age25to29 = "B01001G_024E",
                female_mixed_age30to34 = "B01001G_025E",
                female_mixed_age35to44 = "B01001G_026E",
                female_mixed_age45to54 = "B01001G_027E",
                female_mixed_age55to64 = "B01001G_028E",
                female_mixed_age65to74 = "B01001G_029E",
                female_mixed_age75to84 = "B01001G_030E",
                female_mixed_age85over = "B01001G_031E"),
  #table = "B01001G", 
  cache_table = TRUE,
  #variables = c(male =),  
  output = 'wide'
)
censustract_age_mixed <- censustract_age_mixed %>% select(-contains("B01001"))



censustract_age_white_nothispanic = get_acs(
  geography = "tract",
  state = "WI",
  variables = c(male_w_nothisp_age18to19 = "B01001H_007E", 
                male_w_nothisp_age20to24 = "B01001H_008E",
                male_w_nothisp_age25to29 = "B01001H_009E",
                male_w_nothisp_age30to34 = "B01001H_010E",
                male_w_nothisp_age35to44 = "B01001H_011E",
                male_w_nothisp_age45to54 = "B01001H_012E",
                male_w_nothisp_age55to64 = "B01001H_013E",
                male_w_nothisp_age65to74 = "B01001H_014E",
                male_w_nothisp_age75to84 = "B01001H_015E",
                male_w_nothisp_age85over = "B01001H_016E",
                
                female_w_nothisp_age18to19 = "B01001H_022E", 
                female_w_nothisp_age20to24 = "B01001H_023E",
                female_w_nothisp_age25to29 = "B01001H_024E",
                female_w_nothisp_age30to34 = "B01001H_025E",
                female_w_nothisp_age35to44 = "B01001H_026E",
                female_w_nothisp_age45to54 = "B01001H_027E",
                female_w_nothisp_age55to64 = "B01001H_028E",
                female_w_nothisp_age65to74 = "B01001H_029E",
                female_w_nothisp_age75to84 = "B01001H_030E",
                female_w_nothisp_age85over = "B01001H_031E"),
  #table = "B01001H", 
  cache_table = TRUE,
  #variables = c(male =),  
  output = 'wide'
)
censustract_age_white_nothispanic <- censustract_age_white_nothispanic %>% select(-contains("B01001"))


censustract_age_hispanic = get_acs(
  geography = "tract",
  state = "WI",
  variables = c(male_hisp_age18to19 = "B01001I_007E", 
                male_hisp_age20to24 = "B01001I_008E",
                male_hisp_age25to29 = "B01001I_009E",
                male_hisp_age30to34 = "B01001I_010E",
                male_hisp_age35to44 = "B01001I_011E",
                male_hisp_age45to54 = "B01001I_012E",
                male_hisp_age55to64 = "B01001I_013E",
                male_hisp_age65to74 = "B01001I_014E",
                male_hisp_age75to84 = "B01001I_015E",
                male_hisp_age85over = "B01001I_016E",
                
                female_hisp_age18to19 = "B01001I_022E", 
                female_hisp_age20to24 = "B01001I_023E",
                female_hisp_age25to29 = "B01001I_024E",
                female_hisp_age30to34 = "B01001I_025E",
                female_hisp_age35to44 = "B01001I_026E",
                female_hisp_age45to54 = "B01001I_027E",
                female_hisp_age55to64 = "B01001I_028E",
                female_hisp_age65to74 = "B01001I_029E",
                female_hisp_age75to84 = "B01001I_030E",
                female_hisp_age85over = "B01001I_031E"),
  #table = "B01001I", 
  cache_table = TRUE,
  #variables = c(male =),  
  output = 'wide'
)
censustract_age_hispanic <- censustract_age_hispanic %>% select(-contains("B01001"))

# Merge all datasets above
censustract_age<- censustract_age_white %>%
  left_join(censustract_age_black, by='GEOID') %>%
  left_join(censustract_age_aian, by='GEOID') %>%
  left_join(censustract_age_asian, by='GEOID') %>%
  left_join(censustract_age_nhpi, by='GEOID') %>%
  left_join(censustract_age_other, by='GEOID') %>%
  left_join(censustract_age_mixed, by='GEOID') %>%
  left_join(censustract_age_white_nothispanic, by='GEOID') %>%
  left_join(censustract_age_hispanic, by='GEOID')

names(censustract_age)[2] <- "full_geoid"  

censustract_age <- censustract_age %>% select(-contains("NAME"))

censustract_age <- censustract_age %>%
  group_by(GEOID,full_geoid) %>%
  mutate(
    BLACK_FEMALE = sum(female_black_age18to19+female_black_age20to24+female_black_age25to29+female_black_age30to34+
                         female_black_age35to44+female_black_age45to54+female_black_age65to74+female_black_age75to84+
                         female_black_age85over), 
    BLACK_MALE = sum(male_black_age18to19+male_black_age20to24+male_black_age25to29+male_black_age30to34+
                       male_black_age35to44+male_black_age45to54+male_black_age65to74+male_black_age75to84+
                       male_black_age85over),
    BLACK_TOTAL = sum(BLACK_FEMALE+BLACK_MALE),
    BLACK_18_to_24 = sum(male_black_age18to19+male_black_age20to24+female_black_age18to19+female_black_age20to24),
    BLACK_25_to_34 = sum(male_black_age25to29+male_black_age30to34+female_black_age25to29+female_black_age30to34),
    BLACK_35_to_44 = sum(male_black_age35to44+female_black_age35to44),
    BLACK_45_to_54 = sum(male_black_age45to54+female_black_age45to54),
    BLACK_55_to_64 = sum(male_black_age55to64+female_black_age55to64),
    BLACK_65_over =  sum(male_black_age65to74+male_black_age75to84+male_black_age85over+ 
                         female_black_age65to74+female_black_age75to84+female_black_age85over),
    
    HISP_FEMALE = sum(female_hisp_age18to19+female_hisp_age20to24+female_hisp_age25to29+female_hisp_age30to34+
                         female_hisp_age35to44+female_hisp_age45to54+female_hisp_age65to74+female_hisp_age75to84+
                         female_hisp_age85over), 
    HISP_MALE = sum(male_hisp_age18to19+male_hisp_age20to24+male_hisp_age25to29+male_hisp_age30to34+
                       male_hisp_age35to44+male_hisp_age45to54+male_hisp_age65to74+male_hisp_age75to84+
                       male_hisp_age85over),
    HISP_TOTAL = sum(HISP_FEMALE+HISP_MALE),
    HISP_18_to_24 = sum(male_hisp_age18to19+male_hisp_age20to24+female_hisp_age18to19+female_hisp_age20to24),
    HISP_25_to_34 = sum(male_hisp_age25to29+male_hisp_age30to34+female_hisp_age25to29+female_hisp_age30to34),
    HISP_35_to_44 = sum(male_hisp_age35to44+female_hisp_age35to44),
    HISP_45_to_54 = sum(male_hisp_age45to54+female_hisp_age45to54),
    HISP_55_to_64 = sum(male_hisp_age55to64+female_hisp_age55to64),
    HISP_65_over =  sum(male_hisp_age65to74+male_hisp_age75to84+male_hisp_age85over+ 
                           female_hisp_age65to74+female_hisp_age75to84+female_hisp_age85over),
    
    UNDERREP_FEMALE = sum(BLACK_FEMALE+HISP_FEMALE), 
    UNDERREP_MALE = sum(BLACK_MALE+HISP_MALE),
    UNDERREP_TOTAL = sum(UNDERREP_FEMALE+UNDERREP_MALE),
    UNDERREP_18_to_24 = sum(BLACK_18_to_24+HISP_18_to_24),
    UNDERREP_25_to_34 = sum(BLACK_25_to_34+HISP_25_to_34),
    UNDERREP_35_to_44 = sum(BLACK_35_to_44+HISP_35_to_44),
    UNDERREP_45_to_54 = sum(BLACK_45_to_54+HISP_45_to_54),
    UNDERREP_55_to_64 = sum(BLACK_55_to_64+HISP_55_to_64),
    UNDERREP_65_over =  sum(BLACK_65_over+HISP_65_over)
    
  )


# export
write.csv(censustract_age,"/Volumes/cbjackson2/ccs-knowledge/census-data/census/tract/censustract_age.csv")

###############################
############ COUNTY ###########
###############################

censuscounty_age_white = get_acs(
  geography = "county",
  state = "WI",
  variables = c(male_white_age18to19 = "B01001A_007E", 
                male_white_age20to24 = "B01001A_008E",
                male_white_age25to29 = "B01001A_009E",
                male_white_age30to34 = "B01001A_010E",
                male_white_age35to44 = "B01001A_011E",
                male_white_age45to54 = "B01001A_012E",
                male_white_age55to64 = "B01001A_013E",
                male_white_age65to74 = "B01001A_014E",
                male_white_age75to84 = "B01001A_015E",
                male_white_age85over = "B01001A_016E",
                
                female_white_age18to19 = "B01001A_022E", 
                female_white_age20to24 = "B01001A_023E",
                female_white_age25to29 = "B01001A_024E",
                female_white_age30to34 = "B01001A_025E",
                female_white_age35to44 = "B01001A_026E",
                female_white_age45to54 = "B01001A_027E",
                female_white_age55to64 = "B01001A_028E",
                female_white_age65to74 = "B01001A_029E",
                female_white_age75to84 = "B01001A_030E",
                female_white_age85over = "B01001A_031E"),
  #table = "B01001A", 
  cache_table = TRUE,
  #variables = c(male =),  
  output = 'wide'
)
censuscounty_age_white <- censuscounty_age_white %>% select(-contains("B01001"))


censuscounty_age_black = get_acs(
  geography = "county",
  state = "WI",
  variables = c(male_black_age18to19 = "B01001B_007E", 
                male_black_age20to24 = "B01001B_008E",
                male_black_age25to29 = "B01001B_009E",
                male_black_age30to34 = "B01001B_010E",
                male_black_age35to44 = "B01001B_011E",
                male_black_age45to54 = "B01001B_012E",
                male_black_age55to64 = "B01001B_013E",
                male_black_age65to74 = "B01001B_014E",
                male_black_age75to84 = "B01001B_015E",
                male_black_age85over = "B01001B_016E",
                
                female_black_age18to19 = "B01001B_022E", 
                female_black_age20to24 = "B01001B_023E",
                female_black_age25to29 = "B01001B_024E",
                female_black_age30to34 = "B01001B_025E",
                female_black_age35to44 = "B01001B_026E",
                female_black_age45to54 = "B01001B_027E",
                female_black_age55to64 = "B01001B_028E",
                female_black_age65to74 = "B01001B_029E",
                female_black_age75to84 = "B01001B_030E",
                female_black_age85over = "B01001B_031E"),
  #table = "B01001B", 
  cache_table = TRUE,
  #variables = c(male =),  
  output = 'wide'
)
censuscounty_age_black <- censuscounty_age_black %>% select(-contains("B01001"))

censuscounty_age_aian = get_acs(
  geography = "county",
  state = "WI",
  variables = c(male_aian_age18to19 = "B01001B_007E", 
                male_aian_age20to24 = "B01001B_008E",
                male_aian_age25to29 = "B01001B_009E",
                male_aian_age30to34 = "B01001B_010E",
                male_aian_age35to44 = "B01001B_011E",
                male_aian_age45to54 = "B01001B_012E",
                male_aian_age55to64 = "B01001B_013E",
                male_aian_age65to74 = "B01001B_014E",
                male_aian_age75to84 = "B01001B_015E",
                male_aian_age85over = "B01001B_016E",
                
                female_aian_age18to19 = "B01001B_022E", 
                female_aian_age20to24 = "B01001B_023E",
                female_aian_age25to29 = "B01001B_024E",
                female_aian_age30to34 = "B01001B_025E",
                female_aian_age35to44 = "B01001B_026E",
                female_aian_age45to54 = "B01001B_027E",
                female_aian_age55to64 = "B01001B_028E",
                female_aian_age65to74 = "B01001B_029E",
                female_aian_age75to84 = "B01001B_030E",
                female_aian_age85over = "B01001B_031E"),
  #table = "B01001C", 
  cache_table = TRUE,
  #variables = c(male =),  
  output = 'wide'
)
censuscounty_age_aian <- censuscounty_age_aian %>% select(-contains("B01001"))

censuscounty_age_asian = get_acs(
  geography = "county",
  state = "WI",
  variables = c(male_asian_age18to19 = "B01001D_007E", 
                male_asian_age20to24 = "B01001D_008E",
                male_asian_age25to29 = "B01001D_009E",
                male_asian_age30to34 = "B01001D_010E",
                male_asian_age35to44 = "B01001D_011E",
                male_asian_age45to54 = "B01001D_012E",
                male_asian_age55to64 = "B01001D_013E",
                male_asian_age65to74 = "B01001D_014E",
                male_asian_age75to84 = "B01001D_015E",
                male_asian_age85over = "B01001D_016E",
                
                female_asian_age18to19 = "B01001D_022E", 
                female_asian_age20to24 = "B01001D_023E",
                female_asian_age25to29 = "B01001D_024E",
                female_asian_age30to34 = "B01001D_025E",
                female_asian_age35to44 = "B01001D_026E",
                female_asian_age45to54 = "B01001D_027E",
                female_asian_age55to64 = "B01001D_028E",
                female_asian_age65to74 = "B01001D_029E",
                female_asian_age75to84 = "B01001D_030E",
                female_asian_age85over = "B01001D_031E"),
  #table = "B01001D", 
  cache_table = TRUE,
  #variables = c(male =),  
  output = 'wide'
)
censuscounty_age_asian <- censuscounty_age_asian %>% select(-contains("B01001"))

censuscounty_age_nhpi = get_acs(
  geography = "county",
  state = "WI",
  variables = c(male_nhpi_age18to19 = "B01001E_007E", 
                male_nhpi_age20to24 = "B01001E_008E",
                male_nhpi_age25to29 = "B01001E_009E",
                male_nhpi_age30to34 = "B01001E_010E",
                male_nhpi_age35to44 = "B01001E_011E",
                male_nhpi_age45to54 = "B01001E_012E",
                male_nhpi_age55to64 = "B01001E_013E",
                male_nhpi_age65to74 = "B01001E_014E",
                male_nhpi_age75to84 = "B01001E_015E",
                male_nhpi_age85over = "B01001E_016E",
                
                female_nhpi_age18to19 = "B01001E_022E", 
                female_nhpi_age20to24 = "B01001E_023E",
                female_nhpi_age25to29 = "B01001E_024E",
                female_nhpi_age30to34 = "B01001E_025E",
                female_nhpi_age35to44 = "B01001E_026E",
                female_nhpi_age45to54 = "B01001E_027E",
                female_nhpi_age55to64 = "B01001E_028E",
                female_nhpi_age65to74 = "B01001E_029E",
                female_nhpi_age75to84 = "B01001E_030E",
                female_nhpi_age85over = "B01001E_031E"),
  #table = "B01001E", 
  cache_table = TRUE,
  #variables = c(male =),  
  output = 'wide'
)
censuscounty_age_nhpi <- censuscounty_age_nhpi %>% select(-contains("B01001"))

censuscounty_age_other = get_acs(
  geography = "county",
  state = "WI",
  variables = c(male_other_age18to19 = "B01001F_007E", 
                male_other_age20to24 = "B01001F_008E",
                male_other_age25to29 = "B01001F_009E",
                male_other_age30to34 = "B01001F_010E",
                male_other_age35to44 = "B01001F_011E",
                male_other_age45to54 = "B01001F_012E",
                male_other_age55to64 = "B01001F_013E",
                male_other_age65to74 = "B01001F_014E",
                male_other_age75to84 = "B01001F_015E",
                male_other_age85over = "B01001F_016E",
                
                female_other_age18to19 = "B01001F_022E", 
                female_other_age20to24 = "B01001F_023E",
                female_other_age25to29 = "B01001F_024E",
                female_other_age30to34 = "B01001F_025E",
                female_other_age35to44 = "B01001F_026E",
                female_other_age45to54 = "B01001F_027E",
                female_other_age55to64 = "B01001F_028E",
                female_other_age65to74 = "B01001F_029E",
                female_other_age75to84 = "B01001F_030E",
                female_other_age85over = "B01001F_031E"),
  #table = "B01001F", 
  cache_table = TRUE,
  #variables = c(male =),  
  output = 'wide'
)
censuscounty_age_other <- censuscounty_age_other %>% select(-contains("B01001"))

censuscounty_age_mixed = get_acs(
  geography = "county",
  state = "WI",
  variables = c(male_mixed_age18to19 = "B01001G_007E", 
                male_mixed_age20to24 = "B01001G_008E",
                male_mixed_age25to29 = "B01001G_009E",
                male_mixed_age30to34 = "B01001G_010E",
                male_mixed_age35to44 = "B01001G_011E",
                male_mixed_age45to54 = "B01001G_012E",
                male_mixed_age55to64 = "B01001G_013E",
                male_mixed_age65to74 = "B01001G_014E",
                male_mixed_age75to84 = "B01001G_015E",
                male_mixed_age85over = "B01001G_016E",
                
                female_mixed_age18to19 = "B01001G_022E", 
                female_mixed_age20to24 = "B01001G_023E",
                female_mixed_age25to29 = "B01001G_024E",
                female_mixed_age30to34 = "B01001G_025E",
                female_mixed_age35to44 = "B01001G_026E",
                female_mixed_age45to54 = "B01001G_027E",
                female_mixed_age55to64 = "B01001G_028E",
                female_mixed_age65to74 = "B01001G_029E",
                female_mixed_age75to84 = "B01001G_030E",
                female_mixed_age85over = "B01001G_031E"),
  #table = "B01001G", 
  cache_table = TRUE,
  #variables = c(male =),  
  output = 'wide'
)
censuscounty_age_mixed <- censuscounty_age_mixed %>% select(-contains("B01001"))



censuscounty_age_white_nothispanic = get_acs(
  geography = "county",
  state = "WI",
  variables = c(male_w_nothisp_age18to19 = "B01001H_007E", 
                male_w_nothisp_age20to24 = "B01001H_008E",
                male_w_nothisp_age25to29 = "B01001H_009E",
                male_w_nothisp_age30to34 = "B01001H_010E",
                male_w_nothisp_age35to44 = "B01001H_011E",
                male_w_nothisp_age45to54 = "B01001H_012E",
                male_w_nothisp_age55to64 = "B01001H_013E",
                male_w_nothisp_age65to74 = "B01001H_014E",
                male_w_nothisp_age75to84 = "B01001H_015E",
                male_w_nothisp_age85over = "B01001H_016E",
                
                female_w_nothisp_age18to19 = "B01001H_022E", 
                female_w_nothisp_age20to24 = "B01001H_023E",
                female_w_nothisp_age25to29 = "B01001H_024E",
                female_w_nothisp_age30to34 = "B01001H_025E",
                female_w_nothisp_age35to44 = "B01001H_026E",
                female_w_nothisp_age45to54 = "B01001H_027E",
                female_w_nothisp_age55to64 = "B01001H_028E",
                female_w_nothisp_age65to74 = "B01001H_029E",
                female_w_nothisp_age75to84 = "B01001H_030E",
                female_w_nothisp_age85over = "B01001H_031E"),
  #table = "B01001H", 
  cache_table = TRUE,
  #variables = c(male =),  
  output = 'wide'
)
censuscounty_age_white_nothispanic <- censuscounty_age_white_nothispanic %>% select(-contains("B01001"))


censuscounty_age_hispanic = get_acs(
  geography = "county",
  state = "WI",
  variables = c(male_hisp_age18to19 = "B01001I_007E", 
                male_hisp_age20to24 = "B01001I_008E",
                male_hisp_age25to29 = "B01001I_009E",
                male_hisp_age30to34 = "B01001I_010E",
                male_hisp_age35to44 = "B01001I_011E",
                male_hisp_age45to54 = "B01001I_012E",
                male_hisp_age55to64 = "B01001I_013E",
                male_hisp_age65to74 = "B01001I_014E",
                male_hisp_age75to84 = "B01001I_015E",
                male_hisp_age85over = "B01001I_016E",
                
                female_hisp_age18to19 = "B01001I_022E", 
                female_hisp_age20to24 = "B01001I_023E",
                female_hisp_age25to29 = "B01001I_024E",
                female_hisp_age30to34 = "B01001I_025E",
                female_hisp_age35to44 = "B01001I_026E",
                female_hisp_age45to54 = "B01001I_027E",
                female_hisp_age55to64 = "B01001I_028E",
                female_hisp_age65to74 = "B01001I_029E",
                female_hisp_age75to84 = "B01001I_030E",
                female_hisp_age85over = "B01001I_031E"),
  #table = "B01001I", 
  cache_table = TRUE,
  #variables = c(male =),  
  output = 'wide'
)
censuscounty_age_hispanic <- censuscounty_age_hispanic %>% select(-contains("B01001"))

# Merge all datasets above
censuscounty_age<- censuscounty_age_white %>%
  left_join(censuscounty_age_black, by='GEOID') %>%
  left_join(censuscounty_age_aian, by='GEOID') %>%
  left_join(censuscounty_age_asian, by='GEOID') %>%
  left_join(censuscounty_age_nhpi, by='GEOID') %>%
  left_join(censuscounty_age_other, by='GEOID') %>%
  left_join(censuscounty_age_mixed, by='GEOID') %>%
  left_join(censuscounty_age_white_nothispanic, by='GEOID') %>%
  left_join(censuscounty_age_hispanic, by='GEOID')

names(censuscounty_age)[2] <- "full_geoid"  

censuscounty_age <- censuscounty_age %>% select(-contains("NAME"))

censuscounty_age <- censuscounty_age %>%
  group_by(GEOID,full_geoid) %>%
  mutate(
    BLACK_FEMALE = sum(female_black_age18to19+female_black_age20to24+female_black_age25to29+female_black_age30to34+
                         female_black_age35to44+female_black_age45to54+female_black_age65to74+female_black_age75to84+
                         female_black_age85over), 
    BLACK_MALE = sum(male_black_age18to19+male_black_age20to24+male_black_age25to29+male_black_age30to34+
                       male_black_age35to44+male_black_age45to54+male_black_age65to74+male_black_age75to84+
                       male_black_age85over),
    BLACK_TOTAL = sum(BLACK_FEMALE+BLACK_MALE),
    BLACK_18_to_24 = sum(male_black_age18to19+male_black_age20to24+female_black_age18to19+female_black_age20to24),
    BLACK_25_to_34 = sum(male_black_age25to29+male_black_age30to34+female_black_age25to29+female_black_age30to34),
    BLACK_35_to_44 = sum(male_black_age35to44+female_black_age35to44),
    BLACK_45_to_54 = sum(male_black_age45to54+female_black_age45to54),
    BLACK_55_to_64 = sum(male_black_age55to64+female_black_age55to64),
    BLACK_65_over =  sum(male_black_age65to74+male_black_age75to84+male_black_age85over+ 
                           female_black_age65to74+female_black_age75to84+female_black_age85over),
    
    HISP_FEMALE = sum(female_hisp_age18to19+female_hisp_age20to24+female_hisp_age25to29+female_hisp_age30to34+
                        female_hisp_age35to44+female_hisp_age45to54+female_hisp_age65to74+female_hisp_age75to84+
                        female_hisp_age85over), 
    HISP_MALE = sum(male_hisp_age18to19+male_hisp_age20to24+male_hisp_age25to29+male_hisp_age30to34+
                      male_hisp_age35to44+male_hisp_age45to54+male_hisp_age65to74+male_hisp_age75to84+
                      male_hisp_age85over),
    HISP_TOTAL = sum(HISP_FEMALE+HISP_MALE),
    HISP_18_to_24 = sum(male_hisp_age18to19+male_hisp_age20to24+female_hisp_age18to19+female_hisp_age20to24),
    HISP_25_to_34 = sum(male_hisp_age25to29+male_hisp_age30to34+female_hisp_age25to29+female_hisp_age30to34),
    HISP_35_to_44 = sum(male_hisp_age35to44+female_hisp_age35to44),
    HISP_45_to_54 = sum(male_hisp_age45to54+female_hisp_age45to54),
    HISP_55_to_64 = sum(male_hisp_age55to64+female_hisp_age55to64),
    HISP_65_over =  sum(male_hisp_age65to74+male_hisp_age75to84+male_hisp_age85over+ 
                          female_hisp_age65to74+female_hisp_age75to84+female_hisp_age85over),
    
    UNDERREP_FEMALE = sum(BLACK_FEMALE+HISP_FEMALE), 
    UNDERREP_MALE = sum(BLACK_MALE+HISP_MALE),
    UNDERREP_TOTAL = sum(UNDERREP_FEMALE+UNDERREP_MALE),
    UNDERREP_18_to_24 = sum(BLACK_18_to_24+HISP_18_to_24),
    UNDERREP_25_to_34 = sum(BLACK_25_to_34+HISP_25_to_34),
    UNDERREP_35_to_44 = sum(BLACK_35_to_44+HISP_35_to_44),
    UNDERREP_45_to_54 = sum(BLACK_45_to_54+HISP_45_to_54),
    UNDERREP_55_to_64 = sum(BLACK_55_to_64+HISP_55_to_64),
    UNDERREP_65_over =  sum(BLACK_65_over+HISP_65_over)
    
  )

# export
write.csv(censuscounty_age,"/Volumes/cbjackson2/ccs-knowledge/census-data/census/county/censuscounty_age.csv")


###############################
############ STATE ############
###############################

censusstate_age_white = get_acs(
  geography = "state",
  state = "WI",
  variables = c(male_white_age18to19 = "B01001A_007E", 
                male_white_age20to24 = "B01001A_008E",
                male_white_age25to29 = "B01001A_009E",
                male_white_age30to34 = "B01001A_010E",
                male_white_age35to44 = "B01001A_011E",
                male_white_age45to54 = "B01001A_012E",
                male_white_age55to64 = "B01001A_013E",
                male_white_age65to74 = "B01001A_014E",
                male_white_age75to84 = "B01001A_015E",
                male_white_age85over = "B01001A_016E",
                
                female_white_age18to19 = "B01001A_022E", 
                female_white_age20to24 = "B01001A_023E",
                female_white_age25to29 = "B01001A_024E",
                female_white_age30to34 = "B01001A_025E",
                female_white_age35to44 = "B01001A_026E",
                female_white_age45to54 = "B01001A_027E",
                female_white_age55to64 = "B01001A_028E",
                female_white_age65to74 = "B01001A_029E",
                female_white_age75to84 = "B01001A_030E",
                female_white_age85over = "B01001A_031E"),
  #table = "B01001A", 
  cache_table = TRUE,
  #variables = c(male =),  
  output = 'wide'
)
censusstate_age_white <- censusstate_age_white %>% select(-contains("B01001A"))


censusstate_age_black = get_acs(
  geography = "state",
  state = "WI",
  variables = c(male_black_age18to19 = "B01001B_007E", 
                male_black_age20to24 = "B01001B_008E",
                male_black_age25to29 = "B01001B_009E",
                male_black_age30to34 = "B01001B_010E",
                male_black_age35to44 = "B01001B_011E",
                male_black_age45to54 = "B01001B_012E",
                male_black_age55to64 = "B01001B_013E",
                male_black_age65to74 = "B01001B_014E",
                male_black_age75to84 = "B01001B_015E",
                male_black_age85over = "B01001B_016E",
                
                female_black_age18to19 = "B01001B_022E", 
                female_black_age20to24 = "B01001B_023E",
                female_black_age25to29 = "B01001B_024E",
                female_black_age30to34 = "B01001B_025E",
                female_black_age35to44 = "B01001B_026E",
                female_black_age45to54 = "B01001B_027E",
                female_black_age55to64 = "B01001B_028E",
                female_black_age65to74 = "B01001B_029E",
                female_black_age75to84 = "B01001B_030E",
                female_black_age85over = "B01001B_031E"),
  #table = "B01001B", 
  cache_table = TRUE,
  #variables = c(male =),  
  output = 'wide'
)
censusstate_age_black <- censusstate_age_black %>% select(-contains("B01001B"))

censusstate_age_aian = get_acs(
  geography = "state",
  state = "WI",
  variables = c(male_aian_age18to19 = "B01001B_007E", 
                male_aian_age20to24 = "B01001B_008E",
                male_aian_age25to29 = "B01001B_009E",
                male_aian_age30to34 = "B01001B_010E",
                male_aian_age35to44 = "B01001B_011E",
                male_aian_age45to54 = "B01001B_012E",
                male_aian_age55to64 = "B01001B_013E",
                male_aian_age65to74 = "B01001B_014E",
                male_aian_age75to84 = "B01001B_015E",
                male_aian_age85over = "B01001B_016E",
                
                female_aian_age18to19 = "B01001B_022E", 
                female_aian_age20to24 = "B01001B_023E",
                female_aian_age25to29 = "B01001B_024E",
                female_aian_age30to34 = "B01001B_025E",
                female_aian_age35to44 = "B01001B_026E",
                female_aian_age45to54 = "B01001B_027E",
                female_aian_age55to64 = "B01001B_028E",
                female_aian_age65to74 = "B01001B_029E",
                female_aian_age75to84 = "B01001B_030E",
                female_aian_age85over = "B01001B_031E"),
  #table = "B01001C", 
  cache_table = TRUE,
  #variables = c(male =),  
  output = 'wide'
)
censusstate_age_aian <- censusstate_age_aian %>% select(-contains("B01001C"))

censusstate_age_asian = get_acs(
  geography = "state",
  state = "WI",
  variables = c(male_asian_age18to19 = "B01001D_007E", 
                male_asian_age20to24 = "B01001D_008E",
                male_asian_age25to29 = "B01001D_009E",
                male_asian_age30to34 = "B01001D_010E",
                male_asian_age35to44 = "B01001D_011E",
                male_asian_age45to54 = "B01001D_012E",
                male_asian_age55to64 = "B01001D_013E",
                male_asian_age65to74 = "B01001D_014E",
                male_asian_age75to84 = "B01001D_015E",
                male_asian_age85over = "B01001D_016E",
                
                female_asian_age18to19 = "B01001D_022E", 
                female_asian_age20to24 = "B01001D_023E",
                female_asian_age25to29 = "B01001D_024E",
                female_asian_age30to34 = "B01001D_025E",
                female_asian_age35to44 = "B01001D_026E",
                female_asian_age45to54 = "B01001D_027E",
                female_asian_age55to64 = "B01001D_028E",
                female_asian_age65to74 = "B01001D_029E",
                female_asian_age75to84 = "B01001D_030E",
                female_asian_age85over = "B01001D_031E"),
  #table = "B01001D", 
  cache_table = TRUE,
  #variables = c(male =),  
  output = 'wide'
)
censusstate_age_asian <- censusstate_age_asian %>% select(-contains("B01001D"))

censusstate_age_nhpi = get_acs(
  geography = "state",
  state = "WI",
  variables = c(male_nhpi_age18to19 = "B01001E_007E", 
                male_nhpi_age20to24 = "B01001E_008E",
                male_nhpi_age25to29 = "B01001E_009E",
                male_nhpi_age30to34 = "B01001E_010E",
                male_nhpi_age35to44 = "B01001E_011E",
                male_nhpi_age45to54 = "B01001E_012E",
                male_nhpi_age55to64 = "B01001E_013E",
                male_nhpi_age65to74 = "B01001E_014E",
                male_nhpi_age75to84 = "B01001E_015E",
                male_nhpi_age85over = "B01001E_016E",
                
                female_nhpi_age18to19 = "B01001E_022E", 
                female_nhpi_age20to24 = "B01001E_023E",
                female_nhpi_age25to29 = "B01001E_024E",
                female_nhpi_age30to34 = "B01001E_025E",
                female_nhpi_age35to44 = "B01001E_026E",
                female_nhpi_age45to54 = "B01001E_027E",
                female_nhpi_age55to64 = "B01001E_028E",
                female_nhpi_age65to74 = "B01001E_029E",
                female_nhpi_age75to84 = "B01001E_030E",
                female_nhpi_age85over = "B01001E_031E"),
  #table = "B01001E", 
  cache_table = TRUE,
  #variables = c(male =),  
  output = 'wide'
)
censusstate_age_nhpi <- censusstate_age_nhpi %>% select(-contains("B01001E"))

censusstate_age_other = get_acs(
  geography = "state",
  state = "WI",
  variables = c(male_other_age18to19 = "B01001F_007E", 
                male_other_age20to24 = "B01001F_008E",
                male_other_age25to29 = "B01001F_009E",
                male_other_age30to34 = "B01001F_010E",
                male_other_age35to44 = "B01001F_011E",
                male_other_age45to54 = "B01001F_012E",
                male_other_age55to64 = "B01001F_013E",
                male_other_age65to74 = "B01001F_014E",
                male_other_age75to84 = "B01001F_015E",
                male_other_age85over = "B01001F_016E",
                
                female_other_age18to19 = "B01001F_022E", 
                female_other_age20to24 = "B01001F_023E",
                female_other_age25to29 = "B01001F_024E",
                female_other_age30to34 = "B01001F_025E",
                female_other_age35to44 = "B01001F_026E",
                female_other_age45to54 = "B01001F_027E",
                female_other_age55to64 = "B01001F_028E",
                female_other_age65to74 = "B01001F_029E",
                female_other_age75to84 = "B01001F_030E",
                female_other_age85over = "B01001F_031E"),
  #table = "B01001F", 
  cache_table = TRUE,
  #variables = c(male =),  
  output = 'wide'
)
censusstate_age_other <- censusstate_age_other %>% select(-contains("B01001F"))

censusstate_age_mixed = get_acs(
  geography = "state",
  state = "WI",
  variables = c(male_mixed_age18to19 = "B01001G_007E", 
                male_mixed_age20to24 = "B01001G_008E",
                male_mixed_age25to29 = "B01001G_009E",
                male_mixed_age30to34 = "B01001G_010E",
                male_mixed_age35to44 = "B01001G_011E",
                male_mixed_age45to54 = "B01001G_012E",
                male_mixed_age55to64 = "B01001G_013E",
                male_mixed_age65to74 = "B01001G_014E",
                male_mixed_age75to84 = "B01001G_015E",
                male_mixed_age85over = "B01001G_016E",
                
                female_mixed_age18to19 = "B01001G_022E", 
                female_mixed_age20to24 = "B01001G_023E",
                female_mixed_age25to29 = "B01001G_024E",
                female_mixed_age30to34 = "B01001G_025E",
                female_mixed_age35to44 = "B01001G_026E",
                female_mixed_age45to54 = "B01001G_027E",
                female_mixed_age55to64 = "B01001G_028E",
                female_mixed_age65to74 = "B01001G_029E",
                female_mixed_age75to84 = "B01001G_030E",
                female_mixed_age85over = "B01001G_031E"),
  #table = "B01001G", 
  cache_table = TRUE,
  #variables = c(male =),  
  output = 'wide'
)
censusstate_age_mixed <- censusstate_age_mixed %>% select(-contains("B01001G"))



censusstate_age_white_nothispanic = get_acs(
  geography = "state",
  state = "WI",
  variables = c(male_w_nothisp_age18to19 = "B01001H_007E", 
                male_w_nothisp_age20to24 = "B01001H_008E",
                male_w_nothisp_age25to29 = "B01001H_009E",
                male_w_nothisp_age30to34 = "B01001H_010E",
                male_w_nothisp_age35to44 = "B01001H_011E",
                male_w_nothisp_age45to54 = "B01001H_012E",
                male_w_nothisp_age55to64 = "B01001H_013E",
                male_w_nothisp_age65to74 = "B01001H_014E",
                male_w_nothisp_age75to84 = "B01001H_015E",
                male_w_nothisp_age85over = "B01001H_016E",
                
                female_w_nothisp_age18to19 = "B01001H_022E", 
                female_w_nothisp_age20to24 = "B01001H_023E",
                female_w_nothisp_age25to29 = "B01001H_024E",
                female_w_nothisp_age30to34 = "B01001H_025E",
                female_w_nothisp_age35to44 = "B01001H_026E",
                female_w_nothisp_age45to54 = "B01001H_027E",
                female_w_nothisp_age55to64 = "B01001H_028E",
                female_w_nothisp_age65to74 = "B01001H_029E",
                female_w_nothisp_age75to84 = "B01001H_030E",
                female_w_nothisp_age85over = "B01001H_031E"),
  #table = "B01001H", 
  cache_table = TRUE,
  #variables = c(male =),  
  output = 'wide'
)
censusstate_age_white_nothispanic <- censusstate_age_white_nothispanic %>% select(-contains("B01001H"))


censusstate_age_hispanic = get_acs(
  geography = "state",
  state = "WI",
  variables = c(male_hisp_age18to19 = "B01001I_007E", 
                male_hisp_age20to24 = "B01001I_008E",
                male_hisp_age25to29 = "B01001I_009E",
                male_hisp_age30to34 = "B01001I_010E",
                male_hisp_age35to44 = "B01001I_011E",
                male_hisp_age45to54 = "B01001I_012E",
                male_hisp_age55to64 = "B01001I_013E",
                male_hisp_age65to74 = "B01001I_014E",
                male_hisp_age75to84 = "B01001I_015E",
                male_hisp_age85over = "B01001I_016E",
                
                female_hisp_age18to19 = "B01001I_022E", 
                female_hisp_age20to24 = "B01001I_023E",
                female_hisp_age25to29 = "B01001I_024E",
                female_hisp_age30to34 = "B01001I_025E",
                female_hisp_age35to44 = "B01001I_026E",
                female_hisp_age45to54 = "B01001I_027E",
                female_hisp_age55to64 = "B01001I_028E",
                female_hisp_age65to74 = "B01001I_029E",
                female_hisp_age75to84 = "B01001I_030E",
                female_hisp_age85over = "B01001I_031E"),
  #table = "B01001I", 
  cache_table = TRUE,
  #variables = c(male =),  
  output = 'wide'
)
censusstate_age_hispanic <- censusstate_age_hispanic %>% select(-contains("B01001I"))

# Merge all datasets above
censusstate_age<- censusstate_age_white %>%
  left_join(censusstate_age_black, by='GEOID') %>%
  left_join(censusstate_age_aian, by='GEOID') %>%
  left_join(censusstate_age_asian, by='GEOID') %>%
  left_join(censusstate_age_nhpi, by='GEOID') %>%
  left_join(censusstate_age_other, by='GEOID') %>%
  left_join(censusstate_age_mixed, by='GEOID') %>%
  left_join(censusstate_age_white_nothispanic, by='GEOID') %>%
  left_join(censusstate_age_hispanic, by='GEOID')

names(censusstate_age)[2] <- "full_geoid"  

censusstate_age <- censusstate_age %>% select(-contains("NAME"))

censusstate_age <- censusstate_age %>%
  group_by(GEOID,full_geoid) %>%
  mutate(
    BLACK_FEMALE = sum(female_black_age18to19+female_black_age20to24+female_black_age25to29+female_black_age30to34+
                         female_black_age35to44+female_black_age45to54+female_black_age65to74+female_black_age75to84+
                         female_black_age85over), 
    BLACK_MALE = sum(male_black_age18to19+male_black_age20to24+male_black_age25to29+male_black_age30to34+
                       male_black_age35to44+male_black_age45to54+male_black_age65to74+male_black_age75to84+
                       male_black_age85over),
    BLACK_TOTAL = sum(BLACK_FEMALE+BLACK_MALE),
    BLACK_18_to_24 = sum(male_black_age18to19+male_black_age20to24+female_black_age18to19+female_black_age20to24),
    BLACK_25_to_34 = sum(male_black_age25to29+male_black_age30to34+female_black_age25to29+female_black_age30to34),
    BLACK_35_to_44 = sum(male_black_age35to44+female_black_age35to44),
    BLACK_45_to_54 = sum(male_black_age45to54+female_black_age45to54),
    BLACK_55_to_64 = sum(male_black_age55to64+female_black_age55to64),
    BLACK_65_over =  sum(male_black_age65to74+male_black_age75to84+male_black_age85over+ 
                           female_black_age65to74+female_black_age75to84+female_black_age85over),
    
    HISP_FEMALE = sum(female_hisp_age18to19+female_hisp_age20to24+female_hisp_age25to29+female_hisp_age30to34+
                        female_hisp_age35to44+female_hisp_age45to54+female_hisp_age65to74+female_hisp_age75to84+
                        female_hisp_age85over), 
    HISP_MALE = sum(male_hisp_age18to19+male_hisp_age20to24+male_hisp_age25to29+male_hisp_age30to34+
                      male_hisp_age35to44+male_hisp_age45to54+male_hisp_age65to74+male_hisp_age75to84+
                      male_hisp_age85over),
    HISP_TOTAL = sum(HISP_FEMALE+HISP_MALE),
    HISP_18_to_24 = sum(male_hisp_age18to19+male_hisp_age20to24+female_hisp_age18to19+female_hisp_age20to24),
    HISP_25_to_34 = sum(male_hisp_age25to29+male_hisp_age30to34+female_hisp_age25to29+female_hisp_age30to34),
    HISP_35_to_44 = sum(male_hisp_age35to44+female_hisp_age35to44),
    HISP_45_to_54 = sum(male_hisp_age45to54+female_hisp_age45to54),
    HISP_55_to_64 = sum(male_hisp_age55to64+female_hisp_age55to64),
    HISP_65_over =  sum(male_hisp_age65to74+male_hisp_age75to84+male_hisp_age85over+ 
                          female_hisp_age65to74+female_hisp_age75to84+female_hisp_age85over),
    
    UNDERREP_FEMALE = sum(BLACK_FEMALE+HISP_FEMALE), 
    UNDERREP_MALE = sum(BLACK_MALE+HISP_MALE),
    UNDERREP_TOTAL = sum(UNDERREP_FEMALE+UNDERREP_MALE),
    UNDERREP_18_to_24 = sum(BLACK_18_to_24+HISP_18_to_24),
    UNDERREP_25_to_34 = sum(BLACK_25_to_34+HISP_25_to_34),
    UNDERREP_35_to_44 = sum(BLACK_35_to_44+HISP_35_to_44),
    UNDERREP_45_to_54 = sum(BLACK_45_to_54+HISP_45_to_54),
    UNDERREP_55_to_64 = sum(BLACK_55_to_64+HISP_55_to_64),
    UNDERREP_65_over =  sum(BLACK_65_over+HISP_65_over)
    
  )
# export
write.csv(censusstate_age,"/Volumes/cbjackson2/ccs-knowledge/census-data/census/state/censusstate_age.csv")



################################## 
######### RACE ETHNICITY #########
##################################

############# TRACT ############# 
desired_raceeth = c(
all_over18 = "P3_002N",
hisp_over18 = "P4_002N",
white_over18 = "P3_003N",
baa_over18 = "P3_004N",
amin_over18 = "P3_005N",
asian_over18 = "P3_006N",
nhpi_over18 = "P3_007N",
other_over18 = "P3_008N",
multi_over18 = "P3_009N"
)

censustract_raceeth = get_decennial(
  geography = "tract",
  state = "WI",
  variables = desired_raceeth, 
  summary_var = "P2_001N", 
  year = 2020,
  sumfile = "pl",
  output = 'wide'
)
write.csv(censustract_raceeth,"/Volumes/cbjackson2/ccs-knowledge/census-data/census/tract/censustract_raceeth.csv")

############# Block ############# 
censusblock_raceeth = get_decennial(
  geography = "block",
  state = "WI",
  variables = desired_raceeth, 
  summary_var = "P3_001N", 
  year = 2020,
  sumfile = "pl",
  output = 'wide'
)
write.csv(censusblock_raceeth,"/Volumes/cbjackson2/ccs-knowledge/census-data/census/block/censusblock_raceeth.csv")

############# COUNTY  ############# 
censuscounty_raceeth = get_decennial(
  geography = "county",
  state = "WI",
  variables = desired_raceeth, 
  summary_var = "P3_001N", 
  year = 2020,
  sumfile = "pl",
  output = 'wide'
)
write.csv(censuscounty_raceeth,"/Volumes/cbjackson2/ccs-knowledge/census-data/census/county/censuscounty_raceeth.csv")


############# STATE ############# 
censusstate_raceeth = get_decennial(
  geography = "state",
  state = "WI",
  variables = desired_raceeth, 
  summary_var = "P3_001N", 
  year = 2020,
  sumfile = "pl",
  output = 'wide'
)

write.csv(censusstate_raceeth,"/Volumes/cbjackson2/ccs-knowledge/census-data/census/state/censusstate_raceeth.csv")


################################## 
############# INCOME #############
##################################


############# TRACT ############# 

censustract_income_white = get_acs(
  geography = "tract",
  state = "WI",
  variables = c(white_total = "B19001A_001",
                white_less10k = "B19001A_002", 
                white_10kto14k = "B19001A_003",
                white_15kto19k = "B19001A_004",
                white_20kto24k = "B19001A_005",
                white_25kto29k = "B19001A_006",
                white_30kto34k = "B19001A_007",
                white_35kto39k = "B19001A_008",
                white_40kto44k = "B19001A_009",
                white_45kto49k = "B19001A_010",
                white_50kto59k = "B19001A_011",
                white_60kto74k = "B19001A_012", 
                white_75kto99k = "B19001A_013",
                white_100kto124k = "B19001A_014",
                white_125kto149k = "B19001A_015",
                white_150kto199k = "B19001A_016",
                white_200ktomore = "B19001A_017"),
  #table = "B19001A", 
  cache_table = TRUE,
  #variables = c(male =),  
  output = 'wide'
) %>% 
  select(-ends_with("M"))
censustract_income_white <- censustract_income_white %>% select(-contains("B19001A"))


censustract_income_black = get_acs(
  geography = "tract",
  state = "WI",
  variables = c(black_total = "B19001B_001",
                black_less10k = "B19001B_002", 
                black_10kto14k = "B19001B_003",
                black_15kto19k = "B19001B_004",
                black_20kto24k = "B19001B_005",
                black_25kto29k = "B19001B_006",
                black_30kto34k = "B19001B_007",
                black_35kto39k = "B19001B_008",
                black_40kto44k = "B19001B_009",
                black_45kto49k = "B19001B_010",
                black_50kto59k = "B19001B_011",
                black_60kto74k = "B19001B_012", 
                black_75kto99k = "B19001B_013",
                black_100kto124k = "B19001B_014",
                black_125kto149k = "B19001B_015",
                black_150kto199k = "B19001B_016",
                black_200ktomore = "B19001B_017"),
  #table = "B19001B", 
  cache_table = TRUE,
  #variables = c(male =),  
  output = 'wide'
) %>% 
  select(-ends_with("M"))
censustract_income_black <- censustract_income_black %>% select(-contains("B19001"))


censustract_income_aian = get_acs(
  geography = "tract",
  state = "WI",
  variables = c(aian_total = "B19001C_001",
                aian_less10k = "B19001C_002", 
                aian_10kto14k = "B19001C_003",
                aian_15kto19k = "B19001C_004",
                aian_20kto24k = "B19001C_005",
                aian_25kto29k = "B19001C_006",
                aian_30kto34k = "B19001C_007",
                aian_35kto39k = "B19001C_008",
                aian_40kto44k = "B19001C_009",
                aian_45kto49k = "B19001C_010",
                aian_50kto59k = "B19001C_011",
                aian_60kto74k = "B19001C_012", 
                aian_75kto99k = "B19001C_013",
                aian_100kto124k = "B19001C_014",
                aian_125kto149k = "B19001C_015",
                aian_150kto199k = "B19001C_016",
                aian_200ktomore = "B19001C_017"),
  #table = "B19001C", 
  cache_table = TRUE,
  #variables = c(male =),  
  output = 'wide'
) %>% 
  select(-ends_with("M"))
censustract_income_aian <- censustract_income_aian %>% select(-contains("B19001"))


censustract_income_asian = get_acs(
  geography = "tract",
  state = "WI",
  variables = c(asian_total = "B19001D_001",
                asian_less10k = "B19001D_002", 
                asian_10kto14k = "B19001D_003",
                asian_15kto19k = "B19001D_004",
                asian_20kto24k = "B19001D_005",
                asian_25kto29k = "B19001D_006",
                asian_30kto34k = "B19001D_007",
                asian_35kto39k = "B19001D_008",
                asian_40kto44k = "B19001D_009",
                asian_45kto49k = "B19001D_010",
                asian_50kto59k = "B19001D_011",
                asian_60kto74k = "B19001D_012", 
                asian_75kto99k = "B19001D_013",
                asian_100kto124k = "B19001D_014",
                asian_125kto149k = "B19001D_015",
                asian_150kto199k = "B19001D_016",
                asian_200ktomore = "B19001D_017"),
  #table = "B19001D", 
  cache_table = TRUE,
  #variables = c(male =),  
  output = 'wide'
) %>% 
  select(-ends_with("M"))
censustract_income_asian <- censustract_income_asian %>% select(-contains("B19001"))


censustract_income_nhpi = get_acs(
  geography = "tract",
  state = "WI",
  variables = c(nhpi_total = "B19001E_001",
                nhpi_less10k = "B19001E_002", 
                nhpi_10kto14k = "B19001E_003",
                nhpi_15kto19k = "B19001E_004",
                nhpi_20kto24k = "B19001E_005",
                nhpi_25kto29k = "B19001E_006",
                nhpi_30kto34k = "B19001E_007",
                nhpi_35kto39k = "B19001E_008",
                nhpi_40kto44k = "B19001E_009",
                nhpi_45kto49k = "B19001E_010",
                nhpi_50kto59k = "B19001E_011",
                nhpi_60kto74k = "B19001E_012", 
                nhpi_75kto99k = "B19001E_013",
                nhpi_100kto124k = "B19001E_014",
                nhpi_125kto149k = "B19001E_015",
                nhpi_150kto199k = "B19001E_016",
                nhpi_200ktomore = "B19001E_017"),
  #table = "B19001E", 
  cache_table = TRUE,
  #variables = c(male =),  
  output = 'wide'
) %>% 
  select(-ends_with("M"))
censustract_income_nhpi <- censustract_income_nhpi %>% select(-contains("B19001"))


censustract_income_other = get_acs(
  geography = "tract",
  state = "WI",
  variables = c(other_total = "B19001F_001",
                other_less10k = "B19001F_002", 
                other_10kto14k = "B19001F_003",
                other_15kto19k = "B19001F_004",
                other_20kto24k = "B19001F_005",
                other_25kto29k = "B19001F_006",
                other_30kto34k = "B19001F_007",
                other_35kto39k = "B19001F_008",
                other_40kto44k = "B19001F_009",
                other_45kto49k = "B19001F_010",
                other_50kto59k = "B19001F_011",
                other_60kto74k = "B19001F_012", 
                other_75kto99k = "B19001F_013",
                other_100kto124k = "B19001F_014",
                other_125kto149k = "B19001F_015",
                other_150kto199k = "B19001F_016",
                other_200ktomore = "B19001F_017"),
  #table = "B19001F", 
  cache_table = TRUE,
  #variables = c(male =),  
  output = 'wide'
) %>% 
  select(-ends_with("M"))
censustract_income_other <- censustract_income_other %>% select(-contains("B19001"))


censustract_income_mixed = get_acs(
  geography = "tract",
  state = "WI",
  variables = c(mixed_total = "B19001G_001",
                mixed_less10k = "B19001G_002", 
                mixed_10kto14k = "B19001G_003",
                mixed_15kto19k = "B19001G_004",
                mixed_20kto24k = "B19001G_005",
                mixed_25kto29k = "B19001G_006",
                mixed_30kto34k = "B19001G_007",
                mixed_35kto39k = "B19001G_008",
                mixed_40kto44k = "B19001G_009",
                mixed_45kto49k = "B19001G_010",
                mixed_50kto59k = "B19001G_011",
                mixed_60kto74k = "B19001G_012", 
                mixed_75kto99k = "B19001G_013",
                mixed_100kto124k = "B19001G_014",
                mixed_125kto149k = "B19001G_015",
                mixed_150kto199k = "B19001G_016",
                mixed_200ktomore = "B19001G_017"),
  #table = "B19001G", 
  cache_table = TRUE,
  #variables = c(male =),  
  output = 'wide'
) %>% 
  select(-ends_with("M"))
censustract_income_mixed <- censustract_income_mixed %>% select(-contains("B19001"))


censustract_income_white_nothispanic = get_acs(
  geography = "tract",
  state = "WI",
  variables = c(w_nothisp_total = "B19001H_001",
                w_nothisp_less10k = "B19001H_002", 
                w_nothisp_10kto14k = "B19001H_003",
                w_nothisp_15kto19k = "B19001H_004",
                w_nothisp_20kto24k = "B19001H_005",
                w_nothisp_25kto29k = "B19001H_006",
                w_nothisp_30kto34k = "B19001H_007",
                w_nothisp_35kto39k = "B19001H_008",
                w_nothisp_40kto44k = "B19001H_009",
                w_nothisp_45kto49k = "B19001H_010",
                w_nothisp_50kto59k = "B19001H_011",
                w_nothisp_60kto74k = "B19001H_012", 
                w_nothisp_75kto99k = "B19001H_013",
                w_nothisp_100kto124k = "B19001H_014",
                w_nothisp_125kto149k = "B19001H_015",
                w_nothisp_150kto199k = "B19001H_016",
                w_nothisp_200ktomore = "B19001H_017"),
  #table = "B19001H", 
  cache_table = TRUE,
  #variables = c(male =),  
  output = 'wide'
) %>% 
  select(-ends_with("M"))
censustract_income_white_nothispanic <- censustract_income_white_nothispanic %>% select(-contains("B19001"))

censustract_income_hispanic = get_acs(
  geography = "tract",
  state = "WI",
  variables = c(hisp_total = "B19001I_001",
                hisp_less10k = "B19001I_002", 
                hisp_10kto14k = "B19001I_003",
                hisp_15kto19k = "B19001I_004",
                hisp_20kto24k = "B19001I_005",
                hisp_25kto29k = "B19001I_006",
                hisp_30kto34k = "B19001I_007",
                hisp_35kto39k = "B19001I_008",
                hisp_40kto44k = "B19001I_009",
                hisp_45kto49k = "B19001I_010",
                hisp_50kto59k = "B19001I_011",
                hisp_60kto74k = "B19001I_012", 
                hisp_75kto99k = "B19001I_013",
                hisp_100kto124k = "B19001I_014",
                hisp_125kto149k = "B19001I_015",
                hisp_150kto199k = "B19001I_016",
                hisp_200ktomore = "B19001I_017"),
  #table = "B19001I", 
  cache_table = TRUE,
  #variables = c(male =),  
  output = 'wide'
) %>% 
  select(-ends_with("M"))
censustract_income_hispanic <- censustract_income_hispanic %>% select(-contains("B19001"))


censustract_income <- censustract_income_white %>%
  left_join(censustract_income_black, by='GEOID') %>%
  left_join(censustract_income_aian, by='GEOID') %>%
  left_join(censustract_income_asian, by='GEOID') %>%
  left_join(censustract_income_nhpi, by='GEOID') %>%
  left_join(censustract_income_other, by='GEOID') %>%
  left_join(censustract_income_mixed, by='GEOID') %>%
  left_join(censustract_income_white_nothispanic, by='GEOID') %>%
  left_join(censustract_income_hispanic, by='GEOID')

names(censustract_income)[2] <- "full_geoid"  

censustract_income <- censustract_income %>% select(-contains("NAME"))

censustract_income <- censustract_income %>%
  group_by(GEOID,full_geoid) %>%
  mutate(
    BLACK_less25 = sum(black_less10kE+black_10kto14kE+black_15kto19kE+black_20kto24kE),
    BLACK_25_to_34 = sum(black_25kto29kE+black_30kto34kE),
    BLACK_35_to_49 = sum(black_35kto39kE+black_40kto44kE+black_45kto49kE),
    BLACK_50_to_74 = sum(black_50kto59kE+black_60kto74kE),
    BLACK_75_to_99 = black_75kto99kE,
    BLACK_100_to_149 =  sum(black_100kto124kE+black_125kto149kE),
    BLACK_150_to_199 =  black_150kto199kE,
    BLACK_200_more =  black_200ktomoreE,
    BLACK_TOTAL = black_totalE,
    
    HISP_less25 = sum(hisp_less10kE+hisp_10kto14kE+hisp_15kto19kE+hisp_20kto24kE),
    HISP_25_to_34 = sum(hisp_25kto29kE+hisp_30kto34kE),
    HISP_35_to_49 = sum(hisp_35kto39kE+hisp_40kto44kE+hisp_45kto49kE),
    HISP_50_to_74 = sum(hisp_50kto59kE+hisp_60kto74kE),
    HISP_75_to_99 = hisp_75kto99kE,
    HISP_100_to_149 =  sum(hisp_100kto124kE+hisp_125kto149kE),
    HISP_150_to_199 =  hisp_150kto199kE,
    HISP_200_more =  hisp_200ktomoreE,
    HISP_TOTAL = hisp_totalE,
    
    UNDERREP_less25 = sum(BLACK_less25+HISP_less25),
    UNDERREP_25_to_34 = sum(BLACK_25_to_34+HISP_25_to_34),
    UNDERREP_35_to_49 = sum(BLACK_35_to_49+HISP_35_to_49),
    UNDERREP_50_to_74 = sum(BLACK_50_to_74+HISP_50_to_74),
    UNDERREP_75_to_99 = sum(BLACK_75_to_99+BLACK_75_to_99),
    UNDERREP_100_to_149 =  sum(BLACK_100_to_149+HISP_100_to_149),
    UNDERREP_150_to_199 =  sum(BLACK_150_to_199+HISP_150_to_199),
    UNDERREP_200_more =  sum(BLACK_200_more+HISP_200_more),
    UNDERREP_TOTAL = sum(BLACK_TOTAL+HISP_TOTAL)
    
  )


# export
write.csv(censustract_income,"/Volumes/cbjackson2/ccs-knowledge/census-data/census/tract/censustract_income.csv")



#################################
############# State ############# 
#################################

censusstate_income_white = get_acs(
  geography = "state",
  state = "WI",
  variables = c(white_total = "B19001A_001",
                white_less10k = "B19001A_002", 
                white_10kto14k = "B19001A_003",
                white_15kto19k = "B19001A_004",
                white_20kto24k = "B19001A_005",
                white_25kto29k = "B19001A_006",
                white_30kto34k = "B19001A_007",
                white_35kto39k = "B19001A_008",
                white_40kto44k = "B19001A_009",
                white_45kto49k = "B19001A_010",
                white_50kto59k = "B19001A_011",
                white_60kto74k = "B19001A_012", 
                white_75kto99k = "B19001A_013",
                white_100kto124k = "B19001A_014",
                white_125kto149k = "B19001A_015",
                white_150kto199k = "B19001A_016",
                white_200ktomore = "B19001A_017"),
  #table = "B19001A", 
  cache_table = TRUE,
  #variables = c(male =),  
  output = 'wide'
) %>% 
  select(-ends_with("M"))
censusstate_income_white <- censusstate_income_white %>% select(-contains("B19001A"))


censusstate_income_black = get_acs(
  geography = "state",
  state = "WI",
  variables = c(black_total = "B19001B_001",
                black_less10k = "B19001B_002", 
                black_10kto14k = "B19001B_003",
                black_15kto19k = "B19001B_004",
                black_20kto24k = "B19001B_005",
                black_25kto29k = "B19001B_006",
                black_30kto34k = "B19001B_007",
                black_35kto39k = "B19001B_008",
                black_40kto44k = "B19001B_009",
                black_45kto49k = "B19001B_010",
                black_50kto59k = "B19001B_011",
                black_60kto74k = "B19001B_012", 
                black_75kto99k = "B19001B_013",
                black_100kto124k = "B19001B_014",
                black_125kto149k = "B19001B_015",
                black_150kto199k = "B19001B_016",
                black_200ktomore = "B19001B_017"),
  #table = "B19001B", 
  cache_table = TRUE,
  #variables = c(male =),  
  output = 'wide'
) %>% 
  select(-ends_with("M"))
censusstate_income_black <- censusstate_income_black %>% select(-contains("B19001"))


censusstate_income_aian = get_acs(
  geography = "state",
  state = "WI",
  variables = c(aian_total = "B19001C_001",
                aian_less10k = "B19001C_002", 
                aian_10kto14k = "B19001C_003",
                aian_15kto19k = "B19001C_004",
                aian_20kto24k = "B19001C_005",
                aian_25kto29k = "B19001C_006",
                aian_30kto34k = "B19001C_007",
                aian_35kto39k = "B19001C_008",
                aian_40kto44k = "B19001C_009",
                aian_45kto49k = "B19001C_010",
                aian_50kto59k = "B19001C_011",
                aian_60kto74k = "B19001C_012", 
                aian_75kto99k = "B19001C_013",
                aian_100kto124k = "B19001C_014",
                aian_125kto149k = "B19001C_015",
                aian_150kto199k = "B19001C_016",
                aian_200ktomore = "B19001C_017"),
  #table = "B19001C", 
  cache_table = TRUE,
  #variables = c(male =),  
  output = 'wide'
) %>% 
  select(-ends_with("M"))
censusstate_income_aian <- censusstate_income_aian %>% select(-contains("B19001"))


censusstate_income_asian = get_acs(
  geography = "state",
  state = "WI",
  variables = c(asian_total = "B19001D_001",
                asian_less10k = "B19001D_002", 
                asian_10kto14k = "B19001D_003",
                asian_15kto19k = "B19001D_004",
                asian_20kto24k = "B19001D_005",
                asian_25kto29k = "B19001D_006",
                asian_30kto34k = "B19001D_007",
                asian_35kto39k = "B19001D_008",
                asian_40kto44k = "B19001D_009",
                asian_45kto49k = "B19001D_010",
                asian_50kto59k = "B19001D_011",
                asian_60kto74k = "B19001D_012", 
                asian_75kto99k = "B19001D_013",
                asian_100kto124k = "B19001D_014",
                asian_125kto149k = "B19001D_015",
                asian_150kto199k = "B19001D_016",
                asian_200ktomore = "B19001D_017"),
  #table = "B19001D", 
  cache_table = TRUE,
  #variables = c(male =),  
  output = 'wide'
) %>% 
  select(-ends_with("M"))
censusstate_income_asian <- censusstate_income_asian %>% select(-contains("B19001"))


censusstate_income_nhpi = get_acs(
  geography = "state",
  state = "WI",
  variables = c(nhpi_total = "B19001E_001",
                nhpi_less10k = "B19001E_002", 
                nhpi_10kto14k = "B19001E_003",
                nhpi_15kto19k = "B19001E_004",
                nhpi_20kto24k = "B19001E_005",
                nhpi_25kto29k = "B19001E_006",
                nhpi_30kto34k = "B19001E_007",
                nhpi_35kto39k = "B19001E_008",
                nhpi_40kto44k = "B19001E_009",
                nhpi_45kto49k = "B19001E_010",
                nhpi_50kto59k = "B19001E_011",
                nhpi_60kto74k = "B19001E_012", 
                nhpi_75kto99k = "B19001E_013",
                nhpi_100kto124k = "B19001E_014",
                nhpi_125kto149k = "B19001E_015",
                nhpi_150kto199k = "B19001E_016",
                nhpi_200ktomore = "B19001E_017"),
  #table = "B19001E", 
  cache_table = TRUE,
  #variables = c(male =),  
  output = 'wide'
) %>% 
  select(-ends_with("M"))
censusstate_income_nhpi <- censusstate_income_nhpi %>% select(-contains("B19001"))


censusstate_income_other = get_acs(
  geography = "state",
  state = "WI",
  variables = c(other_total = "B19001F_001",
                other_less10k = "B19001F_002", 
                other_10kto14k = "B19001F_003",
                other_15kto19k = "B19001F_004",
                other_20kto24k = "B19001F_005",
                other_25kto29k = "B19001F_006",
                other_30kto34k = "B19001F_007",
                other_35kto39k = "B19001F_008",
                other_40kto44k = "B19001F_009",
                other_45kto49k = "B19001F_010",
                other_50kto59k = "B19001F_011",
                other_60kto74k = "B19001F_012", 
                other_75kto99k = "B19001F_013",
                other_100kto124k = "B19001F_014",
                other_125kto149k = "B19001F_015",
                other_150kto199k = "B19001F_016",
                other_200ktomore = "B19001F_017"),
  #table = "B19001F", 
  cache_table = TRUE,
  #variables = c(male =),  
  output = 'wide'
) %>% 
  select(-ends_with("M"))
censusstate_income_other <- censusstate_income_other %>% select(-contains("B19001"))


censusstate_income_mixed = get_acs(
  geography = "state",
  state = "WI",
  variables = c(mixed_total = "B19001G_001",
                mixed_less10k = "B19001G_002", 
                mixed_10kto14k = "B19001G_003",
                mixed_15kto19k = "B19001G_004",
                mixed_20kto24k = "B19001G_005",
                mixed_25kto29k = "B19001G_006",
                mixed_30kto34k = "B19001G_007",
                mixed_35kto39k = "B19001G_008",
                mixed_40kto44k = "B19001G_009",
                mixed_45kto49k = "B19001G_010",
                mixed_50kto59k = "B19001G_011",
                mixed_60kto74k = "B19001G_012", 
                mixed_75kto99k = "B19001G_013",
                mixed_100kto124k = "B19001G_014",
                mixed_125kto149k = "B19001G_015",
                mixed_150kto199k = "B19001G_016",
                mixed_200ktomore = "B19001G_017"),
  #table = "B19001G", 
  cache_table = TRUE,
  #variables = c(male =),  
  output = 'wide'
) %>% 
  select(-ends_with("M"))
censusstate_income_mixed <- censusstate_income_mixed %>% select(-contains("B19001"))


censusstate_income_white_nothispanic = get_acs(
  geography = "state",
  state = "WI",
  variables = c(w_nothisp_total = "B19001H_001",
                w_nothisp_less10k = "B19001H_002", 
                w_nothisp_10kto14k = "B19001H_003",
                w_nothisp_15kto19k = "B19001H_004",
                w_nothisp_20kto24k = "B19001H_005",
                w_nothisp_25kto29k = "B19001H_006",
                w_nothisp_30kto34k = "B19001H_007",
                w_nothisp_35kto39k = "B19001H_008",
                w_nothisp_40kto44k = "B19001H_009",
                w_nothisp_45kto49k = "B19001H_010",
                w_nothisp_50kto59k = "B19001H_011",
                w_nothisp_60kto74k = "B19001H_012", 
                w_nothisp_75kto99k = "B19001H_013",
                w_nothisp_100kto124k = "B19001H_014",
                w_nothisp_125kto149k = "B19001H_015",
                w_nothisp_150kto199k = "B19001H_016",
                w_nothisp_200ktomore = "B19001H_017"),
  #table = "B19001H", 
  cache_table = TRUE,
  #variables = c(male =),  
  output = 'wide'
) %>% 
  select(-ends_with("M"))
censusstate_income_white_nothispanic <- censusstate_income_white_nothispanic %>% select(-contains("B19001"))

censusstate_income_hispanic = get_acs(
  geography = "state",
  state = "WI",
  variables = c(hisp_total = "B19001I_001",
                hisp_less10k = "B19001I_002", 
                hisp_10kto14k = "B19001I_003",
                hisp_15kto19k = "B19001I_004",
                hisp_20kto24k = "B19001I_005",
                hisp_25kto29k = "B19001I_006",
                hisp_30kto34k = "B19001I_007",
                hisp_35kto39k = "B19001I_008",
                hisp_40kto44k = "B19001I_009",
                hisp_45kto49k = "B19001I_010",
                hisp_50kto59k = "B19001I_011",
                hisp_60kto74k = "B19001I_012", 
                hisp_75kto99k = "B19001I_013",
                hisp_100kto124k = "B19001I_014",
                hisp_125kto149k = "B19001I_015",
                hisp_150kto199k = "B19001I_016",
                hisp_200ktomore = "B19001I_017"),
  #table = "B19001I", 
  cache_table = TRUE,
  #variables = c(male =),  
  output = 'wide'
) %>% 
  select(-ends_with("M"))
censusstate_income_hispanic <- censusstate_income_hispanic %>% select(-contains("B19001"))


censusstate_income <- censusstate_income_white %>%
  left_join(censusstate_income_black, by='GEOID') %>%
  left_join(censusstate_income_aian, by='GEOID') %>%
  left_join(censusstate_income_asian, by='GEOID') %>%
  left_join(censusstate_income_nhpi, by='GEOID') %>%
  left_join(censusstate_income_other, by='GEOID') %>%
  left_join(censusstate_income_mixed, by='GEOID') %>%
  left_join(censusstate_income_white_nothispanic, by='GEOID') %>%
  left_join(censusstate_income_hispanic, by='GEOID')

names(censusstate_income)[2] <- "full_geoid"  

censusstate_income <- censusstate_income %>% select(-contains("NAME"))

censusstate_income <- censusstate_income %>%
  group_by(GEOID,full_geoid) %>%
  mutate(
    BLACK_less25 = sum(black_less10kE+black_10kto14kE+black_15kto19kE+black_20kto24kE),
    BLACK_25_to_34 = sum(black_25kto29kE+black_30kto34kE),
    BLACK_35_to_49 = sum(black_35kto39kE+black_40kto44kE+black_45kto49kE),
    BLACK_50_to_74 = sum(black_50kto59kE+black_60kto74kE),
    BLACK_75_to_99 = black_75kto99kE,
    BLACK_100_to_149 =  sum(black_100kto124kE+black_125kto149kE),
    BLACK_150_to_199 =  black_150kto199kE,
    BLACK_200_more =  black_200ktomoreE,
    BLACK_TOTAL = black_totalE,
    
    HISP_less25 = sum(hisp_less10kE+hisp_10kto14kE+hisp_15kto19kE+hisp_20kto24kE),
    HISP_25_to_34 = sum(hisp_25kto29kE+hisp_30kto34kE),
    HISP_35_to_49 = sum(hisp_35kto39kE+hisp_40kto44kE+hisp_45kto49kE),
    HISP_50_to_74 = sum(hisp_50kto59kE+hisp_60kto74kE),
    HISP_75_to_99 = hisp_75kto99kE,
    HISP_100_to_149 =  sum(hisp_100kto124kE+hisp_125kto149kE),
    HISP_150_to_199 =  hisp_150kto199kE,
    HISP_200_more =  hisp_200ktomoreE,
    HISP_TOTAL = hisp_totalE,
    
    UNDERREP_less25 = sum(BLACK_less25+HISP_less25),
    UNDERREP_25_to_34 = sum(BLACK_25_to_34+HISP_25_to_34),
    UNDERREP_35_to_49 = sum(BLACK_35_to_49+HISP_35_to_49),
    UNDERREP_50_to_74 = sum(BLACK_50_to_74+HISP_50_to_74),
    UNDERREP_75_to_99 = sum(BLACK_75_to_99+BLACK_75_to_99),
    UNDERREP_100_to_149 =  sum(BLACK_100_to_149+HISP_100_to_149),
    UNDERREP_150_to_199 =  sum(BLACK_150_to_199+HISP_150_to_199),
    UNDERREP_200_more =  sum(BLACK_200_more+HISP_200_more),
    UNDERREP_TOTAL = sum(BLACK_TOTAL+HISP_TOTAL)
    
  )

# export
write.csv(censusstate_income,"/Volumes/cbjackson2/ccs-knowledge/census-data/census/state/censusstate_income.csv")


#################################
############# County ############# 
#################################

censuscounty_income_white = get_acs(
  geography = "county",
  state = "WI",
  variables = c(white_total = "B19001A_001",
                white_less10k = "B19001A_002", 
                white_10kto14k = "B19001A_003",
                white_15kto19k = "B19001A_004",
                white_20kto24k = "B19001A_005",
                white_25kto29k = "B19001A_006",
                white_30kto34k = "B19001A_007",
                white_35kto39k = "B19001A_008",
                white_40kto44k = "B19001A_009",
                white_45kto49k = "B19001A_010",
                white_50kto59k = "B19001A_011",
                white_60kto74k = "B19001A_012", 
                white_75kto99k = "B19001A_013",
                white_100kto124k = "B19001A_014",
                white_125kto149k = "B19001A_015",
                white_150kto199k = "B19001A_016",
                white_200ktomore = "B19001A_017"),
  #table = "B19001A", 
  cache_table = TRUE,
  #variables = c(male =),  
  output = 'wide'
) %>% 
  select(-ends_with("M"))
censuscounty_income_white <- censuscounty_income_white %>% select(-contains("B19001A"))


censuscounty_income_black = get_acs(
  geography = "county",
  state = "WI",
  variables = c(black_total = "B19001B_001",
                black_less10k = "B19001B_002", 
                black_10kto14k = "B19001B_003",
                black_15kto19k = "B19001B_004",
                black_20kto24k = "B19001B_005",
                black_25kto29k = "B19001B_006",
                black_30kto34k = "B19001B_007",
                black_35kto39k = "B19001B_008",
                black_40kto44k = "B19001B_009",
                black_45kto49k = "B19001B_010",
                black_50kto59k = "B19001B_011",
                black_60kto74k = "B19001B_012", 
                black_75kto99k = "B19001B_013",
                black_100kto124k = "B19001B_014",
                black_125kto149k = "B19001B_015",
                black_150kto199k = "B19001B_016",
                black_200ktomore = "B19001B_017"),
  #table = "B19001B", 
  cache_table = TRUE,
  #variables = c(male =),  
  output = 'wide'
) %>% 
  select(-ends_with("M"))
censuscounty_income_black <- censuscounty_income_black %>% select(-contains("B19001"))


censuscounty_income_aian = get_acs(
  geography = "county",
  state = "WI",
  variables = c(aian_total = "B19001C_001",
                aian_less10k = "B19001C_002", 
                aian_10kto14k = "B19001C_003",
                aian_15kto19k = "B19001C_004",
                aian_20kto24k = "B19001C_005",
                aian_25kto29k = "B19001C_006",
                aian_30kto34k = "B19001C_007",
                aian_35kto39k = "B19001C_008",
                aian_40kto44k = "B19001C_009",
                aian_45kto49k = "B19001C_010",
                aian_50kto59k = "B19001C_011",
                aian_60kto74k = "B19001C_012", 
                aian_75kto99k = "B19001C_013",
                aian_100kto124k = "B19001C_014",
                aian_125kto149k = "B19001C_015",
                aian_150kto199k = "B19001C_016",
                aian_200ktomore = "B19001C_017"),
  #table = "B19001C", 
  cache_table = TRUE,
  #variables = c(male =),  
  output = 'wide'
) %>% 
  select(-ends_with("M"))
censuscounty_income_aian <- censuscounty_income_aian %>% select(-contains("B19001"))


censuscounty_income_asian = get_acs(
  geography = "county",
  state = "WI",
  variables = c(asian_total = "B19001D_001",
                asian_less10k = "B19001D_002", 
                asian_10kto14k = "B19001D_003",
                asian_15kto19k = "B19001D_004",
                asian_20kto24k = "B19001D_005",
                asian_25kto29k = "B19001D_006",
                asian_30kto34k = "B19001D_007",
                asian_35kto39k = "B19001D_008",
                asian_40kto44k = "B19001D_009",
                asian_45kto49k = "B19001D_010",
                asian_50kto59k = "B19001D_011",
                asian_60kto74k = "B19001D_012", 
                asian_75kto99k = "B19001D_013",
                asian_100kto124k = "B19001D_014",
                asian_125kto149k = "B19001D_015",
                asian_150kto199k = "B19001D_016",
                asian_200ktomore = "B19001D_017"),
  #table = "B19001D", 
  cache_table = TRUE,
  #variables = c(male =),  
  output = 'wide'
) %>% 
  select(-ends_with("M"))
censuscounty_income_asian <- censuscounty_income_asian %>% select(-contains("B19001"))


censuscounty_income_nhpi = get_acs(
  geography = "county",
  state = "WI",
  variables = c(nhpi_total = "B19001E_001",
                nhpi_less10k = "B19001E_002", 
                nhpi_10kto14k = "B19001E_003",
                nhpi_15kto19k = "B19001E_004",
                nhpi_20kto24k = "B19001E_005",
                nhpi_25kto29k = "B19001E_006",
                nhpi_30kto34k = "B19001E_007",
                nhpi_35kto39k = "B19001E_008",
                nhpi_40kto44k = "B19001E_009",
                nhpi_45kto49k = "B19001E_010",
                nhpi_50kto59k = "B19001E_011",
                nhpi_60kto74k = "B19001E_012", 
                nhpi_75kto99k = "B19001E_013",
                nhpi_100kto124k = "B19001E_014",
                nhpi_125kto149k = "B19001E_015",
                nhpi_150kto199k = "B19001E_016",
                nhpi_200ktomore = "B19001E_017"),
  #table = "B19001E", 
  cache_table = TRUE,
  #variables = c(male =),  
  output = 'wide'
) %>% 
  select(-ends_with("M"))
censuscounty_income_nhpi <- censuscounty_income_nhpi %>% select(-contains("B19001"))


censuscounty_income_other = get_acs(
  geography = "county",
  state = "WI",
  variables = c(other_total = "B19001F_001",
                other_less10k = "B19001F_002", 
                other_10kto14k = "B19001F_003",
                other_15kto19k = "B19001F_004",
                other_20kto24k = "B19001F_005",
                other_25kto29k = "B19001F_006",
                other_30kto34k = "B19001F_007",
                other_35kto39k = "B19001F_008",
                other_40kto44k = "B19001F_009",
                other_45kto49k = "B19001F_010",
                other_50kto59k = "B19001F_011",
                other_60kto74k = "B19001F_012", 
                other_75kto99k = "B19001F_013",
                other_100kto124k = "B19001F_014",
                other_125kto149k = "B19001F_015",
                other_150kto199k = "B19001F_016",
                other_200ktomore = "B19001F_017"),
  #table = "B19001F", 
  cache_table = TRUE,
  #variables = c(male =),  
  output = 'wide'
) %>% 
  select(-ends_with("M"))
censuscounty_income_other <- censuscounty_income_other %>% select(-contains("B19001"))


censuscounty_income_mixed = get_acs(
  geography = "county",
  state = "WI",
  variables = c(mixed_total = "B19001G_001",
                mixed_less10k = "B19001G_002", 
                mixed_10kto14k = "B19001G_003",
                mixed_15kto19k = "B19001G_004",
                mixed_20kto24k = "B19001G_005",
                mixed_25kto29k = "B19001G_006",
                mixed_30kto34k = "B19001G_007",
                mixed_35kto39k = "B19001G_008",
                mixed_40kto44k = "B19001G_009",
                mixed_45kto49k = "B19001G_010",
                mixed_50kto59k = "B19001G_011",
                mixed_60kto74k = "B19001G_012", 
                mixed_75kto99k = "B19001G_013",
                mixed_100kto124k = "B19001G_014",
                mixed_125kto149k = "B19001G_015",
                mixed_150kto199k = "B19001G_016",
                mixed_200ktomore = "B19001G_017"),
  #table = "B19001G", 
  cache_table = TRUE,
  #variables = c(male =),  
  output = 'wide'
) %>% 
  select(-ends_with("M"))
censuscounty_income_mixed <- censuscounty_income_mixed %>% select(-contains("B19001"))


censuscounty_income_white_nothispanic = get_acs(
  geography = "county",
  state = "WI",
  variables = c(w_nothisp_total = "B19001H_001",
                w_nothisp_less10k = "B19001H_002", 
                w_nothisp_10kto14k = "B19001H_003",
                w_nothisp_15kto19k = "B19001H_004",
                w_nothisp_20kto24k = "B19001H_005",
                w_nothisp_25kto29k = "B19001H_006",
                w_nothisp_30kto34k = "B19001H_007",
                w_nothisp_35kto39k = "B19001H_008",
                w_nothisp_40kto44k = "B19001H_009",
                w_nothisp_45kto49k = "B19001H_010",
                w_nothisp_50kto59k = "B19001H_011",
                w_nothisp_60kto74k = "B19001H_012", 
                w_nothisp_75kto99k = "B19001H_013",
                w_nothisp_100kto124k = "B19001H_014",
                w_nothisp_125kto149k = "B19001H_015",
                w_nothisp_150kto199k = "B19001H_016",
                w_nothisp_200ktomore = "B19001H_017"),
  #table = "B19001H", 
  cache_table = TRUE,
  #variables = c(male =),  
  output = 'wide'
) %>% 
  select(-ends_with("M"))
censuscounty_income_white_nothispanic <- censuscounty_income_white_nothispanic %>% select(-contains("B19001"))

censuscounty_income_hispanic = get_acs(
  geography = "county",
  state = "WI",
  variables = c(hisp_total = "B19001I_001",
                hisp_less10k = "B19001I_002", 
                hisp_10kto14k = "B19001I_003",
                hisp_15kto19k = "B19001I_004",
                hisp_20kto24k = "B19001I_005",
                hisp_25kto29k = "B19001I_006",
                hisp_30kto34k = "B19001I_007",
                hisp_35kto39k = "B19001I_008",
                hisp_40kto44k = "B19001I_009",
                hisp_45kto49k = "B19001I_010",
                hisp_50kto59k = "B19001I_011",
                hisp_60kto74k = "B19001I_012", 
                hisp_75kto99k = "B19001I_013",
                hisp_100kto124k = "B19001I_014",
                hisp_125kto149k = "B19001I_015",
                hisp_150kto199k = "B19001I_016",
                hisp_200ktomore = "B19001I_017"),
  #table = "B19001I", 
  cache_table = TRUE,
  #variables = c(male =),  
  output = 'wide'
) %>% 
  select(-ends_with("M"))
censuscounty_income_hispanic <- censuscounty_income_hispanic %>% select(-contains("B19001"))


censuscounty_income <- censuscounty_income_white %>%
  left_join(censuscounty_income_black, by='GEOID') %>%
  left_join(censuscounty_income_aian, by='GEOID') %>%
  left_join(censuscounty_income_asian, by='GEOID') %>%
  left_join(censuscounty_income_nhpi, by='GEOID') %>%
  left_join(censuscounty_income_other, by='GEOID') %>%
  left_join(censuscounty_income_mixed, by='GEOID') %>%
  left_join(censuscounty_income_white_nothispanic, by='GEOID') %>%
  left_join(censuscounty_income_hispanic, by='GEOID')

names(censuscounty_income)[2] <- "full_geoid"  

censuscounty_income <- censuscounty_income %>% select(-contains("NAME"))

censuscounty_income <- censuscounty_income %>%
  group_by(GEOID,full_geoid) %>%
  mutate(
    BLACK_less25 = sum(black_less10kE+black_10kto14kE+black_15kto19kE+black_20kto24kE),
    BLACK_25_to_34 = sum(black_25kto29kE+black_30kto34kE),
    BLACK_35_to_49 = sum(black_35kto39kE+black_40kto44kE+black_45kto49kE),
    BLACK_50_to_74 = sum(black_50kto59kE+black_60kto74kE),
    BLACK_75_to_99 = black_75kto99kE,
    BLACK_100_to_149 =  sum(black_100kto124kE+black_125kto149kE),
    BLACK_150_to_199 =  black_150kto199kE,
    BLACK_200_more =  black_200ktomoreE,
    BLACK_TOTAL = black_totalE,
    
    HISP_less25 = sum(hisp_less10kE+hisp_10kto14kE+hisp_15kto19kE+hisp_20kto24kE),
    HISP_25_to_34 = sum(hisp_25kto29kE+hisp_30kto34kE),
    HISP_35_to_49 = sum(hisp_35kto39kE+hisp_40kto44kE+hisp_45kto49kE),
    HISP_50_to_74 = sum(hisp_50kto59kE+hisp_60kto74kE),
    HISP_75_to_99 = hisp_75kto99kE,
    HISP_100_to_149 =  sum(hisp_100kto124kE+hisp_125kto149kE),
    HISP_150_to_199 =  hisp_150kto199kE,
    HISP_200_more =  hisp_200ktomoreE,
    HISP_TOTAL = hisp_totalE,
    
    UNDERREP_less25 = sum(BLACK_less25+HISP_less25),
    UNDERREP_25_to_34 = sum(BLACK_25_to_34+HISP_25_to_34),
    UNDERREP_35_to_49 = sum(BLACK_35_to_49+HISP_35_to_49),
    UNDERREP_50_to_74 = sum(BLACK_50_to_74+HISP_50_to_74),
    UNDERREP_75_to_99 = sum(BLACK_75_to_99+BLACK_75_to_99),
    UNDERREP_100_to_149 =  sum(BLACK_100_to_149+HISP_100_to_149),
    UNDERREP_150_to_199 =  sum(BLACK_150_to_199+HISP_150_to_199),
    UNDERREP_200_more =  sum(BLACK_200_more+HISP_200_more),
    UNDERREP_TOTAL = sum(BLACK_TOTAL+HISP_TOTAL)
    
  )

# export
write.csv(censuscounty_income,"/Volumes/cbjackson2/ccs-knowledge/census-data/census/county/censuscounty_income.csv")


