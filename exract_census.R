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

################################## 
############# GENDER #############
##################################

############# TRACT ############# 
censustract_gender = get_acs(
  geography = "tract",
  state = "WI",
  variables = c(male ="S0101_C03_026E", 
                female ="S0101_C05_026E"),  
  output = 'wide'
)

write.csv(censustract_gender,"~/Library/CloudStorage/Box-Box/ccs-knowledge/census-data/census/tract/censustract_gender.csv")

############# COUNTY ############# 
censuscounty_gender = get_acs(
  geography = "county",
  state = "WI",
  variables = c(male ="S0101_C03_026E", 
                female ="S0101_C05_026E"),
  output = 'wide'
)
write.csv(censustract_gender,"~/Library/CloudStorage/Box-Box/ccs-knowledge/census-data/census/county/censuscounty_gender.csv")

############# STATE ############# 
censusstate_gender = get_acs(
  geography = "state",
  state = "WI",
  variables = c(male ="S0101_C03_026E", 
                female ="S0101_C05_026E"),
  output = 'wide'
)
write.csv(censusstate_gender,"~/Library/CloudStorage/Box-Box/ccs-knowledge/census-data/census/state/censusstate_gender.csv")


################################## 
############### AGE ##############
##################################
# Common age bands - 18-24, 25-34, 35-44, 45-54, 55-64 and 65 and over
desired_age = c(
  age20_24 = "S0101_C01_006E",
  age25_29 = "S0101_C01_007E",
  age30_34 = "S0101_C01_008E",
  age35_39 = "S0101_C01_009E",
  age40_44 = "S0101_C01_010E",
  age45_49 = "S0101_C01_011E",
  age50_54 = "S0101_C01_012E",
  age55_59 = "S0101_C01_013E",
  age60_64 = "S0101_C01_014E",
  age65_69 = "S0101_C01_015E",
  age70_74 = "S0101_C01_016E",
  age75_79 = "S0101_C01_017E",
  age80_84 = "S0101_C01_018E",
  age85_over = "S0101_C01_019E"
)

############# TRACT ############# 
censustract_age = get_acs(
  geography = "tract",
  state = "WI",
  variables = desired_age, 
  year = 2020,
  output = 'wide'
)

censustract_age <- censustract_age %>%
  group_by(GEOID) %>%
  mutate(
    age20_24 = sum(age20_24),
    age25_34 = sum(age25_29+age30_34),
    age35_44 = sum(age35_39+age40_44),
    age45_54 = sum(age45_49+age50_54),
    age55_64 = sum(age55_59+age60_64),
    age65_over = sum(age65_69+age70_74+age75_79+age80_84+age85_over)
  )
censustract_age <- censustract_age[,c(1,2,31:35)]
write.csv(censustract_age,"~/Library/CloudStorage/Box-Box/ccs-knowledge/census-data/census/tract/censustract_age.csv")

############# COUNTY ############# 
censuscounty_age = get_acs(
  geography = "county",
  state = "WI",
  variables = desired_age, 
  year = 2020,
  output = 'wide'
)

censuscounty_age <- censuscounty_age %>%
  group_by(GEOID) %>%
  mutate(
    age20_24 = sum(age20_24),
    age25_34 = sum(age25_29+age30_34),
    age35_44 = sum(age35_39+age40_44),
    age45_54 = sum(age45_49+age50_54),
    age55_64 = sum(age55_59+age60_64),
    age65_over = sum(age65_69+age70_74+age75_79+age80_84+age85_over)
  )
censuscounty_age <- censuscounty_age[,c(1,2,31:35)]
write.csv(censuscounty_age,"~/Library/CloudStorage/Box-Box/ccs-knowledge/census-data/census/county/censuscounty_age.csv")

############# STATE ############# 
censusstate_age = get_acs(
  geography = "state",
  state = "WI",
  variables = desired_age, 
  output = 'wide'
)

censusstate_age <- censusstate_age %>%
  group_by(GEOID) %>%
  mutate(
    age20_24 = sum(age20_24),
    age25_34 = sum(age25_29+age30_34),
    age35_44 = sum(age35_39+age40_44),
    age45_54 = sum(age45_49+age50_54),
    age55_64 = sum(age55_59+age60_64),
    age65_over = sum(age65_69+age70_74+age75_79+age80_84+age85_over)
  )
censusstate_age <- censusstate_age[,c(1,2,31:35)]
write.csv(censusstate_age,"~/Library/CloudStorage/Box-Box/ccs-knowledge/census-data/census/state/censusstate_age.csv")



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
nhopi_over18 = "P3_007N",
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
write.csv(censustract_raceeth,"~/Library/CloudStorage/Box-Box/ccs-knowledge/census-data/census/tract/censustract_raceeth.csv")

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
write.csv(censusblock_raceeth,"~/Library/CloudStorage/Box-Box/ccs-knowledge/census-data/census/block/censusblock_raceeth.csv")

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
write.csv(censuscounty_raceeth,"~/Library/CloudStorage/Box-Box/ccs-knowledge/census-data/census/county/censuscounty_raceeth.csv")




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

write.csv(censusstate_raceeth,"~/Library/CloudStorage/Box-Box/ccs-knowledge/census-data/census/state/censusstate_raceeth.csv")


################################## 
############# INCOME #############
##################################
desired_income = c(
  hh_less10k = "S1901_C01_002E",
  hh_10k_14k = "S1901_C01_003E",
  hh_15k_24k = "S1901_C01_004E",
  hh_25k_34k = "S1901_C01_005E",
  hh_35k_49k = "S1901_C01_006E",
  hh_50k_74k = "S1901_C01_007E",
  hh_75k_99k = "S1901_C01_008E",
  hh_100k_149k = "S1901_C01_009E",
  hh_150k_199k = "S1901_C01_010E",
  hh_more200k = "S1901_C01_011E"
  
)

############# TRACT #############
censustract_income = get_acs(
  geography = "tract",
  state = "WI",
  variables = desired_income, 
  output = 'wide'
)

censustract_income <- censustract_income %>%
  group_by(GEOID) %>%
  mutate(
    hh_less25k = sum(hh_less10k+hh_10k_14k+hh_15k_24k),
    hh_25_34 = sum(hh_25k_34k),
    hh_35_49 = sum(hh_35k_49k),
    hh_50_74 = sum(hh_50k_74k),
    hh_75_99 = sum(hh_75k_99k),
    hh_100_149 = sum(hh_100k_149k),
    hh_150k_199k = sum(hh_150k_199k),
    hh_more200k = sum(hh_more200k)
  )

write.csv(censustract_income,"~/Library/CloudStorage/Box-Box/ccs-knowledge/census-data/census/tract/censustract_income.csv")

############# COUNTY #############
censuscounty_income = get_acs(
  geography = "county",
  state = "WI",
  variables = desired_income, 
  output = 'wide'
)

censuscounty_income <- censuscounty_income %>%
  group_by(GEOID) %>%
  mutate(
    hh_less25k = sum(hh_less10k+hh_10k_14k+hh_15k_24k),
    hh_25_34 = sum(hh_25k_34k),
    hh_35_49 = sum(hh_35k_49k),
    hh_50_74 = sum(hh_50k_74k),
    hh_75_99 = sum(hh_75k_99k),
    hh_100_149 = sum(hh_100k_149k),
    hh_150k_199k = sum(hh_150k_199k),
    hh_more200k = sum(hh_more200k)
  )

write.csv(censuscounty_income,"~/Library/CloudStorage/Box-Box/ccs-knowledge/census-data/census/county/censuscounty_income.csv")

############# STATE ############# 
censusstate_income = get_acs(
  geography = "state",
  state = "WI",
  variables = desired_income, 
  output = 'wide'
)

censusstate_income <- censusstate_income %>%
  group_by(GEOID) %>%
  mutate(
    hh_less25k = sum(hh_less10k+hh_10k_14k+hh_15k_24k),
    hh_25_34 = sum(hh_25k_34k),
    hh_35_49 = sum(hh_35k_49k),
    hh_50_74 = sum(hh_50k_74k),
    hh_75_99 = sum(hh_75k_99k),
    hh_100_149 = sum(hh_100k_149k),
    hh_150k_199k = sum(hh_150k_199k),
    hh_more200k = sum(hh_more200k)
  )

write.csv(censusstate_income,"~/Library/CloudStorage/Box-Box/ccs-knowledge/census-data/census/state/censusstate_income.csv")



################################## 
############ EDUCATION ###########
##################################
desired_edu = c(
  all_18to24 = "S1501_C01_001E",
  all_25over = "S1501_C01_006E",
  edu_18_24_lessHS = "S1501_C01_002E",
  edu_25over_lesssHS = "S1501_C01_007E",
  edu_18_24_HSequ = "S1501_C01_003E",
  edu_25over_HS_nodegree = "S1501_C01_008E",
  edu_25over_HS_degree_or_equ = "S1501_C01_009E",
  edu_25over_Coll_nodegree = "S1501_C01_010E",
  edu_18_24_assoc = "S1501_C01_004E",
  edu_25over_assoc = "S1501_C01_011E",
  edu_18_24_bach = "S1501_C01_005E",
  edu_25over_bach = "S1501_C01_012E",
  edu_25over_grad = "S1501_C01_013E"
)
############# TRACT #############

censustract_edu  = get_acs(
  geography = "tract",
  state = "WI",
  variables = desired_edu, 
  output = 'wide'
)

censustract_edu <- censustract_edu %>%
  group_by(GEOID) %>%
  mutate(
    edu_lessHS = sum(edu_18_24_lessHS+edu_25over_lesssHS+edu_25over_HS_nodegree),
    edu_HSequ = sum(edu_18_24_HSequ+edu_25over_HS_degree_or_equ),
    edu_somecoll_trade = sum(edu_25over_Coll_nodegree),
    edu_assoc = sum(edu_18_24_assoc+edu_25over_assoc),
    edu_bach = sum(edu_18_24_bach+edu_25over_bach),
    edu_grad_prof = sum(edu_25over_grad)
  )

write.csv(censustract_edu,"~/Library/CloudStorage/Box-Box/ccs-knowledge/census-data/census/tract/censustract_edu.csv")

############# COUNTY #############

censuscounty_edu = get_acs(
  geography = "county",
  state = "WI",
  variables = desired_edu, 
  output = 'wide'
)

censuscounty_edu <- censuscounty_edu %>%
  group_by(GEOID) %>%
  mutate(
    edu_lessHS = sum(edu_18_24_lessHS+edu_25over_lesssHS+edu_25over_HS_nodegree),
    edu_HSequ = sum(edu_18_24_HSequ+edu_25over_HS_degree_or_equ),
    edu_somecoll_trade = sum(edu_25over_Coll_nodegree),
    edu_assoc = sum(edu_18_24_assoc+edu_25over_assoc),
    edu_bach = sum(edu_18_24_bach+edu_25over_bach),
    edu_grad_prof = sum(edu_25over_grad)
  )

write.csv(censuscounty_edu,"~/Library/CloudStorage/Box-Box/ccs-knowledge/census-data/census/county/censuscounty_edu.csv")

############# STATE #############

censusstate_edu = get_acs(
  geography = "state",
  state = "WI",
  variables = desired_edu, 
  output = 'wide'
)

censusstate_edu <- censusstate_edu %>%
  group_by(GEOID) %>%
  mutate(
    edu_lessHS = sum(edu_18_24_lessHS+edu_25over_lesssHS+edu_25over_HS_nodegree),
    edu_HSequ = sum(edu_18_24_HSequ+edu_25over_HS_degree_or_equ),
    edu_somecoll_trade = sum(edu_25over_Coll_nodegree),
    edu_assoc = sum(edu_18_24_assoc+edu_25over_assoc),
    edu_bach = sum(edu_18_24_bach+edu_25over_bach),
    edu_grad_prof = sum(edu_25over_grad)
  )

write.csv(censusstate_edu,"~/Library/CloudStorage/Box-Box/ccs-knowledge/census-data/census/state/censusstate_edu.csv")

