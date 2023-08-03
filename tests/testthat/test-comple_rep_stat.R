library(dplyr)
library(yaml)
library(testit)
library(rlang)

#ownames(tbl_data) = c("Male", "Female", 
#"18_to_24", "25_to_34", 
#"35_to_44", "45_to_54",
#"55_to_64", "65_over",
#"LESS_THAN_HS", "HS", 
#"SOME_COLLEGE", "COLLEGE",
#"Less than $25,000", "$25,000 to $34,999", 
#"$35,000 to $49,999", "$50,000 to $74,999", 
#"$75,000 to $99,999", "$100,000 to $149,999",
#"$150,000 to $199,999",  "$200,000 or more"
#)

test_val_black_male_state = function(results_loc, results_name, data_loc, census_loc, census_name, survey,
                                     s1, s2, 
                                     census_key, 
                                     survey_col, survey_key, test,
                                     rep_level, rep_code,
                                     hisp = F){
  
  # data from the surveys 
  demographic_data = read.csv(paste(data_loc, "/", survey, "/", "demographic_data.csv", sep = ""))
  census_data = read.csv(paste(census_loc, census_name, sep=""))
  
  # compiled statistics
  results_data = get(load(paste(results_loc, "/", survey, "/", results_name, sep = "")))
  
  # get value from computed results
  computed_from_script = results_data[s1, s2]
  
  # calculation of odds
  demographic_data = demographic_data %>% filter(!is.na(state_fips))
  survey_filtered_data = NA
  survey_n = nrow(demographic_data) # number of people that filled out the state column correctly
  if(!hisp){
    survey_filtered_data = demographic_data %>% filter(race_recode == "Black or African American" & 
                                                !!sym(survey_col) == survey_key & 
                                                !!sym(rep_level) == rep_code)
  }else{
    survey_filtered_data = demographic_data %>% filter(hisp_code == "Hispanic" & 
                                                         !!sym(survey_col) == survey_key & 
                                                         !!sym(rep_level) == rep_code)
  }
  count_x = nrow(survey_filtered_data)
  survey_odds = (count_x/survey_n)/(1-(count_x/survey_n))
  
  census_n = census_data[["UNDERREP_TOTAL"]]
  census_x = census_data[[census_key]]
  census_odds = (census_x/census_n)/(1-(census_x/census_n))
  
  assert("Not equal", {
    computed_from_script == log(survey_odds) - log(census_odds)
  })
  print("Passed Test!")
}

results_loc = "/Users/christianvarner/Research/ccs-knowledge-map/data_preprocessing/results"
survey_data_loc = "/Volumes/cbjackson2/ccs-knowledge/ccs-data-demographic_unprocessed/"
CENSUS_MAP = c("tract" = "/Volumes/cbjackson2/ccs-knowledge/census-data/census/tract/censustract",
               "state" = "/Volumes/cbjackson2/ccs-knowledge/census-data/census/state/censusstate",
               "county" = "/Volumes/cbjackson2/ccs-knowledge/census-data/census/county/censuscounty")
surveys = c("air-quality-map", "air-quality-survey", "tree-canopy-map", "tree-canopy-survey", 
            "urban-heat-map", "urban-heat-survey", "ej-report", "ej-storytile", "ej-survey")
subpop_school_levels = c("_LESS_HS_TOTAL", "_HS_TOTAL", 
                        "_SOME_COLLEGE_TOTAL", "_COLLEGE_TOTAL")
subpop_age_levels = c("_18_to_24", "_25_to_34", 
                     "_35_to_44", "_45_to_54",
                     "_55_to_64", "_65_over")
subpop_income_levels = c("_less25", "_25_to_34",
                        "_35_to_49","_50_to_74", 
                        "_75_to_99","_100_to_149", 
                        "_150_to_199", "_200_more")
subpop_gender_levels = c("_FEMALE", "_MALE")


# Gender tests: african american
test_val_black_male_state(results_loc, "air-quality-survey-state-55.RData",survey_data_loc, CENSUS_MAP[["state"]], "_genderedu.csv",
                          "air-quality-survey", "Male", "Black", "BLACK_MALE", "Gender", "Male", "state_fips", "55")
test_val_black_male_state(results_loc, "air-quality-survey-state-55.RData",survey_data_loc, CENSUS_MAP[["state"]], "_genderedu.csv",
                          "air-quality-survey", "Female", "Black", "BLACK_FEMALE", "Gender", "Female","state_fips", "55")
print("Done with gender tests")

test_val_black_male_state(results_loc, "air-quality-survey-state-55.RData",survey_data_loc, CENSUS_MAP[["state"]], "_age.csv",
                          "air-quality-survey", "18_to_24", "Black", "BLACK_18_to_24", "Year.of.Birth", "18_to_24", "state_fips", "55")
test_val_black_male_state(results_loc, "air-quality-survey-state-55.RData",survey_data_loc, CENSUS_MAP[["state"]], "_age.csv",
                          "air-quality-survey", "25_to_34", "Black", "BLACK_25_to_34", "Year.of.Birth", "25_to_34", "state_fips", "55")
test_val_black_male_state(results_loc, "air-quality-survey-state-55.RData",survey_data_loc, CENSUS_MAP[["state"]], "_age.csv",
                          "air-quality-survey", "35_to_44", "Black", "BLACK_35_to_44", "Year.of.Birth", "35_to_44", "state_fips", "55")
test_val_black_male_state(results_loc, "air-quality-survey-state-55.RData",survey_data_loc, CENSUS_MAP[["state"]], "_age.csv",
                          "air-quality-survey", "45_to_54", "Black", "BLACK_45_to_54", "Year.of.Birth", "45_to_54", "state_fips", "55")
test_val_black_male_state(results_loc, "air-quality-survey-state-55.RData",survey_data_loc, CENSUS_MAP[["state"]], "_age.csv",
                          "air-quality-survey", "55_to_64", "Black", "BLACK_55_to_64", "Year.of.Birth", "55_to_64", "state_fips", "55")
test_val_black_male_state(results_loc, "air-quality-survey-state-55.RData",survey_data_loc, CENSUS_MAP[["state"]], "_age.csv",
                          "air-quality-survey", "65_over", "Black", "BLACK_65_over", "Year.of.Birth", "65_over", "state_fips", "55")
print("Done with age tests")

test_val_black_male_state(results_loc, "air-quality-survey-state-55.RData",survey_data_loc, CENSUS_MAP[["state"]], "_genderedu.csv",
                          "air-quality-survey", "Male", "Hispanic", "HISP_MALE", "Gender", "Male", "state_fips", "55", hisp = T)
test_val_black_male_state(results_loc, "air-quality-survey-state-55.RData",survey_data_loc, CENSUS_MAP[["state"]], "_genderedu.csv",
                          "air-quality-survey", "Female", "Hispanic", "HISP_FEMALE", "Gender", "Female", "state_fips", "55", hisp=T)
print("Done with gender tests")

test_val_black_male_state(results_loc, "air-quality-survey-state-55.RData",survey_data_loc, CENSUS_MAP[["state"]], "_age.csv",
                          "air-quality-survey", "18_to_24", "Hispanic", "HISP_18_to_24", "Year.of.Birth", "18_to_24", "state_fips", "55", hisp=T)
test_val_black_male_state(results_loc, "air-quality-survey-state-55.RData",survey_data_loc, CENSUS_MAP[["state"]], "_age.csv",
                          "air-quality-survey", "25_to_34", "Hispanic", "HISP_25_to_34", "Year.of.Birth", "25_to_34", "state_fips", "55", hisp=T)
test_val_black_male_state(results_loc, "air-quality-survey-state-55.RData",survey_data_loc, CENSUS_MAP[["state"]], "_age.csv",
                          "air-quality-survey", "35_to_44", "Hispanic", "HISP_35_to_44", "Year.of.Birth", "35_to_44", "state_fips", "55", hisp=T)
test_val_black_male_state(results_loc, "air-quality-survey-state-55.RData",survey_data_loc, CENSUS_MAP[["state"]], "_age.csv",
                          "air-quality-survey", "45_to_54", "Hispanic", "HISP_45_to_54", "Year.of.Birth", "45_to_54", "state_fips", "55", hisp=T)
test_val_black_male_state(results_loc, "air-quality-survey-state-55.RData",survey_data_loc, CENSUS_MAP[["state"]], "_age.csv",
                          "air-quality-survey", "55_to_64", "Hispanic", "HISP_55_to_64", "Year.of.Birth", "55_to_64", "state_fips", "55", hisp=T)
test_val_black_male_state(results_loc, "air-quality-survey-state-55.RData",survey_data_loc, CENSUS_MAP[["state"]], "_age.csv",
                          "air-quality-survey", "65_over", "Hispanic", "HISP_65_over", "Year.of.Birth", "65_over", "state_fips", "55", hisp=T)
print("Done with age tests")
