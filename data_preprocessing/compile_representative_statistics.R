# Generate representative-ness metric for each of the surveys
# and save them in csv files for use by the app.

#race:
#race_recode:
#hisp_recode:

library(yaml)
library(methods)
library(dplyr)
source("data_preprocessing/log_disparity.R")

# query methods for creating calls to the csv files
surv_query_generator = setRefClass("surveillance_query", fields = list(query = "list", subpop = "character", target_pop = "character"))
survey_query_generator = setRefClass("survey_query", fields = list(query = "list"))

generate_queries = function(query, census_codes, tp){
  survey_query_data = query$survey_query
  survey_query = survey_query_generator(query = survey_query_data$query)
  
  surveillance_query_data = query$surveillance_query
  surveillance_query_data$query = list("GEOID"=census_codes)
  surveillance_query_data$target_pop = "UNDERREP_TOTAL"
  
  surveillance_query = surv_query_generator(query = surveillance_query_data$query, 
                                            subpop = surveillance_query_data$subpop,
                                            target_pop = surveillance_query_data$target_pop)
  to_return = list("surveyq" = survey_query, "surveillanceq" = surveillance_query)
  return(to_return)
}

get_yaml = function(loc){
  # Read file at location that should have
  # be a text file with characteristics
  queries_to_compile = read_yaml(loc)
  return(queries_to_compile)
}

get_survey_statistics = function(survey, census_level, queries, census_code, target_pop, save = T, fname = ""){
  # Get the log disparity metric for the survey and corresponding surveillance dataset.
  #
  # Parameters:
  #   survey: survey to generate statistics from
  #   surveillance: surveillance dataset to get statistics from
  #   survey_queries: type c(), an array of survey_query objects
  #   surveillance_queries: type c(), an array of surveillance_query objects
  #   save: save an R object
  #   fname: names of the R object to be saved
  #
  # Return:
  #   list of survey and log-disparty scores
  #
  #
  
  n = length(queries)
  survey_df = read.csv(survey)
  
  tbl_data = data.frame(matrix(ncol=3, nrow = 20))
  colnames(tbl_data) = c("Black", "Hispanic", "Total")
  rownames(tbl_data) = c("Male", "Female", 
                         "18_to_24", "25_to_34", 
                         "35_to_44", "45_to_54",
                         "55_to_64", "65_over",
                         "LESS_THAN_HS", "HS", 
                         "SOME_COLLEGE", "COLLEGE",
                         "Less than $25,000", "$25,000 to $34,999", 
                         "$35,000 to $49,999", "$50,000 to $74,999", 
                         "$75,000 to $99,999", "$100,000 to $149,999",
                         "$150,000 to $199,999",  "$200,000 or more"
  )
  
  for(i in 1:n){
    query_to_be_generated = queries[[i]]
    
    # add census code to the query
    if(census_level == "county"){
      if(census_code == 55025){
        query_to_be_generated$survey_query$query[[ CENSUS_TO_SURVEY_COL[[census_level]] ]] = 25
      }else{
        query_to_be_generated$survey_query$query[[ CENSUS_TO_SURVEY_COL[[census_level]] ]] = 55
      }
    }else{
      query_to_be_generated$survey_query$query[[ CENSUS_TO_SURVEY_COL[[census_level]] ]] = census_code
    }
    query = generate_queries(query_to_be_generated, census_code, target_pop)
    query_type = queries[[i]]$type
    
    # get statistics
    surveillance_data_loc = paste(CENSUS_MAP[[census_level]], QUERY_TYPE_MAP[[query_type]], sep = "")
    surveillance = read.csv(surveillance_data_loc)
    statistics = log_disparity(survey_df, surveillance, query$surveyq, query$surveillanceq, CENSUS_TO_SURVEY_COL[[census_level]])
    data = statistics[["log_disparity"]]
    if(is.na(data) | is.infinite(data)){
      data = NA
    }
    tbl_data[query_to_be_generated$row, query_to_be_generated$col] = data
  }
  save(tbl_data, file = fname)
}

QUERY_TYPE_MAP = c("age" = "_age.csv", 
                   "income" = "_income.csv", 
                   "education" = "_genderedu.csv",
                   "gender" = "_genderedu.csv")
CENSUS_MAP = c("tract" = "/Volumes/cbjackson2/ccs-knowledge/census-data/census/tract/censustract",
               "state" = "/Volumes/cbjackson2/ccs-knowledge/census-data/census/state/censusstate",
               "county" = "/Volumes/cbjackson2/ccs-knowledge/census-data/census/county/censuscounty")
CENSUS_TO_SURVEY_COL = c("tract" = "census_tract_full", 
                         "state" = "state_fips",
                         "county" = "county_fips")
HEAD = "/Users/christianvarner/Research/ccs-knowledge-map/data_preprocessing/results_representativeness/"

# Main
# ----------------------------------------------------------------------------------------------------------
demographic_data = "/Volumes/cbjackson2/ccs-knowledge/ccs-data-demographic_unprocessed/"
SURVEYS = c("ej-report", "ej-storytile", "ej-survey")
#SURVEYS = c("urban-heat-map", "urban-heat-survey", "ej-report", "ej-storytile", "ej-survey")
#surveys = c("air-quality-survey")
#surveys = c("ej-survey")

census_codes = get_yaml("data_preprocessing/rosetta/stone.yaml")
queries_to_compile = get_yaml("data_preprocessing/queries/generated_queries.yaml") # all african american income
target_pops = get_yaml("data_preprocessing/queries/target_populations.yaml")

start_time <- Sys.time()
i = 0
for(survey in SURVEYS){
  survey_dataset = paste(demographic_data,survey,"/","demographic_data.csv",sep="")
  for(rep_level in names(census_codes)){
    for(census_code in census_codes[[rep_level]]){
      name = paste(HEAD,survey,"/",survey, "-", rep_level, "-", census_code, ".RData", sep = "")
      print(paste("Starting:", name))
      get_survey_statistics(survey_dataset, rep_level, queries_to_compile, census_code, target_pops, save = T, fname = name)
      print(paste("Finished:", name))
    }
  }
}
end_time <- Sys.time()
print(end_time - start_time)
#---------------------------------------------------------------------------------------------------------















