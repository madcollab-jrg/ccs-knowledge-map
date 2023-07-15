# Generate representative-ness metric for each of the surveys
# and save them in csv files for use by the app.

#race:
#race_recode:
#hisp_recode:

library(yaml)
library(methods)
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
  to_return = list()
  survey_df = read.csv(survey)
  for(i in 1:n){
    query_to_be_generated = queries[[i]]
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
    statistics = log_disparity(survey_df, surveillance, query$surveyq, query$surveillanceq)
    to_return[[paste(query_type,"_",query$surveillanceq$subpop, sep = "")]] = statistics[["log_disparity"]]
  }
  
  if(save){
    save(to_return, file = fname)
  }
  
  return(to_return)
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
HEAD = "/Users/christianvarner/Research/ccs-knowledge-map/data_preprocessing/results/"
# Main
# ----------------------------------------------------------------------------------------------------------
demographic_data = "/Volumes/cbjackson2/ccs-knowledge/ccs-data-demographic_unprocessed/"
#surveys = c("air-quality-map", "air-quality-survey", 
            #"tree-canopy-map", "tree-canopy-survey", 
            #"urban-heat-map", "urban-heat-survey", 
            #"ej-report", "ej-storytile", "ej-survey")

surveys = c("air-quality-survey")

census_codes = get_yaml("data_preprocessing/rosetta/stone.yaml")
queries_to_compile = get_yaml("data_preprocessing/queries/generated_queries.yaml") # all african american income
target_pops = get_yaml("data_preprocessing/queries/target_populations.yaml")

start_time <- Sys.time()
i = 0
for(survey in surveys){
  survey_dataset = paste(demographic_data,survey,"/","demographic_data.csv",sep="")
  for(rep_level in names(census_codes)){
    for(census_code in census_codes[[rep_level]]){
      name = paste(HEAD,survey,"/",survey, "-", rep_level, "-", census_code, ".RData", sep = "")
      get_survey_statistics(survey_dataset, rep_level, queries_to_compile, census_code, target_pops, save = T, fname = name)
      print(name)
    }
  }
}
end_time <- Sys.time()
print(end_time - start_time)
#---------------------------------------------------------------------------------------------------------















