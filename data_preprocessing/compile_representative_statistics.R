# Generate representative-ness metric for each of the surveys
# and save them in csv files for use by the app.

#race:
#race_recode:
#hisp_recode:

library(yaml)
library(methods)
source("log_disparity.R")

# query methods for creating calls to the csv files
surv_query_generator = setRefClass("surveillance_query", fields = list(query = "list", subpop = "character", target_pop = "character"))
survey_query_generator = setRefClass("survey_query", fields = list(query = "list"))

# given a race what is the associates subpopulation 
race_recode = c(""="")

generate_queries = function(query, census_codes, tp){
  survey_query_data = query$survey_query
  #survey_query_data$ = census_codes # $ something
  survey_query = survey_query_generator(query = survey_query_data$query)
  
  surveillance_query_data = query$surveillance_query
  surveillance_query_data$query = census_codes
  surveillance_query_data$target_pop = tp[[query$type]]
  
  surveillance_query = surv_query_generator(query = surveillance_query_data$query, 
                                            subpop = surveillance_query_data$subpop,
                                            target_pop = surveillance_query_data$target_pop)
  to_return("surveyq" = survey_query, "surveillanceq" = surveillance_query)
  return(to_return)
}

get_yaml = function(loc){
  # Read file at location that should have
  # be a text file with characteristics
  queries_to_compile = read_yaml(loc)
  return(queries_to_compile)
}

get_survey_statistics = function(survey, surveillance_level, queries, census_code, target_pop, save = T, fname = ""){
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
  to_return = list("characteristics" = survey_queries, "metric"=c())
  for(i in 1:n){
    query = generate_queries(queries[i], census_code, target_pop)
    query_type = query$surveyq$type 
    surveillance = 
    statistics = log_disparity(survey, surveillance, query$surveyq, surveillance_queries$surveillanceq)
    to_return[["metric"]][i] = statistics[["log_disparity"]]
  }
  
  if(save){
    saveRDS(to_return, file = fname)
  }
  
  return(to_return)
}



# Main
# ----------------------------------------------------------------------------------------------------------
demographic_data = "/Volumes/cbjackson2/ccs-knowledge/ccs-data-demographic_unprocessed/"
surveys = c("air-quality", "tree-canopy", "urban-heat", "ej-report", "ej-storytile", "ej-survey")
census_codes = get_yaml("data_preprocessing/rosetta/stone.yaml")
queries_to_compile = get_yaml("data_proprocessing/queries/queries_survey.yaml")
target_pops = get_yaml("data_proprocessing/queries/target_populations.yaml")

for(survey in surveys){
  survey_dataset = paste(demographic_data,survey,"/","demographic_data.csv",sep="")
  for(rep_level in names(census_codes)){
    for(census_code in census_codes$level){
      name = paste(surveys, "-", level, "-", census_code, ".RData", sep = "")
      get_survey_statistics(survey_dataset, rep_level, queries_to_compile, census_code, target_pops, save = T, fname = name)
    }
  }
}
#---------------------------------------------------------------------------------------------------------















