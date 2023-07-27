# Implementation of log - disparity metric found
# in:
#
# Quantifying representativeness in randomized clinical 
# trials using machine learning fairness metrics

library(rlang)
library(dplyr)

survey_characteristic_odds = function(df, x, log = T){
  # Given df that assumed to hold survey data 
  # calculate the probability of
  # being of type x (g(x) = 1) divided by probability
  # of not begin of type x (the odds). 
  #
  # Parameters:
  #   df: dataframe of characteristics from survey that will
  #        be matched with x
  #   x: type list, contains characteristics you want to match on
  #       ex. x = list("race": ..., "education": ..., etc.). Should have
  #       the same columns names as the dataframe for each characteristic
  #
  # Return:
  #   list containing odds of having characteristic x and the filtered dataset
  
  
  # count the number of times x occurs
  if (length(x) == 0){
    stop("No characteristics given!")
  }
  
  df = df %>% filter( !is.na(state_fips) )
  n = nrow(df)
  
  if (n == 0){
    warning("n is 0")
  }
  
  count_x = 0
  for(characteristic in names(x)){
    df = df %>% 
      filter( !! sym(characteristic) == x[[characteristic]] )
  }
  count_x = nrow(df)
  
  # calculation of odds
  prob_x = count_x/n
  prob_not_x = (1-prob_x)
  metric = NA
  if(!log){
    metric = prob_x/prob_not_x
  }else{
    metric = log(prob_x/prob_not_x)
  }
  if(!log){
    return(metric)
  }else{
    return(metric)
  }
}

compute_target_pop = function(df, x){
  
}

surveillance_characteristic_odds = function(df, x, log = T){
  count_x = 0
  
  query = x[['query']] # should be a list of atributes:values to filter on
  subpop = x[['subpop']] # column name where data is
  
  # get data from query and check that there is a unique record
  for(c in names(query)){
    df = df %>% filter( !! sym(c) == query[[c]] )
  }
  
  # get sum of target sub-population and subpopulation counts
  count_x = 0
  n = df[[ x[['target_pop']] ]]
  
  if(n == 0){
    warning("n is 0")
  }
  
  for(t in subpop){
    count_x = count_x + df[[t]]
  }
  
  prob_x = count_x/n
  prob_not_x = 1-prob_x
  odds = prob_x/prob_not_x
  if(!log){
    return(odds)
  }else{
    return(log(odds))
  }
}

log_disparity = function(survey_df,surv_df,x_survey,x_surv){
  # Implementation of log difference between odds
  # of having characteristics x in the survey and 
  # odds of having characteristics x in the surveillance
  # dataset. There is two x_... characteristics as
  # the survey and survelliance datasets might have different columns.
  # Importantly, they need to have the same values however.
  #
  # Parameters:
  #   survey_df: type dataframe, survey data
  #   surv_df: type dataframe, surveillance dataset - most likely will be census data
  #   x_survey: type list, characteristics to be matched in the survey
  #   x_surv: type list, characteristics to be matched in surveillance dataset
  #
  # Return:
  #   Differences in odds and the filtered dataset
  
  to_return = list("log_disparity" = NA)
  survey_odds = survey_characteristic_odds(survey_df, x_survey[['query']], log = T)
  surveillance_odds = surveillance_characteristic_odds(surv_df, x_surv, log = T)
  
  log_disp = survey_odds - surveillance_odds
  to_return$log_disparity = log_disp

  return(to_return)
}

