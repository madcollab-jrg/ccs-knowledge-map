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
    stop("Dataframe has 0 rows!")
  }
  
  count_x = 0
  temp = df
  for(characteristic in names(x)){
    temp = temp %>% 
      filter( !! sym(characteristic) == x[[characteristic]] )
  }
  count_x = nrow(temp)
  
  if (count_x == 0){
    warning("Number of records matching the characteristics given is 0, this could be a mistake!")
  }
  
  # calculation of odds
  prob_x = count_x/n
  prob_not_x = (1-prob_x)
  metric = NA
  if(!log){
    metric = prob_x/prob_not_x
  }else{
    metric = log(prob_x/prob_not_x)
  }
  to_return = list("metric"=metric, "data"=temp)
  if(!log){
    return(to_return)
  }else{
    return(to_return)
  }
}

compute_target_pop = function(df, x){
  
}

surveillance_characteristic_odds = function(df, x, log = T){
  count_x = 0
  
  query = x[['query']] # should be a list of atributes:values to filter on
  subpop = x[['subpop']] # column name where data is
  
  # get data from query and check that there is a unique record
  temp = df
  for(c in names(query)){
    temp = temp %>% filter( !! sym(c) == query[[c]] )
  }
  if(nrow(temp) > 1){
    stop("Multiple records in surveillance dataset with this query!")
  }
  
  # get sum of target sub-population and subpopulation counts
  count_x = 0
  n = temp[[ x[['target_pop']] ]]
  for(t in subpop){
    count_x = count_x + temp[[t]]
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
  
  survey_odds = survey_characteristic_odds(survey_df, x_survey[['query']], log = T)
  surveillance_odds = surveillance_characteristic_odds(surv_df, x_surv, log = T)
  
  log_disparity = survey_odds[["metric"]] - surveillance_odds
  to_return = list("log_disparity" = log_disparity)
  return(to_return)
}

