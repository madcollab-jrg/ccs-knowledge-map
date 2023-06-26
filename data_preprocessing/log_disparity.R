# Implementation of log - disparity metric found
# in:
#
# Quantifying representativeness in randomized clinical 
# trials using machine learning fairness metrics

library(rlang)
library(dplyr)

characteristic_odds = function(df, x, log = T){
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
  #   odds of being having characteristic x
  
  
  # count the number of times x occurs
  if (length(x) == 0){
    stop("No characteristics given!")
  }
  
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
  #   Differences in odds
  
  survey_odds = characteristic_odds(survey_df, x_survey, log = T)
  surveillance_odds = characteristic_odds(surv_df, x_surv, log = T)
  return(survey_odds - surveillance_odds)
}

