# Implementation of log - disparity metric found
# in:
#
# Quantifying representativeness in randomized clinical
# trials using machine learning fairness metrics

library(rlang)
library(dplyr)

# Function to calculate characteristic odds in survey data
survey_characteristic_odds <- function(df, x, census_level, log = TRUE) {
  # print(head(df))
  if (length(x) == 0) {
    stop("No characteristics given!")
  }

  print(paste("arg:", census_level, x))

  # print(paste("census_level", x[[census_level]]))

  census_level <- "district_GEOID"

  # df <-
  #   df %>% filter(!is.na(state_fips) & !!sym(census_level) == x[[census_level]])
  # df <- df %>% filter(!!sym(census_level) == x[[census_level]])

  # df <- df %>% filter(!!sym("district_GEOID") == x[["district_GEOID"]])
  df <- df %>% filter(!!sym("district_GEOID") %in% x[["district_GEOID"]])

  n <- nrow(df)

  print(paste("nrow(df):", nrow(df)))

  if (n == 0) {
    warning("n is 0")
  }

  count_x <- 0
  for (characteristic in names(x)) {
    df <- df %>% filter(!!sym(characteristic) == x[[characteristic]])
  }
  count_x <- nrow(df)

  prob_x <- count_x / n
  prob_not_x <- (1 - prob_x)
  metric <- NA
  if (!log) {
    metric <- prob_x / prob_not_x
  } else {
    metric <- log(prob_x / prob_not_x)
  }
  if (!log) {
    return(metric)
  } else {
    return(metric)
  }
}

# Function to calculate characteristic odds in surveillance data
surveillance_characteristic_odds <- function(df, x, query_type, log = TRUE) {
  count_x <- 0

  query <- x[["query"]]
  subpop <- x[["subpop"]]

  print(query)

  for (c in names(query)) {
    df <- df %>% filter(!!sym(c) == query[[c]])
  }
  print(query_type)

  count_x <- 0
  # n <- df[[x[["target_pop"]]]]
  if (query_type == "race") {
    n <- df[["ALL_TOTAL"]]
  } else {
    n <- df[[x[["target_pop"]]]]
  }

  print(paste("n:", n, x[["target_pop"]]))

  if (n == 0) {
    warning("n is 0")
  }

  for (t in subpop) {
    count_x <- count_x + df[[t]]
  }

  prob_x <- count_x / n
  prob_not_x <- 1 - prob_x
  odds <- prob_x / prob_not_x
  if (!log) {
    return(odds)
  } else {
    return(log(odds))
  }
}

# Function to calculate log disparity between survey and surveillance data
log_disparity <- function(
    survey_df, surv_df, x_survey, x_surv, census_level,
    query_type) {
  # census_level <- "zipcode"
  to_return <- list("log_disparity" = NA)
  survey_odds <-
    survey_characteristic_odds(survey_df, x_survey[["query"]],
      census_level,
      log = TRUE
    )
  surveillance_odds <-
    surveillance_characteristic_odds(surv_df, x_surv, query_type, log = TRUE)

  print("----")
  print(survey_odds)
  print(surveillance_odds)
  print("----")
  log_disp <- survey_odds - surveillance_odds
  print("*****")
  print(log_disp)
  print("*****")
  to_return$log_disparity <- log_disp

  return(to_return)
}
