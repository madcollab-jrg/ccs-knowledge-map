# Load necessary libraries
library(yaml)
library(methods)
library(dplyr)

# Source external script containing log-disparity functions
source("data_preprocessing/log.R")

# Define a class for generating surveillance queries
surv_query_generator <- setRefClass("surveillance_query",
  fields = list(query = "list", subpop = "character", target_pop = "character")
)

# Define a class for generating survey queries
survey_query_generator <- setRefClass("survey_query",
  fields = list(query = "list")
)

# Function to generate queries based on input parameters
generate_queries <- function(query, census_codes, tp) {
  survey_query_data <- query$survey_query
  survey_query <- survey_query_generator(query = survey_query_data$query)

  surveillance_query_data <- query$surveillance_query
  surveillance_query_data$query <- list("GEOID" = census_codes)
  surveillance_query_data$target_pop <- "UNDERREP_TOTAL"

  surveillance_query <- surv_query_generator(
    query = surveillance_query_data$query,
    subpop = surveillance_query_data$subpop,
    target_pop = surveillance_query_data$target_pop
  )

  to_return <- list(
    "surveyq" = survey_query,
    "surveillanceq" = surveillance_query
  )
  return(to_return)
}

# Function to read YAML file at a specified location
get_yaml <- function(loc) {
  queries_to_compile <- read_yaml(loc)
  return(queries_to_compile)
}

# Function to calculate survey statistics and log disparity
get_survey_statistics <- function(
    survey, census_level, queries,
    census_code, target_pop, save = TRUE, fname = "") {
  tryCatch(
    {
      # Read survey data
      survey_df <- read.csv(survey)

      # Create a dataframe for storing statistics
      tbl_data <- data.frame(matrix(ncol = 1, nrow = 26))
      colnames(tbl_data) <- c("Total")
      rownames(tbl_data) <- c(
        "Male", "Female",
        "18_to_24", "25_to_34",
        "35_to_44", "45_to_54",
        "55_to_64", "65_over",
        "LESS_THAN_HS", "HS",
        "SOME_COLLEGE", "COLLEGE",
        "Less than $25,000", "$25,000 to $34,999",
        "$35,000 to $49,999", "$50,000 to $74,999",
        "$75,000 to $99,999", "$100,000 to $149,999",
        "$150,000 to $199,999", "$200,000 or more",
        "Black or African American",
        "Hispanic", "White", "Asian",
        "Native Hawaiian Pacific Islander",
        "American Indian Alaskan Native"
      )

      n <- length(queries)

      # Loop over queries
      for (i in 1:n) {
        query_to_be_generated <- queries[[i]]

        # Add census code to the query
        if (census_level == "county") {
          if (census_code == 55025) {
            query_to_be_generated$survey_query$query[[CENSUS_TO_SURVEY_COL[[census_level]]]] <- 25
          } else {
            query_to_be_generated$survey_query$query[[CENSUS_TO_SURVEY_COL[[census_level]]]] <- 55
          }
        } else {
          query_to_be_generated$survey_query$query[[CENSUS_TO_SURVEY_COL[[census_level]]]] <- census_code
        }

        # Generate queries
        query <-
          generate_queries(query_to_be_generated, census_code, target_pop)
        query_type <- queries[[i]]$type

        # Get statistics
        surveillance_data_loc <- paste(CENSUS_MAP[[census_level]],
          QUERY_TYPE_MAP[[query_type]],
          sep = ""
        )
        surveillance <- read.csv(surveillance_data_loc)
        statistics <- log_disparity(
          survey_df, surveillance, query$surveyq, query$surveillanceq,
          CENSUS_TO_SURVEY_COL[[census_level]]
        )
        data <- statistics[["log_disparity"]]

        if (is.na(data) | is.infinite(data)) {
          data <- NA
        }

        tbl_data[query_to_be_generated$row, query_to_be_generated$col] <- data
      }
      print(tbl_data)
      print(fname)

      # Save statistics
      save(tbl_data, file = fname)
    },
    error = function(e) {
      # Print error message if an error occurs
      cat("Error:", conditionMessage(e), "\n")
      # Print traceback for debugging
      traceback()
    }
  )
}

# Main Execution
# ------------------------------

# Constants
QUERY_TYPE_MAP <- c(
  "age" = "_age.csv",
  "income" = "_income.csv",
  "education" = "_genderedu.csv",
  "gender" = "_genderedu.csv"
)
CENSUS_MAP <- c(
  "tract" = "/Volumes/cbjackson2/ccs-knowledge/census-data/census/tract/censustract",
  "state" = "/Volumes/cbjackson2/ccs-knowledge/census-data/census/state/censusstate",
  "county" = "/Volumes/cbjackson2/ccs-knowledge/census-data/census/county/censuscounty",
  "zipcode" = "/Volumes/cbjackson2/ccs-knowledge/census-data/census/zip/censuszip",
  "state_upper" = "/Volumes/cbjackson2/ccs-knowledge/census-data/census/state_upper/state_upper",
  "state_lower" = "/Volumes/cbjackson2/ccs-knowledge/census-data/census/state_lower/state_lower",
  "congress" = "/Volumes/cbjackson2/ccs-knowledge/census-data/census/congress/censuscongress"
)
CENSUS_TO_SURVEY_COL <- c(
  "tract" = "census_tract_full",
  "state" = "state_fips",
  "county" = "county_fips",
  "zipcode" = "zip",
  "state_upper" = "district_GEOID",
  "state_lower" = "assembly_geoid",
  "congress" = "congress_district"
)
HEAD <-
  "/Volumes/cbjackson2/ccs-knowledge/results_representativeness/"

# Main
# ------------------------------

# Define paths to data directories
demographic_data <-
  "/Volumes/cbjackson2/ccs-knowledge/ccs-data-demographic_unprocessed/"
SURVEYS <- c("air-quality-survey")

# Read YAML files containing census codes, queries, and target populations
census_codes <- get_yaml("data_preprocessing/rosetta/stone.yaml")
queries_to_compile <-
  get_yaml("data_preprocessing/queries/generated_queries.yaml")
target_pops <- get_yaml("data_preprocessing/queries/target_populations.yaml")

# Record start time
start_time <- Sys.time()

# Iterate over survey datasets
for (survey in SURVEYS) {
  survey_dataset <- paste(demographic_data, survey, "/",
    "demographic_data.csv",
    sep = ""
  )

  cat("Loading Survey Dataset:", survey_dataset, "\n")

  # Check if the file exists
  if (!file.exists(survey_dataset)) {
    cat("Error: Survey Dataset not found.\n")
    next # Skip to the next iteration
  }

  # Iterate over census levels and census codes
  for (rep_level in names(census_codes)) {
    for (census_code in census_codes[[rep_level]]) {
      name <- paste(HEAD, survey, "/", survey, "-",
        rep_level, "-", census_code, ".RData",
        sep = ""
      )
      print(paste("Starting:", name))
      # print(str(queries_to_compile))

      # Calculate survey statistics
      get_survey_statistics(survey_dataset, rep_level, queries_to_compile,
        census_code, target_pops,
        save = TRUE, fname = name
      )
      print(paste("Finished:", name))
    }
  }
}

# Record end time
end_time <- Sys.time()
print(end_time - start_time)
