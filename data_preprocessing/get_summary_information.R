library(yaml)
library(methods)
library(dplyr)

get_summary_statistics <- function(
    survey, census_level, queries,
    census_code, save = TRUE, fname = "") {
  # print(census_code)
  # print(census_level)
  # print(queries)
  n <- length(queries)
  survey_df <- read.csv(survey)

  tbl_data <- data.frame(matrix(ncol = 3, nrow = 20))
  colnames(tbl_data) <- c("Black", "Hispanic", "Total")
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
    "$150,000 to $199,999", "$200,000 or more"
  )

  for (i in 1:n) {
    query_to_be_generated <- queries[[i]]

    # print(census_level)

    # add census code to the query
    if (census_level == "tract") {
      query_to_be_generated$survey_query$query[[CENSUS_TO_SURVEY_COL[[census_level]]]] <- census_code
    }
    # if (census_level == "county") {
    #   if (census_code == 55025) {
    #     query_to_be_generated$survey_query$query[[CENSUS_TO_SURVEY_COL[[census_level]]]] <- 25
    #   } else {
    #     query_to_be_generated$survey_query$query[[CENSUS_TO_SURVEY_COL[[census_level]]]] <- 55
    #   }
    # } else {
    #   query_to_be_generated$survey_query$query[[CENSUS_TO_SURVEY_COL[[census_level]]]] <- census_code
    # }

    query <- query_to_be_generated$survey_query$query
    temp <- survey_df
    for (x in names(query)) {
      temp <- temp %>% filter(!!sym(x) == query[[x]])
    }

    tbl_data[query_to_be_generated$row, query_to_be_generated$col] <- nrow(temp)
  }

  save(tbl_data, file = fname)
}

demographic_data <-
  "/Volumes/cbjackson2/ccs-knowledge/ccs-data-demographic_unprocessed/"


SURVEYS <- c(
  # "air-quality-map",
  # "air-quality-survey",
  # "urban-heat-map",
  # "urban-heat-survey",
  # "ej-report",
  # "ej-storytile",
  # "ej-survey",
  # "air-quality-survey",
  # "tree-canopy-map",
  # "tree-canopy-survey"
  "carbon-concerns",
  "energy-concerns",
  "general-survey",
  "health-impacts",
  "tree-knowledge"
)

# SURVEYS = c("air-quality-map")
CENSUS_TO_SURVEY_COL <- c(
  "tract" = "zip"
  # "tract" = "census_tract_full",
  # "state" = "state_fips",
  # "county" = "county_fips"
)
# HEAD <- "/Users/christianvarner/Research/ccs-knowledge-map/data_preprocessing/results_summary/"

HEAD <- "/Volumes/cbjackson2/ccs-knowledge/results_summary/"

census_codes <- yaml::yaml.load_file("data_preprocessing/rosetta/stone.yaml")
queries_to_compile <-
  yaml::yaml.load_file("data_preprocessing/queries/generated_queries.yaml")


start_time <- Sys.time()
i <- 0
for (survey in SURVEYS) {
  survey_dataset <- paste(demographic_data, survey, "/",
    "demographic_data.csv",
    sep = ""
  )
  for (rep_level in names(census_codes)) {
    for (census_code in census_codes[[rep_level]]) {
      name <- paste(HEAD, survey, "/", survey, "-", rep_level, "-",
        census_code, ".RData",
        sep = ""
      )
      print(paste("Starting:", name))
      get_summary_statistics(survey_dataset, rep_level,
        queries_to_compile, census_code,
        save = TRUE, fname = name
      )
      print(paste("Finished:", name))
    }
  }
}
end_time <- Sys.time()
print(end_time - start_time)
