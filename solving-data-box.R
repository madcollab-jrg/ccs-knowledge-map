library(gt)

# DEFINED IN data_description_box.R+
result_rows <- list(
  "gender" = 1:2,
  "age" = 3:8,
  "education" = 9:12,
  "income" = 13:20,
  "race" = 21:27
)


### A FUNCTION IN table.R
get_table <- function(file) {
  tbl_data <- NA
  exists <- FALSE
  if (file.exists(file)) {
    tbl_data <- get(load(file))
    exists <- TRUE
  } else {
    tbl_data <- data.frame(matrix(ncol = 1, nrow = 27))
  }
  colnames(tbl_data) <- c("Total")
  rownames(tbl_data) <- c(
    "Male", "Female",
    "18-24", "25-34", "35-44", "45-54", "55-64", "65 or over",
    "Less than high school", "High-school graduate",
    "Some college/Technical school",
    "Bachelor's and higher",
    "Less than $25,000", "$25,000 to $34,999",
    "$35,000 to $49,999", "$50,000 to $74,999",
    "$75,000 to $99,999", "$100,000 to $149,999",
    "$150,000 to $199,999", "$200,000 or more",
    "Black or African American", "Hispanic", "White", "Asian", "Native Hawaiian Pacific Islander", "American Indian Alaskan Native", ""
  )
  return(list(tbl_data, exists))
}



data_loc <- paste("/Volumes/cbjackson2/ccs-knowledge/results_summary/air-quality-map/air-quality-map-county-55025.RData") # change
data_loc_rep <- paste("/Volumes/cbjackson2/ccs-knowledge/results_representativeness/air-quality-survey/air-quality-survey-county-55025.RData")


tbl_data <- get_table(data_loc)[[1]]
rows_to_extract <- result_rows[["race"]] ### FOR TESTING OUTSIDE FUNCTION
# rows_to_extract <- result_rows[[demographic_desc]]
tbl_data_filtered <- tbl_data[rows_to_extract, ]
# tbl_data_filtered <- data.frame(Value = tbl_data_filtered) # NEW CODE TO HANDLE ERROR
gt_tbl <- gt(tbl_data_filtered, rownames_to_stub = TRUE)

loaded_data <- get_table(data_loc_rep)
tbl_data_rep <- loaded_data[[1]]
rows_to_extract <- result_rows[["race"]] ### FOR TESTING OUTSIDE FUNCTION
# rows_to_extract <- result_rows[[demographic_desc]]
tbl_data_filtered_rep <- tbl_data_rep[rows_to_extract, ]
# tbl_data_filtered_rep <- data.frame(Value = tbl_data_filtered_rep) # NEW CODE TO HANDLE ERROR
gt_tbl <- gt(tbl_data_filtered_rep, rownames_to_stub = TRUE)

tbl_data_filtered$row_names <- rownames(tbl_data_filtered)
tbl_data_filtered_rep$row_names <- rownames(tbl_data_filtered_rep)

# Merge two tables by row name
merged_tbl_data <- merge(
  tbl_data_filtered,
  tbl_data_filtered_rep,
  by.x = "row_names", by.y = "row_names",
  all = TRUE
)


rownames(merged_tbl_data) <- NULL


print(merged_tbl_data)
