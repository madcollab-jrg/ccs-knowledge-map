get_table <- function(file) {
  tbl_data <- NA
  exists <- FALSE
  if (file.exists(file)) {
    tbl_data <- get(load(file))
    exists <- TRUE
  } else {
    tbl_data <- data.frame(matrix(ncol = 3, nrow = 20))
  }
  colnames(tbl_data) <- c("Black", "Hispanic", "Total")
  rownames(tbl_data) <- c(
    "Male", "Female",
    "18-24", "25-34", "35-44", "45-54", "55-64", "65 or over",
    "Less than high school", "High-school graduate",
    "Some college/Technical school",
    "Bachelor's and higher",
    "Less than $25,000", "$25,000 to $34,999",
    "$35,000 to $49,999", "$50,000 to $74,999",
    "$75,000 to $99,999", "$100,000 to $149,999",
    "$150,000 to $199,999", "$200,000 or more"
  )
  return(list(tbl_data, exists))
}
