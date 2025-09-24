library(httr)
library(tidycensus)
library(purrr)

#https://stackoverflow.com/questions/76601873/get-the-census-data-from-block-group-based-on-specific-addresses-in-r

options(tigris_use_cache = TRUE)
census_api_key("087b597eb48c2a5edee92ce40fb7b889f0aa69ac", overwrite=TRUE) 


get_geocode_data <- function(address) {
  # The URL for the geocoding request
  url <- "https://geocoding.geo.census.gov/geocoder/geographies/onelineaddress"
  
  # The parameters for the request
  params <- list(
    address = address,
    format = "json",
    benchmark = "Public_AR_Current",
    vintage = "Current_Current"
  )
  
  # Make the request
  response <- GET(url, query = params)
  
  # Parse the response
  content <- content(response, "parsed", "application/json")
  
  # Extract the geographies data if it exists
  if (length(content$result$addressMatches) > 0) {
    geographies <- content$result$addressMatches[[1]]$geographies
  } else {
    geographies <- NULL
  }
  
  return(geographies)
}

# Initialize empty vectors to store the data
upper_names <- c()
upper_geoids <- c()
lower_names <- c()
lower_geoids <- c()
congress_districts <- c()
congress_district_geoids <- c()
tract_geoids <- c()
tract_names <- c()
block_geoids <- c()
block_names <- c()
county_geoids <- c()
county_names <- c()

# Loop through each address in the dataframe
for (address in addresses_df$address) {
  # Fetch the geocode data for the address
  geocoding_data <- get_geocode_data("address")
  
  # Extract data with checks for NULL values
  upper_name <- ifelse(!is.null(geocoding_data$`2022 State Legislative Districts - Upper`),
                       geocoding_data$`2022 State Legislative Districts - Upper`[[1]]$NAME, NA)
  upper_geoid <- ifelse(!is.null(geocoding_data$`2022 State Legislative Districts - Upper`),
                        geocoding_data$`2022 State Legislative Districts - Upper`[[1]]$GEOID, NA)
  lower_name <- ifelse(!is.null(geocoding_data$`2022 State Legislative Districts - Lower`),
                       geocoding_data$`2022 State Legislative Districts - Lower`[[1]]$NAME, NA)
  lower_geoid <- ifelse(!is.null(geocoding_data$`2022 State Legislative Districts - Lower`),
                        geocoding_data$`2022 State Legislative Districts - Lower`[[1]]$GEOID, NA)
  congress_district <- ifelse(!is.null(geocoding_data$`118th Congressional Districts`),
                              geocoding_data$`118th Congressional Districts`[[1]]$NAME, NA)
  congress_district_geoid <- ifelse(!is.null(geocoding_data$`118th Congressional Districts`),
                                    geocoding_data$`118th Congressional Districts`[[1]]$GEOID, NA)
  
  tract_geoid <- ifelse(!is.null(geocoding_data$`Census Tracts`),
                        geocoding_data$`Census Tracts`[[1]]$GEOID, NA)
  tract_name <- ifelse(!is.null(geocoding_data$`Census Tracts`),
                        geocoding_data$`Census Tracts`[[1]]$NAME, NA)
  block_geoid <- ifelse(!is.null(geocoding_data$`2020 Census Blocks`),
                        geocoding_data$`2020 Census Blocks`[[1]]$GEOID, NA)
  block_name <- ifelse(!is.null(geocoding_data$`2020 Census Blocks`),
                        geocoding_data$`2020 Census Blocks`[[1]]$NAME, NA)
  county_geoid <- ifelse(!is.null(geocoding_data$Counties),
                         geocoding_data$Counties[[1]]$GEOID, NA)
  county_name <- ifelse(!is.null(geocoding_data$Counties),
                        geocoding_data$Counties[[1]]$NAME, NA)
  
  # Append the extracted data to the vectors
  upper_names <- c(upper_names, upper_name)
  upper_geoids <- c(upper_geoids, upper_geoid)
  lower_names <- c(lower_names, lower_name)
  lower_geoids <- c(lower_geoids, lower_geoid)
  congress_districts <- c(congress_districts, congress_district)
  congress_district_geoids <- c(congress_district_geoids, congress_district_geoid)
  tract_geoids <- c(tract_geoids, tract_geoid)
  tract_names <- c(tract_names, tract_name)
  block_geoids <- c(block_geoids, block_geoid)
  block_names <- c(block_names, block_name)
  county_geoids <- c(county_geoids, county_geoid)
  county_names <- c(county_names, county_name)

}

# Combine the vectors into a data frame
geocode_df <- data.frame(upper_name = upper_names,
                              upper_geoid = upper_geoids,
                              lower_name = lower_names,
                              lower_geoid = lower_geoids,
                              congress_district = congress_districts,
                              congress_district_geoid = congress_district_geoids,
                              tract_geoid = tract_geoids,
                              tract_name = tract_names,
                              block_geoid = block_geoids,
                              block_name = block_names,
                              county_geoid = county_geoids,
                              county_names = county_name,
                              stringsAsFactors = FALSE)



