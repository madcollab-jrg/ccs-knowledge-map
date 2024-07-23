library(httr)
library(jsonlite)
library(dplyr)
library(readr)
library(tidygeocoder)

ccs_participants_data <- read.csv("/Volumes/cbjackson2/ccs-knowledge/ccs-data[papers]/ccs_participant_data.csv")

ccs_participants_data <- ccs_participants_data %>%
  select(c(1:2,4:22,24:27,34:37))
# CLEAN ccs_participants_data to accomodate geographies

ccs_participants_data  <- ccs_participants_data %>%
  mutate(merge_name = if_else(is.na(matched_address),
                              paste(lat, long, sep=", "),
                              matched_address))

ccs_participants_data_short <- ccs_participants_data %>%
  select("Member.ID","matched_address")
names(ccs_participants_data_short)[2] <- c("address")

# Function to create URL from address
create_url_2010 <- function(address) {
  base_url <- "https://geocoding.geo.census.gov/geocoder/geographies/onelineaddress"
  formatted_address <- gsub(" ", "+", address)
  formatted_address <- gsub(",", "%2C", formatted_address)
  paste0(base_url, "?address=", formatted_address, "&benchmark=Public_AR_Census2020&vintage=Census2010_Census2020&format=json")
}

create_url_2020 <- function(address) {
  base_url <- "https://geocoding.geo.census.gov/geocoder/geographies/onelineaddress"
  formatted_address <- gsub(" ", "+", address)
  formatted_address <- gsub(",", "%2C", formatted_address)
  paste0(base_url, "?address=", formatted_address, "&benchmark=Public_AR_Census2020&vintage=Census2020_Census2020&format=json")
}

query_api <- function(url) {
  response <- GET(url)
  if (status_code(response) == 200) {
    content <- content(response, "text", encoding = "UTF-8")
    json_data <- fromJSON(content)
    return(json_data)
  } else {
    return(list(error = "Request failed"))
  }
}

# Iterate over the dataframe, query the API, and store results
results_2010 <- lapply(ccs_participants_data_short$address, function(x) query_api(create_url_2010(x)))

extracted_data_2010 <- data.frame(
  address = character(),
  Upper_SLDU_GEOID_2010 = character(),
  Upper_SLDU_NAME_2010 = character(),
  Lower_SLDL_GEOID_2010 = character(),
  Lower_SLDL_NAME_2010 = character(),
  Census_Block_GEOID_2010 = character(),
  Census_Block_NAME_2010 = character(),
  Census_Tract_GEOID_2010 = character(),
  Census_Tract_NAME_2010 = character(),
  Congressional_District_NAME_2010 = character(),
  Congressional_District_GEOID_2010 = character(),
  stringsAsFactors = FALSE # To keep strings as characters
)

for (res in results_2010)
{
  
  if (length(res$result$addressMatches) > 0 && !is.null(res$result$addressMatches[[1]])) {
    

  new_row <- data.frame(
    address = res$result$input$address[[1]],
    Upper_SLDU_GEOID_2010 = res$result$addressMatches$geographies$`State Legislative Districts - Upper`[[1]]$GEOID,
    Upper_SLDU_NAME_2010 = res$result$addressMatches$geographies$`State Legislative Districts - Upper`[[1]]$NAME,
    Lower_SLDL_GEOID_2010 = res$result$addressMatches$geographies$`State Legislative Districts - Lower`[[1]]$GEOID,
    Lower_SLDL_NAME_2010 = res$result$addressMatches$geographies$`State Legislative Districts - Lower`[[1]]$NAME,
    Census_Block_GEOID_2010 = res$result$addressMatches$geographies$`Census Blocks`[[1]]$GEOID,
    Census_Block_NAME_2010 = res$result$addressMatches$geographies$`Census Blocks`[[1]]$NAME,
    Census_Tract_GEOID_2010 = res$result$addressMatches$geographies$`Census Tract`[[1]]$GEOID,
    Census_Tract_NAME_2010 = res$result$addressMatches$geographies$`Census Tract`[[1]]$NAME,
    Congressional_District_NAME_2010 = res$result$addressMatches$geographies$`111th Congressional Districts`[[1]]$NAME,
    Congressional_District_GEOID_2010 = res$result$addressMatches$geographies$`111th Congressional Districts`[[1]]$GEOID,
    stringsAsFactors = FALSE
  )
  
  extracted_data_2010 <- rbind(extracted_data_2010, new_row)
}
}

extracted_data_2010 <- extracted_data_2010 %>% 
  distinct(address, .keep_all = TRUE)

results_2020 <- lapply(ccs_participants_data_short$address, function(x) query_api(create_url_2020(x)))

extracted_data_2020 <- data.frame(
  address = character(),
  Upper_SLDU_GEOID_2020 = character(),
  Upper_SLDU_NAME_2020 = character(),
  Lower_SLDL_GEOID_2020 = character(),
  Lower_SLDL_NAME_2020 = character(),
  Census_Block_GEOID_2020 = character(),
  Census_Block_NAME_2020 = character(),
  Census_Tract_GEOID_2020 = character(),
  Census_Tract_NAME_2020 = character(),
  Congressional_District_NAME_2020 = character(),
  Congressional_District_GEOID_2020 = character(),
  stringsAsFactors = FALSE # To keep strings as characters
)

for (res2020 in results_2020)
{
  
  if (length(res2020$result$addressMatches) > 0 && !is.null(res2020$result$addressMatches[[1]])) {
    
  new_row_n <- data.frame(
    address = res2020$result$input$address[[1]],
    Upper_SLDU_GEOID_2020 = res2020$result$addressMatches$geographies$`State Legislative Districts - Upper`[[1]]$GEOID,
    Upper_SLDU_NAME_2020 = res2020$result$addressMatches$geographies$`State Legislative Districts - Upper`[[1]]$NAME,
    Lower_SLDL_GEOID_2020 = res2020$result$addressMatches$geographies$`State Legislative Districts - Lower`[[1]]$GEOID,
    Lower_SLDL_NAME_2020 = res2020$result$addressMatches$geographies$`State Legislative Districts - Lower`[[1]]$NAME,
    Census_Block_GEOID_2020 = res2020$result$addressMatches$geographies$`Census Blocks`[[1]]$GEOID,
    Census_Block_NAME_2020 = res2020$result$addressMatches$geographies$`Census Blocks`[[1]]$NAME,
    Census_Tract_GEOID_2020 = res2020$result$addressMatches$geographies$`Census Tract`[[1]]$GEOID,
    Census_Tract_NAME_2020 = res2020$result$addressMatches$geographies$`Census Tract`[[1]]$NAME,
    Congressional_District_NAME_2020 = res2020$result$addressMatches$geographies$`116th Congressional Districts`[[1]]$NAME,
    Congressional_District_GEOID_2020 = res2020$result$addressMatches$geographies$`116th Congressional Districts`[[1]]$GEOID,
    stringsAsFactors = FALSE
  )
  
  extracted_data_2020 <- rbind(extracted_data_2020, new_row_n)
}
}

extracted_data_2020 <- extracted_data_2020 %>% 
  distinct(address, .keep_all = TRUE)

address_data_geo <- merge(extracted_data_2010,extracted_data_2020, by="address")
names(address_data_geo)[1] <- "merge_name"

###########################################
# ADD MANUALL MATCHED DATA BY COORDINATES #
###########################################

non_matched <- read.csv("/Users/coreyjackson/Desktop/non_match.csv")

create_url_coordinates2010 <- function(long,lat) {
  base_url <- "https://geocoding.geo.census.gov/geocoder/geographies/coordinates"
  # Construct the URL with the coordinates
  paste0(base_url, "?x=", long, "&y=", lat, "&benchmark=Public_AR_Census2020&vintage=Census2010_Census2020&format=json")
}

#https://geocoding.geo.census.gov/geocoder/geographies/coordinates?x=-89.76957998987092&y=43.21569865077441&benchmark=Public_AR_Census2020&vintage=Census2010_Census2020&format=json

results_coordinates2010 <- Map(function(long,lat) query_api(create_url_coordinates2010(long,lat)), 
                    non_matched$long,
                    non_matched$lat 
                    )

extracted_coorddata_2010 <- data.frame(
  lon_x = character(),
  lat_y = character(),
  Upper_SLDU_GEOID_2010 = character(),
  Upper_SLDU_NAME_2010 = character(),
  Lower_SLDL_GEOID_2010 = character(),
  Lower_SLDL_NAME_2010 = character(),
  Census_Block_GEOID_2010 = character(),
  Census_Block_NAME_2010 = character(),
  Census_Tract_GEOID_2010 = character(),
  Census_Tract_NAME_2010 = character(),
  Congressional_District_NAME_2010 = character(),
  Congressional_District_GEOID_2010 = character(),
  stringsAsFactors = FALSE # To keep strings as characters
)

for (rescoord2010 in results_coordinates2010)
{
  
    new_row_n <- data.frame(
      lon_x = rescoord2010$result$input$location$x,
      lat_y = rescoord2010$result$input$location$y,
      Upper_SLDU_GEOID_2010 = rescoord2010$result$geographies$`State Legislative Districts - Upper`$GEOID,
      Upper_SLDU_NAME_2010 = rescoord2010$result$geographies$`State Legislative Districts - Upper`$NAME,
      Lower_SLDL_GEOID_2010 = rescoord2010$result$geographies$`State Legislative Districts - Lower`$GEOID,
      Lower_SLDL_NAME_2010 = rescoord2010$result$geographies$`State Legislative Districts - Lower`$NAME,
      Census_Block_GEOID_2010 = rescoord2010$result$geographies$`Census Blocks`$GEOID,
      Census_Block_NAME_2010 = rescoord2010$result$geographies$`Census Blocks`$NAME,
      Census_Tract_GEOID_2010 = rescoord2010$result$geographies$`Census Tract`$GEOID,
      Census_Tract_NAME_2010 = rescoord2010$result$geographies$`Census Tract`$NAME,
      Congressional_District_NAME_2010 = rescoord2010$result$geographies$`111th Congressional Districts`$NAME,
      Congressional_District_GEOID_2010 = rescoord2010$result$geographies$`111th Congressional Districts`$GEOID,
      stringsAsFactors = FALSE
    )
    
    extracted_coorddata_2010 <- rbind(extracted_coorddata_2010, new_row_n)
  }

extracted_coorddata_2010 <- extracted_coorddata_2010 %>% 
  distinct(lon_x, .keep_all = TRUE)

create_url_coordinates2020 <- function(long,lat) {
  base_url <- "https://geocoding.geo.census.gov/geocoder/geographies/coordinates"
  # Construct the URL with the coordinates
  paste0(base_url, "?x=", long, "&y=", lat, "&benchmark=Public_AR_Census2020&vintage=Census2020_Census2020&format=json")
}

#https://geocoding.geo.census.gov/geocoder/geographies/coordinates?x=-89.76957998987092&y=43.21569865077441&benchmark=Public_AR_Census2020&vintage=Census2010_Census2020&format=json

results_coordinates2020 <- Map(function(long,lat) query_api(create_url_coordinates2020(long,lat)), 
                               non_matched$long,
                               non_matched$lat 
)

extracted_coorddata_2020 <- data.frame(
  lon_x = character(),
  lat_y = character(),
  Upper_SLDU_GEOID_2020 = character(),
  Upper_SLDU_NAME_2020 = character(),
  Lower_SLDL_GEOID_2020 = character(),
  Lower_SLDL_NAME_2020 = character(),
  Census_Block_GEOID_2020 = character(),
  Census_Block_NAME_2020 = character(),
  Census_Tract_GEOID_2020 = character(),
  Census_Tract_NAME_2020 = character(),
  Congressional_District_NAME_2020 = character(),
  Congressional_District_GEOID_2020 = character(),
  stringsAsFactors = FALSE # To keep strings as characters
)

for (rescoord2020 in results_coordinates2020)
{
  
  new_row_2020 <- data.frame(
    lon_x = rescoord2020$result$input$location$x,
    lat_y = rescoord2020$result$input$location$y,
    Upper_SLDU_GEOID_2020 = rescoord2020$result$geographies$`State Legislative Districts - Upper`$GEOID,
    Upper_SLDU_NAME_2020 = rescoord2020$result$geographies$`State Legislative Districts - Upper`$NAME,
    Lower_SLDL_GEOID_2020 = rescoord2020$result$geographies$`State Legislative Districts - Lower`$GEOID,
    Lower_SLDL_NAME_2020 = rescoord2020$result$geographies$`State Legislative Districts - Lower`$NAME,
    Census_Block_GEOID_2020 = rescoord2020$result$geographies$`Census Blocks`$GEOID,
    Census_Block_NAME_2020 = rescoord2020$result$geographies$`Census Blocks`$NAME,
    Census_Tract_GEOID_2020 = rescoord2020$result$geographies$`Census Tract`$GEOID,
    Census_Tract_NAME_2020 = rescoord2020$result$geographies$`Census Tract`$NAME,
    Congressional_District_NAME_2020 = rescoord2020$result$geographies$`116th Congressional Districts`$NAME,
    Congressional_District_GEOID_2020 = rescoord2020$result$geographies$`116th Congressional Districts`$GEOID,
    stringsAsFactors = FALSE
  )
  
  extracted_coorddata_2020 <- rbind(extracted_coorddata_2020, new_row_2020)
}

extracted_coorddata_2020 <- extracted_coorddata_2020 %>% 
  distinct(lon_x, .keep_all = TRUE)

## COMBINE ALL DATA INTO A SINGLE USER FILE
coord_data_geo <- merge(extracted_coorddata_2010,extracted_coorddata_2020, by=c("lon_x","lat_y"))
coord_data_geo$merge_name <- paste(coord_data_geo$lat_y,coord_data_geo$lon_x, sep=", ")
coord_data_geo$lon_x <- NULL
coord_data_geo$lat_y <- NULL
coord_data_geo$type <- "coordinates"
address_data_geo$type <- "address"

merged_geog <- rbind(coord_data_geo,address_data_geo)
participant_geographies <- merge(ccs_participants_data,merged_geog, by="merge_name",all.x=TRUE)
write.csv(participant_geographies,"/Volumes/cbjackson2/ccs-knowledge/ccs-data[papers]/participant_geographies.csv")
