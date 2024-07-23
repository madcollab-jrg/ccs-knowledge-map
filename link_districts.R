### GET CONGRESSIONAL DISTRICTS

library(sf)
library(tigris)


### GET CONGRESSIONAL DISTRICTS 
# Load the shapefile for census blocks (you would get this from the Census Bureau or the tigris package)
#census_blocks_sf <- tigris::blocks(state = "55", county = "025", year = 2020)
census_blocks_sf <- tigris::blocks(state = "55", year = 2020)

# Load the shapefile for congressional districts
congressional_districts_sf <- tigris::congressional_districts(year = 2020, cb = TRUE)
# Transform the coordinate reference system if needed (make sure they match)
census_blocks_sf <- st_transform(census_blocks_sf, crs = st_crs(congressional_districts_sf))
# Perform the spatial join to get the congressional district for each block
blocks_with_districts <- st_join(census_blocks_sf, congressional_districts_sf)
# Extract the congressional district GEOID
blocks_with_districts$district_GEOID <- blocks_with_districts$GEOID

blocks_with_districts$geometry <- NULL
# Remove data for plotting
write.csv(blocks_with_districts,"/Volumes/cbjackson2/ccs-knowledge/ccs-data/census_add/congressdistrict_match.csv")

### GET STATE
# https://legis.wisconsin.gov/ltsb/gisdocs/AssemblyMaps/Statewide_Assembly_Map_Letter.pdf 


options(tigris_use_cache = TRUE)
options(tigris_class = "sf")

# Load the shapefile for census blocks or tracts
# census_blocks_sf <- blocks(state = "WI", county = "YourCountyCode", year = 2018, cb = TRUE) # For blocks
# OR
census_tracts_sf2 <- tracts(state = "WI", year = 2018) # For tracts

# Load the shapefile for state legislative (assembly) districts
state_assembly_districts_sf <- state_legislative_districts(state = "WI", year = 2018, cb = TRUE)

# Make sure the coordinate reference systems match
#census_blocks_sf <- st_transform(census_blocks_sf, crs = st_crs(state_assembly_districts_sf))
# OR
census_tracts_sf2 <- st_transform(census_tracts_sf2, crs = st_crs(state_assembly_districts_sf))

# Perform the spatial join to get the assembly district for each block or tract
#blocks_with_assembly_districts <- st_join(census_blocks_sf, state_assembly_districts_sf)
# OR
tracts_with_assembly_districts <- st_join(census_tracts_sf2, state_assembly_districts_sf)

# Extract the state assembly district identifier
#blocks_with_assembly_districts$assembly_district <- blocks_with_assembly_districts$SLDLST
# OR
tracts_with_assembly_districts$assembly_district <- tracts_with_assembly_districts$SLDLST
tracts_with_assembly_districts$geometry <- NULL

write.csv(tracts_with_assembly_districts,"/Volumes/cbjackson2/ccs-knowledge/ccs-data/census_add/assemblydistrict_match.csv")


### GET LOWER
census_tracts_sf3 <- tracts(state = "WI", year = 2018) # For tracts

# Load the shapefile for state legislative (assembly) districts
state_lower_districts_sf <- state_legislative_districts(state = "WI",   house = "lower", year = 2018, cb = TRUE)

# Make sure the coordinate reference systems match
#census_blocks_sf <- st_transform(census_blocks_sf, crs = st_crs(state_assembly_districts_sf))
# OR
census_tracts_sf3 <- st_transform(census_tracts_sf3, crs = st_crs(state_lower_districts_sf))

# Perform the spatial join to get the assembly district for each block or tract
#blocks_with_assembly_districts <- st_join(census_blocks_sf, state_assembly_districts_sf)
# OR
tracts_with_lower_districts <- st_join(census_tracts_sf3, state_lower_districts_sf)

# Extract the state assembly district identifier
#blocks_with_assembly_districts$assembly_district <- blocks_with_assembly_districts$SLDLST
# OR
tracts_with_lower_districts$lower_district <- tracts_with_lower_districts$NAME.y
tracts_with_lower_districts$geometry <- NULL

write.csv(tracts_with_lower_districts,"/Volumes/cbjackson2/ccs-knowledge/ccs-data/census_add/lowerdistrict_match.csv")


