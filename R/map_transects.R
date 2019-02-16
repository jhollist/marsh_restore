source(here::here("R/functions.R"))

# Read in csv files
wells <- read_csv(here("data/wells.csv"))
prof_13 <- read_csv(here("data/prof_2013.csv"))
prof_16 <- read_csv(here("data/prof_2016.csv"))

# Convert dfs files into POINT
wells_sf <- wells %>%
  st_as_sf(coords = c("long", "lat"),
           crs = "+proj=longlat +datum=WGS84", 
           agr = "constant")

prof_13_sf <- prof_13 %>%
  st_as_sf(coords = c("longitude", "latitude"),
           crs = "+proj=longlat +datum=WGS84", 
           agr = "constant")

prof_16_sf <- prof_16 %>%
  st_as_sf(coords = c("longitude", "latitude"),
           crs = "+proj=longlat +datum=WGS84", 
           agr = "constant")

mapview(wells_sf) + prof_13_sf + prof_16_sf

# Something screwy with units