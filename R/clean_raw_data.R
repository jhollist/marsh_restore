source(here::here("R/functions.R"))

prof_2013 <- read_excel(here("data/raw/Cogg rest elevation data for Jeff.xlsx"),
                        sheet = "2013 profile data") %>%
  select(Name:DISTANCE_12) %>%
  rename_all(tolower) %>%
  rename(distance = distance_12) %>%
  mutate(longitude_dd = -dmds(rtk(gsub("-","",longitude))), 
                              latitude_dd = dmds(rtk(latitude)),
         year = 2013) %>%
  na.omit() %>% #no na's exepcted so should be fine (look into assertr)
  write_csv("data/prof_2013.csv")

prof_2016 <- read_excel(here("data/raw/Cogg rest elevation data for Jeff.xlsx"),
                        sheet = "2016 profile data") %>%
  select(Name:DISTANCE) %>%
  rename_all(tolower) %>%
  na.omit() %>% #no na's exepcted so should be fine (look into assertr)
  mutate(longitude_dd = -dmds(rtk(gsub("-","",longitude))), 
         latitude_dd = dmds(rtk(latitude)),
         year = 2016) %>%
  write_csv("data/prof_2016.csv")

# Truncate to start and end of marsh
t1 <- c(19, 75)
t2 <- c(20, 118)
t3 <- c(13, 89)

#Meathead way to do this
# No long done... distance filter moved to profile plot after smoothed/habitat stuff
profile_1 <- prof_2013 %>%
  rbind(prof_2016) %>%
  filter(transect == 1) #%>%
  #filter(distance >= t1[1] & distance <= t1[2])

profile_2 <- prof_2013 %>%
  rbind(prof_2016) %>%
  filter(transect == 2) #%>%
  #filter(distance >= t2[1] & distance <= t2[2])

profile_3 <- prof_2013 %>%
  rbind(prof_2016) %>%
  filter(transect == 3) #%>%
  #filter(distance >= t3[1] & distance <= t3[2])

rescale <- function(x){
  (x - min(x))/(max(x) - min(x))
}  

# Add in habitat stuff
habitat <- read_excel(here("data/raw/JHo cogg rest habitat transition data.xlsx")) %>%
  rename_all(tolower) %>%
  rename(distance = distance_12) %>%
  mutate(longitude_dd = -dmds(rtk(gsub("-","",longitude))), 
         latitude_dd = dmds(rtk(latitude))) %>%
  select(name,transect,elevation,latitude,longitude,distance,longitude_dd, 
         latitude_dd,year,habitat)
  
profile <- profile_1 %>%
  rbind(profile_2) %>%
  rbind(profile_3) %>%
  mutate(habitat = NA) %>%
  rbind(habitat) %>%
  filter(year == 2013 | year == 2016) %>%
  arrange(year, transect, distance) %>%
  mutate(habitat = zoo::na.locf(habitat, na.rm = FALSE)) %>%
  mutate(habitat_agg = case_when(habitat == "Salt meadow" ~ "high marsh",
                                 habitat == "High marsh mix" ~ "high marsh mix",
                                 habitat == "Spartina alterniflora" | 
                                   habitat ==  "Bare/die-off (platform)" ~  "s. alt and bare",
                                 TRUE ~ NA_character_)) %>%
  write_csv("data/profiles.csv")



