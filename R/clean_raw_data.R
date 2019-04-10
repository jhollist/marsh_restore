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
profile_1 <- prof_2013 %>%
  rbind(prof_2016) %>%
  filter(transect == 1) %>%
  filter(distance >= t1[1] & distance <= t1[2])

profile_2 <- prof_2013 %>%
  rbind(prof_2016) %>%
  filter(transect == 2) %>%
  filter(distance >= t2[1] & distance <= t2[2])

profile_3 <- prof_2013 %>%
  rbind(prof_2016) %>%
  filter(transect == 3) %>%
  filter(distance >= t3[1] & distance <= t3[2])

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
  group_by(transect) %>%
  mutate(prop_distance = rescale(distance)) %>%
  ungroup() %>%
  arrange(year, transect, distance) %>%
  mutate(habitat = zoo::na.locf(habitat, na.rm = FALSE)) %>%
  mutate(habitat_13 = case_when(year == 2013 ~ habitat)) %>%
  mutate(habitat_13 = case_when(habitat_13 == "Salt meadow" ~ "high marsh", 
                                habitat_13 == "High marsh mix" ~ 
                                  "high marsh mix",
                                habitat_13 == "Spartina alterniflora" | 
                                  habitat_13 ==  "Bare/die-off (platform)" ~ 
                                  "s. alt and bare",
                                TRUE ~ NA_character_))

#Classify 2016 sections based on 2013 habitat
  
habitat_13_distances <- profile %>%
  filter(year == 2013) %>%
  mutate(habitat_id = create_habitat_id(habitat_13)) %>%
  group_by(transect, habitat_id, habitat_13) %>%
  summarize(start_dist = min(distance),
            end_dist = max(distance)) %>%
  ungroup() %>%
  select(transect, habitat_13, start_dist, end_dist) %>%
  na.omit() %>%
  gather("dist_type", "distance", start_dist:end_dist) %>%
  arrange(transect, distance) %>%
  select(transect, habitat_13, distance)


profile <- profile %>%
  filter(year == 2016) %>%
  select(transect, habitat_13, distance) %>%
  rbind(habitat_13_distances) %>%
  arrange(transect,distance) %>% 
  mutate(habitat_13_x = zoo::na.locf(habitat_13, na.rm = F),
         year = 2016) %>%
  select(year, transect, distance, habitat_13_x) %>%
  na.omit() %>%
  right_join(profile) %>%
  mutate(habitat_13 = case_when(is.na(habitat_13) ~ habitat_13_x,
                                TRUE ~ habitat_13)) %>%
  select(-habitat_13_x) %>%
  write_csv("data/profiles.csv")



