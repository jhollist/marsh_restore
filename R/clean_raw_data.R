
source(here::here("R/functions.R"))

wells <- read_excel(here("data/raw/Cogg rest elevation data for Jeff.xlsx"),
                    sheet = "Well locations") %>%
  rename_all(tolower) %>%
  select(well_num = `well #`, name, long = long, lat = lat) %>%
  mutate(long = -dmds(long), lat = dmds(lat)) %>%
  na.omit() %>% #no na's exepcted so should be fine (look into assertr)
  write_csv(here("data/wells.csv"))


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

profile <- profile_1 %>%
  rbind(profile_2) %>%
  rbind(profile_3) %>%
  group_by(transect) %>%
  mutate(prop_distance = rescale(distance)) %>%
  write_csv("data/profiles.csv")



