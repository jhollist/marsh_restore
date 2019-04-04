
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

profiles <- prof_2013 %>%
  rbind(prof_2016) %>% 
  write_csv("data/profiles.csv")



