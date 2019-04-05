source(here::here("R/functions.R"))

profiles <- read_csv(here("data/profiles.csv"))
wells <- read_csv(here("data/wells.csv"))

# If we need to have closer start and stop distance (e.g. Transect 1, 2013 starts earlier)
# Different min and max distances.  Want these to more closely match
# Get the max min distance across years per transect and cut off there
# Get the min max distance across years per transect and cut off there

start_end_dists <- profiles %>%
  group_by(transect, year) %>%
  summarize(min_dist = min(distance),
            max_dist = max(distance)) %>%
  ungroup() %>%
  group_by(transect) %>%
  summarize(start_dist = max(min_dist),
            end_dist = min(max_dist))

profiles %>%
  ggplot(aes(x = prop_distance, y = elevation, color = factor(year))) +
  geom_line() +
  facet_grid(transect ~ ., scales = "free") 

prof_1_13 <- profiles %>%
  filter(transect == 1) %>%
  filter(year == 2013) %>%
  mutate(approximate = approx(distance, elevation))

#https://stackoverflow.com/questions/13073686/interpolating-timeseries
appr <- approx(prof_1_13$distance, prof_1_13$elevation)

plot(prof_1_13$distance, prof_1_13$loess_predict)
points(prof_1_13$distance, prof_1_13$elevation)      
