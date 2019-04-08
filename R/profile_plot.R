source(here::here("R/functions.R"))

profiles <- read_csv(here("data/profiles.csv"))

habitat_frq <- profiles %>%
  group_by(habitat) %>%
  summarize(count = n()) %>% #Count should be close to total distance as each 
                             #point was ~ 1 meter apart
  arrange(desc(count)) %>%
  slice(1:5) # Get top 5

profiles %>%
  ggplot(aes(x = distance, y = elevation, color = factor(year))) +
  geom_point() +
  stat_smooth(method = "loess", span = 0.125, se = FALSE) + 
  geom_rect(aes(xmin = distance, xmax = distance, fill = habitat_13), ymin = -Inf, ymax = Inf) +
  facet_grid(transect ~ ., scales = "free")

# Build out figure with dist (or proportional dist) on x and elevation on y
# Show different habitat sections of profiles
# maybe break out habitats into separate plots
# Do per transect
# Smooth profiles (neg exponential used prior)