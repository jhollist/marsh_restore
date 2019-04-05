source(here::here("R/functions.R"))

profiles <- read_csv(here("data/profiles.csv"))

profiles %>%
  ggplot(aes(x = prop_distance, y = elevation, color = factor(year))) +
  geom_line() +
  facet_grid(transect ~ ., scales = "free")

# Build out figure with dist (or proportional dist) on x and elevation on y
# Show different habitat sections of profiles
# maybe break out habitats into separate plots
# Do per transect
# Smooth profiles (neg exponential used prior)