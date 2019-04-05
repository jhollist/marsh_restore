source(here::here("R/functions.R"))

profiles <- read_csv(here("data/profiles.csv"))

profiles %>%
  ggplot(aes(x = prop_distance, y = elevation, color = factor(year))) +
  geom_line() +
  facet_grid(transect ~ ., scales = "free")