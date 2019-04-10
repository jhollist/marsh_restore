source(here::here("R/functions.R"))

profiles <- read_csv(here("data/profiles.csv"))


high_marsh <- profile_figure(profiledf = profiles, habitat = "high marsh", 
                             title = "A. High Marsh Elevation Profile")

high_marsh_ <- profile_figure(profiledf = profiles, habitat = "high marsh mix", 
                             title = "A. High Marsh Mix Elevation Profile")

sa_bare <- profile_figure(profiledf = profiles, habitat = "s. alt and bare", 
                             title = "A. Spartina alterniflora and bare Elevation Profile")
  

# Build out figure with dist (or proportional dist) on x and elevation on y
# Show different habitat sections of profiles
# maybe break out habitats into separate plots
# Do per transect
# Smooth profiles (neg exponential used prior)