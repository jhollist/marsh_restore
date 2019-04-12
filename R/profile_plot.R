source(here::here("R/functions.R"))

profiles <- read_csv(here("data/profiles.csv"))

profiles_smooth <- classify_smooth(profiles, span = 0.15)

high_marsh <- profile_figure(profiledf = profiles_smooth, habitat = "high marsh", title = "A. High marsh elevation profile")

high_marsh
ggsave(here("figures/high_marsh_figure .jpg"), high_marsh, width = 7.5, 
        height = 5.625, units = "in", dpi = 300)

high_marsh_mix <- profile_figure(profiledf = profiles_smooth, habitat = "high marsh mix", 
                             title = "B. High marsh mix elevation profile")

ggsave(here("figures/high_marsh_mix_figure .jpg"), high_marsh_mix, width = 7.5, 
       height = 5.625, units = "in", dpi = 300)

sa_bare <- profile_figure(profiledf = profiles_smooth, habitat = "s. alt and bare", 
                             title = "C. Spartina alterniflora and bare elevation profile")

ggsave(here("figures/sa_bare_figure .jpg"), sa_bare, width = 7.5, 
       height = 5.625, units = "in", dpi = 300)

combine_profile <- cowplot::plot_grid(high_marsh, high_marsh_mix, sa_bare, 
                                      ncol = 1)

# Build out figure with dist (or proportional dist) on x and elevation on y
# Show different habitat sections of profiles
# maybe break out habitats into separate plots
# Do per transect
# Smooth profiles (neg exponential used prior)
