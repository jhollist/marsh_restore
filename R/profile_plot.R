source(here::here("R/functions.R"))

profiles <- read_csv(here("data/profiles.csv"))

profiles_smooth <- classify_smooth(profiles, span = 0.15)

# Truncate to start and end of marsh
t1 <- c(19, 75)
t2 <- c(20, 118)
t3 <- c(13, 89)

#Meathead way to do this
profile_1 <- profiles_smooth %>%
  filter(transect == 1) %>%
  filter(distance >= t1[1] & distance <= t1[2])

profile_2 <- profiles_smooth %>%
  filter(transect == 2) %>%
  filter(distance >= t2[1] & distance <= t2[2])

profile_3 <- profiles_smooth %>%
  filter(transect == 3) %>%
  filter(distance >= t3[1] & distance <= t3[2])

profiles_smooth <- profile_1 %>%
  rbind(profile_2) %>%
  rbind(profile_3)

high_marsh <- profile_figure(profiledf = profiles_smooth, 
                             habitat = "high marsh", 
                             title = "A. High marsh")

high_marsh
ggsave(here("figures/high_marsh_figure .jpg"), high_marsh, width = 7.5, 
        height = 5.625, units = "in", dpi = 300)

high_marsh_mix <- profile_figure(profiledf = profiles_smooth, 
                                 habitat = "high marsh mix", 
                                 title = "B. High marsh mix")

ggsave(here("figures/high_marsh_mix_figure .jpg"), high_marsh_mix, width = 7.5, 
       height = 5.625, units = "in", dpi = 300)

sa_bare <- profile_figure(profiledf = profiles_smooth, 
                          habitat = "s. alt and bare", 
                          title = expression(paste("C. ", 
                                                   italic("Spartina alterniflora"), 
                                                   "and bare")))

ggsave(here("figures/sa_bare_figure .jpg"), sa_bare, width = 7.5, 
       height = 5.625, units = "in", dpi = 300)

combine_profile <- cowplot::plot_grid(high_marsh, high_marsh_mix, sa_bare, 
                                      ncol = 1)

# Build out figure with dist (or proportional dist) on x and elevation on y
# Show different habitat sections of profiles
# maybe break out habitats into separate plots
# Do per transect
# Smooth profiles (neg exponential used prior)
