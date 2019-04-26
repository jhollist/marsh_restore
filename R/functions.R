#' All required packages
library(here)
library(readxl)
library(readr)

library(dplyr)
library(stringr)
library(tidyr)

library(ggplot2)
library(hrbrthemes)
library(cowplot)

#' convert degrees minutes decimal seconds to decimal degrees
#' 
#' @param dmds a character vector with degrees, minutes, and decimal seconds
#' @param sep separater that separates the values
#' 
#' @import stringr
#' 
#' @export
dmds <- function(dmds, sep = " "){
  xdf <- data.frame(str_split(dmds, " ", simplify = T), stringsAsFactors = FALSE)
  names(xdf) <- c("degrees", "minutes", "seconds")  
  xdf %>%
    mutate_all(as.numeric) %>%
    mutate(decimal_degrees = degrees + (minutes + seconds/60)/60) %>%
    pull(decimal_degrees)
}

#' convert crazy RTK units
#' 
#' @param dmds a numeric vector with whacked units
#' 
#' @import stringr
#' 
#' @export
rtk <- function(rtks, neg = F){
  rtks <- as.character(rtks)
  if(neg){
    d <- str_sub(rtks, 1, 3)
    m <- str_sub(rtks, 5, 6)
    s <- str_sub(rtks, 7, 8)
    sd <- str_sub(rtks, 9, str_length(rtks))
    dms <- paste(paste(d, m, s), sd, sep = ".")
  } else {
    d <- str_sub(rtks, 1, 2)
    m <- str_sub(rtks, 4, 5)
    s <- str_sub(rtks, 6, 7)
    sd <- str_sub(rtks, 8, str_length(rtks))
    dms <- paste(paste(d, m, s), sd, sep = ".")
  }
  dms
}

#' expand it
#' @param x numeric, ascending ordered vector of indices for a dataframe
expand_it <- function(x){
  # from https://stackoverflow.com/questions/43403282/add-row-in-each-group-using-dplyr-and-add-row
  y <- seq(nrow(x)) %>% 
    split(group_indices(x, col)) %>% 
    map(~c(NA, .x)) %>%
    unlist
  for(i in seq_along(y)){
    if(i == 1){
      y[i] <- x[1] - 1
    } else {
      y[i] <- ifelse(x[i] == x[i - 1] + 1, x[i-1], x[i] + 1)
    }
  }
}


#' Assign id to each unique section of habitat
#' @param x A vector of habitats, in order based on distance along transect
#' @export
create_habitat_id <- function(x){
  y<-vector("numeric", length(x))
  y_id<-vector("numeric", length(x))
  for(i in seq_along(x)){
    if(i == 1){
      y[i] <- i
    } else {
      y[i] <- ifelse(x[i]==x[i-1], FALSE,i)
    }
  }
  for(i in seq_along(y)){
    if(i == 1){
      y[1] <- y[1]
    } else {
      y[i] <- ifelse(y[i] == 0, y[i-1], y[i])
    }
  }
  y
}

#' function to generate profile plot
#' @param profile_df data frame with the profile data in it
#' @param hab which habitat to plot
#' @param title title for the plot
#' @export
profile_figure <- function(profiledf, 
                           hab = c("high marsh","s. alt and bare", 
                                       "high marsh mix"),
                           title){
  
  hab <- match.arg(hab)
  
  profiledf <- profiledf %>%
    mutate(transect = paste("Transect", transect))
  
  profiledf_hab <- profiledf %>%
    filter(habitat_agg == hab) 
  
  profiledf_2013 <- profiledf %>%
    filter(year == 2013)
  
  profiledf_2016 <- profiledf %>%
    filter(year == 2016)
  
  #profiledf_creek_2013 <- profiledf_2013 %>%
  #  filter(habitat_agg == "creek")
  
  #profiledf_creek_2016 <- profiledf_2016 %>%
  #  filter(habitat_agg == "creek")
  
  creek_dist <- read_csv(here("data/habitats.csv")) %>%
    creek_dists() %>%
    mutate(transect = paste("Transect", transect))
  
  #browser()
  
  profiledf %>%
    ggplot() +
    geom_rect(data = creek_dist,
              aes(xmin = creek_strat, xmax = creek_end, ymin = -Inf, ymax = Inf),
              fill = "grey", alpha = 0.5) +
    geom_line(data = profiledf_2013, aes(x = distance, y = loess_smooth_elev),
              color = "darkred", alpha = 0.5) +
    geom_line(data = profiledf_2016, aes(x = distance, y = loess_smooth_elev),
              color = "darkblue", alpha = 0.5) +
    #geom_line(data = profiledf_creek_2013, aes(x = distance, y = loess_smooth_elev),
    #          color = "black", size = 1.1) +
    #geom_line(data = profiledf_creek_2016, aes(x = distance, y = loess_smooth_elev),
    #          color = "black", size = 1.1) +
    geom_line(data = profiledf_hab, aes(x = distance, y = loess_smooth_elev,
                                        group = habitat_13_id, 
                                        color = factor(year)), size = 1.1) +
    scale_color_manual(values = c("darkred", "darkblue")) +
    facet_grid(transect ~ ., scales = "free") +
    theme_ipsum(axis_title_size = 11) +
    labs(title = title, x = "Distance along transect (m)", 
         y = "Elevation (m NAVD88)", color = "Year") +
    theme(plot.title = element_text(face = "plain"))
}

#' Classify 2016 points based on 2013 habitats
#' 
#' This code embarasses me...  Please do not judge.
#' 
#' @param profiledf data frame of cleaned 2013 and 2016 profiles
#' @param smooth logical to use a loess to smooth and predict to a common set of
#'               distances, or to use min - max distance from 2013 to classify 
#'               points in 2016 between those distances.
#' @export
classify_smooth <- function(profiledf, smooth = TRUE, span = 0.15){
  
  
  # This is ugly code...  but I think it works...
  
  # loess models for smoothing
  t113_loess <- profiledf %>%
    filter(year == 2013, transect == 1) %>%
    loess(elevation ~ distance, span = span, data = .)
  t213_loess <- profiledf %>%
    filter(year == 2013, transect == 2) %>%
    loess(elevation ~ distance, span = span, data = .)
  t313_loess <- profiledf %>%
    filter(year == 2013, transect == 3) %>%
    loess(elevation ~ distance, span = span, data = .)
  t116_loess <- profiledf %>%
    filter(year == 2016, transect == 1) %>%
    loess(elevation ~ distance, span = span, data = .)
  t216_loess <- profiledf %>%
    filter(year == 2016, transect == 2) %>%
    loess(elevation ~ distance, span = span, data = .)
  t316_loess <- profiledf %>%
    filter(year == 2016, transect == 3) %>%
    loess(elevation ~ distance, span = span, data = .)
  
  # merged distances - oops, problem was here.  Only had t1 distances! 
  t1_newdata <- profiledf %>%
    filter(transect == 1) %>%
    select(distance) 
  t2_newdata <- profiledf %>%
    filter(transect == 2) %>%
    select(distance)
  t3_newdata <- profiledf %>%
    filter(transect == 3) %>%
    select(distance)
  
  # smoothed elevations predicted to merged distances
  t113_smooth_elev <- t1_newdata %>%
    mutate(transect = 1, year = 2013, 
           loess_smooth_elev = predict(t113_loess, newdata = .)) %>%
    arrange(year, transect, distance) %>%
    select(transect, year, distance, loess_smooth_elev)
  t213_smooth_elev <- t2_newdata %>%
    mutate(transect = 2, year = 2013, 
           loess_smooth_elev = predict(t213_loess, newdata = .))%>%
    arrange(year, transect, distance) %>%
    select(transect, year, distance, loess_smooth_elev)
  t313_smooth_elev <- t3_newdata %>%
    mutate(transect = 3, year = 2013, 
           loess_smooth_elev = predict(t313_loess, newdata = .)) %>%
    arrange(year, transect, distance) %>%
    select(transect, year, distance, loess_smooth_elev)
  t116_smooth_elev <- t1_newdata %>%
    mutate(transect = 1, year = 2016, 
           loess_smooth_elev = predict(t116_loess, newdata = .)) %>%
    arrange(year, transect, distance) %>%
    select(transect, year, distance, loess_smooth_elev)
  t216_smooth_elev <- t2_newdata %>%
    mutate(transect = 2, year = 2016, 
           loess_smooth_elev = predict(t216_loess, newdata = .)) %>%
    arrange(year, transect, distance) %>%
    select(transect, year, distance, loess_smooth_elev)
  t316_smooth_elev <- t3_newdata %>%
    mutate(transect = 3, year = 2016, 
           loess_smooth_elev = predict(t316_loess, newdata = .)) %>%
    arrange(year, transect, distance) %>%
    select(transect, year, distance, loess_smooth_elev)
  
  profiledf_13 <- profiledf %>%
    filter(year == 2013) %>%
    #mutate(loess_smooth_elev = NA) %>%
    select(transect, year, distance, habitat) 
  
  profiledf_16 <- profiledf %>%
    filter(year == 2016) %>%
    #mutate(loess_smooth_elev = NA) %>%
    select(transect, year, distance, habitat) 
  
  profiledf_smoothed_13 <- t113_smooth_elev %>%
    rbind(t213_smooth_elev) %>%
    rbind(t313_smooth_elev) %>%
    left_join(profiledf_13) %>%
    arrange(transect, distance) %>%
    group_by(transect) %>%
    mutate(habitat = zoo::na.locf(habitat, na.rm = FALSE)) %>%
    ungroup() %>%
    filter(!is.na(loess_smooth_elev)) # drops rows without a smoothe elevation prediction
  
  profiledf_smoothed_16 <-t116_smooth_elev %>%
    rbind(t216_smooth_elev) %>%
    rbind(t316_smooth_elev) %>%
    left_join(profiledf_16) %>%
    arrange(transect, distance) %>%
    group_by(transect) %>%
    mutate(habitat = zoo::na.locf(habitat, na.rm = FALSE)) %>%
    ungroup() %>%
    filter(!is.na(loess_smooth_elev)) # drops rows without a smoothe elevation prediction
  
  profiledf_smoothed <- profiledf_smoothed_13 %>%
    rbind(profiledf_smoothed_16)
  
  habitat_dists_2013 <- profiledf_smoothed %>% 
    filter(year == 2013) %>%
    group_by(transect) %>%
    mutate(habitat_id = create_habitat_id(habitat)) %>%
    ungroup() %>%
    group_by(transect, habitat, habitat_id) %>%
    summarize(min_dist = min(distance),
              max_dist = max(distance)) %>%
    ungroup() %>%
    select(transect, habitat, min_dist, max_dist) %>%
    # Need to figure out NA's getting lost
    gather(type,distance, 3:4) %>%
    mutate(year = NA, loess_smooth_elev = NA) %>%
    arrange(transect, distance) %>%
    select(transect, year, distance, loess_smooth_elev, habitat)

  habitat_13_on_16 <- profiledf_smoothed%>%
    select(transect, year, distance, loess_smooth_elev, habitat) %>%
    filter(year == 2016) %>%
    mutate(habitat = NA) %>%
    rbind(habitat_dists_2013) %>%
    arrange(transect, distance) %>%
    group_by(transect) %>%
    mutate(habitat_13 = zoo::na.locf(habitat, na.rm = FALSE, fromLast = TRUE)) %>%
    ungroup() %>%
    select(transect, year, distance, loess_smooth_elev, habitat_13) %>%
    na.omit() #removes all rows that aren't 2016 plus any rows outside of hab dists

  # Really add the creeks this time (have I mentioned that this code is awful)
  
  #creek_dist <- read_csv(here("data/habitats.csv")) %>%
  #  creek_dists() %>%
  #  data.frame
  
  profiledf <- profiledf_smoothed %>% 
    left_join(habitat_13_on_16) %>%
    # adds in creeks
    # mutate(habitat = case_when(transect == 1 & 
    #                              distance >= creek_dist[1,2] & 
    #                             distance <= creek_dist[1,3] ~ 
    #                               "creek",
    #                             transect == 2 & 
     #                            distance >= creek_dist[2,2] & 
    #                             distance <= creek_dist[2,3] ~ 
    #                             "creek",
    #                           transect == 3 & 
    #                             distance >= creek_dist[3,2] & 
    #                             distance <= creek_dist[3,3] ~ 
    #                             "creek",
    #                           TRUE ~ habitat)) %>%
    mutate(habitat_13 = case_when(year == 2013 ~ habitat,
                                  TRUE ~ habitat_13)) %>%
    filter(!is.na(habitat_13)) %>%
    mutate(habitat_agg = case_when(habitat_13 == "Salt meadow" ~ "high marsh",
                                   habitat_13 == "High marsh mix" ~ "high marsh mix",
                                   habitat_13 == "Spartina alterniflora" | 
                                     habitat_13 ==  "Bare/die-off (platform)" ~  "s. alt and bare",
                                   #habitat_13 == "creek" ~ "creek",
                                   TRUE ~ NA_character_)) %>%
    group_by(year, transect) %>%
    mutate(habitat_13_id = paste0(year,"_",transect, "_", 
                                  create_habitat_id(habitat_13)),
           habitat_agg_id = paste0(year,"_",transect, "_", 
                                  create_habitat_id(habitat_agg))) %>%
    ungroup() 

  
  #max_row_idx <- profiledf %>% 
  #  group_by(habitat_agg_id) %>% 
  #  mutate(grp_max = max(distance)) %>% 
  #  ungroup() %>% 
  #  mutate(grp_max_logic = distance == grp_max) %>%
  #  pull(grp_max_logic) %>%
  #  which
  
  #max_row_idx <- max_row_idx[-length(max_row_idx)]
  
  #browser()
  #insert_me <- profiledf %>% 
  #  slice(max_row_idx) %>%
  #  mutate(habitat_agg = NA)
  
  #profiledf <- profiledf %>%
  #  rbind(insert_me) %>%
  #  arrange(transect, year, distance) %>%
  #  mutate(habitat_agg = case_when(!is.na(habitat_agg) ~
  #                                   habitat_agg,
  #                                 habitat_13 != "creek" ~ 
  #                                   zoo::na.locf(habitat_agg, na.rm = FALSE, 
  #                                                fromLast = TRUE),
  #                                 habitat_13 == "creek" ~
  #                                   "creek"))
  
  profiledf
}

#' Manual assignment of creek distances becuase I dont have time...
#' 
creek_dists <- function(habdf){
  
 t1_14 <- tibble(transect = 1, year = 2014, start_dist = 44.84261,
                 end_dist = 46.11924)
 t2_14 <- tibble(transect = 2, year = 2014, start_dist = 47.65672,
                 end_dist = 49.26648) 
 t3_14 <- tibble(transect = 3, year = 2014, start_dist = 23.89871,
                 end_dist = 25.09615)
 t1_15 <- tibble(transect = 1, year = 2015, start_dist = 44.89415,
                 end_dist = 46.34214)
 t2_15 <- tibble(transect = 2, year = 2015, start_dist = 47.64392,
                 end_dist = 49.10619) 
 t3_15 <- tibble(transect = 3, year = 2015, start_dist = 23.94900,
                 end_dist = 25.35244)
 t1_16 <- tibble(transect = 1, year = 2016, start_dist = 45.22229,
                 end_dist = 45.99429)
 t2_16 <- tibble(transect = 2, year = 2016, start_dist = 47.40146,
                 end_dist = 49.28349) 
 t3_16 <- tibble(transect = 3, year = 2016, start_dist = 23.86280,
                 end_dist = 25.05876)
 
 dists <- rbind(t1_14) %>%
   rbind(t2_14) %>%
   rbind(t3_14) %>%
   rbind(t1_15) %>%
   rbind(t2_15) %>%
   rbind(t3_15) %>%
   rbind(t1_16) %>%
   rbind(t2_16) %>%
   rbind(t3_16) %>%
   group_by(transect) %>%
   summarize(creek_strat = min(start_dist), 
             creek_end = max(end_dist))
  
 dists
}































