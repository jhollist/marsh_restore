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
    
  
  profiledf %>%
    ggplot(aes(x = distance, y = loess_smooth_elev)) +
    geom_line(data = profiledf_2013, aes(x = distance, y = loess_smooth_elev),
              color = "darkred", alpha = 0.5) +
    geom_line(data = profiledf_2016, aes(x = distance, y = loess_smooth_elev),
              color = "darkblue", alpha = 0.5) +
    geom_line(data = profiledf_hab, aes(group = habitat_13_id, 
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
    mutate(habitat_13 = zoo::na.locf(habitat, na.rm = FALSE)) %>%
    ungroup() %>%
    select(transect, year, distance, loess_smooth_elev, habitat_13) %>%
    na.omit() #removes all rows that aren't 2016 plus any rows outside of hab dists

  profiledf <- profiledf_smoothed %>% 
    left_join(habitat_13_on_16) %>%
    mutate(habitat_13 = case_when(year == 2013 ~ habitat,
                                  TRUE ~ habitat_13)) %>%
    filter(!is.na(habitat_13)) %>%
    mutate(habitat_agg = case_when(habitat_13 == "Salt meadow" ~ "high marsh",
                                   habitat_13 == "High marsh mix" ~ "high marsh mix",
                                   habitat_13 == "Spartina alterniflora" | 
                                     habitat_13 ==  "Bare/die-off (platform)" ~  "s. alt and bare",
                                   TRUE ~ NA_character_)) %>%
    group_by(year, transect) %>%
    mutate(habitat_13_id = paste0(year,"_",transect, "_", 
                                  create_habitat_id(habitat_13))) %>%
    ungroup() 
    
  
  profiledf
}
