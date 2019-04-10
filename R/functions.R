#' All required packages
library(dplyr)
library(sp)
library(sf)
library(mapview)
library(stringr)
library(readxl)
library(readr)
library(here)
library(ggplot2)
library(tidyr)
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
    browser()
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
#' @param habitat which habitat to plot
#' @param title title for the plot
#' @export
profile_figure <- function(profiledf, 
                           habitat = c("high marsh","s. alt and bare", 
                                       "high marsh mix"),
                           title){
  
  habitat <- match.arg(habitat)
  profiledf_id <- profiledf %>%
    mutate(habitat_13_id = create_habitat_id(replace_na(habitat_13,""))) %>%
    group_by(habitat_13_id) %>%
    mutate(hab_segment_distance = max(distance) - min(distance)) %>%
    ungroup()
  
  profiledf_loess <- profiledf_id %>%
    group_by(transect, year) %>%
    mutate(loess_smooth_elev = predict(loess(elevation ~ distance, span = 0.2))) %>%
    select(year, transect, distance, elevation, loess_smooth_elev, habitat_13, habitat_13_id)
  
  profile_hab <- profiledf_loess %>%
    filter(habitat_13 == habitat)
  
  profiledf_loess %>%
    ggplot(aes(x = distance, y = loess_smooth_elev)) +
    geom_point(aes(y=elevation, fill = factor(year)), alpha = 0.5) +
    scale_fill_manual(values = c("grey70", "grey50")) +
    geom_line(data = profile_hab, aes(group = habitat_13_id, color = factor(year)), size = 1.5) +
    scale_color_manual(values = c("darkred", "darkblue")) +
    facet_grid(transect ~ ., scales = "free") +
    theme_ipsum() +
    scale_y_continuous(limits = c(0,1)) +
    scale_x_continuous(limits = c(0,100)) +
    labs(title = title)
}

