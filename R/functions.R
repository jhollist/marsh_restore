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
