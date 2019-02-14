#' All required packages
library(dplyr)
library(sp)
library(sf)
library(mapview)
library(stringr)
library(readxl)
library(readr)
library(here)

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
