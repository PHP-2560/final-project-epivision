#' Create National Prevalence Map
#'
#'This function maps national prevalence of a variable of choice across
#'    all time points of the dataset.
#'
#' Arguments:
#' data: dataset of choice formatted as specified
#' existing cases: variable for prevalence calculation
#' population: variable for population estimate
#' state: state arugment must be "state_full"
#' year: year argument must be "year"
#'
#'@Example
#' map_national_prevalence(Fatalities_clean,existing_cases= "fatal",population="pop", state="state", year="year")
#'
#'@export

library(maps)
library(usmap)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(ggmap)
library(mapdata)
library(viridis)
library(AER)

map_national_prevalence <- function(data,existing_cases,population,state,year) {
  us_map <- map_data("state")
  data %>%
    group_by(!! sym(state), !! sym(year)) %>%
    mutate(prevalence = !! sym(existing_cases) / !!sym(population)) %>%
    right_join(us_map, by = c("state_full" = "region")) %>%
    ggplot(aes(x = long, y = lat, group = group, fill = `prevalence`)) +
    geom_polygon(color = "white") + ggtitle("National Prevalence of Car Fatalities 1982-1988") +
    theme_void() +
    scale_fill_viridis(name = "Prevalence")
}
