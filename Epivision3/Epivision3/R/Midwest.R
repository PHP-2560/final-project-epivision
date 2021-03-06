#' Create Midwest Regional Prevalence Map
#'
#'This function maps regional prevalence of a variable of choice across all time points
#'of the dataset. Midwest includes: Ohio, Michigan, Indiana, Wisconsin, Illinois,
#'Minnesota, Iowa, Missouri, North Dakota, South Dakota, Nebraska, and Kansas.
#'
#'
#' Arguments:
#' data: dataset of choice formatted as specified
#' existing cases: variable for prevalence calculation
#' population: variable for population estimate
#' state: state arugment must be "state_full"
#' year: year argument must be "year"
#'
#'@Example
#'map_midwest_prevalence(Fatalities_clean,existing_cases= "fatal",population="pop", state="state", year="year")
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

map_midwest_prevalence <- function(data,existing_cases,population,state,year) {
  us_map <- map_data("state")
  data %>%
    group_by(!! sym(state), !! sym(year)) %>%
    mutate(prevalence = !! sym(existing_cases) / !!sym(population)) %>%

    #left_join(state_abbs, by = c("state" = "abb")) %>%
    right_join(us_map, by = c("state_full" = "region")) %>%

    filter(!! sym(state) %in% c("oh","mi","in","wi","il","mn","ia","mo","nd","sd","ne","ks")) %>%
    ggplot(aes(x = long, y = lat, group = group, fill = `prevalence`)) +
    geom_polygon(color = "white") + ggtitle("Prevalence in the Midwest") +
    theme_void() +
    scale_fill_viridis(name = "Prevalence")
}
