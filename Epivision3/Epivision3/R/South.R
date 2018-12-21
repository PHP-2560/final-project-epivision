#' Create South Regional Prevalence Map
#'
#'This function maps regional prevalence of a variable of choice across all time points
#'of the dataset. South includes: Delaware, Maryland, West Virginia, Virginia, Kentucky,
#'Tennessee, North Carolina, South Carolina, Georgia, Alabama, Mississippi, Arkansas,
#'Louisiana, Florida, Texas, and Oklahoma.
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
#'map_south_prevalence(Fatalities_clean,existing_cases= "fatal",population="pop", state="state", year="year")
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

map_south_prevalence <- function(data,existing_cases,population,state,year) {
  us_map <- map_data("state")
  data %>%
    group_by(!! sym(state), !! sym(year)) %>%
    mutate(prevalence = !! sym(existing_cases) / !!sym(population)) %>%

    #left_join(state_abbs, by = c("state" = "abb")) %>%
    right_join(us_map, by = c("state_full" = "region")) %>%

    filter(!! sym(state) %in% c("de","md","wv","va","ky","tn","nc","sc",
                                "ga","al","ms","ar","la","fl","tx","ok")) %>%
    ggplot(aes(x = long, y = lat, group = group, fill = `prevalence`)) +
    geom_polygon(color = "white") + ggtitle("Prevalence in the South") +
    theme_void() +
    scale_fill_viridis(name = "Prevalence")
}
