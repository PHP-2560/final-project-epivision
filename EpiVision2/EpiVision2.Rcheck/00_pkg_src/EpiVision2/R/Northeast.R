#' Create Northeast Regional Prevalence Map
#'
#'This function maps regional prevalence of a variable of choice across all time points
#'of the dataset. Northeast includes: Massachusetts, Rhode Island, Connecticut, Maine,
#'Vermont, New Hampshire, New York, New Jersey, and Pennsylvania.
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
#'map_northeast_prevalence(Fatalities_clean,existing_cases= "fatal",population="pop", state="state", year="year")
#'@export

map_northeast_prevalence <- function(data,existing_cases,population,state,year) {
  us_map <- map_data("state")
  data %>%
    group_by(!! sym(state), !! sym(year)) %>%
    mutate(prevalence = !! sym(existing_cases) / !!sym(population)) %>%

    #left_join(state_abbs, by = c("state" = "abb")) %>%
    right_join(us_map, by = c("state_full" = "region")) %>%

    filter(!! sym(state) %in% c("ma","ri","ct","me","vt","nh","ny","nj","pa")) %>%
    ggplot(aes(x = long, y = lat, group = group, fill = `prevalence`)) +
    geom_polygon(color = "white") + ggtitle("Prevalence in the Northeast") +
    theme_void() +
    scale_fill_viridis(name = "Prevalence")
}
