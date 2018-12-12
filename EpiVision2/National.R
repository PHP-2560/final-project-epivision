map_national_prevalence <- function(data,existing_cases,population,state,year) {
  us_map <- map_data("state")
  data %>%
    group_by(!! sym(state), !! sym(year)) %>%
    mutate(prevalence = !! sym(existing_cases) / !!sym(population)) %>%
    #left_join(state_abbs, by = c("state" = "abb")) %>%
    right_join(us_map, by = c("state_full" = "region")) %>%
    ggplot(aes(x = long, y = lat, group = group, fill = `prevalence`)) +
    geom_polygon(color = "white") + ggtitle("National Prevalence of Car Fatalities 1982-1988") +
    theme_void() + 
    scale_fill_viridis(name = "Prevalence")
}