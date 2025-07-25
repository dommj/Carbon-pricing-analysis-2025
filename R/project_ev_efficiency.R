project_ev_efficiency <- function(ev_efficiency_file){
  
  ev_efficiency <- read.csv(ev_efficiency_file)
  
  interpolated_ev_efficiency <- ev_efficiency %>% 
    complete(year = seq(2024, 2050, by = 1), 
             size) %>%
    # Group by size and interpolate
    group_by(size) %>%
    arrange(year) %>%
    mutate(kwh_km = approx(year, kwh_km, year, method = "linear")$y) %>%
    ungroup()
  
  
  interpolated_ev_efficiency %>% 
    ggplot(aes(x = year, y = kwh_km, colour = size)) +
    geom_line()
  
  return(interpolated_ev_efficiency)
}