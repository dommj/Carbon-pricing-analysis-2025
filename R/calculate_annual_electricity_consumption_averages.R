
calculate_annual_electricity_consumption_averages <- function(average_profiles){
  
  #calculate annual electricity consumption and exports from tou_profiles
  annual_consumption_exports <- average_profiles %>% 
    group_by(year, state, season, pv, electrification, hour) %>% 
    summarise(power_kwh = sum(power_kwh)) %>% 
    mutate(consumption_export = if_else(power_kwh < 0, "Exports", "Consumption")) %>%
    group_by(year, state, season, pv, electrification, consumption_export) %>% 
    summarise(power_kwh = sum(power_kwh)) %>%  
    ungroup() %>% 
    mutate(season_total = power_kwh * (365 / 4)) %>% 
    group_by(year, state, pv, electrification, consumption_export) %>% 
    summarise(annual_consumption_kwh = sum(season_total)) %>%  
    ungroup()
  
  annual_consumption_exports
  
}