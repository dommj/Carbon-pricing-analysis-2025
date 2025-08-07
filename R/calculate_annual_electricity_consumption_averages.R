
calculate_annual_electricity_consumption_averages <- function(all_average_profiles,
                                                              jacobs_curtailment){
  
  #calculate annual electricity consumption and exports from tou_profiles
  annual_consumption_exports <- all_average_profiles %>% 
    group_by(year, state, season, pv, battery, electrification, hour) %>% 
    summarise(power_kwh = sum(power_kwh)) %>% 
    mutate(consumption_export = if_else(power_kwh < 0, "Exports", "Consumption")) %>%
    group_by(year, state, season, pv, battery, electrification, consumption_export) %>% 
    summarise(power_kwh = sum(power_kwh)) %>%  
    ungroup() %>% 
    mutate(season_total = power_kwh * (365 / 4)) %>% 
    group_by(year, state, pv, battery, electrification, consumption_export) %>% 
    summarise(annual_consumption_kwh = sum(season_total)) %>%  
    ungroup() %>% 
    #remove WA data post 2034 becuase esoo only goes out til thenb
    filter(!(state== "WA" & year > 2034),
           year >= 2025) %>% # we only have all data from 2025
    left_join(jacobs_curtailment, by = c('year', 'state'),
              relationship = 'many-to-many') %>%
    mutate(annual_consumption_kwh = if_else(consumption_export == 'Exports',
          (1-curtailment) * annual_consumption_kwh, annual_consumption_kwh))
  
  annual_consumption_exports
  
}