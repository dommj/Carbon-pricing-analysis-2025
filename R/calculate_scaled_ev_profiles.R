calculate_scaled_ev_profiles <- function(ev_consumption_profiles,
                                         ev_efficiency_projections,
                                         average_km_per_vehicle,
                                         ev_fleet_data){
  
  #calculate average efficiency by weighting efficiency by stock type
  average_efficiency <- ev_fleet_data %>% 
    filter(fuel_type == "BEV") %>% 
    mutate(size = str_match(vehicle_type, "\\D*\\s") %>% 
                              str_to_lower() %>% 
                              str_remove("\\s")) %>% 
    group_by(year, size, state) %>% 
    summarise(vehicles_count = sum(vehicles_count)) %>% 
    left_join(ev_efficiency_projections) %>% 
    group_by(year, state) %>% 
    summarise(kwh_km = weighted.mean(kwh_km, vehicles_count))
  
  
  average_annual_consumption <- average_km_per_vehicle %>% 
    left_join(average_efficiency) %>% 
    mutate(annual_consumption_kwh = average_kilometres_travelled * kwh_km)
  
  
  
  #appears to be semi substantial differences for Victoria and SA, why?
  ev_consumption_profiles %>% 
    filter(year == 2024) %>% 
    group_by(year, state) %>% 
    summarise(annual_consumption_kwh_2 = sum(power_kwh * 365)) %>% 
    arrange(annual_consumption_kwh_2) %>% 
    left_join(average_annual_consumption) %>% 
    mutate(pct_error = (annual_consumption_kwh - annual_consumption_kwh_2) / annual_consumption_kwh_2)
  
  
  
}