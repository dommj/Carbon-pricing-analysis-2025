
#calculate annual consumption based on km driven in 2020 SMVU multiplied by average efficiency of vehicles over time.

calculate_scaled_ev_consumption_profiles <- function(average_km_per_vehicle,
                                            ev_efficiency_projections,
                                            ev_fleet_data,
                                            ev_consumption_profiles){

  vehicle_type_weights <- ev_fleet_data %>% 
    filter(fuel_type == "BEV") %>% # battery EVs dominate the charging profile
    select(year, state, vehicle_type, vehicles_count)
  
  ev_fleet_efficiency <- ev_efficiency_projections %>% 
    mutate(vehicle_type = paste0(str_to_sentence(size), " Residential")) %>% 
    left_join(vehicle_type_weights) %>% 
    group_by(year, state) %>% 
    summarise(kwh_km = weighted.mean(kwh_km, vehicles_count))
  
  
  ev_annual_consumption <- ev_fleet_efficiency %>% 
    left_join(average_km_per_vehicle) %>% 
    mutate(smvu_annual_kwh = kwh_km * average_kilometres_travelled)
    
  
  #normalised consumption profiles:
  
  #calculate annual consumptions for non-scaled profiles
  
  non_scaled_totals <- ev_consumption_profiles %>% 
    group_by(year, state) %>% 
    summarise(aemo_annual_kwh = sum(power_kwh) * 365)
  
  #normalise and then scale
  scaled_profiles <- ev_consumption_profiles %>% 
    left_join(non_scaled_totals) %>% 
    left_join(ev_annual_consumption) %>% 
    mutate(power_kwh = power_kwh * (smvu_annual_kwh / aemo_annual_kwh)) %>% 
    filter(year >= 2024) %>% 
    select(year, state, hour, power_kwh)
  
  return(scaled_profiles)
  

}