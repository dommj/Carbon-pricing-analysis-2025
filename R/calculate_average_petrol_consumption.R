#calculate average petrol consumption per household

calculate_average_petrol_consumption <- function(ev_fleet_data, 
                                                 average_petrol_use_per_km, 
                                                 average_km_per_vehicle){
  
  
  num_ice_timeseries <- ev_fleet_data %>%
    
    #plug in hybrids are never estimated to represent more than 1.5% of fleet so for purposes of additional electricity demand we assume electricty consumption equal to EV, this will inflate demand and deflate overall savings slightly
    
    mutate(fuel_type = if_else(fuel_type == "PHEV", "BEV", fuel_type)) %>%
    group_by(year, state, fuel_type) %>%
    summarise(vehicles_count = sum(vehicles_count)) %>% 
    group_by(year, state) %>% 
    mutate(prop_ice = vehicles_count/ sum(vehicles_count),
           num_ice = prop_ice * 1.8) %>% 
    filter(fuel_type == "ICE") %>% 
    select(year, state, fuel_type, num_ice)

  
  petrol_use <- num_ice_timeseries %>% 
    left_join(average_petrol_use_per_km) %>% 
    left_join(average_km_per_vehicle) %>% 
    mutate(average_petrol_use_per_household = num_ice * average_kilometres_travelled * fuel_l_km,
           electrification = T) %>% 
    select(year, state, average_petrol_use_per_household, electrification) %>% 
    ungroup() %>% 
    filter(year <= 2050)
  
  #and create a parallel dataset for ICE use staying the same from 2024
  
  petrol_use_no_electrification <- petrol_use %>% 
    filter(year == 2025) %>% 
    select(-year) %>% 
    cross_join(tibble(year = seq(2025, 2050))) %>% 
    mutate(electrification = F)
  
  petrol_use_all <- bind_rows(petrol_use, petrol_use_no_electrification)
  
  return(petrol_use_all)
}

