#calculate average petrol consumption per household

calculate_average_petrol_consumption <- function(ev_fleet_data, average_petrol_use_per_km, average_km_per_vehicle, vehicles_per_household){
  
  
    ev_fleet_data %>% 
    group_by(year, state, fuel_type) %>%
    summarise(vehicles_count = sum(vehicles_count)) %>%
    group_by(year, state) %>%
    mutate(fleet_prop = vehicles_count/sum(vehicles_count)) %>%
    ungroup() %>% 
    
    select(year, state, fuel_type, fleet_prop) %>% 
    left_join(average_petrol_use_per_km) %>% 
    left_join(average_km_per_vehicle) %>% 
    left_join(vehicles_per_household %>% 
                select(state, year, vehicles_per_household),
              relationship = "many-to-many") %>% 
    mutate(effective_vehicles_per_household = fleet_prop * vehicles_per_household,
           average_petrol_use_per_household = effective_vehicles_per_household * average_kilometres_travelled * fuel_l_km) %>% 
    group_by(year, state) %>% 
    summarise(average_annual_consumption_litres = sum(average_petrol_use_per_household)) %>% 
    mutate(category = 'Petrol')
  
}

# av_fuel_consumption <- calculate_average_petrol_consumption(ev_fleet_data, average_petrol_use_per_km, average_km_per_vehicle, vehicles_per_household)
# 
# ev_fleet_data %>% 
#   select(state, fuel_type, fleet_prop) %>% 
#   left_join(average_petrol_use_per_km) %>% 
#   left_join(average_km_per_vehicle) %>% 
#   left_join(vehicles_per_household %>% 
#               select(state, year, vehicles_per_household))