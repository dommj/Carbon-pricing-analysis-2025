# calculate petrol costs

calculate_cameo_petrol_costs <- function(average_petrol_use_per_km, 
                                         average_km_per_vehicle,
                                         petrol_price_projections){
  
  fuel_consumption_ice <- average_petrol_use_per_km %>% 
    filter(fuel_type == "ICE") %>% 
    pull(fuel_l_km)
  
  average_fuel_use_per_vehicle <- average_km_per_vehicle %>% 
   mutate(avg_fuel_use = average_kilometres_travelled * fuel_consumption_ice)
 
  
  #attach petrol price projections to average fuel use (like a full join but theres no matching variable)
  
  average_fuel_cost_per_vehicle <- average_fuel_use_per_vehicle %>% 
    cross_join(petrol_price_projections) %>% 
    mutate(petrol_cost = avg_fuel_use * c_litre) %>% 
    mutate(`0` = 0 * petrol_cost / 100,
           `1` = 1 * petrol_cost / 100,
           `2` = 2 * petrol_cost / 100) %>% 
    pivot_longer(cols = c(`0`, `1`, `2`), names_to = "ice", values_to = "average_cost_dollars") %>% 
    select(year, state, category, ice, average_cost_dollars)
   
  
  average_fuel_cost_per_vehicle
}