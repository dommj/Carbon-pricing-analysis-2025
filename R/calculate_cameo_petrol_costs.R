# calculate petrol costs

calculate_cameo_petrol_costs <- function(average_petrol_use_per_km, 
                                         average_km_per_vehicle,
                                         petrol_price_data){
  
  fuel_consumption_ice <- average_petrol_use_per_km %>% 
    filter(fuel_type == "ICE")
  
  average_fuel_use_per_vehicle <- average_km_per_vehicle %>% 
    cross_join(fuel_consumption_ice) %>% 
   mutate(avg_fuel_use = average_kilometres_travelled * fuel_l_km)
 
  
  #attach petrol price projections to average fuel use (like a full join but theres no matching variable)
  
  average_fuel_cost_per_vehicle <- average_fuel_use_per_vehicle %>% 
    left_join(petrol_price_data) %>% 
    mutate(petrol_cost = avg_fuel_use * c_litre) %>% 
    mutate(`0` = 0 * petrol_cost / 100,
           `1` = 1 * petrol_cost / 100,
           `2` = 2 * petrol_cost / 100) %>% 
    pivot_longer(cols = c(`0`, `1`, `2`), names_to = "ice", values_to = "annual_cost_dollars") %>% 
    
    ## getting an error here -- can't select year? this is year-invariant data?
    select(#year, 
      state, category, ice, annual_cost_dollars)
   
  params <- expand_grid(
    cooking = c("gas", "electric"),
    water_heating = c("gas", "electric"),
    space_heating = c("gas", "electric"),
    pv = c(TRUE, FALSE),
    battery = c(TRUE, FALSE),
    ev = c(0, 1, 2)
  )
  
  average_fuel_cost_per_vehicle <- average_fuel_cost_per_vehicle %>% 
    cross_join(params) %>% 
    mutate(ice = as.numeric(ice)) %>% 
    #cross join with battery or no (gas consumption the same), only allow batteries with PV
    filter(!(pv == F & battery == T))
  
  
  average_fuel_cost_per_vehicle
}