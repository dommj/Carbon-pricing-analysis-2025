#calculate household costs

calculate_household_costs <- function(retail_price_data, 
                                      gas_volume_price_data,
                                      #gas_supply_charges_projections,
                                      petrol_price_projections,
                                      average_residential_operational_demand,
                                      average_gas_consumption,
                                      average_petrol_consumption){
  
  
  #electricity costs
  
  electricity_costs <- left_join(retail_price_data, average_residential_operational_demand) %>% 
    select(year, state, category, average_annual_consumption_kwh, c_kwh) %>% 
    mutate(average_cost_dollars = average_annual_consumption_kwh * c_kwh / 100) %>% 
    select(year, state, category, average_cost_dollars)
  
  petrol_costs <- left_join(petrol_price_projections, average_petrol_consumption) %>% 
    select(year, state, category, average_annual_consumption_litres, c_litre) %>% 
    mutate(average_cost_dollars = average_annual_consumption_litres * c_litre / 100) %>%
    select(year, state, category, average_cost_dollars)
  
  #even just volume costs seem way off, VIc costs seem to be on the order of $35 per GJ not $12
  gas_costs <- left_join(gas_volume_price_data, average_gas_consumption) %>% 
    select(year, state, category, average_annual_consumption_gj, dollars_per_gj) %>% 
    mutate(average_cost_dollars = average_annual_consumption_gj * dollars_per_gj) %>% 
    select(year, state, category, average_cost_dollars)
  
  household_costs <- bind_rows(electricity_costs, petrol_costs, gas_costs)
  
  household_costs
}


calculate_household_costs(retail_price_data, 
                          gas_volume_price_data,
                          #gas_supply_charges_projections,
                          petrol_price_projections,
                          average_residential_operational_demand,
                          gsoo_consumption_data,
                          average_petrol_consumption)
