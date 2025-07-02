
calculate_average_gas_costs <- function(gas_retail_volumetric_price_projections,
                            gas_connection_charge_projections,
                            gas_network_charge_revenue,
                            household_connections,
                            average_gas_consumption){

  gas_costs <- left_join(average_gas_consumption, gas_retail_volumetric_price_projections) %>% 
    left_join(gas_network_charge_revenue) %>% 
    left_join(household_connections) %>%
    select(year, state, category, electrification, average_annual_consumption_gj, dollars_per_gj, network_revenue_dollars, connections) %>% 
    mutate(average_volume_cost_dollars = average_annual_consumption_gj * dollars_per_gj,
           #total connection revenue from gas customers divided by total number of households
           average_connection_cost_dollars = network_revenue_dollars / connections,
           average_cost_dollars = average_volume_cost_dollars + average_connection_cost_dollars) %>% 
    select(year, state, category, electrification, average_cost_dollars)
  
  
  gas_costs %>% 
    filter(state == "Vic") %>% 
    ggplot(aes(x = year, y = average_cost_dollars, colour = electrification)) +
    geom_line()
  
  
  return(gas_costs)

}