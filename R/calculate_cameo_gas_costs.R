# calculate gas costs

function(gas_retail_volumetric_price_projections,
         gas_connection_charge_projections){

gas_costs <- left_join(gas_retail_volumetric_price_projections, average_gas_consumption) %>% 
  left_join(gas_connection_charge_projections) %>% 
  select(year, state, category, average_annual_consumption_gj, dollars_per_gj, network_revenue_dollars, connections) %>% 
  mutate(average_volume_cost_dollars = average_annual_consumption_gj * dollars_per_gj,
         connection_cost_dollars = network_revenue_dollars / gas_connections,
         average_cost_dollars = average_volume_cost_dollars + average_connection_cost_dollars) %>% 
  select(year, state, category, average_cost_dollars)


}
