# this script allocates the supply revenue across the decreasing residential customer base

project_gas_connection_charges <- function(gas_network_charge_revenue, residential_gas_consumption_projections){
  
  projected_connection_charges <- full_join(gas_network_charge_revenue, 
            residential_gas_consumption_projections %>% 
              select(-residential_consumption_gj)) %>% 
    mutate(annual_connection_charge = network_revenue_dollars / residential_gas_connections) %>% 
    select(year, state, annual_connection_charge)
  
  
  return(projected_connection_charges)
  
}