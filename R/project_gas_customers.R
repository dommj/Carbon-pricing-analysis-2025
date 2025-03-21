#project out customer numbers

project_gas_customers <- function(gas_consumption_per_customer, gsoo_consumption_data){
  
  #gas consumption data from GSOO for residential and small business  
  gas_consumption <- gsoo_consumption_data
  
  gas_consumption_per_customer <- gas_consumption_per_customer
  
  gas_customer_projections <- left_join(gas_consumption, gas_consumption_per_customer, by = c('state' = 'state')) %>% 
    filter(!is.na(gas_consumption_per_customer_gj)) %>% 
    mutate(gas_customers = annual_consumption_gj / gas_consumption_per_customer_gj) %>% 
    select(year, state, gas_customers)
  
  gas_customer_projections
}

# source('R/helpers.R')
# 
# 
# gas_customer_projections <- project_gas_customers(gas_consumption_per_customer, gsoo_consumption_data)

