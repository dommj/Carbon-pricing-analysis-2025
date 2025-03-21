#This currently wrong, as it includes small commercial gas demand

#Instead, we will use estimates from the residential baseline study and decline use in line with electrification estimates.

get_average_gas_consumption <- function(household_connections,
                                        gsoo_consumption_data){
  
  full_join(household_connections, gsoo_consumption_data) %>% 
    mutate(average_annual_consumption_gj = annual_consumption_gj / connections,
           category = 'gas')
  
}