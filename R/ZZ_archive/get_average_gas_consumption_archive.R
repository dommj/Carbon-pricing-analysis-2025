#This currently wrong, as it includes small commercial gas demand

#Instead, we will use estimates from the residential baseline study and decline use in line with electrification estimates.

get_average_gas_consumption <- function(household_connections,
                                        rbs_fuel_end_use_by_state,
                                        aer_gas_benchmarks_file,
                                        gsoo_consumption_data){
  
  nat_gas_use_21 <- rbs_fuel_end_use_by_state %>% 
    filter(year == 2021,
           fuel == 'Natural Gas') %>% 
    mutate(state = convert_states(state),
           state = if_else(state == 'NSW' | state == 'ACT', 'NSW and ACT', state)) %>% 
    group_by(year, state) %>% 
    summarise(pj = sum(pj)) %>% 
    mutate(rbs_annual_consumption_gj = pj * 1e6) %>% 
    select(state, rbs_annual_consumption_gj) %>% 
    left_join(gsoo_consumption_data) %>% 
    mutate(pct_residential = rbs_annual_consumption_gj / annual_consumption_gj) 
  
  
  full_join(household_connections, gsoo_consumption_data) %>% 
    mutate(average_annual_consumption_gj = annual_consumption_gj / connections,
           category = 'gas')
  
}