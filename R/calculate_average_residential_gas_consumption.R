calculate_average_residential_gas_consumption <- function(household_connections,
                                                          residential_gas_consumption_projections){
  
  #now calculate the average consumption if spread across all households.
  full_join(household_connections, residential_gas_consumption_projections) %>%
    mutate(average_annual_consumption_gj = residential_consumption_gj / connections,
           category = 'Gas') %>% 
    select(year, state, category, average_annual_consumption_gj) %>% 
    filter(!is.na(average_annual_consumption_gj))
 
}

#compare with RBS estimates:

# nat_gas_use_21 <- rbs_fuel_end_use_by_state %>% 
#   filter(year == 2021,
#          fuel == 'Natural Gas') %>% 
#   mutate(state = convert_states(state),
#          state = if_else(state == 'NSW' | state == 'ACT', 'NSW and ACT', state)) %>% 
#   group_by(year, state) %>% 
#   summarise(pj = sum(pj)) %>% 
#   mutate(rbs_annual_consumption_gj = pj * 1e6) %>% 
#   select(state, rbs_annual_consumption_gj) %>% 
#   left_join(gsoo_consumption_data) %>% 
#   mutate(pct_residential = rbs_annual_consumption_gj / annual_consumption_gj) %>% 
#   #estimate the implied energyt use per connection of the rbs data
#   left_join(gas_connections_data %>% 
#               select(state, residential)) %>% 
#   mutate(use_per_connection = rbs_annual_consumption_gj / residential) 
# #QLD gas use is way too high. in the RBS. Comfortable using the fronteir economics benchmarks


# full_join(household_connections, gsoo_consumption_data) %>% 
#   mutate(average_annual_consumption_gj = annual_consumption_gj / connections,
#          category = 'gas')
