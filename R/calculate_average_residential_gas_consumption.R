calculate_average_residential_gas_consumption <- function(household_connections,
                                                          residential_gas_consumption_projections){
  
  #now calculate the average consumption if spread across all households.
  average_gas_consumption <- full_join(household_connections, 
                                       residential_gas_consumption_projections) %>%
    mutate(average_annual_consumption_gj = residential_consumption_gj / connections,
           category = 'Gas',
           electrification = T) %>% 
    
    ## Why is electrification = T here? Elsehwere, you use this notation to mean ALL gas use is converted to electricity, which 
    ## is clearly not what's happening here since gas consumption > 0?
    
    select(year, state, category, average_annual_consumption_gj, electrification) %>% 
    filter(!is.na(average_annual_consumption_gj),
           year >= 2024) 
    

  #and create a parallel dataset where average gas consumption stays the same as 2024
  average_gas_consumption_no_electrification <- average_gas_consumption %>% 
    filter(year == 2024) %>% 
    select(-year) %>% 
    cross_join(tibble(year = seq(2025, 2050))) %>% 
    mutate(electrification = F)
  

  average_gas_consumption_all <- bind_rows(average_gas_consumption,
                                           average_gas_consumption_no_electrification) %>% 
    filter(year >= 2025)
  
  return(average_gas_consumption_all)
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
