

compile_average_net_costs <- function(weighted_average_electricity_costs,
                                      average_gas_costs,
                                      average_petrol_costs){


net_average_electricity_costs <- weighted_average_electricity_costs %>% 
  mutate(category = "Electricity") %>% 
  group_by(year, state, scenario, electrification, category) %>% 
  summarise(average_cost_dollars = sum(average_cost_dollars)) %>% 
  ungroup()

#cross join petrol and gas costs with our scenarios (values are the same across scenarios)
net_energy_costs <- bind_rows(average_gas_costs,
                                  average_petrol_costs) %>% 
  cross_join(tibble(scenario = net_average_electricity_costs %>% 
                      select(scenario) %>% 
                      unique() %>% 
                      pull())) %>% 
  bind_rows(net_average_electricity_costs)


return(net_energy_costs)
}