

get_rbs_baseline_consumption <- function(integrated_fuel_use, 
                                            rbs_households){
  
  
  #aggregate microwave back into cooking for integrated fuel use data
  
  integrated_fuel_use <- integrated_fuel_use %>% 
    mutate(end_use = if_else(end_use == "Microwave", "Cooking", end_use)) %>% 
    group_by(year, state, fuel, end_use, conversion) %>% 
    summarise(pj = sum(pj)) %>% 
    ungroup()
  
  
  consumption <- integrated_fuel_use %>% 
    filter(conversion == "unconverted",
           state != "Aus") %>% 
    select(-conversion) %>% 
    left_join(rbs_households %>% 
                filter(year == 2020)) %>% 
    mutate(annual_consumption_gj = (pj / occupied_households) * 1e6) %>% 
    select(year, state, fuel, end_use, annual_consumption_gj)
  
  
  #aggregate nsw and act with a connections weighted average
  #aggregate nsw and act together
  nsw_act_agg <- consumption %>% 
    filter(state %in% c("NSW", "ACT")) %>% 
    left_join(rbs_households %>% 
                filter(year == 2020) %>% 
                select(-year)) %>% 
    group_by(year, fuel, end_use) %>%
    summarise(annual_consumption_gj = weighted.mean(annual_consumption_gj, occupied_households)) %>% 
    ungroup() %>% 
    mutate(state = "NSW and ACT")
  
  rbs_consumption_agg <- consumption %>% 
    filter(state %nin% c("NSW", "ACT")) %>% 
    bind_rows(nsw_act_agg)
  
  rbs_consumption_agg
}

# gas_consumption %>%
#   group_by(state) %>%
#   summarise(annual_consumption_gj = sum(annual_consumption_gj))
