# calculate gas costs

calculate_cameo_gas_costs <- function(gas_retail_volumetric_price_projections,
         gas_connection_charge_projections,
         rbs_fuel_consumption_profiles,
         rbs_households){
  
  rbs_households <- rbs_households %>% 
    filter(year == 2020) %>% 
    select(-year)
  
  #aggregate nsw and act together
  nsw_act_agg <- rbs_fuel_consumption_profiles %>% 
    unnest(cols = output) %>% 
    filter(state %in% c("NSW", "ACT")) %>% 
    left_join(rbs_households) %>% 
    group_by(across(everything())) %>%
    ungroup(state, annual_consumption_gj, occupied_households) %>% 
    summarise(annual_consumption_gj = weighted.mean(annual_consumption_gj, occupied_households)) %>% 
    ungroup() %>% 
    mutate(state = "NSW and ACT")
  
  rbs_fuel_consumption_profiles_agg <- rbs_fuel_consumption_profiles %>% 
    unnest(cols = output) %>% 
    filter(state %nin% c("NSW", "ACT")) %>% 
    bind_rows(nsw_act_agg)
  
  total_gas_consumption <- rbs_fuel_consumption_profiles_agg %>% 
    filter(fuel == "Natural Gas") %>% 
    #no gas efficiency improvements are assumed. So we expand out to 2050
    select(-year) %>% 
    cross_join(tibble(year = seq(2020,2050))) %>% 
    #aggregate to total annual consumption
    group_by(cooking, water_heating, space_heating, ev, pv, year, state) %>% 
    summarise(annual_consumption_gj = sum(annual_consumption_gj)) 
  
  gas_costs <- total_gas_consumption %>% 
    left_join(gas_retail_volumetric_price_projections) %>% 
    left_join(gas_connection_charge_projections) %>% 
    mutate(`Gas volumetric` = annual_consumption_gj * dollars_per_gj,
           `Gas connection` = annual_connection_charge) %>% 
    select(-c(annual_consumption_gj, dollars_per_gj, annual_connection_charge)) %>% 
    pivot_longer(cols = contains("gas"), names_to = "category", values_to = "annual_cost_dollars")

  gas_costs
}



