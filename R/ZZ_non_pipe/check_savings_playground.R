#comparing energy bills 

ea_flat_rate <- 0.2733
ea_daily <- 1.2407
fit <- 0.033

tar_load(annual_electricity_consumption_profiles)
tar_load(cameo_electricity_costs)
tar_load(rbs_fuel_consumption_profiles)
tar_load(cameo_gas_costs)

#all-electric no ev no solar vs all gas no ev no solar

ea_annual_electricity_cost <- annual_electricity_consumption_profiles %>% 
  mutate(consumer_type = paste(cooking, water_heating, space_heating, ev, pv, sep = "_")) %>% 
  filter(consumer_type %in% c("gas_gas_gas_0_FALSE", # all gas with an ICE vehicle
                              "electric_electric_electric_0_FALSE"),
         state == "Vic",
         year == 2025) %>% 
  ungroup() %>% 
  mutate(ea_cost = annual_consumption_kwh * ea_flat_rate + ea_daily * 365) %>% 
  select(year, state, consumer_type, annual_consumption_kwh, ea_cost) %>% 
  left_join(
    cameo_electricity_costs %>% 
      mutate(consumer_type = paste(cooking, water_heating, space_heating, ev, pv, sep = "_")) %>% 
      filter(consumer_type %in% c("gas_gas_gas_0_FALSE", # all gas with an ICE vehicle
                                  "electric_electric_electric_0_FALSE"),
             state == "Vic",
             year == 2025) %>% 
      select(year, state, consumer_type, annual_cost_dollars) %>% 
      rename(jacobs_cost = annual_cost_dollars)
  ) %>% 
  left_join(
    cameo_gas_costs %>% 
      ungroup() %>% 
      mutate(consumer_type = paste(cooking, water_heating, space_heating, ev, pv, sep = "_")) %>% 
      filter(consumer_type %in% c("gas_gas_gas_0_FALSE", # all gas with an ICE vehicle
                                  "electric_electric_electric_0_FALSE"),
             state == "Vic",
             year == 2025) %>% 
      group_by(year, state, consumer_type) %>% 
      summarise(gas_cost = sum(annual_cost_dollars))
  ) %>% 
  mutate(gas_cost = if_else(is.na(gas_cost), 0, gas_cost),
         total_cost_ea = ea_cost + gas_cost,
         total_cost_jacobs = jacobs_cost + gas_cost,
         savings_ea = total_cost_ea[consumer_type == "gas_gas_gas_0_FALSE"] -  total_cost_ea,
         savings_jacobs = total_cost_jacobs[consumer_type == "gas_gas_gas_0_FALSE"] -  total_cost_jacobs) 



rbs_fuel_consumption_profiles %>% 
  unnest(cols = output) %>% 
  mutate(consumer_type = paste(cooking, water_heating, space_heating, ev, pv, sep = "_")) %>% 
  filter(consumer_type %in% c("gas_gas_gas_0_FALSE", # all gas with an ICE vehicle
                              "electric_electric_electric_0_FALSE"),
         state == "NSW")




