#looking at average consumption:

res_underlying_esoo <-   read_excel(esoo_2024_operational_file) %>% 
  clean_names() %>% 
  filter(scenario %in% c('Actual', 'Central'),
         parent_category == 'Operational (Sent Out)',
         #only include explicitly residential categories. residential EV will be added on top.
         category %in% c('Residential', 'Electrification', 'Rooftop PV'),
         
         #only include residential electrification
         sub_category != 'Business',
         region != 'NEM') %>% 
  rename(state = region) %>% 
  mutate(state = convert_states(state),
         state = if_else(state == 'NSW', 'NSW and ACT', state)) %>% 
  #add in residential EV data
  bind_rows(residential_ev_econsumption) %>%
  group_by(year, state) %>% 
  summarise(annual_consumption_t_wh = sum(annual_consumption_t_wh)) %>%
  
  #calculate average annual consumption per connection
  left_join(household_connections, by = c('year', 'state')) %>% 
  ungroup() %>% 
  mutate(esoo_annual_consumption_kwh = annual_consumption_t_wh / connections * 1e9)


rbs_households <- rbs_households %>% 
  mutate(state = if_else(state == 'NSW' | state == "ACT", 'NSW and ACT', state)) %>% 
  group_by(state) %>% 
  summarise(occupied_households = sum(occupied_households))

res_underlying_rbs <- rbs_fuel_end_use_by_state %>% 
  filter(fuel == "Electricity",
         end_use != "Transport",
         year == 2020) %>% 
  mutate(annual_consumption_t_wh = pj / 3.6,
         state = convert_states(state),
         state = if_else(state == 'NSW' | state == "ACT", 'NSW and ACT', state)) %>% 
  bind_rows(residential_ev_econsumption %>% 
              rename(end_use = category)) %>% 
  group_by(year, state) %>% 
  summarise(annual_consumption_t_wh = sum(annual_consumption_t_wh)) %>% 
    left_join(rbs_households) %>% 
  ungroup() %>% 
  mutate(rbs_annual_consumption_kwh = annual_consumption_t_wh / occupied_households * 1e9) %>% 
  select()
    
  
  
  