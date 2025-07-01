#Compare average consumption to ESOO consumption

average_adj_tou_consumption,

household_connections,
esoo_2024_operational_file


#esoo residential electrification per household
esoo_underlying_non_ev <- read_excel(esoo_2024_operational_file) %>% 
  clean_names() %>% 
  filter(scenario %in% c('Actual', 'Central'),
         parent_category == 'Operational (Sent Out)',
         #only include explicitly residential categories. residential EV will be added on top.
         category %in% c('Rooftop PV', 'Residential', "Electrification"),
         #only include residential electrification
         sub_category != 'Business',
         region != 'NEM') %>% 
  rename(state = region) %>% 
  mutate(state = convert_states(state),
         state = if_else(state == 'NSW', 'NSW and ACT', state),
         source = if_else(category == "Electrification", "Electrification", "baseline")) %>% 
  group_by(year, state) %>% 
  summarise(annual_consumption_t_wh = sum(annual_consumption_t_wh)) %>% 
  #calculate average annual consumption per connection
  left_join(household_connections, by = c('year', 'state')) %>% 
  ungroup() %>% 
  mutate(power_kwh = annual_consumption_t_wh / connections * 1e9,
         source = "ESOO") %>% 
  select(year, state, power_kwh, source)


rbs_consumption <- average_adj_tou_consumption %>% 
  filter(end_use != "Electric vehicles") %>% 
  group_by(year, state, season) %>% 
  summarise(power_kwh = sum(power_kwh)) %>% 
  ungroup() %>% 
  mutate(power_kwh = power_kwh * 365/4) %>% 
  group_by(year, state) %>% 
  summarise(power_kwh = sum(power_kwh)) %>% 
  mutate(source = "RBS")

bind_rows(esoo_underlying_non_ev, rbs_consumption) %>% 
ggplot(aes(x= year, y = power_kwh, colour = source)) +
  facet_wrap(~state) +
  geom_line() +
  grattan_y_continuous(limits = c(0, 7000))


rbs_consumption %>% 
  filter(state == "Vic") %>% 
  ggplot(aes(x=year, y = power_kwh, fill = source)) +
  geom_line() 
  
  

##########################################################################################
#Look at RBS 2020 and 2024 energy consumption relative to AER benchmark and ESOO
##########################################################################################
rbs_fuel_end_use_by_state,
rbs_households

x <- rbs_fuel_end_use_by_state %>% 
  filter(end_use != "Transport",
         fuel == "Electricity") %>% 
  mutate(fuel = if_else(fuel == "LPG", "Natural Gas", fuel)) %>% 
  group_by(year, state, fuel) %>% 
  summarise(pj = sum(pj)) %>% 
  mutate(state = convert_states(state)) %>% 
  left_join(rbs_households) %>% 
  mutate(state = if_else(state == "NSW" | state == "ACT", "NSW and ACT", state)) %>% 
  group_by(year, state, fuel) %>% 
  summarise(pj = sum(pj),
            occupied_households =sum(occupied_households)) %>% 
  mutate(mean_gj = pj / occupied_households * 1e6,
         power_kwh = mean_gj * 1e3/3.6,
         source = "RBS") %>% 
  select(year, state, source, power_kwh) %>% 
  filter(year >= 2020)

bind_rows(x, esoo_underlying_non_ev) %>% 
  ggplot(aes(x= year, y = power_kwh, colour = source)) +
  facet_wrap(~state) +
  geom_line()



