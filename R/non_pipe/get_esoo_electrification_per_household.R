
get_esoo_electrification_per_household <- function(esoo_2024_operational_file, household_connections, rbs_baseline_consumption){

#esoo residential electrification per household
esoo_electrification <- read_excel(esoo_2024_operational_file) %>% 
  clean_names() %>% 
  filter(scenario %in% c('Actual', 'Central'),
         parent_category == 'Operational (Sent Out)',
         #only include explicitly residential categories. residential EV will be added on top.
         category %in% c('Electrification'),
         #only include residential electrification
         sub_category != 'Business',
         region != 'NEM') %>% 
  rename(state = region) %>% 
  mutate(state = convert_states(state),
         state = if_else(state == 'NSW', 'NSW and ACT', state)) %>% 
  group_by(year, state, category) %>% 
  summarise(annual_consumption_t_wh = sum(annual_consumption_t_wh)) %>% 
  #calculate average annual consumption per connection
  left_join(household_connections, by = c('year', 'state')) %>% 
  ungroup() %>% 
  mutate(average_annual_consumption_kwh = annual_consumption_t_wh / connections * 1e9) %>% 
  select(year, state, category, average_annual_consumption_kwh)

rbs_baseline_consumption %>% 
  filter(end_use %in% c("Cooking", "Water heating", "Space conditioning - heating"),
         fuel == "Natural Gas") %>% 
  group_by(year, state) %>% 
  mutate(prop = annual_consumption_gj / sum(annual_consumption_gj)) %>% 
  ungroup() %>% 
  select(- year) %>% 
  left_join(esoo_electrification %>% select(-category)) %>% 
  mutate(average_annual_consumption_kwh = if_else(is.na(average_annual_consumption_kwh), 0 , average_annual_consumption_kwh),
         annual_consumption_kwh = prop * average_annual_consumption_kwh,
         annual_consumption_gj = annual_consumption_kwh * 3.6 / 1e3,
         source = "displaced_gas") %>% 
  select(-c( prop, average_annual_consumption_kwh))



}

