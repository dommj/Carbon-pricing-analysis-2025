
get_esoo_electrification_per_household <- function(esoo_2024_operational_file, household_connections){

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

esoo_electrification
}

