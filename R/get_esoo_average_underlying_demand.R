
#this script generates two demand profiles over time, a baseline demand (which declines with energy efficiency) and an electrification demand (which shows electrification of gas use from 2025 onwards)

# we then create two demand traces, baseline is just in the shape of 2020 ToU
#electrification is in the shape of cooking, hot water and space heating, weighted by the proportion of coverted demand for each end use.

#We will need to add in WEM from jacobs consolidated demand file

get_esoo_average_underlying_demand <- function(esoo_2024_operational_file,
                                               wem_esoo_2024_operational_file,
                                               household_connections){

#define underlying demand 
esoo_underlying_non_ev <- read_excel(esoo_2024_operational_file) %>% 
  clean_names() %>% 
  filter(scenario %in% c('Actual', 'Central'),
         parent_category == 'Operational (Sent Out)',
         #only include explicitly residential categories. residential EV will be added on top.
         category %in% c('Rooftop PV', 'Residential', "Electrification", "Energy Efficiency"),
         #only include residential electrification
         sub_category != 'Business',
         region != 'NEM') %>% 
  rename(state = region) %>% 
  mutate(state = convert_states(state),
         state = if_else(state == 'NSW', 'NSW and ACT', state),
         source = case_when(category == "Electrification" ~ "electrification", 
                            category == "Energy Efficiency" ~ "energy efficiency", 
                            .default = "baseline")) %>% 
  group_by(year, state, source) %>% 
  summarise(annual_consumption_t_wh = sum(annual_consumption_t_wh)) %>% 
  #calculate average annual consumption per connection
  left_join(household_connections, by = c('year', 'state')) %>% 
  ungroup() %>% 
  mutate(power_kwh = annual_consumption_t_wh / connections * 1e9) %>% 
  select(year, state, power_kwh, source) %>% 
  filter(source != "energy efficiency",
         year <= 2050)

#get western australia data
wem_esoo_underlying_non_ev <- read_excel(wem_esoo_2024_operational_file) %>% 
  clean_names() %>% 
  filter(scenario %in% c('Actual', 'Expected (Step Change)'),
         parent_category == 'Operational (Sent Out)',
         #only include explicitly residential categories. residential EV will be added on top.
         category %in% c('Rooftop PV', 'Residential', "Electrification", "Energy Efficiency"),
         #only include residential electrification
         sub_category != 'Business') %>% 
  mutate(state = "WA",
         source = case_when(category == "Electrification" ~ "electrification", 
                            category == "Energy Efficiency" ~ "energy efficiency", 
                            .default = "baseline")) %>% 
  group_by(year, state, source) %>% 
  summarise(annual_consumption_t_wh = sum(annual_consumption_t_wh)) %>% 
  #calculate average annual consumption per connection
  left_join(household_connections, by = c('year', 'state')) %>% 
  ungroup() %>% 
  mutate(power_kwh = annual_consumption_t_wh / connections * 1e9) %>% 
  select(year, state, power_kwh, source) %>% 
  filter(source != "energy efficiency",
         year <= 2050)


plot <- esoo_underlying_non_ev %>% 
  filter(year <= 2050) %>% 
  ggplot(aes(x = year, y = power_kwh, fill = source)) +
  geom_area() +
  facet_wrap(~state)


return(esoo_underlying_non_ev)
}
