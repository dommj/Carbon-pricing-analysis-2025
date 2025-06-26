#calculate per cent additonal electricty of current load -> scale current tou profiles by pct increase

calculate_gas_n_efficiency_adj_tou_consumption <- function(rbs_baseline_consumption,
                                              additional_electricty_consumption,
                                              rbs_tou_consumption_data,
                                              heating_cooling_profiles,
                                              rbs_households){
  
adj_electricity_consumption <- rbs_baseline_consumption %>% 
  filter(fuel == "Electricity") %>% 
  select(-year) %>% 
  right_join(additional_electricty_consumption %>% 
              rename(additional_consumption_gj = annual_consumption_gj) %>% 
              select(- c(annual_consumption_kwh, annual_consumption_kwh)), relationship = "many-to-many") %>% 
  mutate(scale_factor = additional_consumption_gj / annual_consumption_gj) %>% 
  filter(!is.na(scale_factor)) %>% 
  select(year, state, fuel, end_use, source, scale_factor) %>% 
  
#bind rows with scalefactors for original, this way we can seperate out displaced gas consumption if we want.
  bind_rows(
    expand_grid(year = unique(additional_electricty_consumption$year), 
                 state = unique(additional_electricty_consumption$state), 
                 fuel = "Electricity", 
                 source = "baseline",
                 end_use = unique(rbs_baseline_consumption$end_use),
                 scale_factor = 1))

##########################################
#join to tou consumption traces
##########################################

#group appliance end uses together split space conditioning into heating and cooling profiles
rbs_tou_consumption <- rbs_tou_consumption_data %>% 
  mutate(end_use = if_else(end_use_category %in% c("IT&HE", "White goods", "Other Equipment"), "Appliances", end_use_category)) %>% 
  group_by(state, season, day_type, end_use, year, hour) %>% 
  summarise(power = sum(power)) %>% 
  ungroup() %>% 
  #split space conditioning into heating and cooling profiles
  filter(end_use != "Space conditioning") %>% 
  bind_rows(heating_cooling_profiles %>%  rename(end_use = end_use_category)) %>% 
  #take weighted average of WD and WE to average day profile
  pivot_wider(names_from = day_type, values_from = power) %>% 
  mutate(power = (5* WD + 2 * WE)/7) %>% 
  #convert state aggregates (in MW) to household average consumption (KW)
  left_join(rbs_households) %>%
  mutate(power_kwh = power / occupied_households * 1e3) %>% 
  select(-c(WD, WE, year, power, occupied_households)) 


#aggregate nsw and act together
nsw_act_agg <- rbs_tou_consumption %>% 
  filter(state %in% c("NSW", "ACT")) %>% 
  left_join(rbs_households %>% 
              filter(year == 2020) %>% 
              select(-year)) %>% 
  group_by(season, end_use, hour) %>%
  summarise(power_kwh = weighted.mean(power_kwh, occupied_households)) %>% 
  ungroup() %>% 
  mutate(state = "NSW and ACT")

rbs_tou_consumption_time_series <- rbs_tou_consumption %>% 
  filter(state %nin% c("NSW", "ACT")) %>% 
  bind_rows(nsw_act_agg) %>% 
  right_join(adj_electricity_consumption, relationship = "many-to-many") %>% 
  mutate(power_kwh = power_kwh * scale_factor) %>% 
  select(-scale_factor)

rbs_tou_consumption_time_series
}


#checking out some plots
function(){

rbs_tou_consumption_time_series %>% 
  filter(state == "Vic",
         year %in% c(2020, 2040),
         season == "Winter") %>% 
  group_by(year, hour, source) %>% 
  summarise(power_kwh = sum(power_kwh)) %>% 
  mutate(source = fct(source, levels = c('displaced_gas', "baseline"))) %>% 
  ggplot(aes(x = hour, y = power_kwh, fill = source)) +
  geom_area() +
  facet_wrap(~year)
  
  
  
rbs_tou_consumption_time_series %>% 
  filter(state == "Vic",
         year %in% c(2040),
         season == "Winter",
         source == "displaced_gas") %>% 
  ggplot(aes(x = hour, y = power_kwh, fill = end_use)) +
  geom_area() 

rbs_tou_consumption_time_series %>% 
  filter(state == "Vic",
         year %in% c(2040),
         season == "Winter",
         source == "baseline") %>% 
  ggplot(aes(x = hour, y = power_kwh, fill = end_use)) +
  geom_area()

rbs_tou_consumption_time_series %>% 
  filter(state == "NSW and ACT",
         year %in% c(2040),
         season == "Autumn",
         source == "displaced_gas") %>% 
  ggplot(aes(x = hour, y = power_kwh, fill = end_use)) +
  geom_area() 


rbs_tou_consumption_time_series %>% 
  filter(state == "NSW and ACT",
         year %in% c(2040),
         season == "Autumn",
         source == "baseline") %>% 
  ggplot(aes(x = hour, y = power_kwh, fill = end_use)) +
  geom_area()
  

rbs_tou_consumption_time_series %>% 
  filter(state == "NSW and ACT",
         year %in% c(2020, 2040),
         season == "Winter") %>% 
  group_by(year, hour, source) %>% 
  summarise(power_kwh = sum(power_kwh)) %>% 
  mutate(source = fct(source, levels = c('displaced_gas', "baseline"))) %>% 
  ggplot(aes(x = hour, y = power_kwh, fill = source)) +
  geom_area() +
  facet_wrap(~year)
}
