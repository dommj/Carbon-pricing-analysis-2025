#calculate per cent additonal electricty of current load -> scale current tou profiles by pct increase

calculate_average_adj_tou_consumption <- function(rbs_baseline_consumption,
                                              additional_electricty_consumption,
                                              rbs_tou_consumption_data,
                                              heating_cooling_profiles,
                                              rbs_households,
                                              household_energy_efficiency,
                                              ev_consumption_profiles,
                                              household_connections,
                                              ev_fleet_data){
  
#calculate scale factors that represent the increased electricity consumption from displaced gas as a proportion of current consumption
#this chunk creates a table of baseline and displaced gas end_uses and corresponding scale factors to multiply by the existing usage. This will give us data for the baseline (scale factor = 1) and for new electricity consumption (scale factor = additonal_consumption / baseline consumption)  
adj_electricity_consumption <- rbs_baseline_consumption %>% 
  filter(fuel == "Electricity") %>% 
  select(-year) %>% 
  right_join(additional_electricty_consumption %>% 
              rename(additional_consumption_gj = annual_consumption_gj) %>% 
              select(- c(annual_consumption_kwh, annual_consumption_kwh)), relationship = "many-to-many") %>% 
  mutate(scale_factor = additional_consumption_gj / annual_consumption_gj) %>% 
  filter(!is.na(scale_factor)) %>% 
  select(year, state, fuel, end_use, source, scale_factor) %>% 
  
#bind rows with scale factors for original, this way we can separate out displaced gas consumption if we want. Include tas here ?? and assume gas declines at average rate of GSOO?
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

#group appliance end uses together and split space conditioning into heating and cooling profiles (only space heating has additional consumption from gas)
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

#multiply consumption by scale factors to get our new consumption and baseline consumption results
rbs_tou_consumption_time_series <- rbs_tou_consumption %>% 
  filter(state %nin% c("NSW", "ACT")) %>% 
  bind_rows(nsw_act_agg) %>% 
  right_join(adj_electricity_consumption, relationship = "many-to-many") %>% 
  mutate(power_kwh = power_kwh * scale_factor) %>% 
  select(-scale_factor)


#scale all consumption (new and baseline) down according to predicted energy efficiency gains

eff_adj_rbs_tou_consumption_time_series <- rbs_tou_consumption_time_series %>% 
  left_join(household_energy_efficiency) %>% 
  mutate(power_kwh = power_kwh * efficiency_multiplier) %>% 
  select(-efficiency_multiplier)
  

#################################################
#add in EV demand
#################################################

seasons <- eff_adj_rbs_tou_consumption_time_series %>% 
  select(season) %>% 
  unique()

#determine number of EVs perh household by taking the proportion of fleet that are EVs over time from AEMO data and multiplying by national average household ownership of cars ->  1.8,
#source: https://www.abs.gov.au/statistics/industry/tourism-and-transport/transport-census/2021#data-downloads
num_ev_timeseries <- ev_fleet_data %>%
  
  #plug in hybrids are never estimated to represent more than 1.5% of fleet so for purposes of additional electricity demand we assume electricty consumption equal to EV, this will inflate demand and deflate overall savings slightly
  
  mutate(fuel_type = if_else(fuel_type == "PHEV", "BEV", fuel_type)) %>%
  group_by(year, state, fuel_type) %>%
  summarise(vehicles_count = sum(vehicles_count)) %>% 
  group_by(year, state) %>% 
  mutate(prop_ev = vehicles_count/ sum(vehicles_count),
         num_ev = prop_ev * 1.8) %>% 
  filter(fuel_type == "BEV") %>% 
  select(year, state, num_ev)

ev_consumption_per_household <- num_ev_timeseries %>% 
  #calulate total consumption per household
  left_join(ev_consumption_profiles) %>% 
  mutate(power_kwh = power_kwh * num_ev,
         end_use = "Electric vehicles",
         source = "Electric vehicles",
         fuel = "Electricity") %>% 
  select(year, state, fuel, end_use, source, hour, power_kwh) %>% 
  #assume same daily consumption across all seasons
  cross_join(seasons)

total_adjusted_consumption <- eff_adj_rbs_tou_consumption_time_series %>% 
  bind_rows(ev_consumption_per_household)


return(total_adjusted_consumption)
}


#checking out some plots
function(){

total_adjusted_consumption %>% 
  filter(state == "Vic",
         year %in% c(2020, 2050),
         season == "Winter") %>% 
  group_by(year, hour, source) %>% 
  summarise(power_kwh = sum(power_kwh)) %>% 
  mutate(source = fct(source, levels = c('displaced_gas', "Electric vehicles", "baseline"))) %>% 
  ggplot(aes(x = hour, y = power_kwh, fill = source)) +
  geom_area() +
  facet_wrap(~year) + 
    scale_x_continuous(
      breaks = seq(0, 24, 4),
      labels = c("12am", "4am", "8am", "12pm", "4pm", "8pm", "12am"),
      expand = expansion(mult = c(0.02, 0.02))
    ) +
    scale_y_continuous(
      labels = scales::comma_format()) +
    grattan_label(data = . %>%
                    filter(year == 2050,
                           hour == 12) %>%
                    mutate(year = 2020,
                           power_kwh_cum = cumsum(rev(power_kwh)),
                           power_kwh_cum = power_kwh_cum - power_kwh / 2),
                  aes(x = hour, y = power_kwh_cum,
                      label = str_wrap(source, width = 15),
                      colour = source,
                      fill = NA),
                  hjust = 0.5,
                  label.size = NA) +
    theme_grattan() +
    labs(
      title = "Victorian winter electricity demand set to shift dramatically",
      subtitle = "Hourly electricity consumption in KW by source, winter average",
      x = "Time of day",
      y = "",
      fill = "Source",
      caption = "Source: Grattan Institute analysis."
    )
  
grattan_save_pptx("vic_winter_test.pptx")
  
total_adjusted_consumption %>% 
  filter(state == "Vic",
         year %in% c(2040),
         season == "Winter",
         source == "displaced_gas") %>% 
  ggplot(aes(x = hour, y = power_kwh, fill = end_use)) +
  geom_area() 

total_adjusted_consumption %>% 
  filter(state == "Vic",
         year %in% c(2040),
         season == "Winter",
         source == "baseline") %>% 
  ggplot(aes(x = hour, y = power_kwh, fill = end_use)) +
  geom_area()

total_adjusted_consumption %>% 
  filter(state == "NSW and ACT",
         year %in% c(2040),
         season == "Autumn",
         source == "displaced_gas") %>% 
  ggplot(aes(x = hour, y = power_kwh, fill = end_use)) +
  geom_area() 


total_adjusted_consumption %>% 
  filter(state == "NSW and ACT",
         year %in% c(2040),
         season == "Autumn",
         source == "baseline") %>% 
  ggplot(aes(x = hour, y = power_kwh, fill = end_use)) +
  geom_area()
  

total_adjusted_consumption %>% 
  filter(state == "NSW and ACT",
         year %in% c(2020, 2040),
         season == "Winter") %>% 
  group_by(year, hour, source) %>% 
  summarise(power_kwh = sum(power_kwh)) %>% 
  mutate(source = fct(source, levels = c('displaced_gas', "Electric vehicles", "baseline"))) %>% 
  ggplot(aes(x = hour, y = power_kwh, fill = source)) +
  geom_area() +
  facet_wrap(~year)
}
