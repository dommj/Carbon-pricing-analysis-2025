#generate time of use electricity demand profiles for cameo consumers

calculate_tou_consumer_profiles <- function(rbs_fuel_consumption_profiles,
                                            integrated_fuel_use,
                                            rbs_tou_consumption_data,
                                            ev_consumption_profiles,
                                            pv_profiles,
                                            rbs_households,
                                            heating_cooling_profiles){
  
  rbs_households <- rbs_households %>% 
    filter(year == 2020) %>% 
    select(-year)
  
  #group appliance end uses together split space conditioning into heating and cooling profiles
  rbs_tou_consumption_data <- rbs_tou_consumption_data %>% 
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
    select(-c(WD, WE))
  
  
  #aggregate microwave back into cooking for integrated fuel use data
  
  integrated_fuel_use <- integrated_fuel_use %>% 
    mutate(end_use = if_else(end_use == "Microwave", "Cooking", end_use)) %>% 
    group_by(year, state, fuel, end_use, conversion) %>% 
    summarise(pj = sum(pj)) %>% 
    ungroup()
  
  #get annual consumption figures and convert to MWH
  
  annual_consumption_end_use <- integrated_fuel_use %>% 
    filter(year == 2020,
           conversion  == "unconverted",
           fuel == "Electricity") %>%
    mutate(annual_consumption_mwh = pj * (1/3.6e-6),
           state = convert_states(state)) %>% 
    select(-c(fuel, pj))
  
  #normalise the demand curves for each end use such that they sum to 1 over a year
  
  normalised_tou_curves <- rbs_tou_consumption_data %>% 
    left_join(annual_consumption_end_use) %>%
    mutate(power_normalised = power / annual_consumption_mwh,
           #there is 0 total space conditioning - heating consumption in NT, giving us infinity. Set to zero
           power_normalised = if_else(state == "NT" & end_use == "Space conditioning - heating", 0, power_normalised)) %>% 
    select(state, season, end_use, year, hour, power_normalised) 
  
  #create tou profiles for all customer classes
  
  consumer_tou_profiles <- normalised_tou_curves %>% 
    left_join(
      rbs_fuel_consumption_profiles %>% 
        unnest(cols = output) %>% 
        filter(fuel == "Electricity") %>% 
        #convert to mwh
        mutate(annual_consumption_kwh = annual_consumption_gj * (1/3.6e-3)),
      relationship = "many-to-many"
        ) %>% 
    mutate(power_kwh = power_normalised * annual_consumption_kwh) %>% 
    select(cooking, water_heating, space_heating, ev, pv, state, season, end_use, hour, power_kwh)
  
  #aggregate nsw and act together
  nsw_act_agg <- consumer_tou_profiles %>% 
    filter(state %in% c("NSW", "ACT")) %>% 
    left_join(rbs_households) %>% 
    group_by(across(everything())) %>%
    ungroup(state, power_kwh, occupied_households) %>% 
    summarise(power_kwh = weighted.mean(power_kwh, occupied_households)) %>% 
    ungroup() %>% 
    mutate(state = "NSW and ACT")
  
  consumer_tou_profiles <- consumer_tou_profiles %>% 
    filter(state %nin% c("NSW", "ACT")) %>% 
    bind_rows(nsw_act_agg)
  
  #add in EVs and PV
  consumer_tou_profiles_all <- consumer_tou_profiles %>% 
    bind_rows(ev_consumption_profiles, pv_profiles)

  
  consumer_tou_profiles_all
}









function(){
chart_data <- tou_consumer_profiles %>% 
  mutate(consumer_type = paste(cooking, water_heating, space_heating, ev, pv, sep = "_")) %>% 
  group_by(consumer_type, state, season, day_type, hour) %>% 
  summarise(power_kwh = sum(power_kwh)) 

#extend the data to -1 and 24 to allow smoothing function to work

chart_data_24 <- chart_data %>% 
  filter(hour == 0) %>% 
  mutate(hour = 24)

chart_data_neg_1 <- chart_data %>% 
  filter(hour == 23) %>% 
  mutate(hour = -1)

chart_data_final <- chart_data %>% 
  bind_rows(chart_data_24, chart_data_neg_1) %>% 
  group_by(consumer_type, state, season, day_type) %>% 
  arrange(hour) %>% 
  mutate(power_smoothed = zoo::rollmean(power_kwh, 3, fill = NA)) %>% 
  filter(!is.na(power_smoothed))

chart_data_final %>% 
  filter(season == "Summer",
         day_type == "WD",
         #state == "NSW",
         consumer_type %in% c("electric_electric_electric_1_FALSE")) %>% 
  ggplot(aes(x = hour, y = power_smoothed, colour = state)) +
  geom_line()

tou_consumer_profiles %>% 
  mutate(consumer_type = paste(cooking, water_heating, space_heating, ev, pv, sep = "_")) %>% 
  filter(season == "Winter",
         day_type == "WD",
         state == "NSW and ACT",
         consumer_type %in% c("electric_electric_electric_1_FALSE")) %>% 
  ggplot(aes(x = hour, y = power_kwh, colour = end_use)) +
  geom_line()


tou_consumer_profiles %>% 
  mutate(consumer_type = paste(cooking, water_heating, space_heating, ev, pv, sep = "_")) %>% 
  filter(day_type == "WD",
         end_use == "Space conditioning - cooling",
         state == "NSW and ACT",
         consumer_type %in% c("electric_electric_electric_1_FALSE")) %>% 
  ggplot(aes(x = hour, y = power_kwh, colour = season)) +
  geom_line()

total_usage <- tou_consumer_profiles %>% 
  mutate(consumer_type = paste(cooking, water_heating, space_heating, ev, pv, sep = "_")) %>% 
  group_by(consumer_type, state, season, day_type) %>% 
  summarise(power_kwh = sum(power_kwh)) 

}


# all_electric_profile <- rbs_fuel_consumption_profiles %>%
#   filter(cooking == "electric",
#          water_heating == "electric",
#          space_heating == "electric") %>%
#   select(output) %>%
#   unnest(cols = c(output))

#compare annualised tou values (of end uses that haven't been converted) to totals in "Energy.Elec.EndUse-State"
#
# Very close alignment, some have ~ 0.1% error presumably from rounding differences

function(){
#NSW cooking
(4.8210908 - 4.8155756632956)/ 4.8155756632956 # = 0.11% dif

#ACT lighting
(0.2761530 - 0.2758410874116) / 0.2758410874116 # = - 0.11% dif

x <- rbs_tou_consumption_data %>%
  group_by(state, season, day_type, end_use) %>%
  summarise(power = sum(power)) %>%
  pivot_wider(names_from = day_type, values_from = power) %>%
  rowwise() %>%
  mutate(daily_average = weighted.mean(c(WD,WE), c(5,2)),
         season_total = daily_average * (365.25 / 4)) %>%
  group_by(state, end_use) %>%
  summarise(annual_consumption = sum(season_total) * 3.6e-6) #sum and convert MWH to pj
}


