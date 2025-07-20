# we now create two demand traces, baseline is just in the shape of 2020 ToU
#electrification is in the shape of cooking, hot water and space heating, weighted by the proportion of converted demand for each end use.

calculate_average_load_shapes <- function(rbs_tou_consumption_data,
                                          integrated_fuel_use,
                                          heating_cooling_profiles){
  
  ########################################
  #Baseline load shape
  ########################################
  
  baseline_load_shape <- rbs_tou_consumption_data %>% 
      #split space conditioning into heating and cooling profiles
      filter(end_use_category != "Space conditioning") %>% 
      rename(end_use = end_use_category) %>% 
      bind_rows(heating_cooling_profiles %>%  rename(end_use = end_use_category)) %>% 
      #take weighted average of WD and WE to average day profile
      pivot_wider(names_from = day_type, values_from = power) %>% 
      mutate(power = (5* WD + 2 * WE)/7, 
             source = "baseline",
             #aggregate NSW and ACT
             state = if_else(state %in% c("NSW", "ACT"), "NSW and ACT", state)) %>% 
      select(-c(WD, WE)) %>% 
      group_by(year, state, season, source, hour) %>% 
    #this is total consumption (tou) for each state
      summarise(power = sum(power))
  
  
  #get annual consumption figures and convert to MWH
  total_annual_consumption <- integrated_fuel_use %>% 
    filter(year == 2020,
           conversion  == "unconverted",
           fuel == "Electricity",
           state != "Aus") %>%
    mutate(annual_consumption_mwh = pj * (1/3.6e-6),
           state = convert_states(state),
           #aggregate NSW and ACT
           state = if_else(state %in% c("NSW", "ACT"), "NSW and ACT", state),
           source = "baseline") %>% 
    group_by(year, state, source) %>%  
    summarise(annual_consumption_mwh = sum(annual_consumption_mwh))
  
  #normalise the demand curves for each end use such that they sum to 1 over a year
  
  normalised_baseline_load_shape <- baseline_load_shape %>% 
    left_join(total_annual_consumption)  %>%
    mutate(power_normalised = power / annual_consumption_mwh) %>% 
    select(state, season, source, hour, power_normalised)
  
  #check that values sum to one 
  # normalised_baseline_load_shape %>%
  #   group_by(state) %>%
  #   summarise(power = sum(power_normalised) * 365/4)
  
  
  ########################################
  #Electrified load shape
  ########################################
  
  #take out the loads that will be involved in electrification
  electrification_load_shape <- rbs_tou_consumption_data %>% 
    #split space conditioning into heating and cooling profiles
    filter(end_use_category != "Space conditioning") %>% 
    rename(end_use = end_use_category) %>% 
    bind_rows(heating_cooling_profiles %>%  rename(end_use = end_use_category)) %>% 
    #take weighted average of WD and WE to average day profile
    pivot_wider(names_from = day_type, values_from = power) %>% 
    mutate(power = (5* WD + 2 * WE)/7, 
           source = "baseline",
           #aggregate NSW and ACT
           state = if_else(state %in% c("NSW", "ACT"), "NSW and ACT", state)) %>% 
    select(-c(WD, WE)) %>% 
    #filter for the end uses that will be electrified
    filter(end_use %in% c("Cooking", "Space conditioning - heating", "Water heating")) %>% 
    group_by(year, state, season, source, end_use, hour) %>% 
    summarise(power = sum(power))
  
  
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
           state = convert_states(state),
           #aggregate NSW and ACT
           state = if_else(state %in% c("NSW", "ACT"), "NSW and ACT", state)) %>% 
    group_by(year, state, end_use) %>% 
    summarise(annual_consumption_mwh = sum(annual_consumption_mwh))
  
  #normalise the demand curves for each end use such that they sum to 1 over a year
  
  normalised_electrification_load_shape <- electrification_load_shape %>% 
    left_join(annual_consumption_end_use) %>%
    mutate(power_normalised = power / annual_consumption_mwh,
           #there is 0 total space conditioning - heating consumption in NT, giving us infinity. Set to zero
           power_normalised = if_else(state == "NT" & end_use == "Space conditioning - heating", 0, power_normalised)) %>% 
    select(state, season, end_use, year, hour, power_normalised) 
  
  #calculate weights for final loads
  weights <- integrated_fuel_use %>% 
    #filter for the final electricity loads of each end use when all gas is converted
    filter(conversion == "gas_to_electric_converted") %>% 
    mutate(#aggregate NSW and ACT
      state = if_else(state %in% c("NSW", "ACT"), "NSW and ACT", state)) %>% 
    group_by(year, state, end_use) %>% 
    summarise(pj = sum(pj)) %>% 
    group_by(year, state) %>% 
    mutate(weight = pj /sum(pj)) %>% 
    select(year, state, end_use, weight)
  
  
  normalised_electrification_load_shape_weighted <- normalised_electrification_load_shape %>% 
    left_join(weights) %>% 
    mutate(power_normalised = power_normalised * weight)
  
  #sense check plots
  # normalised_electrification_load_shape_weighted %>% 
  #   filter(season == "Autumn") %>% 
  #   ggplot(aes(x = hour, y = power_normalised, colour = end_use)) +
  #   geom_line() +
  #   facet_wrap(~state)
  
  normalised_electrification_load_shape_weighted <- normalised_electrification_load_shape_weighted %>% 
    group_by(year, state, season, hour) %>% 
    summarise(power_normalised = sum(power_normalised)) %>% 
    mutate(source = "electrification")
  

  #check that values sum to one - slight rounding errors
  # normalised_electrification_load_shape_weighted %>%
  #   group_by(state) %>%
  #   summarise(power = sum(power_normalised) * 365/4)
  
  
  load_shapes <- bind_rows(normalised_baseline_load_shape,
                           normalised_electrification_load_shape_weighted) %>% 
    ungroup()
  
}