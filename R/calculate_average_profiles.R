
calculate_average_profiles <- function(esoo_average_underlying_demand,
                                       average_load_shapes,
                                       average_pv_profile,
                                       ev_consumption_profiles,
                                       ev_fleet_data,
                                       vehicles_per_household){
  
  #multiply normalised profiles by esoo demand to get average profiles (without EV)
  average_base_profile <- average_load_shapes %>% 
    ungroup() %>% 
    select(-year) %>% 
    right_join(esoo_average_underlying_demand, relationship = "many-to-many") %>% 
    ## surely you want to filter for baseline here? I don't think power_normalised (average baseline load normalised) * total electrification demand has any meaning?
    
    #power = normalised load shape * total demand
    mutate(power_kwh = power_kwh * power_normalised) %>% 
    select(-power_normalised)
  
  
  #################################################
  #add in EV demand
  #################################################
  
  seasons <- average_load_shapes %>% 
    ungroup() %>% 
    select(season) %>% 
    unique()

  #determine number of EVs perh household by taking the proportion of fleet that are EVs over time from AEMO data and multiplying by national   average household ownership of cars ->  1.8,
  #source: https://www.abs.gov.au/statistics/industry/tourism-and-transport/transport-census/2021#data-downloads
  num_ev_timeseries <- ev_fleet_data %>%
    
    #plug in hybrids are never estimated to represent more than 1.5% of fleet so for purposes of additional electricity demand we assume electricty consumption equal to EV, this will inflate demand and deflate overall savings slightly
    
    mutate(fuel_type = if_else(fuel_type == "PHEV", "BEV", fuel_type)) %>%
    group_by(year, state, fuel_type) %>%
    summarise(vehicles_count = sum(vehicles_count)) %>% 
    group_by(year, state) %>% 
    mutate(prop_ev = vehicles_count/ sum(vehicles_count),
           num_ev = prop_ev * vehicles_per_household) %>% 
    filter(fuel_type == "BEV") %>% 
    select(year, state, num_ev)
  
  ev_consumption_per_household <- num_ev_timeseries %>% 
    #calulate total consumption per household
    left_join(ev_consumption_profiles) %>% 
    mutate(power_kwh = power_kwh * num_ev,
           end_use = "Electric vehicles",
           source = "Electric vehicles",
           fuel = "Electricity") %>% 
    select(year, state, source, hour, power_kwh) %>% 
    #assume same daily consumption across all seasons
    cross_join(seasons) %>% 
    ungroup() %>% 
    filter(year <= 2050)
  
  #create an ev consumption profile for all years in the counterfactual where there is no further ev uptake after 2024
  ev_consumption_per_household_no_change <- ev_consumption_per_household %>% 
    filter(year == 2024) %>% 
    select(- year) %>% 
    cross_join(tibble(year = 2025:2050))
  
  
  ############################################################
  #Create profiles for two scenarios: one with electrification 
  #of gas and cars and one where gas and car use stay the same
  ############################################################
  
  #profiles with projected electrification
  no_pv_household_w_electrification <- average_base_profile %>% 
    bind_rows(ev_consumption_per_household) %>% 
    mutate(pv = 0,
           electrification = T)
  
  pv_household_w_electrification <- average_base_profile %>% 
    bind_rows(ev_consumption_per_household, 
              average_pv_profile %>%
                rename(source = end_use) %>% 
                select(-system_size)) %>% 
    mutate(pv = 1,
           electrification = T)
  
  #profiles without projected electrification
  #profiles with projected electrification
  no_pv_household_no_electrification <- average_base_profile %>% 
    filter(source != "electrification") %>% 
    bind_rows(ev_consumption_per_household_no_change) %>% 
    mutate(pv = 0,
           electrification = F)
  
  pv_household_no_electrification <- average_base_profile %>% 
    filter(source != "electrification") %>% 
    bind_rows(ev_consumption_per_household_no_change, 
              average_pv_profile %>%
                rename(source = end_use) %>% 
                select(-system_size)) %>% 
    mutate(pv = 1,
           electrification = F)

  average_household_profiles <- bind_rows(no_pv_household_w_electrification,
                                          pv_household_w_electrification,
                                          no_pv_household_no_electrification,
                                          pv_household_no_electrification)
  
}

function(){
  
  average_profiles %>% 
  filter(state == "Vic",
         season == "Winter",
         pv == 0,
         electrification == T,
         year %in% c(2025, 2050)) %>% 
  ggplot(aes(x = hour, y = power_kwh, fill = source)) +
  geom_area() +
  facet_wrap(~year)
  
}
