#take aggregate fuel use values for non-pv households. for PV households, net out hourly consumption to determine total grid consumption and total exports

calculate_annual_electricity_consumption_profiles <- function(all_tou_consumer_profiles,
                                                            rbs_fuel_consumption_profiles,
                                                            jacobs_curtailment,
                                                            rbs_households){
  
  rbs_households <- rbs_households %>% 
    filter(year == 2020) %>% 
    select(-year)

  
  #calculate annual electricity consumption and exports from tou_profiles
  annual_consumption_exports <- all_tou_consumer_profiles %>% 

    group_by(cooking, water_heating, space_heating, ev, pv, battery,
             year, 
             state, season, hour) %>% 
    summarise(power_kwh = sum(power_kwh)) %>% 
    
    
    mutate(consumption_export = if_else(power_kwh < 0, "Exports", "Consumption")) %>% 
    group_by(cooking, water_heating, space_heating, ev, pv, battery,
             year, 
             state, season, consumption_export) %>% 
    summarise(power_kwh = sum(power_kwh)) %>%  
    ungroup() %>% 
    mutate(season_total = power_kwh * (365 / 4)) %>% 
    group_by(cooking, water_heating, space_heating, ev, pv, battery,
             year, 
             state, consumption_export) %>% 
    summarise(annual_consumption_kwh = sum(season_total)) %>% 
    ungroup() %>%
    left_join(jacobs_curtailment, by = c('year', 'state'),
              relationship = 'many-to-many') %>%
    mutate(annual_consumption_kwh = if_else(consumption_export == 'Exports',
            (1-curtailment) * annual_consumption_kwh, annual_consumption_kwh))
  
  ############################################################
  #Sense Checking: confirm that summed tou matches aggregate inputs
  ############################################################
  
  
  #aggregate nsw and act together
  nsw_act_agg <- rbs_fuel_consumption_profiles %>% 
    unnest(cols = output) %>% 
    filter(state %in% c("NSW", "ACT")) %>% 
    left_join(rbs_households) %>% 
    group_by(across(everything())) %>%
    ungroup(state, annual_consumption_gj, occupied_households) %>% 
    summarise(annual_consumption_gj = weighted.mean(annual_consumption_gj, occupied_households)) %>% 
    ungroup() %>% 
    mutate(state = "NSW and ACT")
  
  rbs_fuel_consumption_profiles_agg <- rbs_fuel_consumption_profiles %>% 
    unnest(cols = output) %>% 
    filter(state %nin% c("NSW", "ACT")) %>% 
    bind_rows(nsw_act_agg)
  
  
  #largest difference is 0.05% likely due to rounding of the tou curves supplied by RBS (previously used 365.25 days in a year and led to slightly larger errors ~ 0.1%)->
  
  rbs_annual_electricity_consumption <- rbs_fuel_consumption_profiles_agg %>% 
    filter(pv == FALSE,
           ev == 0,
           fuel == "Electricity") %>% 
    group_by(cooking, water_heating, space_heating, ev, pv, state) %>% 
    summarise(annual_consumption_kwh_1 = sum(annual_consumption_gj) * 1/(3.6e-3)) %>% 
    left_join(annual_consumption_exports %>% filter(consumption_export == "Consumption",
                                                    year == 2023
                                                    ) %>% select(-consumption_export)) %>% 
    mutate(pct_diff = (annual_consumption_kwh - annual_consumption_kwh_1) / annual_consumption_kwh_1) %>%
  
  
  return(annual_consumption_exports)
}



#look at aggregate consumption decline over time and see if reasonable.

# annual_consumption_exports %>% 
#   filter(cooking == "electric",
#          water_heating == "electric",
#          space_heating == "electric",
#          state == "NSW and ACT",
#          pv == FALSE,
#          ev == 0) %>% 
#   ggplot(aes(x = year, y = annual_consumption_kwh)) +
#   geom_area()



