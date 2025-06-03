calculate_fuel_use_conversions <- function(fuel_conversion_coefficients,
                                           rbs_outputs_data_file,
                                           rbs_fuel_end_use_by_state,
                                           heating_cooling_profiles){
  
  #read in resisdential baseline output sheet
  rbs_output_cells <- xlsx_cells(rbs_outputs_data_file)
  
  #isolate cooking electricity from microwaves
  cooking_microwave <- rbs_output_cells %>% 
    filter(row > 5,
           sheet == "Energy.Cook.State-Grp") %>% 
    behead("up-left", "appliance_type") %>% 
    behead("up", "state") %>% 
    behead("left", "year") %>% 
    select(year, state, appliance_type, content) %>% 
    rename(pj = content) %>% 
    filter(appliance_type == "Microwave") %>% 
    mutate(pj = as.numeric(pj),
           end_use = 'Cooking',
           state = convert_states(state)) %>% 
    filter(year == 2020)
  
  #calculate aggregate heating and cooling data into annual totals
  agg_heating_cooling <- heating_cooling_profiles %>% 
    pivot_wider(names_from = day_type, values_from = power) %>% 
    mutate(avg = (5 * WD + 2 * WE)/7,
           end_use = end_use_category,
           fuel = "Electricity") %>% 
    group_by(year, state, season, fuel,end_use) %>% 
    summarise(avg_day = sum(avg)) %>%  #get daily total
    ungroup() %>% 
    
    #convert to season total and from power (MWh) to energy (pj)
    
    mutate(pj = avg_day * (365.25/4) * 3.6e-6) %>% 
    group_by(year, state, fuel, end_use) %>% 
    summarise(pj = sum(pj))
  
  
  #2020 is our base year as it represents real data, not projections
  rbs_fuel_end_use_by_state <- rbs_fuel_end_use_by_state %>% 
    filter(year == 2020) %>% 
    #convert all lpg use to natural gas for simplicity (assumed 1:1 for efficiency)
    mutate(fuel = if_else(fuel == "LPG", "Natural Gas", fuel),
           state = convert_states(state)) %>% 
    group_by(year, state, fuel, end_use) %>% 
    summarise(pj = sum(pj)) %>% 
    ungroup() %>% 
    left_join(cooking_microwave %>% 
                select(-appliance_type) %>% 
                rename(microwave_pj = pj)) %>% 
    mutate(pj = if_else(end_use == 'Cooking' & fuel == "Electricity", 
                        pj - microwave_pj,
                        pj)) %>% 
    #add separate microwave data because that isn't converted
    bind_rows(cooking_microwave %>% 
                mutate(fuel = "Electricity",
                       end_use = "Microwave") %>% 
                select(-c(appliance_type))) %>% 
    select(-microwave_pj) %>% 
    #swap aggregated space conditioning data for seperate heating and cooling data
    filter(!(fuel == "Electricity" & end_use == "Space conditioning")) %>% 
    bind_rows(agg_heating_cooling) %>% 
    
    #gas is essentially only used for heating (not cooling) so we allocate all gas space conditioning to heating
    mutate(end_use = if_else(fuel == "Natural Gas" & end_use == "Space conditioning", "Space conditioning - heating", end_use))
                   
  
  
  #complete conversions
  
  gas_to_elec_conv <- rbs_fuel_end_use_by_state %>% 
    filter(end_use %in% c(c("Water heating", "Space conditioning - heating", 'Cooking')),
           fuel %in% c("Natural Gas")) %>% 
    left_join(fuel_conversion_coefficients) %>% 
    #change fuel to electric and multiply by conversion factor
    mutate(pj = pj * gas_to_electric_cf,
           fuel = "Electricity",
           conversion = 'gas_to_electric_converted') %>% 
    select(-c(gas_to_electric_cf)) 
  
  elec_to_gas_conv <- rbs_fuel_end_use_by_state %>% 
    filter(end_use %in% c(c("Water heating", "Space conditioning - heating", 'Cooking')),
           fuel %in% c("Electricity")) %>% 
    left_join(fuel_conversion_coefficients) %>% 
  
  #change fuel to gas and multiply by reciprocal of conversion factor
    
    # Change fuel to gas and multiply by reciprocal of conversion factor
    mutate(pj =  pj * 1/ gas_to_electric_cf, 
           fuel = "Natural Gas",
           conversion = 'electric_to_gas_converted') %>%
    select(-c(gas_to_electric_cf)) 
  
  
  integrated_fuel_use <- bind_rows(rbs_fuel_end_use_by_state,
                                   gas_to_elec_conv,
                                   elec_to_gas_conv) %>% 
    mutate(conversion = if_else(is.na(conversion), 'unconverted', conversion)) %>% 
    #filter out transport consumption (added in from CSIRO EV dat)
    #filter out Natural Gas appliance use (negligible, only applies to gas pool heaters and isn't needed to represent a standard profile)
    filter(end_use != "Transport",
           !(fuel == 'Natural Gas' & end_use == "Appliances")) 
    
  #add australian total
    
    integrated_fuel_use <- integrated_fuel_use %>%
      group_by(year, fuel, end_use, conversion) %>%
      summarise(pj = sum(pj), .groups = "drop") %>%
      mutate(state = "Australia") %>%
      bind_rows(integrated_fuel_use) %>% 
      mutate(state = convert_states(state))
  
  return(integrated_fuel_use)
  
  # #check per cent natural gas use that comes from appliances
  # integrated_fuel_use %>% 
  #   filter(conversion == "unconverted",
  #          fuel == "Natural Gas") %>% 
  #   group_by(state) %>% 
  #   mutate(pct = pj / sum(pj))
  # 
  # 
  # #check per cent appliance use that is natural gas
  # integrated_fuel_use %>% 
  #   filter(conversion == "unconverted",
  #          end_use == "Appliances") %>% 
  #   group_by(state) %>% 
  #   mutate(pct = pj / sum(pj))
  
  # 1-4 per cent. I think we can safely exclude from our profile...
  
}


function(){
#compare aggregated tou to published aggregates for electricity space conditioning

rbs_fuel_end_use_by_state %>% 
  filter(end_use == "Space conditioning",
         fuel == "Electricity") %>% 
  mutate(state = convert_states(state)) %>% 
  left_join(agg_heating_cooling %>% 
  group_by(year, state, fuel) %>% 
  summarise(pj_2 = sum(pj))) %>% 
  mutate(pct_diff = (pj_2 - pj)/pj)

#all less than 1% difference in total consumption\
}