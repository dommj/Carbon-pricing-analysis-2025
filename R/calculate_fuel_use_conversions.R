calculate_fuel_use_conversions <- function(fuel_conversion_coefficients,
                                           rbs_outputs_data_file,
                                           rbs_fuel_end_use_by_state){
  
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
           end_use = 'Cooking') %>% 
    filter(year == 2020)
  
  
  #2020 is our base year as it represents real data, not projections
  rbs_fuel_end_use_by_state <- rbs_fuel_end_use_by_state %>% 
    filter(year == 2020) %>% 
    #convert all lpg use to natural gas for simplicity (assumed 1:1 for efficiency)
    mutate(fuel = if_else(fuel == "LPG", "Natural Gas", fuel)) %>% 
    group_by(year, state, fuel, end_use) %>% 
    summarise(pj = sum(pj)) %>% 
    ungroup() %>% 
    left_join(cooking_microwave %>% 
                select(-appliance_type) %>% 
                rename(microwave_pj = pj)) %>% 
    mutate(pj = if_else(end_use == 'Cooking', 
                        pj - microwave_pj,
                        pj)) %>% 
    bind_rows(cooking_microwave %>% 
                mutate(fuel = "Electricity",
                       end_use = "Microwave") %>% 
                select(-c(appliance_type))) %>% 
    select(-microwave_pj)
                   
  
  
  #complete conversions
  
  gas_to_elec_conv <- rbs_fuel_end_use_by_state %>% 
    filter(end_use %in% c(c("Water heating", "Space conditioning", 'Cooking')),
           fuel %in% c("Natural Gas")) %>% 
    left_join(fuel_conversion_coefficients) %>% 
    #change fuel to electric and multiply by conversion factor
    mutate(pj = pj * gas_to_electric_cf,
           fuel = "Electricity",
           conversion = 'gas_to_electric_converted') %>% 
    select(-c(gas_to_electric_cf)) 
  
  elec_to_gas_conv <- rbs_fuel_end_use_by_state %>% 
    filter(end_use %in% c(c("Water heating", "Space conditioning", 'Cooking')),
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





