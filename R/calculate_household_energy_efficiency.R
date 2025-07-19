calculate_household_energy_efficiency <- function(esoo_2024_operational_file, 
                                                  household_connections,
                                                  esoo_2020_operational_file,
                                                  wem_esoo_2024_operational_file){
  
  #underlying demand = total residential delivered + total residential PV
  
  #define underlying demand for 2020-2024
  res_underlying_esoo_2020_24 <-   read_excel(esoo_2020_operational_file) %>% 
    clean_names() %>% 
    filter(scenario %in% c('Actual', 'Central'),
           parent_category == 'Operational (Sent Out)',
           category %in% c('Residential', 'Electrification', 'Rooftop PV'),
           
           #only include residential electrification
           sub_category != 'Business',
           region != 'NEM',
           year <= 2024,
           year >= 2020) %>% 
    rename(state = region) %>% 
    mutate(state = convert_states(state),
           state = if_else(state == 'NSW', 'NSW and ACT', state)) %>% 
    group_by(year, state) %>% 
    summarise(annual_consumption_t_wh = sum(annual_consumption_t_wh)) %>% 
    mutate(category = "Underlying")
  
  #define underlying demand for 2025-2050
  res_underlying_esoo_2025 <- read_excel(esoo_2024_operational_file) %>% 
    clean_names() %>% 
    filter(scenario %in% c('Actual', 'Central'),
           parent_category == 'Operational (Sent Out)',
           category %in% c('Residential', 'Electrification', 'Rooftop PV'),
           
           #only include residential electrification
           sub_category != 'Business',
           region != 'NEM') %>% 
    rename(state = region) %>% 
    mutate(state = convert_states(state),
           state = if_else(state == 'NSW', 'NSW and ACT', state)) %>% 
    group_by(year, state) %>% 
    summarise(annual_consumption_t_wh = sum(annual_consumption_t_wh)) %>% 
    mutate(category = "Underlying")
  
  #define energy efficiency for 2021-2024
  res_energy_efficiency_esoo_2020_24 <- read_excel(esoo_2020_operational_file) %>% 
    clean_names() %>% 
    filter(scenario %in% c('Actual', 'Central'),
           parent_category == 'Operational (Sent Out)',
           category %in% c('Energy Efficiency'),
           
           #only include residential 
           sub_category != 'Business',
           region != 'NEM',
           year <= 2024) %>% 
    rename(state = region) %>% 
    mutate(state = convert_states(state),
           state = if_else(state == 'NSW', 'NSW and ACT', state)) %>% 
    group_by(year, state, category) %>% 
    summarise(annual_consumption_t_wh = sum(annual_consumption_t_wh)) %>% 
    ungroup()
  
  #pull out cumulative efficiency gains to 2024 to add on to the gains starting in 2025
  #We are essentially assuming here that energy efficiency predicted from 2020-2024 did actually occur
  cumulative_efficiency_2024 <- res_energy_efficiency_esoo_2020_24 %>% 
    filter(year == 2024) %>% 
    select(- year) %>% 
    rename(add_on = annual_consumption_t_wh)
  
  #define energy efficiency for 2025-2050
  res_energy_efficiency_esoo_2025 <- read_excel(esoo_2024_operational_file) %>% 
    clean_names() %>% 
    filter(scenario %in% c('Actual', 'Central'),
           parent_category == 'Operational (Sent Out)',
           #only include explicitly residential categories. residential EV will be added on top.
           category %in% c('Energy Efficiency'),
           
           #only include residential electrification
           sub_category != 'Business',
           region != 'NEM') %>% 
    rename(state = region) %>% 
    mutate(state = convert_states(state),
           state = if_else(state == 'NSW', 'NSW and ACT', state)) %>% 
    group_by(year, state, category) %>% 
    summarise(annual_consumption_t_wh = sum(annual_consumption_t_wh)) %>% 
    #add cumulative efficiency up to 2024
    left_join(cumulative_efficiency_2024) %>% 
    mutate(annual_consumption_t_wh = annual_consumption_t_wh + add_on) %>% 
    select(-add_on)
  
  #calculate estimated energy efficiency gains as a per cent of estimated underlying 2021-2050
  pct_efficiency_underlying <- bind_rows(res_underlying_esoo_2020_24, res_underlying_esoo_2025,
                                         res_energy_efficiency_esoo_2020_24, res_energy_efficiency_esoo_2025) %>% 
    pivot_wider(names_from = category, values_from = annual_consumption_t_wh) %>% 
    
    #energy efficiency + underlying represents the total underlying consumption if there were no efficiency gains
    #the energy efficiency multiplier represents the value that we should scale down underlying consumption in that year
    mutate(efficiency_multiplier = Underlying / (`Energy Efficiency` + Underlying)) %>% 
    
    #note that efficiency gains flatline after 2050, with some uplift in underlying demand (likely due to economic growth / appliance consumption) - not really relevant for us as we just go out to 2050
    
    select(year, state, efficiency_multiplier) %>% 
    ungroup() %>% 
    complete(year = seq(from = 2020, to = 2050), 
             state = unique(state),
             fill = list(efficiency_multiplier = 1))
    
    
    ################
    #WEM
    ################
    
    #define underlying demand for 2025-2050
    res_underlying_wem_2025 <- read_excel(wem_esoo_2024_operational_file) %>% 
      clean_names() %>% 
      filter(scenario %in% c('Actual', 'Expected (Step Change)'),
             parent_category == 'Operational (Sent Out)',
             #only include explicitly residential categories. residential EV will be added on top.
             category %in% c('Residential', 'Electrification', 'Rooftop PV'),
             
             #only include residential electrification
             sub_category != 'Business') %>% 
      mutate(state = "WA") %>% 
      group_by(year, state) %>% 
      summarise(annual_consumption_t_wh = sum(annual_consumption_t_wh)) %>% 
      mutate(category = "Underlying")
    
    #WEM only has ESOO for 2024 so we can't back calculate any efficiency gains to 2020 (assumed to be small)
    res_energy_efficiency_wem_2025 <- read_excel(wem_esoo_2024_operational_file) %>% 
      clean_names() %>% 
      filter(scenario %in% c('Actual', 'Expected (Step Change)'),
             parent_category == 'Operational (Sent Out)',
             category %in% c('Energy Efficiency'),
             #only include residential 
             sub_category != 'Business') %>% 
      mutate(state = "WA") %>% 
      select(year, state, category, annual_consumption_t_wh)
    
    
    #calculate estimated energy efficiency gains as a per cent of estimated underlying 2021-2050
    pct_efficiency_underlying_wem <- bind_rows(res_energy_efficiency_wem_2025,
                                               res_underlying_wem_2025) %>% 
      pivot_wider(names_from = category, values_from = annual_consumption_t_wh) %>% 
      
      #energy efficiency + underlying represents the total underlying consumption if there were no efficiency gains
      #the energy efficiency multiplier represents the value that we should scale down underlying consumption in that year
      mutate(efficiency_multiplier = Underlying / (`Energy Efficiency` + Underlying)) %>% 
      
      select(year, state, efficiency_multiplier) %>% 
      ungroup() %>% 
      complete(year = seq(from = 2020, to = 2034), 
               state = unique(state),
               fill = list(efficiency_multiplier = 1))
    
    
    
    pct_efficiency_underlying_total <- bind_rows(pct_efficiency_underlying, pct_efficiency_underlying_wem)
    
    return(pct_efficiency_underlying_total)
}
