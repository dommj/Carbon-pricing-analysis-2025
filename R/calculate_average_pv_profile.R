
calculate_average_pv_profile <- function(pv_profiles,
                                         esoo_2024_operational_file,
                                         pv_system_stock){
  
  pv_generation_per_system <-  read_excel(esoo_2024_operational_file) %>% 
    clean_names() %>% 
    filter(scenario %in% c('Actual', 'Central'),
           parent_category == 'Operational (Sent Out)',
           #only include explicitly residential categories. residential EV will be added on top.
           category %in% c('Rooftop PV'),
           #only include residential PV
           sub_category != 'Business',
           region != 'NEM') %>% 
    rename(state = region) %>% 
    mutate(state = convert_states(state),
           state = if_else(state == 'NSW', 'NSW and ACT', state)) %>% 
    group_by(year, state, category) %>% 
    summarise(annual_consumption_t_wh = sum(annual_consumption_t_wh)) %>% 
    left_join(pv_system_stock) %>% 
    mutate(power_kwh = annual_consumption_t_wh / pv_stock * 1e9)
  
  
  ##############################
  #calculate implied system size
  ##############################
  
  #get PV profile for 7 kw system
  pv_profile_7kw <- pv_profiles %>% 
    filter(pv == TRUE) %>% 
    select(state, season, hour, end_use, power_kwh) %>% 
    unique()
  
  yearly_aggregate_7kw <- pv_profile_7kw %>% 
    mutate(power_kwh = power_kwh * 365/4) %>% 
    group_by(state, end_use) %>% 
    summarise(power_kwh_7kw = sum(power_kwh))
  
  implied_average_system_size <- pv_generation_per_system %>% 
    left_join(yearly_aggregate_7kw) %>% 
    mutate(system_size = power_kwh / -power_kwh_7kw * 7,
           scale_factor = system_size / 7)
    
  
  implied_average_system_size %>% 
    ggplot(aes(x = year, y = system_size, colour = state)) +
    geom_line()
  
  ####################################################################
  #scale 7 Kw profiles to equal average generation
  ####################################################################
  
  pv_profile_average <- pv_profile_7kw %>% 
    right_join(implied_average_system_size %>% 
                select(year, state, scale_factor, system_size)) %>% 
    mutate(power_kwh = power_kwh * scale_factor) %>% 
    select(-scale_factor) %>% 
    filter(year <= 2050)
  
  
  return(pv_profile_average)
}
