#load PV generation profiles

get_pv_profiles <- function(pv_data_path, rbs_households, csiro_pv_prevalance_file){
  
  rbs_households <- rbs_households %>% 
    filter(year == 2020) %>% 
    select(-year)
  
  pv_files <- list.files(path = pv_data_path)
  
  read_pv_data <- function(pv_file){
    
    state_name <- str_extract(pv_file, "(?<=hourly_)\\D*(?=_\\d*kw)")
    system_size <- str_extract(pv_file, "\\d*(?=kw)") 
    
    data <- read_csv(paste0(pv_data_path, "/", pv_file),
                       skip = 31) %>% 
      clean_names() %>% 
      select(month, day, hour, ac_system_output_w) %>% 
      mutate(state = convert_states(state_name),
             size_kw = system_size)
  }
  
  pv_data_list <- map(pv_files, read_pv_data)
  
  pv_data <- bind_rows(pv_data_list) %>% 
    mutate(season = case_when(month %in% c(12,1,2) ~ "Summer",
                              month %in% c(3,4,5) ~ "Autumn",
                              month %in% c(6,7,8) ~ "Winter",
                              month %in% c(9,10,11) ~ "Spring")) %>% 
    group_by(state, size_kw, season, hour) %>% 
    summarise(power_kwh = -mean(ac_system_output_w) / 1000) %>% 
    mutate(end_use = "PV")
  
  #aggregate nsw and act together
  nsw_act_agg <- pv_data %>% 
    filter(state %in% c("NSW", "ACT")) %>% 
    left_join(rbs_households) %>% 
    group_by(across(everything())) %>%
    ungroup(state, power_kwh, occupied_households) %>% 
    summarise(power_kwh = weighted.mean(power_kwh, occupied_households)) %>% 
    ungroup() %>% 
    mutate(state = "NSW and ACT")
  
  pv_data <- pv_data %>% 
    filter(state %nin% c("NSW", "ACT")) %>% 
    bind_rows(nsw_act_agg)
  
  no_pv_profile <- pv_data %>% 
    mutate(pv = FALSE,
           power_kwh = 0)
  
  pv_profile <- pv_data %>% 
    mutate(pv = TRUE)
  
  ##################################################################
  #adjust capacity factors to equal AEMO estimates
  ##################################################################
  
  aemo_capacity_factors <- read_excel(csiro_pv_prevalance_file, sheet = "Sheet2") %>% 
    clean_names() %>% 
    mutate(state = str_remove(state, " \\(SWIS\\)"),
           state = convert_states(state),
           state = if_else(state == "NSW", "NSW and ACT", state)) %>% 
    select(state, capacity_factor)
  
  #calculate capacity factors for our profiles
  pv_watts_factors <- pv_profile %>% 
    filter(size_kw ==  7) %>% 
    #find total generation
    mutate(power_kwh = power_kwh * 365/4) %>% 
    group_by(state) %>% 
    summarise(power_kwh = sum(power_kwh)) %>% 
    #calculate maximum output (24/7 at 7 kw)
    mutate(theoretical_max = 7 * 24 * 365,
           rbs_cf = - power_kwh / theoretical_max) %>% 
    select(state, rbs_cf)
  
  #create factors to scale down output to reflect AEMO actuals
  scale_factors <- left_join(pv_watts_factors, aemo_capacity_factors) %>% 
    mutate(scale_factor = capacity_factor / rbs_cf) %>% 
    select(state, scale_factor)
  
  #apply scale factors to pv profiles
  pv_profile <- pv_profile %>% 
    left_join(scale_factors) %>% 
    mutate(power_kwh = power_kwh * scale_factor) %>% 
    select(-scale_factor)
  
  
  
  # we may want to scale up PV sizes over time, at this stage a PV cameo is just assumed to have a standard 7KW system throughout time.
  
  #Question: is 10 KW system producing 10/7 times as much electricity as a 7 kw system?
  #Answer yes!
  
  # pv_profile %>% 
  #   group_by(state, size_kw, season) %>% 
  #   summarise(power_kwh = sum(power_kwh)) %>% 
  #   pivot_wider(names_from = size_kw, values_from = power_kwh) %>% 
  #   mutate(factor = `10` / `7`,
  #          ref = 10/7)
  
  #expand over all customer types and include day type for consistency (PV generation is the same regardless of day type)
  cust_types <- expand_grid(
    year = seq(2020,2050),
    cooking = c("gas", "electric"),
    water_heating = c("gas", "electric"),
    space_heating = c("gas", "electric"),
    ev = c(0, 1, 2)
  )
  
  
  pv_profiles_all <-bind_rows(no_pv_profile, pv_profile) %>% 
    ungroup() %>% 
    cross_join(cust_types) %>% 
    
    #we're just gonna use 7kw systems for simplicity
    filter(size_kw == 7) %>% 
    select(-size_kw)
  
  return(pv_profiles_all)
}

