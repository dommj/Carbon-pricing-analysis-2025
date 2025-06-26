#load PV generation profiles

get_pv_profiles <- function(pv_data_path, rbs_households){
  
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
  
  #expand over all customer types and include day type for consistency (PV generation is the same regardless of day type)
  cust_types <- expand_grid(
    season = c("Summer", "Autumn", "Winter", "Spring"),
    cooking = c("gas", "electric"),
    water_heating = c("gas", "electric"),
    space_heating = c("gas", "electric"),
    ev = c(0, 1, 2)
  )
  
  return(bind_rows(no_pv_profile, pv_profile) %>% 
           ungroup() %>% 
           full_join(cust_types, relationship = "many-to-many") %>% 
           
           #we're just gonna use 7kw systems for simplicity
           filter(size_kw == 7) %>% 
           select(-size_kw))
}

