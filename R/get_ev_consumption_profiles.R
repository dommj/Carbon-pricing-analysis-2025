#calculate electricity consumption by vehicle type

get_ev_consumption_profiles <- function(electric_vehicle_workbook_file,
                                        wem_esoo_2024_ev_projections_file,
                                                 ev_fleet_data){
  ###################################
  #NEM
  ###################################
  
  ev_workbook_cells <- xlsx_cells(electric_vehicle_workbook_file)
  
  formats <- xlsx_formats(electric_vehicle_workbook_file)
  
  theme <- formats$local$fill$patternFill$fgColor$theme
  
  weekday_profiles <- ev_workbook_cells %>% 
    filter(row > 6,
           sheet == "BEV_PHEV_Profile_kW (Weekday)") %>% 
    filter(!is_blank) %>%
    behead_if(theme[local_format_id] == "background1",
              direction = "left-up", 
              name = "state") %>% 
    behead("up", "time") %>% 
    behead("left", "charge_type") %>% 
    select(state, charge_type, time, content) %>% 
    filter(!is.na(state),
           !is.na(charge_type)) %>% 
    mutate(hour = hour(time),
           time = format(as_datetime(time), format = "%H:%M:%S"),
           vehicle_type = str_remove(charge_type, ",.*"),
           day_type = "WD") %>% 
    group_by(state, vehicle_type, day_type, hour) %>% 
    summarise(power_kwh = mean(as.numeric(content))) %>% 
    filter(str_detect(vehicle_type, "Residential")) 
  
  weekend_profiles <- ev_workbook_cells %>% 
    filter(row > 6,
           sheet == "BEV_PHEV_Profile_kW (Weekend)") %>% 
    filter(!is_blank) %>%
    behead_if(theme[local_format_id] == "background1",
              direction = "left-up", 
              name = "state") %>% 
    behead("up", "time") %>% 
    behead("left", "charge_type") %>% 
    select(state, charge_type, time, content) %>% 
    filter(!is.na(state),
           !is.na(charge_type)) %>% 
    mutate(hour = hour(time),
           time = format(as_datetime(time), format = "%H:%M:%S"),
           vehicle_type = str_remove(charge_type, ",.*"),
           day_type = "WE") %>% 
    group_by(state, vehicle_type, day_type, hour) %>% 
    summarise(power_kwh = mean(as.numeric(content))) %>% 
    filter(str_detect(vehicle_type, "Residential")) 
  
  nem_unweighted_ev_profiles <- bind_rows(weekday_profiles, 
                                      weekend_profiles) %>% 
    mutate(state = convert_states(state),
           state = if_else(state == "NSW", "NSW and ACT", state)) 
  
  ###################################
  #WEM
  ###################################
  
  #note that wem profiles are based on 2030 stock whereas nem are based on 2040 stock
  
  wem_ev_workbook_cells <- xlsx_cells(wem_esoo_2024_ev_projections_file)
  
  wem_weekday_profiles <- wem_ev_workbook_cells %>% 
    filter(row > 15,
           sheet == "BEV_PHEV_Profile_kW (Weekday)") %>% 
    filter(!is_blank) %>%
    behead("up", "time") %>% 
    behead("left", "charge_type") %>% 
    mutate(state = "WA") %>% 
    select(state, charge_type, time, content) %>% 
    filter(!is.na(state),
           !is.na(charge_type)) %>% 
    mutate(hour = hour(time),
           time = format(as_datetime(time), format = "%H:%M:%S"),
           vehicle_type = str_remove(charge_type, ",.*"),
           vehicle_type = str_remove(vehicle_type, "\r\n"),
           day_type = "WD") %>% 
    group_by(state, vehicle_type, day_type, hour) %>% 
    summarise(power_kwh = mean(as.numeric(content))) %>% 
    filter(str_detect(vehicle_type, "Residential")) 
  
  
  wem_weekend_profiles <- wem_ev_workbook_cells %>% 
    filter(row > 16,
           sheet == "BEV_PHEV_Profile_kW (Weekend)") %>% 
    filter(!is_blank) %>%
    behead("up", "time") %>% 
    behead("left", "charge_type") %>% 
    mutate(state = "WA") %>% 
    select(state, charge_type, time, content) %>% 
    filter(!is.na(state),
           !is.na(charge_type)) %>% 
    mutate(hour = hour(time),
           time = format(as_datetime(time), format = "%H:%M:%S"),
           vehicle_type = str_remove(charge_type, ",.*"),
           vehicle_type = str_remove(vehicle_type, "\r\n"),
           day_type = "WE") %>% 
    group_by(state, vehicle_type, day_type, hour) %>% 
    summarise(power_kwh = mean(as.numeric(content))) %>% 
    filter(str_detect(vehicle_type, "Residential")) 
  
  
  wem_unweighted_ev_profiles <- bind_rows(wem_weekday_profiles, 
                                          wem_weekend_profiles)  
  
  ###################################
  #total
  ###################################
  
  total_unweighted_ev_profiles <- bind_rows(wem_unweighted_ev_profiles, nem_unweighted_ev_profiles)
  
  vehicle_type_weights <- ev_fleet_data %>% 
    filter(fuel_type == "BEV") %>% # battery EVs dominate the charging profile
    select(year, state, vehicle_type, vehicles_count)
  
  #weight profile according to vehicle count
  weighted_ev_profiles <- total_unweighted_ev_profiles %>% 
    left_join(vehicle_type_weights, relationship = "many-to-many") %>% 
    group_by(year, state, day_type, hour) %>% 
    summarise(power_kwh = weighted.mean(power_kwh, vehicles_count)) %>% 
    #take weighted average of WD and WE to average day profile
    pivot_wider(names_from = day_type, values_from = power_kwh) %>% 
    mutate(power_kwh = (5* WD + 2 * WE)/7) %>% 
    select(-c(WD, WE))
  
  return(weighted_ev_profiles)
}

#totals are slightly lower than expected, due to lack of commercial vehicle inclusion? Or inclusion of PHEVs in the per vehile charging profiles? Numbers are reasonable however.

# totals <- weighted_ev_profiles %>% 
#   group_by(state) %>% 
#   summarise(power_kwh = sum(power_kwh))

##########################################
#old
##########################################
function(){

weekday <- read_excel("Data/2024 ESOO/2024 Electric Vehicle Workbook.xlsx", 
                      sheet = "BEV_PHEV_Profile_kW (Weekday)",
                      skip = 4) %>% 
  mutate(state = case_when(
    # Identify rows that are state headings (not NA in first column, but NA in others)
    !is.na(.[[1]]) & is.na(.[[2]]) ~ .[[1]],
    TRUE ~ NA
  )) %>%
  # Fill the state values down
  fill(state, .direction = "down") %>%
  # Remove the state heading rows
  filter(!is.na(.[[1]]), !is.na(.[[2]])) %>% 
  rename(charging_profile = 1)


weekend <- read_excel("Data/2024 ESOO/2024 Electric Vehicle Workbook.xlsx", 
                      sheet = "BEV_PHEV_Profile_kW (Weekend)",
                      skip = 4) %>% 
  mutate(state = case_when(
    # Identify rows that are state headings (not NA in first column, but NA in others)
    !is.na(.[[1]]) & is.na(.[[2]]) ~ .[[1]],
    TRUE ~ NA
  )) %>%
  # Fill the state values down
  fill(state, .direction = "down") %>%
  # Remove the state heading rows
  filter(!is.na(.[[1]]), !is.na(.[[2]])) %>% 
  rename(charging_profile = 1)



residential_charging <- weekday %>% 
  rowwise() %>%
  mutate(total = sum(c_across(contains('..')), na.rm = TRUE),
         total_kwh = total * 0.5) %>% #convert 30min increments to kwh consumption
  ungroup() %>% 
  select(state, charging_profile, total_kwh) %>% 
  filter(str_detect(charging_profile, "Residential")) 
}

