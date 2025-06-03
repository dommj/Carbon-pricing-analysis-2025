#calculate electricity consumption by vehicle type

get_ev_consumption_profiles <- function(electric_vehicle_workbook_file,
                                                 ev_fleet_data){
  
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
  
  unweighted_ev_profiles <- bind_rows(weekday_profiles, 
                                      weekend_profiles) %>% 
    mutate(state = convert_states(state),
           state = if_else(state == "NSW", "NSW and ACT", state)) 
  
  
  vehicle_type_weights <- ev_fleet_data %>% 
    filter(year == 2040,
           fuel_type == "BEV") %>% # battery EVs dominate the charging profile
    select(state, vehicle_type, vehicles_count)
  
  #weight profile according to vehicle count
  weighted_ev_profiles <- unweighted_ev_profiles %>% 
    left_join(vehicle_type_weights) %>% 
    group_by(state, day_type, hour) %>% 
    summarise(power_kwh = weighted.mean(power_kwh, vehicles_count)) %>% 
    #take weighted average of WD and WE to average day profile
    pivot_wider(names_from = day_type, values_from = power_kwh) %>% 
    mutate(power_kwh = (5* WD + 2 * WE)/7) %>% 
    select(-c(WD, WE))
  
  
  no_ev_profile <- weighted_ev_profiles %>% 
    mutate(ev = 0,
           end_use = "Electric vehicle",
           power_kwh = 0)
  
  one_ev_profile <- weighted_ev_profiles %>% 
    mutate(ev = 1,
           end_use = "Electric vehicle")
  
  two_ev_profile <- weighted_ev_profiles %>% 
    mutate(ev = 2,
           end_use = "Electric vehicle",
           power_kwh = 2 * power_kwh)
  
  
  #assign same daily profile across all seasons and all consumer types
  seasons_n_cust_types <- expand_grid(
    state = weighted_ev_profiles %>% 
      select(state) %>% 
      unique() %>% 
      pull(),
    season = c("Summer", "Autumn", "Winter", "Spring"),
    cooking = c("gas", "electric"),
    water_heating = c("gas", "electric"),
    space_heating = c("gas", "electric"),
    pv = c(TRUE, FALSE)
  )
  
  ev_consumption_profiles <- bind_rows(no_ev_profile, one_ev_profile, two_ev_profile) %>% 
    full_join(seasons_n_cust_types, relationship = "many-to-many")
  
  return(ev_consumption_profiles)
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

