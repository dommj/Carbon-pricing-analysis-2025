#calculate electricity consumption by vehicle type

get_ev_consumption_profiles <- function(electric_vehicle_workbook_file,
                                        wem_esoo_2024_ev_projections_file,
                                                 ev_fleet_data,
                                        ev_efficiency_projections){
  ###################################
  #NEM
  ###################################
  
  ev_workbook_cells <- xlsx_cells(electric_vehicle_workbook_file)
  
  formats <- xlsx_formats(electric_vehicle_workbook_file)
  
  theme <- formats$local$fill$patternFill$fgColor$theme
  
  ##########################
  #NEM charge type weights
  ##########################
  
  nsw_weights <- read_excel(electric_vehicle_workbook_file,
                            sheet = "BEV_PHEV_Charge_Type (%)",
                            range = "B31:AF48") %>% 
    mutate(state = "NSW")
  
  qld_weights <- read_excel(electric_vehicle_workbook_file,
                            sheet = "BEV_PHEV_Charge_Type (%)",
                            range = "B93:AF110") %>% 
    mutate(state = "Qld")
  
  sa_weights <- read_excel(electric_vehicle_workbook_file,
                           sheet = "BEV_PHEV_Charge_Type (%)",
                           range = "B156:AF173") %>% 
    mutate(state = "SA")
  
  tas_weights <- read_excel(electric_vehicle_workbook_file,
                            sheet = "BEV_PHEV_Charge_Type (%)",
                            range = "B218:AF235") %>% 
    mutate(state = "Tas")
  
  vic_weights <- read_excel(electric_vehicle_workbook_file,
                            sheet = "BEV_PHEV_Charge_Type (%)",
                            range = "B280:AF297") %>% 
    mutate(state = "Vic")
  
  
  nem_charge_type_weights <- bind_rows(nsw_weights,
                                   qld_weights,
                                   sa_weights,
                                   tas_weights,
                                   vic_weights) %>% 
    rename(charge_type = 1) %>% 
    pivot_longer(cols = contains("20"), 
                 names_to = "year",
                 values_to = "pct") %>% 
    filter(str_detect(charge_type, "Residential")) %>% 
    mutate(year = str_remove(year, "\\d\\d-") %>% 
             as.numeric(),
           pct = as.numeric(pct),
           charge_type = str_remove(charge_type, ".*-\\s"))
  
  
  # charge_type_weights <- ev_workbook_cells %>%
  #   filter(row > 7,
  #          sheet == "BEV_PHEV_Charge_Type (%)") %>%                  
  #   filter(!is_blank) %>%
  #   behead_if(theme[local_format_id] == "background1" &
  #               str_detect(character, "(New|Queen|South|Tas|Vic)"),
  #             direction = "up-left",
  #             name = "state") %>%
  #   behead_if(theme[local_format_id] == "background1" &
  #               str_detect(character, "(Step|Green|Prog)"),
  #             direction = "up-left",
  #             name = "scenario") %>%
  #   behead("up", "year") %>%
  #   behead("left", "charge_type") %>%
  #   select(state, scenario, charge_type, year, content)
  
  
  
  ##########################
  #NEM profiles
  ##########################
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
           charge_type = str_remove(charge_type, ".*,\\s"),
           day_type = "WD",
           state = convert_states(state)) %>% 
    filter(str_detect(vehicle_type, "Residential")) %>% 
    left_join(nem_charge_type_weights) %>% 
    group_by(year, state, vehicle_type, day_type, time, hour) %>% 
    #weight by charging weights
    summarise(power_kwh = weighted.mean(as.numeric(content), pct)) %>% 
    # hourly rate = mean of half hourly rate
    group_by(year, state, vehicle_type, day_type, hour) %>% 
    #weight by charging weights
    summarise(power_kwh = mean(as.numeric(power_kwh))) 
  
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
           charge_type = str_remove(charge_type, ".*,\\s"),
           day_type = "WE",
           state = convert_states(state)) %>% 
    filter(str_detect(vehicle_type, "Residential")) %>% 
    left_join(nem_charge_type_weights) %>% 
    group_by(year, state, vehicle_type, day_type, time, hour) %>% 
    #weight by charging weights
    summarise(power_kwh = weighted.mean(as.numeric(content), pct)) %>% 
    # hourly rate = mean of half hourly rate
    group_by(year, state, vehicle_type, day_type, hour) %>% 
    #weight by charging weights
    summarise(power_kwh = mean(as.numeric(power_kwh))) 
  
  nem_unweighted_ev_profiles <- bind_rows(weekday_profiles, 
                                      weekend_profiles) %>% 
    mutate(state = convert_states(state),
           state = if_else(state == "NSW", "NSW and ACT", state)) 
  
  #apply EV efficiency projections (NEM data assumes a 2040 fleet)
  
  nem_efficiency_index <- ev_efficiency_projections %>% 
    group_by(size) %>% 
    mutate(index = kwh_km / kwh_km[year == 2040],
           vehicle_type = paste0(str_to_sentence(size), " Residential")) %>% 
    ungroup() %>% 
    select(-c(size, kwh_km))
  
  
  nem_unweighted_ev_profiles <- nem_unweighted_ev_profiles %>% 
    left_join(nem_efficiency_index) %>% 
    mutate(power_kwh = power_kwh * index) %>% 
    select(-index) %>% 
    filter(year <= 2050)
  
  
  ###################################
  #WEM
  ###################################
  
  wem_ev_workbook_cells <- xlsx_cells(wem_esoo_2024_ev_projections_file)
  
  wa_weights <- read_excel(wem_esoo_2024_ev_projections_file,
                            sheet = "BEV_PHEV_Charge_Type (%)",
                            range = "B39:M55") %>% 
    mutate(state = "WA") %>% 
    rename(charge_type = 1) %>% 
    pivot_longer(cols = contains("20"), 
                 names_to = "year",
                 values_to = "pct") %>% 
    filter(str_detect(charge_type, "Residential")) %>% 
    mutate(year = str_remove(year, "\\d\\d-") %>% 
             as.numeric(),
           pct = as.numeric(pct),
           charge_type = str_remove(charge_type, ".*-\\s"))
  
  wa_weights <- wa_weights %>% 
    #add in 2034 and assume same weights as 2033 (all other WA data goes out to 2034)
    bind_rows(
      wa_weights %>% 
        filter(year == 2033) %>% 
        mutate(year = 2034)
    ) %>% 
    #we have an annoying matching problem with the profiles, this is to fix this.
    mutate(charge_type =  case_when(
      str_detect(charge_type, "^Public Charging$") ~ "Public Charging",
      str_detect(charge_type, "^TOU Grid Solar$") ~ "TOU Grid Solar Charging",
      str_detect(charge_type, "^TOU Home Solar$") ~ "TOU Home Solar Charging",
      str_detect(charge_type, "^Unscheduled Charging$") ~ "Unscheduled Charging",
      str_detect(charge_type, "^Vehicle to Grid$") ~ "Vehicle to Grid",
      str_detect(charge_type, "^Vehicle to Home$") ~ "Vehicle to Home",
      # Keep existing standardized names
      str_detect(charge_type, "^TOU Grid Solar Charging$") ~ "TOU Grid Solar Charging",
      str_detect(charge_type, "^TOU Home Solar Charging$") ~ "TOU Home Solar Charging",
      TRUE ~ charge_type  # Keep anything else unchanged
    ))
  
  #note that wem profiles are based on 2030 stock whereas nem are based on 2040 stock, we correct for this using the efficiency projections
  
  # wa_weights %>%
  #   select(charge_type) %>%
  #   unique() %>%
  #   pull()
  # 
  # wem_weekday_profiles %>%
  #   select(charge_type) %>%
  #   unique() %>%
  #   pull()
  # 
  # 
  # wem_weekday_profiles %>% 
  #   select(vehicle_type) %>% 
  #   unique() %>% 
  #   pull()

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
           vehicle_type = str_remove(charge_type, ",.*") %>% 
             str_remove("\r\n") %>% 
             str_remove("(?<=\\D)\\s(?=\\s\\D)"),
           charge_type = str_remove(charge_type, ".*,\\s"),
           #we have an annoying matching problem with the profiles, this is to fix this.
           charge_type =  case_when(
             str_detect(charge_type, "Public Charging$") ~ "Public Charging",
             str_detect(charge_type, "TOU Grid Solar$") ~ "TOU Grid Solar Charging",
             str_detect(charge_type, "TOU Home Solar$") ~ "TOU Home Solar Charging",
             str_detect(charge_type, "Unscheduled Charging$") ~ "Unscheduled Charging",
             str_detect(charge_type, "^Vehicle to Grid$") ~ "Vehicle to Grid",
             str_detect(charge_type, "^Vehicle to Home$") ~ "Vehicle to Home",
             # Keep existing standardized names
             str_detect(charge_type, "TOU Grid Solar Charging$") ~ "TOU Grid Solar Charging",
             str_detect(charge_type, "TOU Home Solar Charging$") ~ "TOU Home Solar Charging",
             TRUE ~ charge_type  # Keep anything else unchanged
           )%>% 
             str_remove(" \r\n") %>% 
             str_remove(" - vehicle charging"),
           day_type = "WD",
           state = convert_states(state)) %>% 
    filter(str_detect(vehicle_type, "Residential")) %>% 
    left_join(wa_weights) %>% 
    group_by(year, state, vehicle_type, day_type, time, hour) %>% 
    #weight by charging weights
    summarise(power_kwh = weighted.mean(as.numeric(content), pct)) %>% 
    # hourly rate = mean of half hourly rate
    group_by(year, state, vehicle_type, day_type, hour) %>% 
    summarise(power_kwh = mean(as.numeric(power_kwh))) %>% 
    ungroup()
  
  
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
           vehicle_type = str_remove(charge_type, ",.*") %>% 
             str_remove("\r\n") %>% 
             str_remove("(?<=\\D)\\s(?=\\s\\D)"),
           charge_type = str_remove(charge_type, ".*,\\s"),
           #we have an annoying matching problem with the profiles, this is to fix this.
           charge_type =  case_when(
             str_detect(charge_type, "Public Charging$") ~ "Public Charging",
             str_detect(charge_type, "TOU Grid Solar$") ~ "TOU Grid Solar Charging",
             str_detect(charge_type, "TOU Home Solar$") ~ "TOU Home Solar Charging",
             str_detect(charge_type, "Unscheduled Charging$") ~ "Unscheduled Charging",
             str_detect(charge_type, "^Vehicle to Grid$") ~ "Vehicle to Grid",
             str_detect(charge_type, "^Vehicle to Home$") ~ "Vehicle to Home",
             # Keep existing standardized names
             str_detect(charge_type, "TOU Grid Solar Charging$") ~ "TOU Grid Solar Charging",
             str_detect(charge_type, "TOU Home Solar Charging$") ~ "TOU Home Solar Charging",
             TRUE ~ charge_type  # Keep anything else unchanged
           )%>% 
             str_remove(" \r\n") %>% 
             str_remove(" - vehicle charging"),
           day_type = "WE",
           state = convert_states(state)) %>% 
    filter(str_detect(vehicle_type, "Residential")) %>% 
    left_join(wa_weights) %>% 
    group_by(year, state, vehicle_type, day_type, time, hour) %>% 
    #weight by charging weights
    summarise(power_kwh = weighted.mean(as.numeric(content), pct)) %>% 
    # hourly rate = mean of half hourly rate
    group_by(year, state, vehicle_type, day_type, hour) %>% 
    summarise(power_kwh = mean(as.numeric(power_kwh))) %>% 
    ungroup()
  
  wa_unweighted_ev_profiles <- bind_rows(wem_weekday_profiles, 
                                          wem_weekend_profiles)  
  
  #apply EV efficiency projections (WA data assumes a 2030 fleet)
  
  wa_efficiency_index <- ev_efficiency_projections %>% 
    group_by(size) %>% 
    mutate(index = kwh_km / kwh_km[year == 2030],
           vehicle_type = paste0(str_to_sentence(size), " Residential")) %>% 
    ungroup() %>% 
    select(-c(size, kwh_km))
  
  
  wa_unweighted_ev_profiles <- wa_unweighted_ev_profiles %>% 
    left_join(wa_efficiency_index) %>% 
    mutate(power_kwh = power_kwh * index) %>% 
    select(-index)
  
  ###################################
  #total
  ###################################
  
  
  total_unweighted_ev_profiles <- bind_rows(wa_unweighted_ev_profiles, nem_unweighted_ev_profiles) %>% 
    filter(year > 2023,
           year <= 2050)
  
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
  
  
#calculate implied km driven by state and year...
  
  
  
  

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



km_travelled <- total_unweighted_ev_profiles %>% 
  #filter(vehicle_type == "Medium Residential") %>% 
  group_by(year, state, vehicle_type, day_type) %>% 
  summarise(power_kwh = sum(power_kwh)) %>% 
  pivot_wider(names_from = day_type, values_from = power_kwh) %>% 
  mutate(total_daily = (5 * WD + 2 *WE)/7,
         total_annual = total_daily * 365) %>% 
  left_join(ev_efficiency_projections %>% 
              mutate(vehicle_type = paste0(str_to_sentence(size), " Residential"))) %>% 
  mutate(km = total_annual / kwh_km) %>% 
  select(year, state, vehicle_type, km) %>% 
  left_join(vehicle_type_weights) %>% 
  group_by(year, state) %>% 
  summarise(km = weighted.mean(km, vehicles_count))


km_travelled %>% 
  ggplot(aes(x = year, y= km, colour = state)) +
  geom_line()


}

# weighted_ev_profiles %>%
#   filter(year == 2030 ) %>%
#   group_by(year, state) %>%
#   summarise(power_kwh = sum(power_kwh))
# 
# ev_consumption_profiles %>% 
#   filter(year == 2030 ) %>% 
#   group_by(year, state) %>% 
#   summarise(power_kwh = sum(power_kwh))


# 
# total_unweighted_ev_profiles %>%
#   filter(year == 2030) %>%
#   group_by(year, state, vehicle_type) %>%
#   summarise(power_kwh = sum(power_kwh)) %>% 
#   left_join(ev_efficiency_projections)
