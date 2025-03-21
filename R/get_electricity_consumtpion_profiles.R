#calculate electricity consumption by vehicle type

get_electricity_consumption_profiles <- function(electric_vehicle_workbook_file){
  
  
  weekday <- read_excel(electric_vehicle_workbook_file, sheet = "" ) %>%

  
 
  
  
}

time_increments <- time_increments <- format(
  seq(
    from = as.POSIXct("2023-01-01 00:00:00"),
    to = as.POSIXct("2023-01-01 23:30:00"),
    by = "30 min"
  ),
  format = "%I:%M %p"
) %>%
  # Remove leading zeros for hours
  str_replace("^0", "")

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


