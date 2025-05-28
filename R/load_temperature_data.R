#load typical year temperature data
function(temp_data_folder, comfort_temp = 20){
  
  temp_files <- list.files(path = temp_data_folder)
  
  read_temp_data <- function(file_name){
    
    file_path <- paste0(temp_data_folder, "/", file_name)
  
    headers <- c(
      "Year", 
      "Month", 
      "Day", 
      "Hour", 
      "Minute", 
      "DataSourceAndUncertaintyFlags", 
      "DryBulbTemp_C", 
      "DewPointTemp_C", 
      "RelativeHumidity", 
      "AtmosphericStationPressure_mb", 
      "ExtraterrestrialHorizontalRadiation_Whm2", 
      "ExtraterrestrialDirectNormalRadiation_Whm2", 
      "HorizontalInfraredRadiationFromSky_Whm2", 
      "GlobalHorizontalRadiation_Whm2", 
      "DirectNormalRadiation_Whm2", 
      "DiffuseHorizontalRadiation_Whm2", 
      "GlobalHorizontalIlluminance_lux", 
      "DirectNormalIlluminance_lux", 
      "DiffuseHorizontalIlluminance_lux", 
      "ZenithLuminance_Cdm2", 
      "WindDirection_degrees", 
      "WindSpeed_ms", 
      "TotalSkyCover", 
      "OpaqueSkyCover", 
      "Visibility_km", 
      "CeilingHeight_m", 
      "PresentWeatherObservation", 
      "PresentWeatherCodes", 
      "PrecipitableWater_mm", 
      "AerosolOpticalDepth_thousandths", 
      "SnowDepth_cm", 
      "DaysSinceLastSnowfall"
    )
    
    
    location <- read_lines(file_path) 
    
    location <- location[1] %>% 
      str_extract("(?<=LOCATION,)[\\w\\s]*,[\\w\\s]*,[\\w\\s]*")
    
    weather_data <- read_csv(file_path, skip = 8, col_names = headers) %>% 
      mutate(location = location) %>% 
      select(location, Year, Month, Day, Hour, DryBulbTemp_C) %>% 
      clean_names() %>% 
      group_by(location, month, day) %>% 
      summarise(min_c = min(dry_bulb_temp_c),
                max_c = max(dry_bulb_temp_c))
   
    
    weather_data 
  }

  temp_data_list <- map(temp_files, read_temp_data)
  
  state_list <- c("NSW", "Vic", "NT", "WA", "Qld", "Tas", "ACT", "SA")
  
  temp_data <- bind_rows(temp_data_list) %>% 
    mutate(location = case_when(str_detect(location, "Sydney") ~ "NSW",
                                str_detect(location, "Melbourne") ~ "Vic",
                                str_detect(location, "Darwin") ~ "NT",
                                str_detect(location, "Brisbane") ~ "Qld",
                                str_detect(location, "Canberra") ~ "ACT",
                                str_detect(location, "Hobart") ~ "Tas",
                                str_detect(location, "Adelaide") ~ "SA",
                                str_detect(location, "Perth") ~ "WA",
                                ),
           average = (max_c + min_c)/2,
           cooling_deg_days = if_else(average > comfort_temp, average - comfort_temp, 0),
           heating_deg_days = if_else(average < comfort_temp, comfort_temp - average, 0),
           month = as.numeric(month),
           season = fct_case_when(month %in% c(12,1,2) ~ "Summer",
                                  month %in% c(3,4,5) ~ "Autumn",
                                  month %in% c(6,7,8) ~ "Winter",
                                  month %in% c(9,10,11) ~ "Spring")) %>% 
    filter(location %in% state_list) %>% 
    group_by(location, season) %>% 
    summarise(cooling_deg_days = sum(cooling_deg_days),
              heating_deg_days = sum(heating_deg_days)) %>% 
    ungroup() %>% 
    mutate(pct_load_heating = heating_deg_days /(heating_deg_days + cooling_deg_days))
  
  
}
