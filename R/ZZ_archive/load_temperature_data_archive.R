# create list of heating and cooling days
load_temperature_data <- function(temp_data_folder, comfort_temp = 20){
  
  temp_files <- list.files(path = temp_data_folder)
  
  read_temp_data <- function(temp_file){
    
    state_name <- str_extract(temp_file, "(?<=daily_)\\D*(?=.csv)")
    max_min_name <- str_extract(temp_file, "(?<=t)\\D\\D\\D")
    
    col_name <- case_when(max_min_name == "max" ~ "maximum_temperature_deg_c",
                          max_min_name == "min" ~ "minimum_temperature_deg_c")
    
    data <- read_csv(paste0(temp_data_folder, "/", temp_file)) %>% 
      clean_names() %>% 
      select(date, all_of(col_name)) %>% 
      mutate(state = convert_states(state_name),
             max_min = max_min_name) %>% 
      rename(temp = 2) %>% 
      filter(!is.na(date))
    
    data
  }
  
  temp_data_list <- map(temp_files, read_temp_data)
  
  temp_data <- bind_rows(temp_data_list) %>% 
    filter(year(date) >= 2000) %>% 
    pivot_wider(names_from = max_min, values_from = temp) 
  
  
    # mutate(average = (max + min)/2,
    #        cooling_deg_days = if_else(average > comfort_temp, average - comfort_temp, 0),
    #        month = month(date),
    #        day = day(date)) %>% 
    # group_by(state, month) %>% 
    # summarise(cooling_deg_days = mean(cooling, na.rm = T))
  
  
}

temp_file <- temp_files[1]

read_temp_data(temp_files[1])

max_min_name <- str_extract(temp_files[1], "(?<=t)\\D\\D\\D")

col_name <- case_when(max_min_name == "max" ~ "maximum",
                      max_min_name == "min" ~ "minimum")
comfort_temp <- 20
