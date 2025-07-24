#get average driving distances

get_average_km_per_vehicle <- function(mv_survey_data_file){
  
km_per_vehicle <-  read_excel(mv_survey_data_file,
             sheet = "Table_4",
             range = "A5:G78") %>% 
    clean_names() %>% 
    rename(vehicle = 1) %>% 
    mutate(state = case_when(
      # Identify rows that are state headings (not NA in first column, but NA in others)
      !is.na(vehicle) & is.na(average_kilometres_travelled) ~ vehicle,
      TRUE ~ NA
    ),
    state = convert_states(state),
    
    #take nsw average for nsw and act (won't make much difference, can change later if we want)
    
    state = if_else(state == 'NSW'|state=="ACT", 'NSW and ACT', state),
    average_kilometres_travelled = average_kilometres_travelled %>% as.numeric() * 1000
    
    ) %>%
    # Fill the state values down
    fill(state, .direction = "down") %>%
  group_by(state, vehicle) %>%
  summarise(average_kilometres_travelled = weighted.mean(average_kilometres_travelled, w = as.numeric(number_of_vehicles), na.rm = TRUE)) %>%
    # Remove the state heading rows
    filter(!is.na(vehicle), !is.na(average_kilometres_travelled),
           vehicle == 'Passenger vehicles') %>% 
    select(state, average_kilometres_travelled) 

return(km_per_vehicle)
}

#get_average_km_per_vehicle('Data/92080DO001_202006.xls')
