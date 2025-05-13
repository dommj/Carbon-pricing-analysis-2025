# this script reads in estimates for average behind-the-meter ToU electricity power consumption by state and season from 2020-2040

get_rbs_electricity_consumption_data <- function(rbs_electricity_consumption_data_file) {
  # read in data
  consumption_data <- read_excel(rbs_electricity_consumption_data_file, sheet = "Demand.Data") %>% 
    clean_names() %>% 
    mutate(state = convert_states(region)) %>% 
    group_by(state, season, day_type, end_use_category, year, hour) %>% 
    summarise(power = sum(power)) %>% 
    ungroup() %>% 
    filter(end_use_category %nin% c("Generation", "Transport"),
           year == 2020,
           !is.na(state)) #filter out new zealand
  
  consumption_data
}

# data <- get_rbs_electricity_consumption_data(rbs_electricity_consumption_data_file, rbs_outputs_data_file)
# 
# data_ed <- data %>% 
#   clean_names() %>% 
#   mutate(state = convert_states(region),
#          state = if_else(str_detect(state, "NSW|ACT"), "NSW and ACT", state)) %>% 
#   group_by(state, season, day_type, end_use_category, year, hour) %>% 
#   summarise(power = sum(power)) %>% 
#   ungroup() %>% 
#   filter(end_use_category %nin% c("Generation", "Transport")) 
# 
#   
# end_uses <- data %>% 
#   clean_names() %>% 
#   select(end_use_category) %>%
#   unique()
# 
# rbs_connections_data <- read_excel(rbs_outputs_data_file, 
#                                    sheet = "HH-State",
#                                    skip = 5)
