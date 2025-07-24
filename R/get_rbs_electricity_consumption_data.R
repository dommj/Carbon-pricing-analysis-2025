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
  
  
  #clean vic space conditioning data from heyfield community paper
  vic_space_conditioning_data <- read_excel(rbs_electricity_consumption_data_file, sheet = "vic_space_conditioning") %>% 
    clean_names() %>% 
    mutate(state = "Vic",
           end_use_category = "Space conditioning",
           hour = hour(hour),
           year = 2020) %>% 
    select(year, hour, spring, summer, autumn, winter, state, end_use_category) %>% 
    pivot_longer(cols = c(autumn, spring, summer, winter), names_to = "season", values_to = "power") %>% 
    #assume same consumption on weekday as week end (doesn't matter as this is an average and we take an average at the end anyway)
    cross_join(tibble(day_type = c("WD", "WE")))
  
  
  vic_space_conditioning_totals <- vic_space_conditioning_data %>% 
    filter(day_type == "WD") %>% 
    group_by(state, end_use_category) %>% 
    summarise(total_heyfield = sum(power) * 365 / 4)
  
  #rbs totals
  rbs_sc_totals <- consumption_data %>% 
    filter(state == "Vic",
           end_use_category == "Space conditioning") %>% 
    pivot_wider(names_from = day_type, values_from = power) %>% 
    mutate(power = (5*WD + 2*WE)/7) %>% 
    group_by(state, end_use_category) %>% 
    summarise(total_rbs = sum(power) * 365 / 4)
  
  
  vic_space_conditioning_normalised <- vic_space_conditioning_data %>% 
    left_join(vic_space_conditioning_totals) %>% 
    left_join(rbs_sc_totals) %>% 
    mutate(power = power * total_rbs / total_heyfield,
           season = str_to_sentence(season)) %>% 
    select(-c(total_heyfield, total_rbs))
  ## So, just confirming, you're assuming RBS is right on total electricity consumption for space heating in Vic, 
  ## and Heyfield is more accurate on the time of day?
  
  
  #double check total adds to RBS total
  vic_space_conditioning_normalised %>% 
    pivot_wider(names_from = day_type, values_from = power) %>% 
    mutate(power = (5*WD + 2*WE)/7) %>% 
    group_by(state, end_use_category) %>% 
    summarise(total_heyfield = sum(power) * 365 / 4)
  
  
  consumption_data_adj <- consumption_data %>% 
    filter(!(state == "Vic" & end_use_category == "Space conditioning")) %>% 
    bind_rows(vic_space_conditioning_normalised)
  
  #compare the two profiles
  
   bind_rows(consumption_data %>% 
    filter((state == "Vic" & end_use_category == "Space conditioning")) %>% 
      mutate(cat = "RBS") %>% 
      pivot_wider(names_from = day_type, values_from = power) %>% 
      mutate(power = (5*WD + 2*WE)/7), 
    vic_space_conditioning_normalised %>% 
      filter(day_type == "WD") %>% 
      mutate(cat = "heyfield")) %>% 
     ggplot(aes(x = hour, y = power, colour = cat)) +
     facet_wrap(~season) +
     geom_line()
  
  return(consumption_data_adj)
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
