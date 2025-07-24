#split space conditioning consumption profile into heating and cooling using temp data

#then calculate totals for each season (check that they sum to the same annual total as the aggregate method)

#in the integrated fuel calculation switch in the distinct space heating end uses for the calculation.

#in the full tou replace the space conditioning end use with both end uses
 #enjoy the fruits of your labour.

#add all gas use to the heating curve (have a look at seasonal totals and see how closely it matches guys numbers)

calculate_space_heating_tou <- function(rbs_tou_consumption_data, temperature_data, 
                                        comfort_temp_heating = 18, comfort_temp_cooling = 18){
  
  comfort_temp_heating <- 18
  comfort_temp_cooling <- 18
  
  heating_cooling_degree_days <- temperature_data %>% 
    mutate(cooling_deg_days = if_else(average > comfort_temp_cooling, average - comfort_temp_cooling, 0),
           heating_deg_days = if_else(average < comfort_temp_heating, comfort_temp_heating - average, 0),
           month = as.numeric(month),
           season = fct_case_when(month %in% c(12,1,2) ~ "Summer",
                                  month %in% c(3,4,5) ~ "Autumn",
                                  month %in% c(6,7,8) ~ "Winter",
                                  month %in% c(9,10,11) ~ "Spring")) %>% 
    group_by(state, season) %>% 
    summarise(cooling_deg_days = sum(cooling_deg_days),
              heating_deg_days = sum(heating_deg_days)) %>% 
    ungroup() %>% 
    mutate(pct_load_heating = heating_deg_days /(heating_deg_days + cooling_deg_days))
  
  
  heating_cooling_profiles <- rbs_tou_consumption_data %>% 
    filter(end_use_category == "Space conditioning") %>% 
    left_join(heating_cooling_degree_days %>% select(state, season, pct_load_heating)) %>% 
    mutate(heating = pct_load_heating * power,
           cooling = (1 - pct_load_heating) * power) %>% 
    select(-c(power, pct_load_heating)) %>% 
    pivot_longer(cols = c(heating, cooling), names_to = "type", values_to = "power") %>% 
    mutate(end_use_category = paste0(end_use_category, ' - ', type)) %>% 
    select(-type)
  
  ## checked totals match - they do :) 
  heating_cooling_profiles
}


# heating_cooling_profiles %>%
#   filter(state == "Vic",
#          day_type == "WD") %>%
#   ggplot(aes(x= hour, y = power, colour = end_use_category)) +
#     facet_wrap(~season) +
#   geom_line()
