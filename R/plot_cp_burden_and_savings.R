
plot_cp_burden_and_savings <- function(average_net_costs,
                                       household_connections){
  
  
  chart_fill_palette <- c(
    "Petrol" = grattan_red,
    "Gas" = grattan_orange, 
    "Electricity" = grattan_yellow,
    "Electrification savings" = "transparent",
    "Total costs without electrification" = grattan_black
  )
  
  chart_colour_palette <- c(
    "Petrol" = grattan_red,
    "Gas" = grattan_orange, 
    "Electricity" = grattan_yellow,
    "Electrification savings" = grattan_black,
    "Total costs without electrification" = grattan_black
  )
  
  
  years <- seq(2025, 2050, 5)
  
  degrees <- "2_Opt2"
  
  chart_data <- average_net_costs %>% 
    left_join(household_connections) %>% 
    filter(state != "WA",
           scenario == "Ref" | scenario == degrees,
           year %in% years,
           electrification == T) %>% 
    group_by(year, scenario, category) %>% 
    summarise(average_cost_dollars = weighted.mean(average_cost_dollars, connections)) %>% 
    mutate(category = fct(category, levels = c("Petrol", "Electricity", "Gas"))) %>% 
    ungroup() 
  
  
  
  gap <- chart_data %>% 
    filter(year %in% years) %>% 
    group_by(year, scenario) %>% 
    summarise(average_cost_dollars = sum(average_cost_dollars)) %>% 
    pivot_wider(names_from = scenario, values_from = average_cost_dollars) %>%
    #create a row of costs for the counterfactual case
    mutate(average_cost_dollars = !!sym(degrees) - Ref,
           category = "Carbon price",
           electrification = T) %>% 
    select(-c(!!!degrees, Ref))
  
  
  col_chart_data <- chart_data %>% 
    filter(scenario == "Ref") %>% 
    bind_rows(gap) %>% 
    mutate(category = fct(category, levels = c("Petrol", 
                                               "Electricity", 
                                               "Gas", 
                                               "Carbon price"))) %>% 
    ungroup()
  
  
  savings_box_data <- col_chart_data %>% 
    group_by(year) %>% 
    mutate(cumsum_top = cumsum(average_cost_dollars),
           cumsum_bottom = cumsum_top - average_cost_dollars) %>% 
    filter(category == "Carbon price")
  
  
  label_data <- col_chart_data %>% 
    # Calculate the y positions for the labels at the midpoints of each stack
    group_by(year) %>% 
    arrange(category) %>% 
    mutate(y_position = cumsum(average_cost_dollars),
           y_position = y_position - 0.5 * average_cost_dollars) %>% 
    ungroup()
  
  
  plot <- col_chart_data %>% 
    filter(category != "Electrification savings") %>% 
    ggplot(aes(x = year, y = average_cost_dollars)) +
    geom_col(aes(x = year, y = average_cost_dollars, 
                 colour = fct_rev(category), fill = fct_rev(category))) +
    grattan_label(data = label_data %>% 
                    filter(year == 2050), 
                  aes(x = year, 
                      y = y_position, 
                      label = str_wrap(category, 13),
                      colour = category),
                  hjust = 0,
                  nudge_x = 2.5) +
    
    grattan_y_continuous(labels = scales::dollar_format(), expand_top = 0.1) +
    scale_x_continuous_grattan(expand_right = 0.3,
                               breaks = seq(2025,2050, by = 5)) +
    scale_colour_manual(values = chart_colour_palette) +
    scale_fill_manual(values = chart_fill_palette) +
    theme_grattan() +
    labs(title = paste0("The average household will save $X by "),
         subtitle = 'Average annual household energy costs, by fuel source',
         x = '',
         y = '',
         caption = "Notes: \nSource: Grattan Institute analaysis see app X")
  
  plot
  
  
  ##########################
  #total annualised
  ##########################
  
  chart_data_2 <- average_net_costs %>% 
    left_join(household_connections) %>% 
    filter(state != "WA",
           scenario == "Ref" | scenario == "2_Opt1",
           electrification == T) %>% 
    group_by(year, scenario, category) %>% 
    summarise(average_cost_dollars = weighted.mean(average_cost_dollars, connections)) %>% 
    mutate(category = fct(category, levels = c("Petrol", "Electricity", "Gas"))) %>% 
    ungroup() 
  
  annualised_25_yrs <- chart_data_2 %>% 
    group_by(scenario, category) %>% 
    summarise(annualised_cost = sum(average_cost_dollars) / n())
  
  
  
}

#investigating consumption discontinuities

function(){

annual_electricity_consumption_averages %>% 
  filter(consumption_export == "Consumption",
         electrification == T) %>% 
  mutate(consumer_type = paste0(pv, "_", battery)) %>% 
  left_join(average_consumer_type_weights) %>% 
  group_by(year, state) %>% 
  summarise(annual_consumption_kwh = weighted.mean(annual_consumption_kwh, prop)) %>% 
  ggplot(aes(x = year, y = annual_consumption_kwh, colour = state)) +
  geom_line()


annual_electricity_consumption_averages %>% 
  filter(consumption_export == "Consumption",
         electrification == T,
         battery == 0,
         pv == 0) %>% 
  ggplot(aes(x = year, y = annual_consumption_kwh, colour = state)) +
  geom_line()



esoo_average_underlying_demand %>% 
  group_by(year, state) %>% 
  summarise(power_kwh = sum(power_kwh)) %>% 
  filter(state == "Tas") %>% 
  ggplot(aes(x = year, y = power_kwh))+
  geom_line()



all_average_profiles %>% 
  filter(pv == 0, battery == 0,
         electrification == T) %>% 
  group_by(year, state, season, pv, battery, electrification, source) %>% 
  summarise(power_kwh = sum(power_kwh)) %>% 
  ungroup() %>% 
  mutate(season_total = power_kwh * (365 / 4)) %>% 
  group_by(year, state, pv, battery, electrification, source) %>% 
  summarise(annual_consumption_kwh = sum(season_total)) %>%  
  ungroup() %>% 
  #remove WA data post 2034 becuase esoo only goes out til thenb
  filter(!(state== "WA" & year > 2034),
         year >= 2025) %>% 
  
  filter(state == "Tas") %>% 
  
  ggplot(aes(x = year, y = annual_consumption_kwh, colour = source)) +
  geom_line()
  
  
all_average_profiles %>% 
  filter(pv == 0, battery == 0,
         electrification == T) %>% 
  group_by(year, state, season, pv, battery, electrification, source) %>% 
  summarise(power_kwh = sum(power_kwh)) %>% 
  ungroup() %>% 
  mutate(season_total = power_kwh * (365 / 4)) %>% 
  group_by(year, state, pv, battery, electrification) %>% 
  summarise(annual_consumption_kwh = sum(season_total)) %>%  
  ungroup() %>% 
  #remove WA data post 2034 becuase esoo only goes out til thenb
  filter(!(state== "WA" & year > 2034),
         year >= 2025) %>% 
  
  filter(state == "Tas") %>% 
  
  ggplot(aes(x = year, y = annual_consumption_kwh)) +
  geom_line()  +
  grattan_y_continuous(limits = c(0,9250))



#baseline for all states
all_average_profiles %>% 
  filter(pv == 0, battery == 0,
         electrification == T,
         source == "baseline") %>% 
  group_by(year, state, season, pv, battery, electrification) %>% 
  summarise(power_kwh = sum(power_kwh)) %>% 
  ungroup() %>% 
  mutate(season_total = power_kwh * (365 / 4)) %>% 
  group_by(year, state, pv, battery, electrification) %>% 
  summarise(annual_consumption_kwh = sum(season_total)) %>%  
  ungroup() %>% 
  #remove WA data post 2034 becuase esoo only goes out til thenb
  filter(!(state== "WA" & year > 2034),
         year >= 2025) %>% 
  
  
  ggplot(aes(x = year, y = annual_consumption_kwh, colour = state)) +
  geom_line()



#esoo underlying demand residential...
read_excel(esoo_2024_operational_file) %>% 
  clean_names() %>% 
  filter(scenario %in% c('Actual', 'Central'),
         parent_category == 'Operational (Sent Out)',
         #only include explicitly residential categories. residential EV will be added on top.
         category %in% c('Rooftop PV', 'Residential', "Electrification", "Energy Efficiency"),
         #only include residential electrification
         sub_category != 'Business',
         region != 'NEM') %>% 
  rename(state = region) %>% 
  mutate(state = convert_states(state),
         state = if_else(state == 'NSW', 'NSW and ACT', state),
         #we allocate demand that is note related to electrification or energy efficiency to "baseline"
         source = case_when(category == "Electrification" ~ "electrification", 
                            category == "Energy Efficiency" ~ "energy efficiency", 
                            .default = "baseline")) %>%
  filter(source == "baseline") %>% 
  ggplot(aes(x= year, y = annual_consumption_t_wh, colour = sub_category)) +
  facet_wrap(~state) +
  geom_line()

read_excel(esoo_2024_operational_file) %>% 
  clean_names() %>% 
  filter(scenario %in% c('Actual', 'Central'),
         parent_category == 'Operational (Sent Out)',
         #only include explicitly residential categories. residential EV will be added on top.
         category %in% c('Rooftop PV', 'Residential', "Electrification", "Energy Efficiency"),
         #only include residential electrification
         sub_category != 'Business',
         region != 'NEM') %>% 
  rename(state = region) %>% 
  mutate(state = convert_states(state),
         state = if_else(state == 'NSW', 'NSW and ACT', state),
         #we allocate demand that is note related to electrification or energy efficiency to "baseline"
         source = case_when(category == "Electrification" ~ "electrification", 
                            category == "Energy Efficiency" ~ "energy efficiency", 
                            .default = "baseline")) %>%
  filter(source == "baseline") %>% 
  group_by(year, state, source) %>% 
  summarise(annual_consumption_t_wh = sum(annual_consumption_t_wh)) %>% 
  ggplot(aes(x= year, y = annual_consumption_t_wh)) +
  facet_wrap(~state) +
  geom_line()


  
read_excel(esoo_2024_operational_file) %>% 
  clean_names() %>% 
  filter(scenario %in% c('Actual', 'Central'),
         parent_category == 'Operational (Sent Out)',
         #only include explicitly residential categories. residential EV will be added on top.
         category %in% c('Rooftop PV', 'Residential', "Electrification", "Energy Efficiency"),
         #only include residential electrification
         sub_category != 'Business',
         region != 'NEM') %>% 
  rename(state = region) %>% 
  mutate(state = convert_states(state),
         state = if_else(state == 'NSW', 'NSW and ACT', state),
         #we allocate demand that is note related to electrification or energy efficiency to "baseline"
         source = case_when(category == "Electrification" ~ "electrification", 
                            category == "Energy Efficiency" ~ "energy efficiency", 
                            .default = "baseline")) %>%  
  group_by(year, state, source) %>% 
  summarise(annual_consumption_t_wh = sum(annual_consumption_t_wh)) %>% 
  #calculate average annual consumption per connection
  left_join(household_connections, by = c('year', 'state')) %>% 
  ungroup() %>% 
  mutate(power_kwh = annual_consumption_t_wh / connections * 1e9) %>% 
  select(year, state, power_kwh, source) %>% 
  filter(source != "energy efficiency",
         year <= 2050) %>% 
  filter(source == "baseline") %>% 
  ggplot(aes(x= year, y = power_kwh)) +
  facet_wrap(~state) +
  geom_line()


household_connections %>% 
  ggplot(aes(x= year, y = connections)) +
  facet_wrap(~state) +
  geom_line()


#having gone through this awful process it seems like the apparent discontinuities truly are an accurate reflection of aemo forecasts...

}

