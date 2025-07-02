
data <- bind_rows(cameo_electricity_costs, cameo_gas_costs) %>% 
  cross_join(tibble(ice = c(0,1,2))) %>% 
  bind_rows(cameo_petrol_costs)



consumer_type_1 <- c(cooking = "gas", water_heating = "gas", space_heating = "electric", ev = 0, pv = FALSE, ice = 1)

consumer_type_1_code <- paste(consumer_type_1["cooking"], consumer_type_1["water_heating"],
                              consumer_type_1["space_heating"], consumer_type_1["ev"],
                              consumer_type_1["pv"], consumer_type_1["ice"], sep = "_")


data %>% 
  filter(cooking == consumer_type_1["cooking"], 
         water_heating == consumer_type_1["water_heating"],
         space_heating == consumer_type_1["space_heating"], 
         ev == consumer_type_1["ev"],
         pv == consumer_type_1["pv"],
         ice == consumer_type_1["ice"],
         state == "NSW and ACT",
         year %in% seq(2025, 2034)) %>% 
  ggplot(aes(x = year, y = annual_cost_dollars, fill = category)) +
  geom_col() + 
  grattan_y_continuous() +
  theme_grattan()


ten_year_costs_data <- data %>% 
  filter(year %in% seq(2025, 2034)) %>%
  group_by(cooking, water_heating, space_heating, ev, pv, ice, state) %>% 
  summarise(total_cost_dollars = sum(annual_cost_dollars)) %>% 
  mutate(consumer_type = paste(cooking, water_heating, space_heating, ev, pv, ice, sep = "_")) %>% 
  filter(consumer_type %in% c("gas_gas_gas_0_FALSE_1", # all gas with an ICE vehicle
                              "gas_gas_gas_1_FALSE_0",
                              "gas_gas_gas_1_TRUE_0",
                              "electric_electric_electric_1_TRUE_0")) %>% 
  mutate(consumer_name = fct_case_when(consumer_type == "gas_gas_gas_0_FALSE_1" ~ "Gas home, Petrol car",
                                       consumer_type == "gas_gas_gas_1_FALSE_0" ~ "EV",
                                       consumer_type == "gas_gas_gas_1_TRUE_0" ~ "Solar",
                                       consumer_type == "electric_electric_electric_1_TRUE_0" ~ "Electrify gas") %>% fct_rev())

ten_year_costs_data %>% 
  filter(state == "NSW and ACT") %>% 
    ggplot(aes(x = consumer_name, y = total_cost_dollars)) +
    geom_col() +
  coord_flip()
 
  
ten_year_costs_data %>% 
  filter(state %in% c("Qld", "NSW and ACT", "Vic", "SA")) %>% 
  ggplot(aes(x = total_cost_dollars / 10, y = reorder(state, total_cost_dollars))) +
  geom_line(colour = grattan_darkgrey) +
  grattan_label(data = . %>% filter(state == "Vic"), aes(color = consumer_name,
                    label = str_wrap(consumer_name, 10),
                    label.colour = "white"),
                nudge_y = 0.7,
                nudge_x = 0.5) +
  grattan_point_filled(aes(colour = fct_rev(consumer_name)), size = 3, stroke = 2, fill = "white", shape = 21) +
  scale_x_continuous_grattan(limits = c(0, 6000), 
                             labels = scales::dollar_format(),
                             expand_right = 0.1)+
  scale_y_discrete(expand = c(0,0.4,0.4,0)) +
  #scale_colour_manual(values = c(grattan_orange, grattan_red)) +
  theme_grattan(flipped = T) +
  labs(
    x = "",
    y = "",
    title = "Households that electrify and install solar will save",
    subtitle = "Annualised total energy costs, 2025-2034"
  ) 


# baseline_costs <- ten_year_costs_data %>%
#   filter(consumer_type == "gas_gas_gas_0_FALSE_1") %>% 
#   mutate(baseline_cost = total_cost_dollars) %>% 
#   ungroup() %>% 
#   select(state, baseline_cost)

plot_data <- ten_year_costs_data %>%
  filter(state %in% c("Qld", "NSW and ACT", "Vic", "SA")) %>% 
  group_by(state) %>% 
  arrange(-total_cost_dollars) %>% 
  mutate(annual_savings = (lag(total_cost_dollars) - total_cost_dollars)/10) 

label_data <- plot_data %>% 
  filter(state == "Vic",
         consumer_type != "gas_gas_gas_0_FALSE_1") %>% 
  # Calculate the y positions for the labels at the midpoints of each stack
  mutate(y_position = cumsum(annual_savings),
         y_position = y_position - 0.5 * annual_savings)


plot_data %>% 
  ggplot(aes(x = reorder(state, annual_savings), y = annual_savings, fill = consumer_name, colour = consumer_name)) +
  geom_col() +
  theme_grattan() +
  grattan_label(data = label_data, 
                aes(x = state, 
                    y = y_position, 
                    label = consumer_name),
                hjust = 0,
                nudge_x = 0.5) +
  
  grattan_y_continuous(labels = dollar) + 
  scale_x_discrete(expand = c(0,0,0,2)) +
  labs(title = '10 year annualised savings',
       subtitle = 'savings....',
       x = '',
       y = '')


grattan_save_pptx("test_savings.pptx")

ten_year_costs


#####################################################
#Average costs charts
#####################################################

bind_rows(average_electricity_costs, average_gas_costs, average_petrol_costs) %>% 
  left_join(household_connections) %>% 
  group_by(year, electrification, category) %>% 
  filter(year >= 2025,
         year <= 2034) %>% 
  summarise(average_cost_dollars = weighted.mean(average_cost_dollars, connections)) %>% 
  ggplot(aes(x = year, y = average_cost_dollars, fill = category)) +
    geom_col() +
    facet_wrap(~electrification)


  
  