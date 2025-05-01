#calculate household costs

calculate_average_household_costs <- function(retail_price_data, 
                                      gas_retail_volumetric_price_projections,
                                      gas_network_charge_revenue,
                                      petrol_price_projections,
                                      household_connections_data,
                                      average_residential_operational_demand,
                                      average_gas_consumption,
                                      average_petrol_consumption){
  
  electricity_costs <- left_join(retail_price_data, average_residential_operational_demand) %>% 
    select(year, state, category, average_annual_consumption_kwh, c_kwh) %>% 
    mutate(average_cost_dollars = average_annual_consumption_kwh * c_kwh / 100) %>% 
    select(year, state, category, average_cost_dollars)
  
  petrol_costs <- left_join(petrol_price_projections, average_petrol_consumption) %>% 
    select(year, state, category, average_annual_consumption_litres, c_litre) %>% 
    mutate(average_cost_dollars = average_annual_consumption_litres * c_litre / 100) %>%
    select(year, state, category, average_cost_dollars)
  
  gas_costs <- left_join(gas_retail_volumetric_price_projections, average_gas_consumption) %>% 
    left_join(gas_network_charge_revenue) %>% 
    left_join(household_connections_data) %>%
    select(year, state, category, average_annual_consumption_gj, dollars_per_gj, network_revenue_dollars, connections) %>% 
    mutate(average_volume_cost_dollars = average_annual_consumption_gj * dollars_per_gj,
           average_connection_cost_dollars = network_revenue_dollars / connections,
           average_cost_dollars = average_volume_cost_dollars + average_connection_cost_dollars) %>% 
    select(year, state, category, average_cost_dollars)
  
  household_costs <- bind_rows(electricity_costs, petrol_costs, gas_costs) 
  
  household_costs
}

function(){

household_costs_1 <- calculate_household_costs(retail_price_data, 
                          gas_retail_volumetric_price_projections,
                          gas_network_charge_revenue,
                          petrol_price_projections,
                          household_connections_data,
                          average_residential_operational_demand,
                          average_gas_consumption,
                          average_petrol_consumption)

plot_data <- household_costs_1 %>%
  filter(year >= 2025,
         year <= 2034, 
         category != "petrol",
         state == "Vic") 

levels <- plot_data %>% 
  select(category) %>% 
  unique() %>% 
  pull()

label_data <- plot_data %>% 
  filter(year == 2034) %>% 
  mutate(category = factor(category, levels = levels)) %>% 
  arrange(category) %>%
  # Calculate the y positions for the labels at the midpoints of each stack
  mutate(y_position = rev(cumsum(rev(average_cost_dollars))),
         y_position = y_position - 0.5 * average_cost_dollars)


plot_data %>% 
  mutate(category = factor(category, levels = levels)) %>% 
  # filter(category %in% c("gas", "Electrification")) %>% 
  ggplot(aes(x= year, y = average_cost_dollars, fill = category, colour = category)) +
  geom_area() +
  theme_grattan() +
  grattan_label(data = label_data, 
                aes(x = 2034.1, 
                    y = y_position, 
                    label = category),
                hjust = 0) +

  grattan_y_continuous(labels = dollar) + 
  grattan_x_continuous(breaks = c(2025, 2030, 2034),
                       expand_right = 0.2) +
  labs(title = 'Overall consumer energy costs will decline as prices ease and electrification increases',
       subtitle = 'Projected energy costs to residential consumers, Victoria',
       x = '',
       y = '')
  
grattan_save_all("Output/Atlas/test_houshold_costs_no_petrol.pdf",
                 object = ggplot2::last_plot())

household_costs_1 %>% 
  filter(year == 2025 | year == 2034)

#alignment between gas use decline and residential elecrtification? timing doesn' add up? is our way of allocating gas demand to residential to rough (e.g are commercial actually switching over faster...?
#Note !! that some of the decline in average gas use is associated with new connections that don't use gas, rather than switches from gas to electric, hence some discrepancy is to be expected.

}