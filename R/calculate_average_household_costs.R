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

household_costs_1 <- calculate_average_household_costs(retail_price_data, 
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
         #category != "petrol",
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


  

#make a chart that shows avoided gas and petrol costs... (a la price trends)


#Make dumbell chart

# pivot so that each category has one row with two cost columns

# reorder categories by, say, 2025 cost (optional)
  
  household_costs_1 %>% 
    filter(year %in% c(2025, 2034),
           state == "Vic") %>% 
    mutate(category = case_when())
    # Convert to wide format to calculate differences
    pivot_wider(
      names_from = year,
      values_from = average_cost_dollars
    ) %>%
    # Ensure column names are character strings
    rename("y2025" = `2025`, "y2034" = `2034`) %>%
    ggplot(aes(x = reorder(category, y2025))) +
    # Add segments with arrows
    
    geom_arrowsegment(aes(xend = category, y = y2025, yend = y2034),
                      arrows = grattan_arrow(length = unit(0.3, "cm"), type = "closed"), 
                      position = position_attractsegment(start_shave = 0, 
                                                         end_shave = 50, 
                                                         type_shave = "distance"),
                      colour = grattan_darkgrey3,
                      fill = grattan_darkgrey3,
                      size = 1) +

    # Add points for 2025 and 2034
    geom_point(aes(y = y2025), color = grattan_orange, size = 4) +
    geom_point(aes(y = y2034), color = grattan_red, size = 4) +

    scale_y_continuous_grattan(
      labels = scales::dollar_format(),
    ) +
    coord_flip() +
    labs(
      x = "",
      y = "",
      title = "Petrol gas and electricity costs all decline",
      subtitle = "Average household costs by category, Victoria, 2025 and 2034"
    ) +
    theme_grattan(flipped = TRUE) 

  

#alignment between gas use decline and residential elecrtification? timing doesn' add up? is our way of allocating gas demand to residential to rough (e.g are commercial actually switching over faster...?
#Note !! that some of the decline in average gas use is associated with new connections that don't use gas, rather than switches from gas to electric, hence some discrepancy is to be expected.

}
