
plot_cp_burden_and_savings <- function(average_net_costs,
                                       household_connections){
  
  
  
  chart_data <- average_net_costs %>% 
    left_join(household_connections) %>% 
    group_by(year, electrification, category) %>% 
    filter(year >= 2025,
           year <= 2050,
           state != "WA",
           scenario == "1_5_Opt2") %>% 
    summarise(average_cost_dollars = weighted.mean(average_cost_dollars, connections)) %>% 
    mutate(category = fct(category, levels = c("Petrol", "Electricity", "Gas"))) %>% 
    ungroup()
  
  
  
  
  gap <- chart_data %>% 
    filter(year %in% c(2025, 2030, 2035, 2040)) %>% 
    group_by(year, electrification) %>% 
    summarise(average_cost_dollars = sum(average_cost_dollars)) %>% 
    pivot_wider(names_from = electrification, values_from = average_cost_dollars) %>%
    #create a row of costs for the counterfactual case
    mutate(average_cost_dollars = `FALSE` - `TRUE`,
           category = "Electrification savings",
           electrification = T) %>% 
    select(-c(`FALSE`, `TRUE`))
  
  
  col_chart_data <- chart_data %>% 
    filter(year %in% c(2025, 2030, 2035, 2040),
           electrification == T) %>% 
    bind_rows(gap) %>% 
    mutate(category = fct(category, levels = c("Petrol", 
                                               "Electricity", 
                                               "Gas", 
                                               "Electrification savings"))) %>% 
    ungroup()
  
  
  savings_box_data <- col_chart_data %>% 
    group_by(year) %>% 
    mutate(cumsum_top = cumsum(average_cost_dollars),
           cumsum_bottom = cumsum_top - average_cost_dollars) %>% 
    filter(category == "Electrification savings")
  
  
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
                    filter(year == 2040), 
                  aes(x = year, 
                      y = y_position, 
                      label = str_wrap(category, 13),
                      colour = category),
                  hjust = 0,
                  nudge_x = 2.5) +
    
    
    # Add dotted borders for Electrification savings (top and sides only)
    geom_segment(data = savings_box_data,
                 aes(x = year - 2.25, xend = year + 2.25,
                     y = cumsum_top, yend = cumsum_top),  # top edge
                 linetype = "dashed", color = "black", size = 1) +
    geom_segment(data = savings_box_data,
                 aes(x = year - 2.25, xend = year - 2.25,
                     y = cumsum_bottom, yend = cumsum_top),  # left edge
                 linetype = "dashed", color = "black", size = 1) +
    geom_segment(data = savings_box_data,
                 aes(x = year + 2.25, xend = year + 2.25,
                     y = cumsum_bottom, yend = cumsum_top),  # right edge
                 linetype = "dashed", color = "black", size = 1) +
    
    grattan_y_continuous(labels = scales::dollar_format(), expand_top = 0.1) +
    scale_x_continuous_grattan(expand_right = 0.3,
                               breaks = seq(2025,2040, by = 5)) +
    scale_colour_manual(values = chart_colour_palette) +
    scale_fill_manual(values = chart_fill_palette) +
    theme_grattan() +
    labs(title = paste0("On average, electrification will save Australian households", " $", gap_by_2040, " a year by 2040"),
         subtitle = 'Average annual household energy costs, by fuel source',
         x = '',
         y = '',
         caption = "Notes: \nSource: Grattan Institute analaysis see app X")
  
  plot
  
  
  
}