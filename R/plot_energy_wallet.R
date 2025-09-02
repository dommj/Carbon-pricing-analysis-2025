
plot_energy_wallet <- function(average_net_costs,
                              household_connections){
  

  chart_fill_palette <- c(
    "Petrol" = grattan_red,
    "Gas" = grattan_orange, 
    "Electricity" = grattan_yellow,
    "Electrification savings" = "transparent",
    "Total costs without electrification" = grattan_black
  )
  
  wallet_chart_data_nem <- average_net_costs %>% 
    left_join(household_connections, by = join_by(year, state)) %>% 
    group_by(year, electrification, scenario, category) %>% 
    filter(year == 2025,
           #wallet is the same across scenarios in 2025
           scenario == "Ref",
           electrification == T,
           #only include nem states
           state != "WA") %>% 
    summarise(average_cost_dollars = weighted.mean(average_cost_dollars, 
                                                   connections)) 
  
  plot <- wallet_chart_data_nem %>% 
    ggplot(aes(reorder(x = category, -average_cost_dollars), 
               y = average_cost_dollars, fill = category, colour = category)) +
    geom_col() +
    theme_grattan() +
    scale_colour_manual(values = chart_fill_palette) +
    scale_fill_manual(values = chart_fill_palette) +
    grattan_y_continuous(labels = scales::dollar_format()) +
    labs(title = "Electricity forms just part of total household energy costs",
         subtitle = 'Average annual household energy costs in NEM states, by fuel source, 2025',
         x = '',
         y = '',
         caption = "Notes: \nSource: Grattan Institute analaysis see app X")
  
  grattan_save_all("/Users/bjjefferson/Grattan Institute Dropbox/Ben Jefferson/Apps/Overleaf/energy-2025-carbon-pricing-for-electricity/atlas/average_energy_wallet.pdf",
                   object = plot)
  
}

#Calcs for report 
# 
# elec_2025_2050 <- average_net_costs %>%
#   left_join(household_connections, by = join_by(year, state)) %>%
#   group_by(year, electrification, scenario, category) %>%
#   filter(year == 2025 | year == 2050, 
#          scenario == "Ref", 
#          electrification == T, 
#          state != "WA") %>%
#   summarise(average_cost_dollars = weighted.mean(average_cost_dollars,
#                                                  connections))
# 
# pct_2025 <- 2027/sum(2027, 676, 3148)
# pct_2050 <- 2377/sum(2377, 98.5, 403)
#   
# pct_2025
# pct_2050
# 
# total_2025 <- sum(2027, 676, 3148)
# total_2050 <- sum(2377, 98.5, 403)
# 
# total_2025
# total_2050










