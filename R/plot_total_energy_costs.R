##Plotting total energy costs by state

plot_total_energy_costs <- function(average_net_costs,
                                    household_connections) {

  chart_palette_fuels <- c(
    "Petrol" = grattan_red,
    "Gas" = grattan_orange, 
    "Electricity" = grattan_yellow
  )
  
  
  chart_palette_scenarios <- c("No new policy" = grattan_red,
                               "RET < 2\u00B0C" = grattan_yellow,
                               "Safeguard < 2\u00B0C" = grattan_orange,
                               "RET 1.5\u00B0C" = grattan_yellow2,
                               "Safeguard 1.5\u00B0C" = grattan_orange2)
  
  #Constructing total household energy costs by state and scenario over time for 2C scenario
  
  total_energy_chart_data_2c <- average_net_costs %>% 
    filter(state != "WA",
           scenario == "Ref" | scenario == "2_Opt2",
           #electrification == T is the default scenario that we use for all, showing the expected electrification
           electrification == T) %>% 
    filter(year >= 2025, year <= 2050) %>%
    group_by(year, scenario, state) %>% 
    summarise(energy_cost = sum(average_cost_dollars)) %>%
    ungroup() %>%
    left_join(household_connections) %>% 
    group_by(year, scenario) %>%
    summarise(average_total_cost = weighted.mean(energy_cost, connections)) %>%
    mutate(scenario = fct_case_when(scenario == "Ref" ~ "No new policy",
                                    scenario == "1_5_Opt1" ~ "RET 1.5\u00B0C",
                                    scenario == "1_5_Opt2"  ~ "Safeguard 1.5\u00B0C",
                                    scenario == "2_Opt1"  ~ "RET < 2\u00B0C",
                                    scenario == "2_Opt2" ~ "Safeguard < 2\u00B0C")) %>%
    ungroup()
    
  total_energy_chart_2c <- ggplot(total_energy_chart_data_2c) +
    geom_line(aes(x = year, y = average_total_cost, colour = scenario),
              size = 1) +
    xlab('') +
    grattan_y_continuous(labels = scales::dollar_format(),
                         limits = c(0, 6000)) +
    scale_x_continuous(breaks = seq(2025, 2050, by = 5),
                       limits = c(2025, 2050)) +
    scale_colour_manual(values = chart_palette_scenarios) +
    labs(title = "Safeguard and Reference energy costs are comparable",
         subtitle = "Average NEM household energy costs by scenario") +
    grattan_label(data = . %>%  filter(year == 2040) %>% 
                    mutate(x = year,
                           y = case_when(scenario == "No new policy" ~ 3000,
                                         scenario == "Safeguard < 2\u00B0C" ~ 2600)), 
                  aes(x = year, y = y, label = scenario, colour = scenario),
                  hjust = 0) +
    theme_grattan()
  
  grattan_save_all("/Users/bjjefferson/Grattan Institute Dropbox/Ben Jefferson/Apps/Overleaf/energy-2025-carbon-pricing-for-electricity/atlas/No RET/Energy Bills 2C/total_energy_costs_2C.pdf",
                   object = total_energy_chart_2c)
  
  #Constructing total household energy costs by state and scenario over time for 2C scenario
  
  total_energy_chart_data_15c <- average_net_costs %>% 
    filter(state != "WA",
           scenario == "Ref" | scenario == "1_5_Opt2",
           #electrification == T is the default scenario that we use for all, showing the expected electrification
           electrification == T) %>% 
    filter(year >= 2025, year <= 2050) %>%
    group_by(year, scenario, state) %>% 
    summarise(energy_cost = sum(average_cost_dollars)) %>%
    ungroup() %>%
    left_join(household_connections) %>% 
    group_by(year, scenario) %>%
    summarise(average_total_cost = weighted.mean(energy_cost, connections)) %>%
    mutate(scenario = fct_case_when(scenario == "Ref" ~ "No new policy",
                                    scenario == "1_5_Opt1" ~ "RET 1.5\u00B0C",
                                    scenario == "1_5_Opt2"  ~ "Safeguard 1.5\u00B0C",
                                    scenario == "2_Opt1"  ~ "RET < 2\u00B0C",
                                    scenario == "2_Opt2" ~ "Safeguard < 2\u00B0C")) %>%
    ungroup()
  
  total_energy_chart_15c <- ggplot(total_energy_chart_data_15c) +
    geom_line(aes(x = year, y = average_total_cost, colour = scenario),
              size = 1) +
    xlab('') +
    grattan_y_continuous(labels = scales::dollar_format(),
                         limits = c(0, 6000)) +
    scale_x_continuous(breaks = seq(2025, 2050, by = 5),
                       limits = c(2025, 2050)) +
    scale_colour_manual(values = chart_palette_scenarios) +
    labs(title = "Safeguard and Reference energy costs are comparable",
         subtitle = "Average NEM household energy costs by scenario") +
    grattan_label(data = . %>%  filter(year == 2040) %>% 
                    mutate(x = year,
                           y = case_when(scenario == "No new policy" ~ 3000,
                                         scenario == "Safeguard 1.5\u00B0C" ~ 2600)), 
                  aes(x = year, y = y, label = scenario, colour = scenario),
                  hjust = 0) +
    theme_grattan()
  
  grattan_save_all("/Users/bjjefferson/Grattan Institute Dropbox/Ben Jefferson/Apps/Overleaf/energy-2025-carbon-pricing-for-electricity/atlas/No RET/1.5C/Energy Bills",
                   object = total_energy_chart_15c)

  View(total_energy_chart_2c$data)
  writexl::write_xlsx(total_energy_chart_2c$data, '/Users/bjjefferson/Grattan Institute Dropbox/Ben Jefferson/Carbon-pricing-analysis-2025/Interim Analyses/double_wholesale_prices_2c.xlsx')
  writexl::write_xlsx(total_energy_chart_15c$data, '/Users/bjjefferson/Grattan Institute Dropbox/Ben Jefferson/Carbon-pricing-analysis-2025/Interim Analyses/double_wholesale_prices_15c.xlsx')
  
    return(list(total_energy_chart_15c, total_energy_chart_2c))
}

