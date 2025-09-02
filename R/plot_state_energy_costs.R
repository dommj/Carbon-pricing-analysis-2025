##Plotting total energy costs by state

plot_state_energy_costs <- function(average_net_costs,
                                    household_connections) {

  chart_palette_fuels <- c(
    "Petrol" = grattan_red,
    "Gas" = grattan_orange, 
    "Electricity" = grattan_yellow
  )
  
  
  chart_palette_scenarios <- c("No new policy" = grattan_red,
                               "RET < 2 C" = grattan_yellow,
                               "Safeguard < 2 C" = grattan_orange,
                               "RET < 1.5 C" = grattan_yellow,
                               "Safeguard < 1.5 C" = grattan_orange)
  
  #Constructing total household energy costs by state and scenario over time, 2 degrees
  
  state_energy_costs_plot_2c <- average_net_costs %>% 
    filter(scenario == "Ref" | scenario == "2_Opt2" | scenario == "2_Opt1",
           #electrification == T is the default scenario that we use for all, showing the expected electrification
           electrification == T) %>% 
    filter(year >= 2025, year <= 2050) %>%
    group_by(year, scenario, state) %>% 
    summarise(energy_cost = sum(average_cost_dollars)) %>%
    mutate(scenario = fct_case_when(scenario == "Ref" ~ "No new policy",
                                    scenario == "1_5_Opt1" ~ "RET 1.5 C",
                                    scenario == "1_5_Opt2"  ~ "Safeguard 1.5 C",
                                    scenario == "2_Opt1"  ~ "RET < 2 C",
                                    scenario == "2_Opt2" ~ "Safeguard < 2 C")) %>%
    ungroup() %>%
    
  ggplot() +
    geom_line(aes(x = year, y = energy_cost, colour = scenario),
              size = 1) +
    facet_wrap(~state, ncol = 3) +
    xlab('') +
    grattan_y_continuous(labels = scales::dollar_format(),
                         limits = c(0, 7000)) +
    scale_x_continuous(breaks = seq(2025, 2050, by = 5),
                       limits = c(2025, 2050)) +
    scale_colour_manual(values = chart_palette_scenarios) +
    labs(title = "... but total househld energy bills are set to decline",
         subtitle = "Average household energy costs by scenario") +
    grattan_label(data = . %>%  filter(year == 2040, state == "NSW and ACT") %>% 
                    mutate(x = year,
                           y = case_when(scenario == "No new policy" ~ 3000,
                                         scenario == "RET < 2 C" ~ 4300,
                                         scenario == "Safeguard < 2 C" ~ 2600)), 
                  aes(x = year, y = y, label = scenario, colour = scenario),
                  hjust = 0) +
    theme_grattan()
  
  grattan_save_all("/Users/bjjefferson/Grattan Institute Dropbox/Ben Jefferson/Apps/Overleaf/energy-2025-carbon-pricing-for-electricity/atlas/Energy Bills/2_degrees_state_energy_costs.pdf",
                   object = state_energy_costs_plot_2c)
  
  #Constructing total household energy costs by state and scenario over time, 1.5 Degrees
  
  state_energy_costs_plot_15c <- average_net_costs %>% 
    filter(scenario == "Ref" | scenario == "1_5_Opt2" | scenario == "1_5_Opt1",
           #electrification == T is the default scenario that we use for all, showing the expected electrification
           electrification == T) %>% 
    filter(year >= 2025, year <= 2050) %>%
    group_by(year, scenario, state) %>% 
    summarise(energy_cost = sum(average_cost_dollars)) %>%
    mutate(scenario = fct_case_when(scenario == "Ref" ~ "No new policy",
                                    scenario == "1_5_Opt1" ~ "RET 1.5 C",
                                    scenario == "1_5_Opt2"  ~ "Safeguard 1.5 C",
                                    scenario == "2_Opt1"  ~ "RET < 2 C",
                                    scenario == "2_Opt2" ~ "Safeguard < 2 C")) %>%
    ungroup() %>%
    
    ggplot() +
    geom_line(aes(x = year, y = energy_cost, colour = scenario),
              size = 1) +
    facet_wrap(~state, ncol = 3) +
    xlab('') +
    grattan_y_continuous(labels = scales::dollar_format(),
                         limits = c(0, 7000)) +
    scale_x_continuous(breaks = seq(2025, 2050, by = 5),
                       limits = c(2025, 2050)) +
    scale_colour_manual(values = chart_palette_scenarios) +
    labs(title = "... but total househld energy bills are set to decline",
         subtitle = "Average household energy costs by scenario") +
    grattan_label(data = . %>%  filter(year == 2040, state == "NSW and ACT") %>% 
                    mutate(x = year,
                           y = case_when(scenario == "No new policy" ~ 3000,
                                         scenario == "RET < 1.5 C" ~ 4300,
                                         scenario == "Safeguard < 1.5 C" ~ 2600)), 
                  aes(x = year, y = y, label = scenario, colour = scenario),
                  hjust = 0) +
    theme_grattan()
  
  grattan_save_all("/Users/bjjefferson/Grattan Institute Dropbox/Ben Jefferson/Apps/Overleaf/energy-2025-carbon-pricing-for-electricity/atlas/1.5C/Energy Bills/1_5_degrees_state_energy_costs.pdf",
                   object = state_energy_costs_plot_15c)

  return(list(state_energy_costs_plot_2c, state_energy_costs_plot_15c))
}
