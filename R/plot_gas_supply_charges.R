
plot_gas_supply_charges <- function(gas_connection_charge_projections, 
                                    residential_gas_consumption_projections){
  
  modelled_average_nem_supply_charge <- gas_connection_charge_projections %>% 
    left_join(residential_gas_consumption_projections) %>% 
    filter(state %in% c("NSW and ACT", "Vic", "Qld", "Tas", "SA")) %>% 
    group_by(year) %>% 
    summarise(annual_connection_charge = weighted.mean(annual_connection_charge, residential_gas_connections)) %>% 
    mutate(category = "Our estimates") %>% 
    filter(year <= 2040,
           year >= 2025) 
  
  
  plot <- modelled_average_nem_supply_charge %>% 
    ggplot(aes(x = year, y = annual_connection_charge, fill = category)) +
    geom_col(position = "dodge") +
    grattan_y_continuous(labels = scales::dollar_format()) +
    scale_x_continuous(breaks = c(2023, 2025, 2030, 2035, 2040)) +
    theme_grattan(legend = "below") +
    labs(title = "Residential gas network charges are set to rise rapidly",
         subtitle = "Projected annual average NEM residential network charges ($2025)",
         x = "",
         y = "")
 
  grattan_save_all("C:/Users/domijones/Grattan Institute Dropbox/Dominic  Jones/Apps/Overleaf/energy-2025-carbon-pricing-for-electricity/atlas/projected_nem_gas_supply_charges.pdf",
                   object = plot)
  
  return(plot)
}


#archive
function(){
  
  modelled_average_nem_supply_charge <- gas_connection_charge_projections %>% 
    left_join(residential_gas_consumption_projections) %>% 
    filter(state %in% c("NSW and ACT", "Vic", "Qld", "Tas", "SA"),
           year %in% c(2025, 2030, 2035, 2040, 2045)) %>% 
    group_by(year) %>% 
    summarise(annual_connection_charge = weighted.mean(annual_connection_charge, residential_gas_connections)) %>% 
    mutate(category = "Our estimates")
  
  
  #source: page 22 https://energyconsumersaustralia.com.au/wp-content/uploads/CSIRO-Technical-Report-Stepping-Up.pdf
  csiro_average_nem_supply_charge <- tibble(year = c(2023, 2030, 2035, 2040, 2050), 
                                            annual_connection_charge = c(280, 420, 560, 690, 1170)) %>% 
    mutate(category = "CSIRO and Dynamic Analysis estimates",
           annual_connection_charge = convert_to_2024_dollars(annual_connection_charge,
                                                              2023))
  
  
  combined_estimates <- bind_rows(modelled_average_nem_supply_charge,
                                  csiro_average_nem_supply_charge) 
  
  
  combined_estimates %>% 
    filter(year <= 2040) %>% 
    ggplot(aes(x = year, y = annual_connection_charge, fill = category)) +
    geom_col(position = "dodge") +
    grattan_y_continuous(labels = scales::dollar_format()) +
    scale_x_continuous(breaks = c(2023, 2025, 2030, 2035, 2040)) +
    theme_grattan(legend = "below") +
    labs(title = "Our gas supply charge estimates are comparable to other sources",
         subtitle = "Projected annual average NEM gas connection charges ($2025)",
         x = "",
         y = "")
  
  
  
  nsw_estimates_eca <- tibble(year = c(2026, 2030, 2036, 2039, 2041), 
                              annual_connection_charge = c(288, 322, 474, 567, 640)) %>% 
    mutate(category = "ECA estimates",
           annual_connection_charge = convert_to_2024_dollars(annual_connection_charge,
                                                              2024))
  
  
  combined_estimates <- bind_rows(modelled_average_nem_supply_charge,
                                  csiro_average_nem_supply_charge) 
  
  
  gas_connection_charge_projections %>% 
    filter(year <= 2040,
           year >= 2025,
           state == "NSW and ACT") %>% 
    ggplot(aes(x = year, y = annual_connection_charge)) +
    geom_point() +
    geom_point(data = nsw_estimates_eca)+
    grattan_y_continuous(labels = scales::dollar_format()) +
    scale_x_continuous(breaks = c(2023, 2025, 2030, 2035, 2040)) +
    theme_grattan(legend = "below") +
    labs(title = "Our gas supply charge estimates are comparable to other sources",
         subtitle = "Projected annual average NEM gas connection charges ($2025)",
         x = "",
         y = "")
}