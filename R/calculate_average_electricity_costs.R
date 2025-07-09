calculate_average_electricity_costs <-  function(annual_electricity_consumption_averages,
                                               jacobs_retail_prices,
                                               pv_system_stock){
  
  
  #apply price to consumption data
  
  electricity_costs <- annual_electricity_consumption_averages %>% 
    left_join(jacobs_retail_prices %>% 
                filter(market == "Residential")) %>% 
    mutate(annual_cost_dollars = annual_consumption_kwh * c_kwh / 100,
           category = paste0("Electricity ", str_to_lower(consumption_export))) %>% 
    select(-c(consumption_export, annual_consumption_kwh, c_kwh)) %>% 
    
    #join to PV prevalence estimates and calculate a weighted average of costs to represent average
    
    left_join(pv_system_stock %>% 
                mutate(pv = 1) %>% 
                select(-pv_stock)) %>% 
    group_by(year, state, electrification, category) %>% 
    arrange(year) %>% 
    mutate(prop = if_else(is.na(prop), 1 - lead(prop), prop)) %>% 
    summarise(average_cost_dollars = sum(annual_cost_dollars*prop))

  
  electricity_costs %>% 
    group_by(year, state, electrification) %>% 
    summarise(average_cost_dollars = sum(average_cost_dollars)) %>% 
    filter(state == "Vic") %>% 
    ggplot(aes(x = year, y = average_cost_dollars, colour = electrification)) +
    geom_line()
    
  
  return(electricity_costs)
  
}