calculate_average_electricity_costs <-  function(annual_electricity_consumption_averages,
                                               retail_price_data,
                                               jacobs_retail_model_file,
                                               pv_system_stock){
  
  retail_price_data <- retail_price_data %>% 
    mutate(consumption_export = "Consumption") %>% 
    cross_join(annual_electricity_consumption_averages %>% 
                 select(state) %>% 
                 unique())
  
  #read in feed in tariffs from Jacobs retail model.
  feed_in_tariffs <- read_excel(jacobs_retail_model_file,
                                sheet = "OtherCharges",
                                range = "B36:AL41") %>% 
    rename(state = 1) %>% 
    pivot_longer(cols = -1, names_to = 'year', values_to = 'c_kwh' ) %>% 
    mutate(state = convert_states(state),
           state = if_else(state == "NSW", 
                           "NSW and ACT",
                           state),
           consumption_export = "Exports",
           year = as.numeric(year))
  
  electricity_prices <- bind_rows(retail_price_data, feed_in_tariffs)
  
  #apply price to consumption data
  
  electricity_costs <- annual_electricity_consumption_averages %>% 
    left_join(electricity_prices) %>% 
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