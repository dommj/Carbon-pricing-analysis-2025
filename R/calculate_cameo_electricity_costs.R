#this script takes our total consumption and exports for each consumer type and calculates the total electricity costs / revenue

calculate_cameo_electricity_costs <-  function(annual_electricity_consumption_profiles,
                                               retail_price_data,
                                               jacobs_retail_model_file){
  
  retail_price_data <- retail_price_data %>% 
    mutate(consumption_export = "Consumption") %>% 
    cross_join(annual_electricity_consumption_profiles %>% 
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
  
  electricity_costs <- annual_electricity_consumption_profiles %>% 
    left_join(electricity_prices) %>% 
    mutate(annual_cost_dollars = annual_consumption_kwh * c_kwh / 100,
           category = paste0("Electricity ", str_to_lower(consumption_export))) %>% 
    select(-c(consumption_export, annual_consumption_kwh, c_kwh))
 
  
  electricity_costs
  
}
  
  