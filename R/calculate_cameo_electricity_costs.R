#this script takes our total consumption and exports for each consumer type and calculates the total electricity costs / revenue

calculate_cameo_electricity_costs <-  function(annual_electricity_consumption_profiles,
                                               jacobs_retail_prices){
  

  
  #apply price to consumption data
  
  electricity_costs <- annual_electricity_consumption_profiles %>% 
    left_join(jacobs_retail_prices %>% 
                filter(market == "Residential")) %>% 
    mutate(annual_cost_dollars = annual_consumption_kwh * c_kwh / 100,
           category = paste0("Electricity ", str_to_lower(consumption_export))) %>% 
    select(-c(consumption_export, annual_consumption_kwh, c_kwh))
 
  
  electricity_costs
  
}
  
  