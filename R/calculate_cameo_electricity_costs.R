#this script takes our total consumption and exports for each consumer type and calculates the total electricity costs / revenue

calculate_cameo_electricity_costs <-  function(annual_electricity_consumption_profiles,
                                               jacobs_retail_prices,
                                               retail_electricity_tariffs){
  

  
  
  consumption_tariffs <- retail_electricity_tariffs %>% 
    select(state, year, price_type, price) %>% 
    pivot_wider(names_from = price_type, values_from = price) %>% 
    #select(-`supply charge`) %>% 
    mutate(consumption_export = "Consumption") %>% 
    bind_rows(
      jacobs_retail_prices %>% 
        filter(consumption_export == "Exports",
               market == "Residential") %>% 
        select(state, year, c_kwh, consumption_export) %>% 
        filter(year >= 2025)
    ) %>% 
    mutate(`supply charge` = if_else(is.na(`supply charge`), 0, `supply charge`))
  
  #apply price to consumption data
  
  electricity_costs <- annual_electricity_consumption_profiles %>% 
    left_join(consumption_tariffs) %>% 
    mutate(annual_cost_dollars = annual_consumption_kwh * c_kwh / 100 + `supply charge` *365 / 100,
           category = paste0("Electricity ", str_to_lower(consumption_export))) %>% 
    select(-c(consumption_export, `supply charge`, annual_consumption_kwh, c_kwh)) %>% 
    filter(year >= 2025,
           year<= 2050)
 
  
  electricity_costs
  
}
  
  