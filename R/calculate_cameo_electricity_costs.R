#this script takes our total consumption and exports for each consumer type and calculates the total electricity costs / revenue

calculate_cameo_electricity_costs <-  function(annual_electricity_consumption_profiles,
                                               jacobs_retail_prices,
                                               retail_electricity_tariffs,
                                               rbs_households){
  

  
  
  consumption_tariffs <- retail_electricity_tariffs %>% 
    select(state, year, scenario, price_type, price) %>% 
    pivot_wider(names_from = price_type, values_from = price) %>% 
    #select(-`supply charge`) %>% 
    mutate(consumption_export = "Consumption") %>% 
    bind_rows(
      jacobs_retail_prices %>% 
        filter(consumption_export == "Exports",
               market == "Residential") %>% 
        select(state, year, scenario, c_kwh, consumption_export) %>% 
        filter(year >= 2025)
    ) %>% 
    mutate(`supply charge` = if_else(is.na(`supply charge`), 0, `supply charge`))
  
  #weight nsw and act tariffs by household numbers in 2020 to get aggregate tariff (this aligns with our connection based weighting of consumption)
  
  consumption_tariffs_nsw_act <- consumption_tariffs %>% 
    filter(state %in% c("NSW", "ACT")) %>% 
    left_join(rbs_households %>% 
                filter(year == 2020) %>% 
                select(- year)) %>% 
    group_by(year, scenario, consumption_export) %>% 
    summarise(`supply charge` = weighted.mean(`supply charge`, occupied_households),
              c_kwh = weighted.mean(c_kwh, occupied_households)) %>% 
    ungroup() %>% 
    mutate(state = "NSW and ACT")
  
  #add back in
  
  consumption_tariffs_agg <- consumption_tariffs %>%
    bind_rows(consumption_tariffs_nsw_act %>% 
                filter(state %nin% c("NSW", "ACT")))
  
  #apply price to consumption data
  
  electricity_costs <- annual_electricity_consumption_profiles %>% 
    left_join(consumption_tariffs_agg,
              relationship = "many-to-many") %>% 
    mutate(annual_cost_dollars = annual_consumption_kwh * c_kwh / 100 + `supply charge` *365 / 100,
           category = paste0("Electricity ", str_to_lower(consumption_export))) %>% 
    select(-c(consumption_export, `supply charge`, annual_consumption_kwh, c_kwh)) %>% 
    filter(year >= 2025,
           year<= 2050)
 
  
  electricity_costs
  
}


#archived code

function(){
  
  
  
  
  
  
}

  