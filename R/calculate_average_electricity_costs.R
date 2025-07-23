calculate_average_electricity_costs <-  function(annual_electricity_consumption_averages,
                                               jacobs_retail_prices,
                                               retail_electricity_tariffs,
                                               rbs_households,
                                               pv_system_stock,
                                               battery_n_pv_prop){
  
  
  consumption_tariffs <- retail_electricity_tariffs %>% 
    select(state, year, price_type, scenario, price) %>% 
    pivot_wider(names_from = price_type, values_from = price) %>% 
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
    bind_rows(consumption_tariffs_nsw_act) %>% 
                filter(state %nin% c("NSW", "ACT"))
  
  
  consumer_type_props <- pv_system_stock %>% 
    filter(year >= 2025) %>% 
    left_join(battery_n_pv_prop) %>% 
    mutate(pv_only_prop = prop - battery_and_pv_prop,
           no_pv_prop = 1 - prop) %>% 
    select(-c(prop, pv_stock)) %>% 
    pivot_longer(cols = contains('prop'), 
                 names_to = 'consumer_type',
                 values_to = 'prop') %>% 
    mutate(consumer_type = case_when(consumer_type == "battery_and_pv_prop" ~ "1_1",
                                     consumer_type == "pv_only_prop" ~ "1_0",
                                     consumer_type == "no_pv_prop" ~ "0_0"))
  
  #apply price to consumption data
  
  electricity_costs <- annual_electricity_consumption_averages %>% 
    left_join(consumption_tariffs_agg) %>% 
    mutate(average_cost_dollars = annual_consumption_kwh * c_kwh / 100 + `supply charge` *365 / 100,
           category = paste0("Electricity ", str_to_lower(consumption_export))) %>% 
    
    #join to PV and battery prevalence estimates !!!calculate a weighted average of costs to represent average consumer later !!!
    mutate(consumer_type = paste0(pv, "_", battery)) %>% 
    left_join(consumer_type_props)
  
  
  #############################
  #check
  #############################
  
  electricity_costs %>% 
    group_by(year, state, scenario, consumer_type, electrification) %>% 
    summarise(average_cost_dollars = sum(average_cost_dollars)) %>% 
    filter(state == "Vic",
           scenario == "Ref") %>% 
    ggplot(aes(x = year, y = average_cost_dollars, colour = electrification)) +
    geom_line() +
    facet_wrap(~consumer_type)
    
  
  #supply costs as proportion of bill 
  electricity_costs %>% 
    filter(consumer_type == "0_0",
           scenario == "Ref") %>% 
    mutate(supply_pct_bill = (`supply charge` * 365/100)/average_cost_dollars) %>% 
    ggplot(aes(x = year, y = supply_pct_bill, colour = electrification)) +
    geom_line() +
    facet_wrap(~state)
  
  
  return(electricity_costs)
  
}