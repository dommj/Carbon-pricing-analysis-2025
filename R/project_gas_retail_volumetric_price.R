#calculate average volumetric prices for each state and index according ACIL volumetric gas prices forecast

project_gas_retail_volumetric_price <- function(best_offer_bills,
                                                benchmark_gas_consumption,
                                                gas_volume_price_data){
  
  best_offer_bills <- best_offer_bills %>%
    group_by(state) %>%
    filter(total_cost == min(total_cost)) %>% 
    select(state, usage_cost)
  
  average_volumetric_price_24 <- best_offer_bills %>% 
    left_join(benchmark_gas_consumption) %>% 
    mutate(average_dollars_mj_24 = usage_cost / benchmark_use_mj) %>% 
    select(state, average_dollars_mj_24) 
  
  
  gas_prices_index <- gas_volume_price_data %>% 
    group_by(state) %>% 
    arrange(year) %>% 
    mutate(index = dollars_per_gj / dollars_per_gj[year == 2024]) %>%
    full_join(average_volumetric_price_24) %>% 
    mutate(dollars_per_gj = average_dollars_mj_24 * index * 1000) %>% 
    select(year, state, dollars_per_gj) %>% 
    filter(!is.na(dollars_per_gj))
    
  return(gas_prices_index)
  
  }