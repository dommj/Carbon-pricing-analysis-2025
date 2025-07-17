
get_electricity_tariffs <- function(electricity_tariffs_file, 
                                    jacobs_retail_prices,
                                    household_connections){
  
  electricity_tariffs <- read_excel(electricity_tariffs_file) %>% 
    clean_names() 
  
  jacobs_index <- jacobs_retail_prices %>% 
    filter(market == "Residential",
           year >= 2025,
           consumption_export == "Consumption") %>% 
    group_by(state) %>% 
    mutate(index = c_kwh / c_kwh[year == 2025])
  
  electricity_tariffs_scaled <- electricity_tariffs %>% 
    left_join(jacobs_index %>% 
                select(year, state, index)) %>% 
    mutate(price = price * index)
  
  
  return(electricity_tariffs_scaled)
}

#look at proportion of bills that are supply charges over time...