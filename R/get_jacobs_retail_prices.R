
get_jacobs_retail_prices <- function(jacobs_retail_model_file_base,
                                     household_connections){
  
  #when final results come, check what year the dollars are in
  retail_base <- read_excel(jacobs_retail_model_file_base,
                            sheet = "RetailPriceOutputs",
                            range = "A2:BN26") %>% 
    pivot_longer(cols = matches('\\d\\d\\d\\d'), names_to = 'year', values_to = 'dollars_mwh') %>% 
    clean_names() %>% 
    select(- units) %>% 
    mutate(c_kwh = dollars_mwh / 1000 * 100,
           scenario = "base",
           consumption_export = "Consumption",
           year = as.numeric(year))
    
#aggregate and rename nsw 
  consumption_tariffs <- bind_rows(retail_base) %>% 
    mutate(state = convert_states(state),
           state = if_else(state == "NSW", 
                           "NSW and ACT",
                           state))
  
  
  
  #read in feed in tariffs from Jacobs retail model.
  feed_in_tariffs <- read_excel(jacobs_retail_model_file_base,
                                sheet = "OtherCharges",
                                range = "B36:AL41") %>% 
    rename(state = 1) %>% 
    pivot_longer(cols = -1, names_to = 'year', values_to = 'c_kwh' ) %>% 
    mutate(state = convert_states(state),
           state = if_else(state == "NSW", 
                           "NSW and ACT",
                           state),
           consumption_export = "Exports",
           year = as.numeric(year)) %>% 
    cross_join(consumption_tariffs %>% 
                 select(scenario, market) %>% unique())
  
  retail_tariffs <- bind_rows(consumption_tariffs, feed_in_tariffs)
    
  return(retail_tariffs)
}