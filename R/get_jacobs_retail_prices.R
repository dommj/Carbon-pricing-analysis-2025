
#jacobs_retail_model_file_base <- "Data/Jacobs/RetailPriceProjections_Ref.xlsx"
jacobs_retail_model_file_base <- "Data/Jacobs/RetailPriceProjections_1_5_Opt1.xlsx"
#jacobs_retail_model_file_base <- "Data/Jacobs/RetailPriceProjections_1_5_Opt2.xlsx"

get_jacobs_retail_prices <- function(jacobs_retail_model_file_base,
                                     household_connections){
  
  scenario_name <- str_match(jacobs_retail_model_file_base, "ions_(.*)\\.xlsx")[,2]
  
  #when final results come, check what year the dollars are in
  retail_base <- read_excel(jacobs_retail_model_file_base,
                            sheet = "RetailPriceOutputs",
                            range = "A2:AJ34") %>% 
    pivot_longer(cols = matches('\\d\\d\\d\\d'), names_to = 'year', values_to = 'dollars_mwh') %>% 
    clean_names() %>% 
    select(- units) %>% 
    mutate(c_kwh = as.numeric(dollars_mwh) / 1000 * 100,
           scenario = scenario_name,
           consumption_export = "Consumption",
           year = as.numeric(year),
           state = convert_states(state))
  
  if (scenario_name == "Ref") {
  #read in feed in tariffs from Jacobs retail model.
  feed_in_tariffs <- read_excel(jacobs_retail_model_file_base,
                                sheet = "OtherCharges",
                                range = "A28:Ak36") %>% 
    rename(state = 1) %>% 
    pivot_longer(cols = -1, names_to = 'year', values_to = 'c_kwh' ) %>% 
    mutate(state = convert_states(state),
           consumption_export = "Exports",
           year = as.numeric(year)) %>% 
    cross_join(retail_base %>% 
                 select(scenario, market) %>% unique())
  }
  
  else{
    feed_in_tariffs <- read_excel(jacobs_retail_model_file_base,
                                  sheet = "OtherCharges",
                                  range = "B28:AL36") %>% 
      rename(state = 1) %>% 
      pivot_longer(cols = -1, names_to = 'year', values_to = 'c_kwh' ) %>% 
      mutate(state = convert_states(state),
             consumption_export = "Exports",
             year = as.numeric(year)) %>% 
      cross_join(retail_base %>% 
                   select(scenario, market) %>% unique())
  }
  
  retail_tariffs <- bind_rows(retail_base, feed_in_tariffs) %>% 
    select(-dollars_mwh)
    
  return(retail_tariffs)
}