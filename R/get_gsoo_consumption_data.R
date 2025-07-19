#load GSOO gas consumption data for residential and small commercial customers

get_gsoo_consumption_data <- function(gsoo_consumption_data_file,
                                      wa_gsoo_consumption_data_file){
  
  nem_gsoo <- read_excel(gsoo_consumption_data_file) %>% 
    clean_names() %>% 
    mutate(state = convert_states(region),
           state = if_else(state == 'NSW', 'NSW and ACT', state),
           annual_consumption_gj = annual_consumption_pj * 1e6) %>% 
    filter(publication == 'GSOO 2024',
           version == '2024-03-21',
           scenario %in% c('Actual', 'Step Change'),
           subcategory == "Residential and Commercial",
           !is.na(state)) %>% 
    select(year, state, annual_consumption_gj)
  
  
  wa_gsoo <- read_excel(wa_gsoo_consumption_data_file) %>% 
    clean_names() %>% 
    filter(region == "METRO/SOUTH-WEST", #our model only looks at SWIS area consumption
           category == "Tariff V", #tariff V is for small customers (residential and small business)
           scenario %in% c("Actual", "Step Change")) %>% 
    mutate(state = "WA") %>% 
    group_by(year, state) %>% 
    summarise(average_daily_consumption_tj_day = sum(average_daily_consumption_tj_day)) %>% 
    ungroup() %>% 
    mutate(annual_consumption_gj = average_daily_consumption_tj_day * 365 *1e3) %>% 
    select(- average_daily_consumption_tj_day )
    
  
  gsoo_data <- bind_rows(nem_gsoo, wa_gsoo)
  
  return(gsoo_data)
}

