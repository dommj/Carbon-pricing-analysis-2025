# get all gas data

get_gas_prices_data <- function(gas_prices_file,
                                wa_gas_prices_file){
  
  #Forecasts are presented in calendar years, and are in real 2023 Australian dollars.  
 
  nem_data <- read_excel(gas_prices_file, 
             sheet = "Step Change - ResCom",
             skip = 3) %>% 
    clean_names() %>% 
    rename(year = 1) %>% 
    pivot_longer(cols = -year,
                 names_to = "state",
                 values_to = "dollars_per_gj") %>%
    mutate(state = case_when(state == 'brisbane' ~ 'Qld',
                             state == 'melbourne' ~ 'Vic',
                             state == 'sydney' ~ 'NSW and ACT',
                             state == 'adelaide' ~ 'SA',
                             state == 'perth' ~ 'WA',
                             state == 'hobart' ~ 'Tas',
                             state == 'darwin' ~ 'NT'),
           #need to convert to 2024 dollars
           across(dollars_per_gj, 
                  ~convert_to_2024_dollars(., 2023,  unit_type = "calendar year")))
  
  wa_data <- read_excel(wa_gas_prices_file,
                        sheet = "Industrial",
                        skip = 2) %>% 
    rename(year = 1, dollars_per_gj = 2) %>% 
    select(- c(Low, High)) %>% 
    mutate(state = "WA")
  
  
  data <- bind_rows(nem_data, wa_data)
  
  return(data)
}

#get_gas_prices_data('Data/Gas/ACIL Allen Natural Gas Price Forecast.xlsx')


#sandpit

function(){
  
  
gas_consumption <- read_excel('Data/Gas/Gas GSOO 2024.xlsx') %>% 
  clean_names() %>% 
  filter(publication == 'GSOO 2024',
         version == '2024-03-21',
         scenario %in% c('Actual', 'Step Change'),
         subcategory == "Residential and Commercial")


# read in the gas connections data
gas_connections <- read_excel(gas_connections_file,
                              sheet = "Figure 13",
                              range = "B24:E51") %>% 
  clean_names() %>% 
  rename(year = 1, gas_connections = step_change) %>% 
  mutate(gas_connections = if_else(is.na(gas_connections), actual, gas_connections)) %>% 
  select(year, gas_connections)

}
