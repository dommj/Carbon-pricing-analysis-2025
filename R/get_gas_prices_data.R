# get all gas data

get_gas_prices_data <- function(gas_prices_file){
  
  read_excel(gas_prices_file, 
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
                             state == 'darwin' ~ 'NT'))
  
}

get_gas_prices_data('Data/Gas/ACIL Allen Natural Gas Price Forecast.xlsx')


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
