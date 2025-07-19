# get all gas volumetric price forecast data (NEM and WA)

get_gas_prices_data <- function(gas_prices_file,
                                wa_gas_prices_file){
  
  #Forecasts are presented in calendar years, and are in real 2023 Australian dollars. Converted to Q1 2025 dollars  
 
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
           #convert to 2025 dollars
           across(dollars_per_gj, 
                  ~convert_to_2024_dollars(., 2023,  unit_type = "calendar year")))
  
  wa_data <- read_excel(wa_gas_prices_file,
                        sheet = "Industrial",
                        skip = 2) %>% 
    rename(year = 1, dollars_per_gj = 2) %>% 
    select(- c(Low, High)) %>% 
    mutate(state = "WA",
           #convert to 2025 dollars
           across(dollars_per_gj, 
                  ~convert_to_2024_dollars(., 2024,  unit_type = "calendar year")))
  
  
  data <- bind_rows(nem_data, wa_data)
  
  return(data)
}




