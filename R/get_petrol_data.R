#load petrol price data

#prices are sourced from ACCC June quarter report:
#https://www.accc.gov.au/system/files/petrol-quarterly-report-june24.pdf

#prices in June quarter 2024 dollars.

get_petrol_data <- function(petrol_file) {
  
  #determine inflator for 2024 dollars
  cpi_24 <- read_cpi() %>% 
    filter(year(date) == 2024) %>% 
    summarise(cpi=mean(cpi)) %>% 
    pull(cpi)
  
  cpi_june_24 <- read_cpi() %>% 
    filter(date == '2024-06-01') %>% 
    pull(cpi)
  
  inflator <- cpi_24/cpi_june_24
  
  #load data and inflate to get 2024 dollars
  data <- read_csv(petrol_file) %>% 
    clean_names() %>% 
    mutate(category = "Petrol",
           c_litre = c_litre*inflator) %>% 
    select(year, c_litre, category)
  
  
  data
  
}

#get_petrol_data(petrol_file)

#data <- get_petrol_data('data/accc_retail_fuel_04_24.csv')

# cpi <- read_cpi() %>% 
#   filter(year(date)>2000) %>% 
#   mutate(fy = date2fy(date)) %>% 
#   group_by(fy) %>% 
#   summarise(cpi=mean(cpi)) %>% 
#   ungroup() %>% 
#   mutate(cpi_25=cpi[25]) %>% 
#   mutate(cpi_use=cpi/cpi_25) %>% 
#   select(fy, cpi_use)
