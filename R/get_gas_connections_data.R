#this file gets gas connections data from the AER
get_gas_connections_data <- function(connection_data_aer_file, connection_data_vic_file,
                                     connection_data_wa_file){
  
  
  #residential and small business connections data, nsw, act, qld, SA
  res_connect <- read_excel(connection_data_aer_file,
                            sheet = 'Res Gas Cust#s & Mkt Contr',
                            range = 'A5:E85') %>% 
    clean_names() %>% 
    filter(str_detect(retailer, 'Total'),
           !str_detect(retailer, 'National')) %>% 
    rename(state = retailer) %>% 
    mutate(state = str_remove(state, " Total"),
           state = map_chr(state, convert_states)) %>%
    rowwise() %>%
    mutate(gas_customers = mean(c_across(-state)),
           year = 2023,
           customer_type = 'Residential') %>%
    ungroup() %>% 
    select(year, state, customer_type, gas_customers)
  
  sml_com_connect <- read_excel(connection_data_aer_file,
                                sheet = 'SmlBiz Gas Cust#s & Mkt Contr',
                                range = 'A5:E76') %>% 
    clean_names() %>% 
    filter(str_detect(retailer, 'Total'),
           !str_detect(retailer, 'National')) %>% 
    rename(state = retailer) %>% 
    mutate(state = str_remove(state, " Total"),
           state = map_chr(state, convert_states)) %>%
    rowwise() %>%
    mutate(gas_customers = mean(c_across(-state)),
           year = 2023,
           customer_type = 'Small business') %>%
    ungroup() %>% 
    select(year, state, customer_type, gas_customers)
  
  
  
  customers <- bind_rows(res_connect, sml_com_connect) %>% 
    group_by(year,state, customer_type) %>%
    summarise(gas_customers = sum(gas_customers)) 
  
  #residential and small business connections data for vic
  vic_customers <- read_excel(connection_data_vic_file,
                              skip = 1) %>% 
    clean_names() %>% 
    mutate(state = "Vic",
           year = 2023) %>% 
    filter(fuel == 'Gas',
           customer_type %in% c('Residential', 'Small business'),
           (fin_year == '2022-23' & fin_qtr %in% c('Q3', 'Q4')) |
             (fin_year == '2023-24' & fin_qtr %in% c('Q1', 'Q2')),
           meter_or_consumer == "Gas Customers") %>% 
    group_by(year, state, customer_type, retailer_common_name) %>% 
    summarise(gas_customers = mean(value)) %>% 
    group_by(year, state, customer_type) %>% 
    summarise(gas_customers = sum(gas_customers)) 
  
  
  #TAS data: https://www.economicregulator.tas.gov.au/Documents/25%20437%20Energy%20in%20Tasmania%202023-24.pdf (only two retailers (Aurora and Solstice)
  
  #table 10.6: Residential gas customers from Aurora energy
  aurora_22_23 <- 4681
  aurora_23_24 <- 4546
  
  
  #table 10.6: Residential gas customers from Solstice energy
  solstice_22_23 <- 9093
  solstice_23_24 <- 8910
  

  tas_22_23 <- aurora_22_23 + solstice_22_23
  tas_23_24 <- aurora_23_24 + solstice_23_24
  
  #approximate 23 as mean of 22-23 and 23-24 connections
  tas_23 <- tibble(year = 2023, state = "Tas",
                   residential = (tas_22_23 + tas_23_24)/2)
  
  
  #WA data: ERA performance reporting https://www.erawa.com.au/cproot/24632/4/energy-reports-retailer-data-2014-onwards.XLSX
  
  wa_data <- read_excel(connection_data_wa_file) %>% 
    clean_names() %>% 
    filter(energy_type == "Gas",
           category == "Customer numbers",
           subcategory == "Residential customers",
           indicator_description == "Total",
           date == "30/6/2023") %>% 
    group_by(date) %>% 
    summarise(residential = sum(number_value, na.rm = T)) %>% 
    mutate(state = "WA",
           year = 2023) %>% 
    select(-date)
  
  
  #add victorian, WA and TAS data
  customers <- bind_rows(customers, vic_customers) %>% 
    pivot_wider(names_from = customer_type, values_from = gas_customers) %>%
    mutate(total_customers = (Residential + `Small business`),
           pct_residential = Residential / total_customers) %>% 
    clean_names() %>% 
    ungroup() %>% 
    bind_rows(tas_23, wa_data)

  
  return(customers)

}