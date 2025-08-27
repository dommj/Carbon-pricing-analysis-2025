# This function calculates gas use, per residential / small business customer, in 2023, for each state. These values should only be used to estimate the total number of customers over which network costs are distributed, not the average gas consumption of residential households at a point in time

calc_gas_consumption_per_connection <- function(connection_data_aer_file, connection_data_vic_file, gsoo_consumption_data){
  
  
  #residential and small business connections data, nsw, act, qld, tas, SA
  res_connect <- read_excel(connection_data_aer_file,
                            sheet = 'Res Gas Cust#s & Mkt Contr',
                            range = 'A5:E85') %>% 
    clean_names() %>% 
    filter(str_detect(retailer, 'Total'),
           !str_detect(retailer, 'National')) %>% 
    rename(state = retailer) %>% 
    mutate(state = str_remove(state, " Total"),
           state = map_chr(state, convert_states),
           state = if_else(state == 'NSW' | state == 'ACT', 'NSW and ACT', state)) %>%
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
           state = map_chr(state, convert_states),
           state = if_else(state == 'NSW' | state == 'ACT', 'NSW and ACT', state)) %>%
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
  
  #add victorian data
  customers <- bind_rows(customers, vic_customers) %>% 
    pivot_wider(names_from = customer_type, values_from = gas_customers) %>%
    mutate(total_customers = (Residential + `Small business`),
           pct_residential = Residential / total_customers)
  
    
  #gas consumption data from GSOO for residential and small business  
  gas_consumption_23 <- gsoo_consumption_data %>%
           filter(year == 2023) 
  
  #use 2023 data to set consumption per customer. This does not change over time.
  consumption_per_customer <- left_join(customers, gas_consumption_23, by = c('year', 'state')) %>% 
    mutate(gas_consumption_per_customer_gj = annual_consumption_gj / total_customers) %>% 
    clean_names() %>% 
    select(state, residential, small_business, total_customers, gas_consumption_per_customer_gj)
  
  consumption_per_customer
}


# consumption_per_customer_gj <- calc_gas_consumption_per_connection('Data/Gas/Schedule 2 - Quarter 3 2023-24 Retail Performance Data.xlsm',
#                                                                     "Data/Gas/FY24-Annual-Overview-data.xlsx",
#                                                                     'Data/Gas/Gas GSOO 2024.xlsx')
# 
# # res_connect <- read_excel('Data/Gas/Schedule 2 - Quarter 3 2023-24 Retail Performance Data.xlsm',
# #                           sheet = 'Res Gas Cust#s & Mkt Contr',
# #                           range = 'A5:E85') %>% 
# #   clean_names() %>% 
# #   filter(str_detect(retailer, 'Total'),
# #          !str_detect(retailer, 'National')) %>% 
# #   rename(state = retailer) %>% 
# #   mutate(state = str_remove(state, " Total"),
# #          state = map_chr(state, convert_states)) %>%
# #   rowwise() %>%
# #   mutate(gas_connections = mean(c_across(-state)),
# #          year = 2023) %>%
# #   ungroup() %>% 
# #   select(year, state, gas_connections)
# 
# vic_customers <- read_excel("Data/Gas/FY24-Annual-Overview-data.xlsx",
#                               skip = 1) %>% 
#   clean_names() %>% 
#   mutate(state = "Vic",
#          year = 2023) %>% 
#   filter(fuel == 'Gas',
#          customer_type %in% c('Residential', 'Small business'),
#          (fin_year == '2022-23' & fin_qtr %in% c('Q3', 'Q4')) |
#             (fin_year == '2023-24' & fin_qtr %in% c('Q1', 'Q2')),
#          meter_or_consumer == "Gas Customers") %>% 
#   group_by(year, state, customer_type, retailer_common_name) %>% 
#   summarise(gas_customers = mean(value)) %>% 
#   group_by(year, state) %>% 
#   summarise(gas_customers = sum(gas_customers)) 
# 
# 
# gas_consumption <- read_excel('Data/Gas/Gas GSOO 2024.xlsx') %>% 
#   clean_names() %>% 
#   mutate(state = convert_states(region),
#          annual_consumption_gj = annual_consumption_pj * 1e6) %>% 
#   filter(publication == 'GSOO 2024',
#          version == '2024-03-21',
#          scenario %in% c('Actual', 'Step Change'),
#          subcategory == "Residential and Commercial",
#          year == 2023,
#          !is.na(state)) %>% 
#     select(year, state, annual_consumption_gj)
#   

