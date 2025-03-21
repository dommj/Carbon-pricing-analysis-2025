#get household connections data

get_household_connections_data <- function(esoo_2024_assumptions_workbook_file){
  
  read_excel(esoo_2024_assumptions_workbook_file, 
             sheet = 'Connections Forecasts',
             range = 'B21:AG26') %>% 
    rename(state = 1) %>% 
    clean_names() %>% 
    pivot_longer(cols = -state, 
                 names_to = 'year', 
                 values_to = 'connections') %>%
    mutate(state = convert_states(state),
           state = if_else(state == 'NSW', 'NSW and ACT', state),
           year = paste0('20', 
                         str_match(year, 'x\\d\\d\\d\\d_(\\d\\d)')[,2]) %>% 
             as.numeric())
}

#connections <- get_household_connections_data('Data/2024 ESOO/2024 Forecasting Assumptions Update Workbook.xlsx')
