#load GSOO gas consumption data for residential and small commercial customers

get_gsoo_consumption_data <- function(gsoo_consumption_data_file){
  
  read_excel(gsoo_consumption_data_file) %>% 
    clean_names() %>% 
    mutate(state = convert_states(region),
           state = if_else(state == 'NSW', 'NSW and ACT', state),
           annual_consumption_gj = annual_consumption_pj * 1e6) %>% 
    filter(publication == 'GSOO 2024',
           version == '2024-03-21',
           scenario %in% c('Actual', 'Step Change'),
           subcategory == "Residential and Commercial",
           !is.na(state),
           year >= 2023) %>% 
    select(year, state, annual_consumption_gj)
}

#get_gsoo_consumption_data('Data/Gas/Gas GSOO 2024.xlsx')
