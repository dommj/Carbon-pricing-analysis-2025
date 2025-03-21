#residential electricity demand - Step Change

get_average_residential_operational_demand <- function(esoo_2024_operational_file, residential_ev_econsumption, household_connections){
  
  read_excel(esoo_2024_operational_file) %>% 
    clean_names() %>% 
    filter(scenario %in% c('Actual', 'Central'),
           parent_category == 'Operational (Sent Out)',
           #only include explicitly residential categories. residential EV will be added on top.
           category %in% c('Residential', 'Electrification'),
           
           #only include residential electrification
           sub_category != 'Business',
           region != 'NEM') %>% 
    rename(state = region) %>% 
    mutate(state = convert_states(state),
           state = if_else(state == 'NSW', 'NSW and ACT', state),) %>% 
    group_by(year, state, category) %>% 
    summarise(annual_consumption_t_wh = sum(annual_consumption_t_wh)) %>% 
    
    #add in residential EV data
    bind_rows(residential_ev_econsumption) %>%
    
    #calculate average annual consumption per connection
    left_join(household_connections, by = c('year', 'state')) %>% 
    ungroup() %>% 
    mutate(average_annual_consumption_kwh = annual_consumption_t_wh / connections * 1e9)
  

}

# 
# function(){
# 
# av_demand <- get_average_residential_operational_demand('Data/2024 ESOO/2024 ESOO operational (sent out).xlsx', 
#                                                         residential_ev_econsumption,
#                                                         household_connections) %>%
#   group_by(year, state) %>%
#   summarise(average_annual_consumption_kwh = sum(average_annual_consumption_kwh))
# 
# 
# total_demand <- get_average_residential_operational_demand('Data/2024 ESOO/2024 ESOO operational (sent out).xlsx', 
#                                                            residential_ev_econsumption,
#                                                            household_connections) %>%
#   group_by(year) %>%
#   summarise(annual_consumption_twh = sum(annual_consumption_t_wh))
# 
# 
# esoo_residential_operational_demand <- read_excel('Data/2024 ESOO/2024 ESOO operational (sent out).xlsx') %>%
#   clean_names() %>%
#   filter(scenario %in% c('Actual', 'Central'),
#          parent_category == 'Operational (Sent Out)',
#          category %in% c('Residential', 'Electrification'),
#          sub_category %nin% c('Business', 	
#                               'Res SNSG offset'),
#          region != 'NEM') %>%
#   rename(state = region) %>%
#   group_by(year, category, state) %>%
#   summarise(annual_consumption_t_wh = sum(annual_consumption_t_wh))
# 
# 
# 
# esoo_residential_operational_demand %>% 
#   group_by(year, category) %>%
#   summarise(annual_consumption_twh = sum(annual_consumption_t_wh))
# 
# # national_res_demand <-  esoo_residential_operational_demand %>% 
# #   group_by(year) %>% 
# #   summarise(res_sent_out_t_wh = sum(annual_consumption_t_wh))
# 
# categories <- esoo_residential_operational_demand %>%
#   select(category, sub_category) %>%
#   unique()
# 
# average_residential_operational <- read_excel('Data/2024 ESOO/2024 ESOO operational (sent out).xlsx') %>% 
#   clean_names() %>% 
#   filter(scenario %in% c('Actual', 'Central'),
#          parent_category == 'Operational (Sent Out)',
#          #only include explicitly residential categories. residential EV will be added on top.
#          category %in% c('Residential', 'Electrification'),
#          
#          #only include residential electrification
#          sub_category != 'Business',
#          region == 'NEM') %>% 
#   rename(state = region) %>% 
#   mutate(state = convert_states(state),
#          state = if_else(state == 'NSW', 'NSW and ACT', state),) %>% 
#   group_by(year, state, category) %>% 
#   summarise(annual_consumption_t_wh = sum(annual_consumption_t_wh))
# 
# }















  
