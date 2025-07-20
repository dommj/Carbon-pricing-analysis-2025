get_battery_capacity_per_household <- function(esoo_2024_assumptions_workbook_file, 
                                               wem_esoo_2024_data_register_file,
                                               household_connections,
                                               battery_n_pv_prop){
  
  nem_total_degraded_mwh_capacity <- read_excel(esoo_2024_assumptions_workbook_file,
                                            sheet = "Embedded energy storages",
                                            range = "B24:AF29") %>% 
    rename(state = 1) %>% 
    pivot_longer(cols = contains('20'), names_to = 'year', values_to = 'mwh') %>% 
    mutate(year = str_remove(year, "\\d\\d-") %>% 
             as.numeric(),
           state = convert_states(state),
           state = if_else(state == "NSW", "NSW and ACT", state))
  
  
  wem_total_degraded_mwh_capacity <- read_excel(wem_esoo_2024_data_register_file,
                                                sheet = "A1_F.8",
                                                range = "B23:O25") %>% 
    pivot_longer(cols = contains('20'), names_to = 'year', values_to = 'mwh') %>% 
    mutate(year = str_remove(year, "\\d\\d-") %>% 
             as.numeric(),
           state = "WA") %>% 
    filter(Scenario == "2024 Expected",
           year >= 2025) %>% 
    select(state, year, mwh)
  
  total_degraded_mwh_capacity_household <- bind_rows(nem_total_degraded_mwh_capacity,
                                                     wem_total_degraded_mwh_capacity) %>% 
    left_join(household_connections) %>% 
    left_join(battery_n_pv_prop) %>% 
    mutate(battery_kwh_per_household = mwh / (connections * battery_and_pv_prop) * 1000)
  
  total_degraded_mwh_capacity_household
}