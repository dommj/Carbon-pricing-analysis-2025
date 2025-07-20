#battery and solar sense checking

tar_load(battery_n_pv_prop)
tar_load(pv_system_stock)
tar_load(household_connections)

stock <- pv_system_stock %>% 
  left_join(battery_n_pv_prop) %>% 
  left_join(household_connections) %>% 
  mutate(pv_stock = prop * connections,
         battery_stock = battery_and_pv_prop * connections)

stock %>% group_by(year) %>% summarise(pv_stock = sum(pv_stock))

#very comparable to sres data