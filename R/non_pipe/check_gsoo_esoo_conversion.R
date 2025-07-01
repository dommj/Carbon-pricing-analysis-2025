#check conversion rate from 1 GJ gas to KWH electricity


x <- additional_electricty_consumption %>% 
  group_by(year, state) %>%
  summarise(annual_consumption_kwh = sum(annual_consumption_kwh)) %>% 
  left_join(rbs_displaced_gas_consumption %>% 
              select(year, state, displaced_consumption_gj)) %>% 
  mutate(cf = annual_consumption_kwh / displaced_consumption_gj )


#double check that this was the residential component.....

gsoo_electrification <- 167.99 * 1e6 #PJ 2043
  
esoo_electrification <- 34.98 * 1e9 #2043 TWH


esoo_electrification / gsoo_electrification


#victoria

gsoo_electrification <- 117.34 * 1e6 #PJ 2043

esoo_electrification <- 9.95 * 1e9 #2043 TWH


esoo_electrification / gsoo_electrification

#NSW

gsoo_electrification <- 38.9 * 1e6 #PJ 2043

esoo_electrification <- 7.4 * 1e9 #2043 TWH


esoo_electrification / gsoo_electrification
