#get average RBS fuel consumption

get_average_rbs_gas_consumption <- function(rbs_households, rbs_fuel_end_use_by_state){
  
  
  rbs_gas_consumption <- rbs_fuel_end_use_by_state %>% 
    filter(fuel %in% c("Natural Gas", "LPG")) %>% 
    mutate(fuel = "Gas",
           state = convert_states(state)) %>% 
    group_by(year, state, fuel, end_use) %>% 
    summarise(pj = sum(pj)) %>% 
    left_join(rbs_households) %>% 
    mutate(average_gas_consumption_gj = pj / occupied_households * 1e6)
  

  rbs_gas_consumption %>% 
    filter(year >= 2020) %>% 
    ggplot(aes(x= year, y = average_gas_consumption_gj, fill = end_use)) +
    geom_col()+
    facet_wrap(~state)
  
}

function(){
  
rbs_electricity_consumption <- rbs_fuel_end_use_by_state %>% 
  filter(fuel %in% c("Electricity"),
         end_use != "Transport") %>% 
  mutate(state = convert_states(state)) %>% 
  group_by(year, state, fuel, end_use) %>% 
  summarise(pj = sum(pj)) %>% 
  left_join(rbs_households) %>% 
  mutate(average_electricity_consumption_kwh = pj / occupied_households * 1e9 / 3.6)


rbs_electricity_consumption %>% 
  filter(year >= 2020) %>% 
  ggplot(aes(x= year, y = average_electricity_consumption_kwh, fill = end_use)) +
  geom_col()+
  facet_wrap(~state)

}
