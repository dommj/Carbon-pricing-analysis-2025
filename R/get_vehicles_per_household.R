#get vehicles per household (from aemo numbers)

get_vehicles_per_household <- function(ev_fleet_data, household_connections){
  
  ev_fleet_data %>% 
    group_by(year, state) %>% 
    summarise(vehicles_count = sum(vehicles_count)) %>% 
    left_join(household_connections, by = c("year", "state")) %>%
    mutate(vehicles_per_household = vehicles_count / connections) %>% 
    ungroup()
  
}

#vehicles_per_household <- get_vehicles_per_household(ev_fleet_data, household_connections)
# 
# x <- vehicles_per_household %>% 
#   group_by(year) %>% 
#   summarise(vehicles_per_household = weighted.mean(vehicles_per_household, connections))


