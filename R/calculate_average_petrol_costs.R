
calculate_average_petrol_costs <- function(petrol_price_projections,
                                           average_petrol_consumption){
  
  
  petrol_costs <- left_join(average_petrol_consumption, petrol_price_projections) %>% 
    select(year, state, category, electrification, average_petrol_use_per_household, c_litre) %>% 
    mutate(average_cost_dollars = average_petrol_use_per_household * c_litre / 100) %>%
    select(year, state, category, electrification, average_cost_dollars)
  
  return(petrol_costs)
  
  
  petrol_costs %>% 
    group_by(year, state, electrification) %>% 
    summarise(average_cost_dollars = sum(average_cost_dollars)) %>% 
    filter(state == "Vic") %>% 
    ggplot(aes(x = year, y = average_cost_dollars, colour = electrification)) +
    geom_line()
}