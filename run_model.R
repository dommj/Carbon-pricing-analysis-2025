# script to run the model
library(targets)
library(tarchetypes)

tar_make()

tar_visnetwork()
tar_read(standing_offer_bills)

# average_household_costs
# household_connections
# 
# x <- average_household_costs %>% 
#   left_join(household_connections) %>% 
#   group_by(year, category) %>% 
#   summarise(average_cost_dollars = weighted.mean(average_cost_dollars, connections)) %>% 
#   filter(!is.na(average_cost_dollars))
