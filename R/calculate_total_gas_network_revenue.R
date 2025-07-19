#project out the supply component of gas charges

calculate_total_gas_network_revenue  <- function(best_offer_bills, 
                                                 gas_connections_data){
  
  
  gas_connections_data <- gas_connections_data %>% 
    #amalgamate nsw and act
    bind_rows(
      gas_connections_data %>%
        filter(state %in% c("NSW", "ACT")) %>%
        summarise(
          state = "NSW and ACT",
          residential = sum(residential),
          small_business = sum(small_business),
          total_customers = sum(total_customers),
          year = first(year))) %>%
    # Remove the original NSW and ACT rows 
    filter(!state %in% c("NSW", "ACT")) %>% 
    select(state, residential)
  
  #multiply supply charges by the number of connections in each state
  total_residential_network_revenue <- gas_connections_data %>% 
    full_join(best_offer_bills) %>% 
    mutate(network_revenue_dollars = residential * network_cost) %>% 
    select(state, network_revenue_dollars)
  
  total_residential_network_revenue
}


