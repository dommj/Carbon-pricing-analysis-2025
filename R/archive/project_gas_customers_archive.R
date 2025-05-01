#project out customer numbers

project_gas_customers <- function(gas_connections_data, gsoo_consumption_data){
  
  #gas consumption data from GSOO for residential and small business  
  gas_consumption <- gsoo_consumption_data
  
  gas_connections_data <- gas_connections_data %>% 
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
    mutate(pct_residential = residential / total_customers) 
  
  #use 2023 data to set consumption per customer. This does not change over time.
  gas_consumption_per_customer <- left_join(gas_connections_data, gas_consumption %>% 
                                          filter(year == 2023), 
                                        by = c('year', 'state')) %>% 
    mutate(gas_consumption_per_customer_gj = annual_consumption_gj / total_customers) %>% 
    clean_names() %>% 
    select(state, residential, small_business, total_customers, gas_consumption_per_customer_gj)
  
  gas_customer_projections <- left_join(gas_consumption, gas_consumption_per_customer, by = c('state' = 'state')) %>% 
    filter(!is.na(gas_consumption_per_customer_gj)) %>% 
    mutate(gas_customers = annual_consumption_gj / gas_consumption_per_customer_gj) %>% 
    select(year, state, gas_customers)
  
  #now we want to continue a straightline customer decline to 2050
  
  # fit linear models for 2030-2042 period for each state
  models_by_state <- gas_customer_projections %>%
    filter(year >= 2030 & year <= 2043) %>%
    group_by(state) %>%
    nest() %>%
    mutate(model = map(data, ~lm(gas_customers ~ year, data = .x))) %>% 
    select(state, model)
  
  
  gas_customer_projections_complete <- gas_customer_projections %>% 
    complete(year = 2023:2050, state) %>%
    left_join(models_by_state, by = "state") %>%
    mutate(pred = map2_dbl(model, year, ~ predict(.x, newdata = data.frame(year = .y))),
           gas_customers = if_else(is.na(gas_customers), pred, gas_customers)) %>%
    #gas customer projections for VIC go below zero. There will be a discontinuity if customers go to zero in our   projection... set to 2049 value for now.
    mutate(total_gas_customers = if_else(year == 2050 & state == "Vic",
                                          gas_customers[year == 2049 & state == "Vic"],
                                          gas_customers)) %>% 
    select(year, state, total_gas_customers) 
  
  gas_customer_projections_complete
}


# ggplot(gas_customer_projections_complete, aes(x = year, y = gas_customers, color = state)) +
#   geom_line() +
#   labs(title = "Projected Gas Customers Over Time",
#        x = "Year",
#        y = "Number of Gas Customers") 



