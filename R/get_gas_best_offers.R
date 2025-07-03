get_gas_best_offers <- function(gas_standing_offers_file){
  
  
  offers <- read_xlsx(gas_standing_offers_file, 
                        sheet = 4) %>% 
    clean_names() %>% 
    select(company, state, customer_class, price_type, price) %>% 
    mutate(min_vol = if_else(price_type != "supply charge", 
                             str_extract(price_type, "(^\\d*\\.\\d*|^\\d*)"), 
                             NA) %>% 
             as.numeric(),
           max_vol = if_else(price_type != "supply charge", 
                             str_extract(price_type, "((?<=_)\\d*\\.\\d*|(?<=_)\\d*)"), 
                             NA) %>% 
             as.numeric(),
           state = convert_states(state),
           state = if_else(state == "NSW", "NSW and ACT", state))
  
  # offers <- read_xlsx(gas_standing_offers_file, 
  #                       sheet = 4) %>% 
  #   clean_names() %>% 
  #   select(retailer, usage_block_1_rate, supply_charge, state) %>% 
  #   rename(company = retailer, `supply charge` = supply_charge) %>% 
  #   pivot_longer(cols = c(usage_block_1_rate, `supply charge`), names_to = 'price_type', values_to = 'price') %>% 
  #   #put in form of other offers but set max vol to inf
  #   mutate(min_vol =  if_else(price_type == "supply charge", NA, 0),
  #          max_vol = if_else(price_type == "supply charge", NA, 1e10),
  #          price = str_remove(price, "c") %>%  as.numeric(),
  #          customer_class = "Residential") %>% 
  #   #NSW and ACT very comparable, NSW dominates, so will be used as proxy for the NSW and ACT aggregate
  #   filter(state != "ACT") %>% 
  #   mutate(state = if_else(state == "NSW", "NSW and ACT", state))
  
  
  offers
}