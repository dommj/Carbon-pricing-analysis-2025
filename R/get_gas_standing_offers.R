#load gas standing offers
get_gas_standing_offers <- function(gas_standing_offers_file){
  
  offers_1 <- read_xlsx(gas_standing_offers_file, 
                        sheet = 1) %>% 
    pivot_longer(cols = -c(1:3), 
                 names_to = "price_type", 
                 values_to = "price") %>% 
    filter(!is.na(price)) %>% 
    clean_names()
  
  offers_2 <- read_xlsx(gas_standing_offers_file, 
                        sheet = 2) %>% 
    clean_names() %>% 
    select(company, state, customer_class, price_type, price)
  
  
  offers <- bind_rows(offers_1, offers_2) %>% 
    mutate(min_vol = if_else(price_type != "supply charge", 
                             str_extract(price_type, "(^\\d*\\.\\d*|^\\d*)"), 
                             NA) %>% 
             as.numeric(),
           max_vol = if_else(price_type != "supply charge", 
                             str_extract(price_type, "((?<=_)\\d*\\.\\d*|(?<=_)\\d*)"), 
                             NA) %>% 
             as.numeric(),
           state = convert_states(state),
           
           #Just use NSW tariffs for NSW and ACT
           state = if_else(state == "NSW", "NSW and ACT", state)) %>% 
    filter(state != "ACT")
  
  offers
}