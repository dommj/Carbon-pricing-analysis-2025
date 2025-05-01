# get AER data for gas

get_aer_customer_gas_use_data <- function(aer_retail_markets_file){
  
  customer_gas_use <- read_excel(aer_retail_markets_file,
                                 sheet = "Figure 6.7",
                                 skip = 5) %>% 
    rename(network = 1) %>% 
    pivot_longer(-1,
                 names_to = "year",
                 values_to = "GJ") %>%
    mutate(year = str_remove(year, "\\d\\d–"),
           year = as.numeric(year),
           state = str_extract(network, "(?<=\\().*(?=\\))"))
  
  
#electricity
    
  
}