#load petrol price data

#prices in March quarter 2025 dollars.

get_petrol_data <- function(petrol_file) {
  

  data <- read_excel(petrol_file) %>% 
    rename(c_litre = c_l) %>% 
    select(state, c_litre) %>% 
    mutate(category = "Petrol")
  
  
  data
  
}


