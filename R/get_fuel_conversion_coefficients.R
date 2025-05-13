#read in our conversion coefficients file

get_fuel_conversion_coefficients <- function(electric_to_gas_coefficients_file){
  
  conversion_factors <- read_excel("Data/elec_to_gas_coefficients.xlsx", 
                                   sheet = "coefficients") %>% 
    select(-3)
  
  conversion_factors
}