#read in our conversion coefficients file

get_fuel_conversion_coefficients <- function(electric_to_gas_coefficients_file){
  
  conversion_factors <- read_excel(electric_to_gas_coefficients_file, 
                                   sheet = "coefficients") %>% 
    select(1,2) %>% 
    filter(end_use != "Space conditioning - heating") %>% 
    cross_join(tibble(state = c("WA", "NT", "Qld", "NSW", "SA", "Vic", "Tas", "ACT")))
  
  #adding in state by state space heating conversions
  conversion_factors_sh <- read_excel(electric_to_gas_coefficients_file, 
                                   sheet = "space_heating_2.0") %>% 
    clean_names() %>% 
    select(location_1, ratio) %>% 
    filter(location_1 %in% c("Adelaide", "Canberra", "Brisbane", "Hobart", "Melbourne", "Sydney")) %>% 
    mutate(state = case_when(location_1 == "Adelaide" ~ "SA",
                             location_1 == "Brisbane" ~ "Qld",
                             location_1 == "Canberra" ~ "ACT",
                             location_1 == "Hobart" ~ "Tas",
                             location_1 == "Melbourne" ~ "Vic",
                             location_1 == "Sydney" ~ "NSW")) %>% 
    bind_rows(tibble(state = c("WA", "NT", "Qld"), ratio = c(NA, NA, NA))) %>% 
    mutate(gas_to_electric_cf = if_else(is.na(ratio), ratio[state == "NSW"], ratio),
           end_use = "Space conditioning - heating") %>% 
    select(state, end_use, gas_to_electric_cf)
  
  
  conversion_factors_all <- bind_rows(conversion_factors, conversion_factors_sh)
  
  return(conversion_factors_all)
}