#calculate energy efficiency per household by state

calculate_household_energy_efficiency <- function(#esoo_2024_operational_file, household_connections,
                                                  iasr_2023_file){
  
  #need to join with ESOO residential underlying by state -> get percentage of underlying by state demand that is lost due to energy efficiency
  # for now, use national data from 2024 ISP to estimate percent underlying that is removed due to efficiency
  pct_efficiency_underlying <- read_excel(iasr_2023_file, sheet = "Figure 6", skip = 35) %>%
    rename(category = 1) %>% 
    select(-2) %>% 
    pivot_longer(cols = contains("20"), names_to = "year", values_to = "annual_consumption_twh") %>% 
    pivot_wider(names_from = category, values_from = annual_consumption_twh) %>% 
    #calculate the multiplier that takes you from total -> underlying
    mutate(efficiency_multiplier = Underlying / (Underlying + `Energy efficiency`)) %>% 
    select(year, efficiency_multiplier)
  
  pct_efficiency_underlying
  
}

