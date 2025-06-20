#calculate energy efficiency per household by state

calculate_household_energy_efficiency <- function(#esoo_2024_operational_file, household_connections,
                                                  iasr_2023_file,
                                                  tou_consumer_profiles){
  
  #need to join with ESOO residential underlying by state -> get percentage of underlying by state demand that is lost due to energy efficiency
  # for now, use national data from 2024 ISP to estimate percent underlying that is removed due to efficiency
  
  # once ESOO data provided, join with prior years to get decline from 2021 to 2023
  
  pct_efficiency_underlying <- read_excel(iasr_2023_file, sheet = "Figure 6", skip = 35) %>%
    rename(category = 1) %>% 
    select(-2) %>% 
    pivot_longer(cols = contains("20"), names_to = "year", values_to = "annual_consumption_twh") %>% 
    pivot_wider(names_from = category, values_from = annual_consumption_twh) %>% 
    #calculate the multiplier that takes you from total baseline (underlying without EVs + energy efficiency) -> underlying without EVs
    mutate(efficiency_multiplier = (Underlying - `Electric vehicles`) / (Underlying - `Electric vehicles` + `Energy efficiency`),
           year = fy2yr(year)) %>% 
    select(year, efficiency_multiplier) %>% 
    #create a multiplier for each end_use, but keep EV and PV equal to 1.
    cross_join(tou_consumer_profiles %>% 
                 select(end_use) %>% 
                 unique()) %>% 
    mutate(efficiency_multiplier = if_else(end_use %in% c("Electric vehicle", "PV"), 1, efficiency_multiplier)) %>% 
    
    #add in 2023 as base year with energy efficiency of 1
    complete(year = seq(from = 2023, to = 2050), 
             end_use = unique(end_use),
             fill = list(efficiency_multiplier = 1))
  
  pct_efficiency_underlying
}

