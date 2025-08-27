#calculate energy efficiency per household by state

#esoo_2020_operational_file <- 'Data/2020 ESOO.xlsx'

calculate_household_energy_efficiency <- function(household_energy_efficiency,
                                                  tou_consumer_profiles){
  
  #underlyng demand = total residential delivered + total residential PV
  
  #define underlying demand for 2020-2024
  res_underlying_esoo_2020_24 <-   read_excel(esoo_2020_operational_file) %>% 
    clean_names() %>% 
    filter(scenario %in% c('Actual', 'Central'),
           parent_category == 'Operational (Sent Out)',
           #only include explicitly residential categories. residential EV will be added on top.
           category %in% c('Residential', 'Electrification', 'Rooftop PV'),
           
           #only include residential electrification
           sub_category != 'Business',
           region != 'NEM',
           year <= 2024,
           year >= 2020) %>% 
    rename(state = region) %>% 
    mutate(state = convert_states(state),
           state = if_else(state == 'NSW', 'NSW and ACT', state)) %>% 
    group_by(year, state) %>% 
    summarise(annual_consumption_t_wh = sum(annual_consumption_t_wh)) %>% 
    mutate(category = "Underlying")
  
  #define underlying demand for 2025-2050
  res_underlying_esoo_2025 <- read_excel(esoo_2024_operational_file) %>% 
    clean_names() %>% 
    filter(scenario %in% c('Actual', 'Central'),
           parent_category == 'Operational (Sent Out)',
           #only include explicitly residential categories. residential EV will be added on top.
           category %in% c('Residential', 'Electrification', 'Rooftop PV'),
           
           #only include residential electrification
           sub_category != 'Business',
           region != 'NEM') %>% 
    rename(state = region) %>% 
    mutate(state = convert_states(state),
           state = if_else(state == 'NSW', 'NSW and ACT', state)) %>% 
    group_by(year, state) %>% 
    summarise(annual_consumption_t_wh = sum(annual_consumption_t_wh)) %>% 
    mutate(category = "Underlying")
  
  #define energy efficiency for 2021-2024
  res_energy_efficiency_esoo_2020_24 <- read_excel(esoo_2020_operational_file) %>% 
    clean_names() %>% 
    filter(scenario %in% c('Actual', 'Central'),
           parent_category == 'Operational (Sent Out)',
           #only include explicitly residential categories. residential EV will be added on top.
           category %in% c('Energy Efficiency'),
           
           #only include residential electrification
           sub_category != 'Business',
           region != 'NEM',
           year <= 2024) %>% 
    rename(state = region) %>% 
    mutate(state = convert_states(state),
           state = if_else(state == 'NSW', 'NSW and ACT', state)) %>% 
    group_by(year, state, category) %>% 
    summarise(annual_consumption_t_wh = sum(annual_consumption_t_wh)) %>% 
    ungroup()
  
  #pull out cumulative efficiency gains to 2024 to add on to the gains starting in 2025
  #We are essentially assuming here that energy efficiency predicted from 2020-2024 did actually occur
  cumulative_efficiency_2024 <- res_energy_efficiency_esoo_2020_24 %>% 
    filter(year == 2024) %>% 
    select(- year) %>% 
    rename(add_on = annual_consumption_t_wh)
  
  #define energy efficiency for 2025-2050
  res_energy_efficiency_esoo_2025 <- read_excel(esoo_2024_operational_file) %>% 
    clean_names() %>% 
    filter(scenario %in% c('Actual', 'Central'),
           parent_category == 'Operational (Sent Out)',
           #only include explicitly residential categories. residential EV will be added on top.
           category %in% c('Energy Efficiency'),
           
           #only include residential electrification
           sub_category != 'Business',
           region != 'NEM') %>% 
    rename(state = region) %>% 
    mutate(state = convert_states(state),
           state = if_else(state == 'NSW', 'NSW and ACT', state)) %>% 
    group_by(year, state, category) %>% 
    summarise(annual_consumption_t_wh = sum(annual_consumption_t_wh)) %>% 
    #add cumulative efficiency up to 2024
    left_join(cumulative_efficiency_2024) %>% 
    mutate(annual_consumption_t_wh = annual_consumption_t_wh + add_on) %>% 
    select(-add_on)
    
  #calculate estimated energy efficiency gains as a per cent of estimated underlying 2021-2050
  pct_efficiency_underlying <- bind_rows(res_underlying_esoo_2020_24, res_underlying_esoo_2025,
                                         res_energy_efficiency_esoo_2020_24, res_energy_efficiency_esoo_2025) %>% 
    pivot_wider(names_from = category, values_from = annual_consumption_t_wh) %>% 
    
    #energy efficiency + underlying represents the total underlying consumption if there were no efficiency gains
    #the energy efficiency multiplier represents the value that we should scale down underlying consumption in that year
    mutate(efficiency_multiplier = Underlying / (`Energy Efficiency` + Underlying)) %>% 
  
  #note that efficiency gains flatline after 2050, with some uplift in underlying demand (likely due to economic growth / appliance consumption) - not really relevant for us as we just go out to 2050
  
  select(year, state, efficiency_multiplier) %>% 
    #create a multiplier for each end_use, but keep EV and PV equal to 1.
    #actual rates of efficiency will vary by end_use, meaning time of use will be a bit off but average is good enough.
    #there is limited vehicle efficiency improvements expected for EVs according to projections, can ignore or add in later.
    cross_join(tou_consumer_profiles %>% 
                 select(end_use) %>% 
                 unique()) %>% 
    mutate(efficiency_multiplier = if_else(end_use %in% c("Electric vehicle", "PV"), 1, efficiency_multiplier)) %>% 
    ungroup() %>%
    #add in 2020 as base year with energy efficiency of 1
    complete(year = seq(from = 2020, to = 2050), 
             state = unique(state),
             end_use = unique(end_use),
             fill = list(efficiency_multiplier = 1))
  
  pct_efficiency_underlying

}



##############################
#OLD
##############################

function(){
  
  #need to join with ESOO residential underlying by state -> get percentage of underlying by state demand that is lost due to energy efficiency
  # for now, use national data from 2024 ISP to estimate percent underlying that is removed due to efficiency
  
  # once ESOO data provided, join with prior years to get decline from 2021 to 2023
  
  pct_efficiency_underlying_2 <- read_excel(iasr_2023_file, sheet = "Figure 6", skip = 35) %>%
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

