#This script calculates the total gj of household gas that is removed, on average, from household consumption each year


#rather than using the ESOO we need to take the GSOO and work out the reduction in gas consumed per household from 2020-2050 This gets rid of the problem that electrificatin estimates account for switching but may not relate to a total decline in gas usage per household

calculate_displaced_gas_consumption <- function(average_gas_consumption,
                                                rbs_baseline_consumption,
                                                rbs_households){

############################################
# Quick sense check
############################################

#compare rbs consumption per household to GSOO - very comparable except for QLD, gas use there is small enough that this doesn't really matter
  average_gas_consumption

  rbs_baseline_consumption %>% 
    filter(fuel == "Natural Gas") %>% 
    group_by(state) %>% 
    summarise(annual_consumption_gj = sum(annual_consumption_gj))

####################################################
# Calculation
####################################################
#take the average gas consumption per electricity connection from AEMO (GSOO and ESOO connection forecasts) and calculate a pct of average consumption per household that has been displaced by year (2020 is reference year as that is when RBS study was completed)

displaced_gas_consumption_per_household_index <- average_gas_consumption %>% 
  group_by(state) %>% 
  mutate(displaced_consumption_index = 1 - average_annual_consumption_gj/average_annual_consumption_gj[year==2020]) %>% 
  select(year, state, displaced_consumption_index)

rbs_gas_consumption <- rbs_baseline_consumption %>% 
  filter(fuel == "Natural Gas") %>% 
  group_by(state) %>% 
  summarise(annual_consumption_gj = sum(annual_consumption_gj))


#create a time series of RBS gas usage by declining according to index  
rbs_gas_consumption_time_series <- rbs_gas_consumption %>% 
  full_join(displaced_gas_consumption_per_household_index) %>% 
  mutate(displaced_consumption_gj = annual_consumption_gj * displaced_consumption_index,
         annual_consumption_gj = annual_consumption_gj * (1-displaced_consumption_index),
         fuel = "Natural Gas") %>% 
  select(year, state, fuel, annual_consumption_gj, displaced_consumption_gj, displaced_consumption_index) %>% 
  filter(!is.na(year),
         year >= 2020)

rbs_gas_consumption_time_series
}


