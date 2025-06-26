

calculate_additional_electricty_consumption <- function(fuel_conversion_coefficients, 
                                                        rbs_displaced_gas_consumption, 
                                                        rbs_baseline_consumption){

#calculate increase in electricity consumption for each end use

#tas is only nem state without GSOO data - has negligible gas use.

additional_electricity_consumption <- rbs_baseline_consumption %>% 
  filter(fuel == "Natural Gas") %>% 
  select(-year) %>% 
  
  #THIS SHOULD BE A RIGHT JOIN ? TO AVOID NT WHERE IT SHOULDNT BE
  right_join(rbs_displaced_gas_consumption %>% 
              select(year, state, fuel, displaced_consumption_index), relationship = "many-to-many") %>% 
  left_join(fuel_conversion_coefficients) %>% 
  #additional consumption = displaced gas consumption * conversion coefficient then converted to kwh
  mutate(annual_consumption_gj = annual_consumption_gj * displaced_consumption_index * gas_to_electric_cf,
         annual_consumption_kwh =  annual_consumption_gj * 1e3 / 3.6,
         fuel = "Electricity",
         source = "displaced_gas") %>% 
  select(year, state, fuel, end_use, source, annual_consumption_gj, annual_consumption_kwh)

 additional_electricity_consumption
}





