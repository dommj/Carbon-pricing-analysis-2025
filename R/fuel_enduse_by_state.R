#calculate electricity and gas consumption traces for cooking, space conditioning and water heating

rbs_output_cells <- xlsx_cells(rbs_outputs_data_file)


rbs_elec_end_use_by_state <- rbs_output_cells %>% 
  filter(row > 4,
         sheet == "Energy.Elec.EndUse-State") %>% 
  behead("up-left", "fuel") %>% 
  behead("up-left", "state") %>% 
  behead("up", "end_use") %>% 
  behead("left", "year") %>% 
  select(year, state, fuel, end_use, content) %>% 
  rename(pj = content) %>%
  filter(!is.na(end_use))

rbs_natgas_end_use_by_state <- rbs_output_cells %>% 
  filter(row > 4,
         sheet == "Energy.NG.EndUse-State") %>% 
  behead("up-left", "fuel") %>% 
  behead("up-left", "state") %>% 
  behead("up", "end_use") %>% 
  behead("left", "year") %>% 
  select(year, state, fuel, end_use, content) %>% 
  rename(pj = content) %>%
  filter(!is.na(end_use))

rbs_lpg_end_use_by_state <- rbs_output_cells %>% 
  filter(row > 4,
         sheet == "Energy.LPG.EndUse-State") %>% 
  behead("up-left", "fuel") %>% 
  behead("up-left", "state") %>% 
  behead("up", "end_use") %>% 
  behead("left", "year") %>% 
  select(year, state, fuel, end_use, content) %>% 
  rename(pj = content) %>%
  filter(!is.na(end_use))

rbs_fuel_end_use_by_state <- bind_rows(rbs_elec_end_use_by_state, rbs_natgas_end_use_by_state, rbs_lpg_end_use_by_state) %>% 
  mutate(pj = as.numeric(pj))


conversion_factors <- read_excel("Data/elec_to_gas_coefficients.xlsx", sheet = "coefficients") %>% 
  select(-3)


cooking_microwave <- rbs_output_cells %>% 
  filter(row > 5,
         sheet == "Energy.Cook.State-Grp") %>% 
  behead("up-left", "appliance_type") %>% 
  behead("up", "state") %>% 
  behead("left", "year") %>% 
  select(year, state, appliance_type, content) %>% 
  rename(pj = content) %>% 
  filter(appliance_type == "Microwave") %>% 
  mutate(pj = as.numeric(pj),
         end_use = 'Cooking')

#complete conversions

gas_to_elec_conv <- rbs_fuel_end_use_by_state %>% 
  filter(end_use %in% c(c("Water heating", "Space conditioning", 'Cooking')),
         fuel %in% c("Natural Gas", "LPG")) %>% 
  left_join(conversion_factors) %>% 
  #change fuel to electric and multiply by conversion factor
  mutate(pj = pj * gas_to_electric_cf,
         fuel = 'gas_to_electric_converted')

elec_to_gas_conv <- rbs_fuel_end_use_by_state %>% 
  filter(end_use %in% c(c("Water heating", "Space conditioning", 'Cooking')),
         fuel %in% c("Electricity")) %>% 
  left_join(conversion_factors) %>% 
  left_join()
  
  #change fuel to gas and multiply by reciprocal of conversion factor
  mutate(pj = if_else(end_use == 'Cooking', 
                      (pj - cooking_microwave$pj[year == year, state == state]) * 1/ gas_to_electric_cf,
                      pj * 1/ gas_to_electric_cf), 
         fuel = 'electric_to_gas_converted')
  
  #pivot_wider(names_from = end_use, values_from = pj)
  


#Assume X% of cooking electricity is Microwaves and remains after conveting to gas