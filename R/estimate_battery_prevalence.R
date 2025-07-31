#battery system prevalence

estimate_battery_prevalence <- function(esoo_2024_assumptions_workbook_file,
                                   household_connections){
  
  
  nem_total_degraded_mwh_capacity <- read_excel(esoo_2024_assumptions_workbook_file,
                                                sheet = "Embedded energy storages",
                                                range = "B53:AF58") %>% 
    rename(state = 1) %>% 
    pivot_longer(cols = contains('20'), names_to = 'year', values_to = 'mwh') %>% 
    mutate(year = str_remove(year, "\\d\\d-") %>% 
             as.numeric(),
           state = convert_states(state),
           state = if_else(state == "NSW", "NSW and ACT", state),
           #we deflate total capacity by 7% reflecting assumption that 7% of capacity is small commercial 
           #(CSIRO small scale report 2024, Figure 5-6 and Figure 5-7)
           mwh = 0.93 * mwh)
  
  #we don't have an appropriate data source for the WEM (small commercial batteries seem to dominate their estimates) So we don't include the impact of batteries. Given this will only affect a small number of consumers out to 2034 this means our WEM estimate is more conservative than our NEM estimate for consumer savings.
  
  #we assume battery size is approximately 11 Kw throughout to get stock estimates
  
  battery_prevalence <- nem_total_degraded_mwh_capacity %>% 
    left_join(household_connections) %>% 
    mutate(battery_stock = mwh * 1000 / 11, # how many 11 kw batteries are there? (if all = 11kw). 
           battery_and_pv_prop = battery_stock / connections) #final levels converge to CSIRO prevalence estimates too! see figure 5-7 and table A-1
  
  #add WA in as equal to NEM mean
  wa_prevalence <- battery_prevalence %>% 
    group_by(year) %>% 
    summarise(battery_and_pv_prop = mean(battery_and_pv_prop),
              state = "WA") %>% 
    filter(year <= 2034) #WA data doesn't extend out any further

  battery_prevalence <- bind_rows(battery_prevalence,
                                  wa_prevalence)

  
  return(battery_prevalence)
}


#archived
function(){
  
  
  csiro_battery_prevalance_scraped <- read_excel(csiro_pv_prevalance_file, sheet = "Sheet3") %>% 
    select(year, pct_of_pv) 
  #interpolate values for each year
  
  csiro_battery_prevalance_interpolated <- tibble(
    year = 2025:2050,
    pct_of_pv = approx(x = csiro_battery_prevalance_scraped$year, 
                       y = csiro_battery_prevalance_scraped$pct_of_pv, 
                       xout = 2025:2050, 
                       method = "linear")$y
  )
  
  
  csiro_battery_prevalance <- csiro_battery_prevalance_interpolated %>% 
    #join with state pv estimates
    left_join(pv_system_stock %>% 
                select(year, state, prop)) %>% 
    mutate(battery_and_pv_prop = pct_of_pv * prop) %>% 
    select(year, state, battery_and_pv_prop) %>% 
    left_join(household_connections) %>% 
    left_join(nem_total_degraded_mwh_capacity) %>% 
    mutate(kwh_per_b = mwh / (battery_and_pv_prop * connections) *1000)
    
  
  return(csiro_battery_prevalance)
  
}


function(){
  
  
  read_excel(esoo_2024_assumptions_workbook_file,
             sheet = "Embedded energy storages",
             range = "B53:AF58")
  
  
  
}


