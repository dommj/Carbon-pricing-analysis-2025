#battery system prevalence

estimate_battery_prevalence <- function(pv_system_stock,
                                   csiro_pv_prevalance_file){

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
    select(year, state, battery_and_pv_prop)
    
    return(csiro_battery_prevalance)
}