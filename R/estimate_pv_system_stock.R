#estimate stock of PV systems over time

estimate_pv_system_stock <- function(rbs_outputs_data_file,
                                     rbs_households,
                                     household_connections,
                                     csiro_pv_prevalance_file){
  
  rbs_output_cells <- xlsx_cells(rbs_outputs_data_file)
  
  formats <- xlsx_formats(rbs_outputs_data_file)
  
  indent <- formats$local$alignment$indent
  
  
  rbs_pv_stock_by_state <- rbs_output_cells %>% 
    filter(row > 3,
           sheet == "Stock.EndUse.Cat.Grp-State") %>% 
    behead("up-left", "state") %>% 
    behead("up", "year") %>% 
    behead_if(indent[local_format_id] == 0,
              direction = "left-up",
              name = "end_use") %>% 
    behead_if(indent[local_format_id] == 1,
              direction = "left-up",
              name = "category") %>% 
    behead("left", "appliance") %>% 
    select(year, state, end_use, category, appliance, content) %>% 
    rename(stock = content) %>%
    filter(!is.na(appliance)) %>% 
    mutate(stock = as.numeric(stock)) %>% 
    filter(category == "PV")
  
  rbs_pv_prevalence <- rbs_pv_stock_by_state %>% 
    ungroup() %>% 
    mutate(year = as.numeric(year),
           state = convert_states(state)) %>% 
    filter(year == 2020) %>% 
    group_by(year, state) %>% 
    summarise(stock = sum(stock)) %>% 
    left_join(rbs_households) %>% 
    mutate(prop = stock / occupied_households,
           state = if_else(state %in% c("NSW", "ACT"), "NSW and ACT", state)) %>% 
    #aggregate act and nsw together
    group_by(year, state) %>% 
    summarise(prop = weighted.mean(prop, occupied_households)) %>% 
    #no csiro data for NT  so exclude here
    filter(state != 'NT') 
  
  csiro_pv_prevalance <- read_excel(csiro_pv_prevalance_file, sheet = "Sheet1") %>% 
    rename(prop = `Step Change`) %>% 
    select(year, state, prop) %>% 
    mutate(state =convert_states(state)) %>% 
    #aggregate act and nsw together, assume ratio of households from RBS in 2040 is correct for 2050
    left_join(rbs_households %>% 
                filter(year == 2040) %>% 
                select(-year)) %>% 
    mutate(state = if_else(state %in% c("NSW", "ACT"), "NSW and ACT", state)) %>% 
    group_by(year, state) %>% 
    summarise(prop = weighted.mean(prop, occupied_households)) %>% 
    filter(!is.na(state))
  
  
  #simple linear interpolation to estimate PV uptake
  interpolated_pv_prevalence <- bind_rows(rbs_pv_prevalence, csiro_pv_prevalance) %>% 
    ungroup() %>% 
    # First, ensure we have complete year coverage for each state
    complete(year = seq(2020, 2050, by = 1), state) %>%
    # Group by state and interpolate
    group_by(state) %>%
    arrange(year) %>%
    mutate(prop = approx(year, prop, year, rule = 2)$y) %>%
    ungroup()
    
  #check interpolation  
  interpolated_pv_prevalence %>%
    ggplot(aes(x = year, y = prop, colour = state))+
    geom_line() +
    scale_y_continuous(limits = c(0, 0.7))

  
  #stock = prevalence * number of AEMO households in projections
  interpolated_pv_stock <- interpolated_pv_prevalence %>% 
    left_join(household_connections) %>% 
    mutate(pv_stock = prop * connections) %>% 
    select(-connections)
  
  return(interpolated_pv_stock)
  
}