load_and_deflate_rbs_households <- function(rbs_outputs_data_file){
  
  #load occupancy rates in 2016 Census (used by RBS)
  
  # search_catalogues("Snapshot")
  # show_available_files("snapshot-australia")
  
  occupancy_rates_16_file <- download_abs_data_cube(catalogue_string = "snapshot-australia", "summary")
  
  occupancy_rates_16 <- read_excel(occupancy_rates_16_file, sheet = "Table 11", range = "A9:E22") %>% 
    rename(state = 1) %>% 
    clean_names() %>% 
    mutate(occupancy_rate = as.numeric(occupied) / (as.numeric(occupied) + as.numeric(unoccupied)),
           state = str_remove(state, "\\(b\\)"),
           state = convert_states(state)) %>% 
    filter(!is.na(state)) %>% 
    select(state, occupancy_rate)
  
  #read in resisdential baseline output sheet
  rbs_output_cells <- xlsx_cells(rbs_outputs_data_file)
  
  
  rbs_households <- rbs_output_cells %>% 
    filter(row > 5,
           sheet == "HH-State") %>% 
    behead("up", "year") %>% 
    behead("left", "state") %>% 
    select(year, state, content) %>% 
    rename(households = content) %>% 
    filter(year == 2020) %>% 
    mutate(households = as.numeric(households),
           year = as.numeric(year),
           state = convert_states(state)) %>% 
    filter(year == 2020) %>% 
    left_join(occupancy_rates_16) %>% 
    #deflate by 10% to account for vacancy
    mutate(occupied_households = households * occupancy_rate) %>% 
    select(state, occupied_households)
  
  rbs_households
  
}