#Load Jacobs demand data

load_jacobs_demand_data <- function(jacobs_demand_data_file){

  # Load demand forecast:
  
  nem_underlying <- read_excel(jacobs_demand_data_file,
                               range = "A7:AJ17") %>% 
    rename(source = 1) %>% 
    select(-2) %>% 
    pivot_longer(cols = -source, names_to = "year", values_to = "underlying_demand_gwh") %>% 
    mutate(network = "NEM",
           year = year %>% as.numeric(),
           underlying_demand_gwh = as.numeric(underlying_demand_gwh))
  
  wem_underlying <- read_excel(jacobs_demand_data_file,
                               range = "A23:AJ33") %>% 
    rename(source = 1) %>% 
    select(-2) %>% 
    pivot_longer(cols = -source, names_to = "year", values_to = "underlying_demand_gwh") %>% 
    mutate(network = "WEM",
           year = str_remove(year, "F") %>% as.numeric(),
           underlying_demand_gwh = as.numeric(underlying_demand_gwh))
  
  nwis_underlying <- read_excel(jacobs_demand_data_file,
                                range = "A40:AJ41") %>% 
    rename(source = 1) %>% 
    select(-2) %>% 
    pivot_longer(cols = -source, names_to = "year", values_to = "underlying_demand_gwh") %>% 
    mutate(network = "NWIS",
           year = str_remove(year, "F") %>% as.numeric(),
           underlying_demand_gwh = as.numeric(underlying_demand_gwh))
  
  dkis_and_alice_underlying <- read_excel(jacobs_demand_data_file,
                                range = "A48:AJ67")[19, ] %>% 
    rename(source = 1) %>% 
    select(-2) %>% 
    mutate(across(-source, as.character)) %>%  # Convert all non-source columns to character
    pivot_longer(cols = -source, names_to = "year",  values_to = "underlying_demand_gwh") %>% 
    mutate(network = "DKIS and Alice Springs",
           year = str_remove(year, "F") %>% as.numeric(),
           underlying_demand_gwh = as.numeric(underlying_demand_gwh)) 
  
  
  mtisa_underlying <- read_excel(jacobs_demand_data_file,
                                                    range = "A61:AJ71")[10, ] %>% 
    rename(source = 1) %>% 
    select(-2) %>% 
    mutate(across(-source, as.character)) %>%  # Convert all non-source columns to character
    pivot_longer(cols = -source, names_to = "year",  values_to = "underlying_demand_gwh") %>% 
    mutate(network = "Mt Isa",
           year = str_remove(year, "F") %>% as.numeric(),
           underlying_demand_gwh = as.numeric(underlying_demand_gwh))
  
  
  
  underlying_demand <- bind_rows(nem_underlying, wem_underlying, nwis_underlying, 
                                 dkis_and_alice_underlying, mtisa_underlying) %>% 
    filter(year >= 2025)

  underlying_demand
  
}



# read_excel(esoo_2024_operational_file) %>% 
#   clean_names() %>% 
#   filter(scenario %in% c('Actual', 'Central'),
#          parent_category == 'Operational (Sent Out)',
#          #only include explicitly residential categories. residential EV will be added on top.
#          category %nin% c('Energy Efficiency', "Operational (Sent Out)"),
#          source == 'NEM',
#          year == 2025) %>% 
#   group_by(year, source, category) %>% 
#   summarise(annual_consumption_t_wh = sum(annual_consumption_t_wh)) %>% 
#   pull(annual_consumption_t_wh) %>% sum()

