#get vehicle data
#ignore fuel cell vehicles...

get_aemo_ev_consumption_data <- function(electric_vehicle_workbook_file){
  
  #BEV data
  aemo_consumption_nsw_act_data <- read_excel(electric_vehicle_workbook_file,
                                      sheet = "BEV_PHEV_Consumption (GWh)",
                                      range = "B69:AF79") %>% 
    mutate(state = 'NSW and ACT',
           fuel_type = 'BEV')
  
  aemo_consumption_qld_data <- read_excel(electric_vehicle_workbook_file,
                                  sheet = "BEV_PHEV_Consumption (GWh)",
                                  range = "B81:AF91") %>% 
    mutate(state = 'Qld',
           fuel_type = 'BEV')
  
  aemo_consumption_sa_data <- read_excel(electric_vehicle_workbook_file,
                                 sheet = "BEV_PHEV_Consumption (GWh)",
                                 range = "B93:AF103") %>% 
    mutate(state = 'SA',
           fuel_type = 'BEV')
  
  aemo_consumption_tas_data <- read_excel(electric_vehicle_workbook_file,
                                  sheet = "BEV_PHEV_Consumption (GWh)",
                                  range = "B105:AF115") %>% 
    mutate(state = 'Tas',
           fuel_type = 'BEV')
  
  aemo_consumption_vic_data <- read_excel(electric_vehicle_workbook_file,
                                  sheet = "BEV_PHEV_Consumption (GWh)",
                                  range = "B117:AF127") %>% 
    mutate(state = 'Vic',
           fuel_type = 'BEV')
  

  
  #combine data
  aemo_ev_consumption_data <- bind_rows(aemo_consumption_nsw_act_data,
                                 aemo_consumption_qld_data,
                                 aemo_consumption_sa_data,
                                 aemo_consumption_tas_data,
                                 aemo_consumption_vic_data) %>% 
    pivot_longer(cols = contains("20"),
                 names_to = "year",
                 values_to = "gwh") %>%
    clean_names() %>% 
    mutate(year = fy2yr(year)) %>% 
    filter(str_detect(vehicle_type, 'Residential')) %>%
    group_by(year, state, fuel_type) %>% 
    summarise(gwh = sum(gwh)) 

  
  
    aemo_ev_consumption_data
  
}

x <- get_aemo_ev_consumption_data('Data/2024 ESOO/2024 Electric Vehicle workbook.xlsx') %>% 
  group_by(year) %>% 
  summarise(gwh = sum(gwh))

#EVs considered to contribute to residential demand are solely residential class EVs (no light commercial) (compare to ISP residential demand figure 6)

#note that no vehicle to home EV charging is considered for light commercial vehicles.

#also compare to demand below. ESOO EV demand includes both residential and light commercial vehicles.

esoo_residential_operational_demand <- read_excel('Data/2024 ESOO/2024 ESOO operational (sent out).xlsx') %>%
  clean_names() %>%
  filter(scenario %in% c('Actual', 'Central'),
         parent_category == 'Operational (Sent Out)',
         category %in% c('Electric Vehicles'),
         sub_category != 'Business',
         region != 'NEM') %>%
  rename(state = region) %>%
  group_by(year, category) %>%
  summarise(annual_consumption_t_wh = sum(annual_consumption_t_wh))

