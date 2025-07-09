get_residential_ev_consumption_data <- function(electric_vehicle_workbook_file,
                                                wem_esoo_2024_ev_projections_file){
  
  
  aemo_consumption_nsw_act_data <- read_excel(electric_vehicle_workbook_file,
                                              sheet = "BEV_PHEV_Consumption (GWh)",
                                              range = "B69:AF79") %>% 
    mutate(state = 'NSW and ACT')
  
  aemo_consumption_qld_data <- read_excel(electric_vehicle_workbook_file,
                                          sheet = "BEV_PHEV_Consumption (GWh)",
                                          range = "B81:AF91") %>% 
    mutate(state = 'Qld')
  
  aemo_consumption_sa_data <- read_excel(electric_vehicle_workbook_file,
                                         sheet = "BEV_PHEV_Consumption (GWh)",
                                         range = "B93:AF103") %>% 
    mutate(state = 'SA')
  
  aemo_consumption_tas_data <- read_excel(electric_vehicle_workbook_file,
                                          sheet = "BEV_PHEV_Consumption (GWh)",
                                          range = "B105:AF115") %>% 
    mutate(state = 'Tas')
  
  aemo_consumption_vic_data <- read_excel(electric_vehicle_workbook_file,
                                          sheet = "BEV_PHEV_Consumption (GWh)",
                                          range = "B117:AF127") %>% 
    mutate(state = 'Vic')
  
  
  aemo_consumption_wa_data <- read_excel(wem_esoo_2024_ev_projections_file,
                                         sheet = "BEV_PHEV_Consumption (GWh)",
                                         range = "B21:M31") %>% 
    mutate(state = 'WA')
  
  
  #combine data
  aemo_ev_consumption_data <- bind_rows(aemo_consumption_nsw_act_data,
                                        aemo_consumption_qld_data,
                                        aemo_consumption_sa_data,
                                        aemo_consumption_tas_data,
                                        aemo_consumption_vic_data,
                                        aemo_consumption_wa_data) %>% 
    pivot_longer(cols = contains("20"),
                 names_to = "year",
                 values_to = "bev_phev_gwh") %>%
    clean_names() %>% 
    mutate(year = fy2yr(year),
           category = "Residential EV") %>% 
    filter(str_detect(vehicle_type, 'Residential')) %>%
    group_by(year, state, category) %>% 
    summarise(annual_consumption_t_wh = sum(bev_phev_gwh) / 1000) 
  
  
  aemo_ev_consumption_data %>% 
    ggplot(aes(x = year, y = annual_consumption_t_wh, colour = state)) +
    geom_line()
  
  return(aemo_ev_consumption_data)
  
}

# x <- get_residential_ev_consumption_data('Data/2024 ESOO/2024 Electric Vehicle workbook.xlsx') %>% 
#   group_by(year) %>% 
#   summarise(bev_phev_gwh = sum(bev_phev_gwh))


#get_residential_ev_consumption_data('Data/2024 ESOO/2024 Electric Vehicle workbook.xlsx')

