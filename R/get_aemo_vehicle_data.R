#get vehicle data
#ignore fuel cell vehicles...

get_aemo_vehicle_data <- function(electric_vehicle_workbook_file,
                                  wem_esoo_2024_ev_projections_file){

#BEV data
aemo_bev_nsw_act_data <- read_excel(electric_vehicle_workbook_file,
                            sheet = "BEV_Numbers",
                            range = "B69:AF79") %>% 
  mutate(state = 'NSW and ACT',
         fuel_type = 'BEV')

aemo_bev_qld_data <- read_excel(electric_vehicle_workbook_file,
                                    sheet = "BEV_Numbers",
                                    range = "B81:AF91") %>% 
  mutate(state = 'Qld',
         fuel_type = 'BEV')

aemo_bev_sa_data <- read_excel(electric_vehicle_workbook_file,
                                    sheet = "BEV_Numbers",
                                    range = "B93:AF103") %>% 
  mutate(state = 'SA',
         fuel_type = 'BEV')

aemo_bev_tas_data <- read_excel(electric_vehicle_workbook_file,
                                    sheet = "BEV_Numbers",
                                    range = "B105:AF115") %>% 
  mutate(state = 'Tas',
         fuel_type = 'BEV')

aemo_bev_vic_data <- read_excel(electric_vehicle_workbook_file,
                                    sheet = "BEV_Numbers",
                                    range = "B117:AF127") %>% 
  mutate(state = 'Vic',
         fuel_type = 'BEV')

aemo_bev_wa_data <- read_excel(wem_esoo_2024_ev_projections_file,
                                    sheet = "BEV_Numbers",
                                    range = "B21:M31") %>% 
  mutate(state = 'WA',
         fuel_type = 'BEV')

#PHEV data
aemo_phev_nsw_act_data <- read_excel(electric_vehicle_workbook_file,
                                    sheet = "PHEV_Numbers",
                                    range = "B69:AF79") %>% 
  mutate(state = 'NSW and ACT',
         fuel_type = 'PHEV')

aemo_phev_qld_data <- read_excel(electric_vehicle_workbook_file,
                                sheet = "PHEV_Numbers",
                                range = "B81:AF91") %>% 
  mutate(state = 'Qld',
         fuel_type = 'PHEV')

aemo_phev_sa_data <- read_excel(electric_vehicle_workbook_file,
                               sheet = "PHEV_Numbers",
                               range = "B93:AF103") %>% 
  mutate(state = 'SA',
         fuel_type = 'PHEV')

aemo_phev_tas_data <- read_excel(electric_vehicle_workbook_file,
                                sheet = "PHEV_Numbers",
                                range = "B105:AF115") %>% 
  mutate(state = 'Tas',
         fuel_type = 'PHEV')

aemo_phev_vic_data <- read_excel(electric_vehicle_workbook_file,
                                sheet = "PHEV_Numbers",
                                range = "B117:AF127") %>% 
  mutate(state = 'Vic',
         fuel_type = 'PHEV')

aemo_phev_wa_data <- read_excel(wem_esoo_2024_ev_projections_file,
                               sheet = "PHEV_Numbers",
                               range = "B21:M31") %>% 
  mutate(state = 'WA',
         fuel_type = 'PHEV')

#ICE data
aemo_ice_nsw_act_data <- read_excel(electric_vehicle_workbook_file,
                                    sheet = "ICE_Numbers",
                                    range = "B69:AF79") %>% 
  mutate(state = 'NSW and ACT',
         fuel_type = 'ICE')

aemo_ice_qld_data <- read_excel(electric_vehicle_workbook_file,
                                sheet = "ICE_Numbers",
                                range = "B81:AF91") %>% 
  mutate(state = 'Qld',
         fuel_type = 'ICE')

aemo_ice_sa_data <- read_excel(electric_vehicle_workbook_file,
                               sheet = "ICE_Numbers",
                               range = "B93:AF103") %>% 
  mutate(state = 'SA',
         fuel_type = 'ICE')

aemo_ice_tas_data <- read_excel(electric_vehicle_workbook_file,
                                sheet = "ICE_Numbers",
                                range = "B105:AF115") %>% 
  mutate(state = 'Tas',
         fuel_type = 'ICE')

aemo_ice_vic_data <- read_excel(electric_vehicle_workbook_file,
                                sheet = "ICE_Numbers",
                                range = "B117:AF127") %>% 
  mutate(state = 'Vic',
         fuel_type = 'ICE')


aemo_ice_wa_data <- read_excel(wem_esoo_2024_ev_projections_file,
                               sheet = "ICE_Numbers",
                               range = "B21:M31") %>% 
  mutate(state = 'WA',
         fuel_type = 'ICE')



#combine data
aemo_vehicle_data <- bind_rows(aemo_bev_nsw_act_data,
                               aemo_bev_qld_data,
                               aemo_bev_sa_data,
                               aemo_bev_tas_data,
                               aemo_bev_vic_data,
                               aemo_bev_wa_data,
                               aemo_phev_nsw_act_data,
                               aemo_phev_qld_data,
                               aemo_phev_sa_data,
                               aemo_phev_tas_data,
                               aemo_phev_vic_data,
                               aemo_phev_wa_data,
                               aemo_ice_nsw_act_data,
                               aemo_ice_qld_data,
                               aemo_ice_sa_data,
                               aemo_ice_tas_data,
                               aemo_ice_vic_data,
                               aemo_ice_wa_data,) %>% 
  pivot_longer(cols = contains("20"),
               names_to = "year",
               values_to = "vehicles_count") %>%
  clean_names() %>% 
  mutate(year = fy2yr(year)) %>% 
  filter(str_detect(vehicle_type, 'Residential')) %>% 
  ungroup()

  # group_by(year, state, fuel_type) %>% 
  # summarise(vehicles_count = sum(vehicles_count)) %>% 
  # group_by(year, state) %>% 
  # mutate(fleet_prop = vehicles_count/sum(vehicles_count)) %>% 
  # ungroup()
  

aemo_vehicle_data

}

# ev_fleet_data %>%
#   group_by(year) %>%
#   summarise(vehicles_count = sum(vehicles_count))
# 
# aemo_vehicle_data %>% 
#   group_by(year, vehicle_type) %>% 
#   summarise(vehicles_count = sum(vehicles_count)) %>% 
#   filter(str_detect(vehicle_type, 'Residential|Commercial'))
# 
# aemo_vehicle_data %>% 
#   mutate(vehicle_type = case_when(str_detect(vehicle_type, 'Residential') ~ 'Residential',
#                                   str_detect(vehicle_type, 'Commercial') ~ 'Commercial',
#                                   .default = vehicle_type)) %>%
#   group_by(year, vehicle_type) %>% 
#   summarise(vehicles_count = sum(vehicles_count)) %>% 
#   filter(str_detect(vehicle_type, 'Residential|Commercial'))
