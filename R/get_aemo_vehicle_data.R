#get vehicle data
#ignore fuel cell vehicles...

get_aemo_vehicle_data <- function(electric_vehicle_workbook_file,
                                  iasr_23_ev_workbook_file,
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

#############################
#2024 data
#############################

#I started reading in the step change data, but actually it doesn't matter bcos up to 2024 these are actuals / the same so some are prog change bcos I couldn't be bothered to change the code

#BEV data
aemo_bev_nsw_act_data_24 <- read_excel(iasr_23_ev_workbook_file,
                                    sheet = "BEV_Numbers",
                                    range = "B131:AF141") %>% 
  mutate(state = 'NSW and ACT',
         fuel_type = 'BEV') 

aemo_bev_qld_data_24 <- read_excel(iasr_23_ev_workbook_file,
                                sheet = "BEV_Numbers",
                                range = "B143:AF153") %>% 
  mutate(state = 'Qld',
         fuel_type = 'BEV')

aemo_bev_sa_data_24 <- read_excel(iasr_23_ev_workbook_file,
                               sheet = "BEV_Numbers",
                               range = "B155:AF165") %>% 
  mutate(state = 'SA',
         fuel_type = 'BEV')

aemo_bev_tas_data_24 <- read_excel(iasr_23_ev_workbook_file,
                                sheet = "BEV_Numbers",
                                range = "B167:AF177") %>% 
  mutate(state = 'Tas',
         fuel_type = 'BEV')

aemo_bev_vic_data_24 <- read_excel(iasr_23_ev_workbook_file,
                                sheet = "BEV_Numbers",
                                range = "B179:AF189") %>% 
  mutate(state = 'Vic',
         fuel_type = 'BEV')

#PHEV data
aemo_phev_nsw_act_data_24 <- read_excel(iasr_23_ev_workbook_file,
                                     sheet = "PHEV_Numbers",
                                     range = "B69:AF79") %>% 
  mutate(state = 'NSW and ACT',
         fuel_type = 'PHEV')

aemo_phev_qld_data_24 <- read_excel(iasr_23_ev_workbook_file,
                                 sheet = "PHEV_Numbers",
                                 range = "B81:AF91") %>% 
  mutate(state = 'Qld',
         fuel_type = 'PHEV')

aemo_phev_sa_data_24 <- read_excel(iasr_23_ev_workbook_file,
                                sheet = "PHEV_Numbers",
                                range = "B93:AF103") %>% 
  mutate(state = 'SA',
         fuel_type = 'PHEV')

aemo_phev_tas_data_24 <- read_excel(iasr_23_ev_workbook_file,
                                 sheet = "PHEV_Numbers",
                                 range = "B105:AF115") %>% 
  mutate(state = 'Tas',
         fuel_type = 'PHEV')

aemo_phev_vic_data_24 <- read_excel(iasr_23_ev_workbook_file,
                                 sheet = "PHEV_Numbers",
                                 range = "B117:AF127") %>% 
  mutate(state = 'Vic',
         fuel_type = 'PHEV')


#ICE data
aemo_ice_nsw_act_data_24 <- read_excel(iasr_23_ev_workbook_file,
                                    sheet = "ICE_Numbers",
                                    range = "B69:AF79") %>% 
  mutate(state = 'NSW and ACT',
         fuel_type = 'ICE')

aemo_ice_qld_data_24 <- read_excel(iasr_23_ev_workbook_file,
                                sheet = "ICE_Numbers",
                                range = "B81:AF91") %>% 
  mutate(state = 'Qld',
         fuel_type = 'ICE')

aemo_ice_sa_data_24 <- read_excel(iasr_23_ev_workbook_file,
                               sheet = "ICE_Numbers",
                               range = "B93:AF103") %>% 
  mutate(state = 'SA',
         fuel_type = 'ICE')

aemo_ice_tas_data_24 <- read_excel(iasr_23_ev_workbook_file,
                                sheet = "ICE_Numbers",
                                range = "B105:AF115") %>% 
  mutate(state = 'Tas',
         fuel_type = 'ICE')

aemo_ice_vic_data_24 <- read_excel(iasr_23_ev_workbook_file,
                                sheet = "ICE_Numbers",
                                range = "B117:AF127") %>% 
  mutate(state = 'Vic',
         fuel_type = 'ICE')


#combine data
aemo_vehicle_data_24 <- bind_rows(aemo_bev_nsw_act_data_24,
                               aemo_bev_qld_data_24,
                               aemo_bev_sa_data_24,
                               aemo_bev_tas_data_24,
                               aemo_bev_vic_data_24,
                               aemo_phev_nsw_act_data_24,
                               aemo_phev_qld_data_24,
                               aemo_phev_sa_data_24,
                               aemo_phev_tas_data_24,
                               aemo_phev_vic_data_24,
                               aemo_ice_nsw_act_data_24,
                               aemo_ice_qld_data_24,
                               aemo_ice_sa_data_24,
                               aemo_ice_tas_data_24,
                               aemo_ice_vic_data_24) %>% 
  pivot_longer(cols = contains("20"),
               names_to = "year",
               values_to = "vehicles_count") %>%
  clean_names() %>% 
  mutate(year = fy2yr(year)) %>% 
  filter(str_detect(vehicle_type, 'Residential'),
         year <= 2024) %>% 
  ungroup()



aemo_vehicle_data_all <- bind_rows(aemo_vehicle_data, aemo_vehicle_data_24) %>% 
  filter(!is.na(vehicles_count))
  

return(aemo_vehicle_data_all)

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
