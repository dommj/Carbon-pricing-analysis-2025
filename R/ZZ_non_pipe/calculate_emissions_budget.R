#create carbon budget
library(targets)
library(tarchetypes)
library(dplyr)
library(tidyr) 
library(readr)
library(janitor) 
library(ggplot2)
library(readabs)
library(fy)
library(lubridate)
library(readxl)
library(stringr)
library(purrr)
library(unpivotr)
library(tidyxl)

source("R/helpers.R")

######################################
#load national demand
######################################

#just kept this clunky so all slices are available underneath
tar_load(jacobs_electricity_demand)

total_demand_nem <- jacobs_electricity_demand %>% 
  filter(source == "Underlying Demand - Operational Sent out + Rooftop PV",
         network == "NEM") %>% 
  select(-source)

total_demand_wem <- jacobs_electricity_demand %>% 
  filter(source == "Underlying Demand - Operational Sent out + Rooftop PV",
         network == "WEM") %>% 
  select(-source)

total_demand_other <- jacobs_electricity_demand %>% 
  filter(network %nin% c("NEM", "WEM")) %>% 
  select(-source)


total_demand_national <- bind_rows(total_demand_nem, total_demand_wem, total_demand_other)


aggregate_national_demand <- total_demand_national %>% 
  group_by(year) %>% 
  summarise(underlying_demand_twh = sum(underlying_demand_gwh) / 1000) #convert to Terrawatt hours

######################################
#load CCA national emissions intensity
######################################

cca_path <- "Data/20240911_EP2024-4366_Modelling_Sectoral_Pathways_to_Net_Zero_Emissions_Section 2_Sectors_Charts (1).xlsx"

cca_intensities <- read_excel(cca_path, 
                              sheet = "Fig 13",
                              skip = 6) %>% 
  rename(scenario = 1) %>% 
  pivot_longer(cols = contains("20"), names_to = "year", values_to = "mt_co2e_twh") %>% 
  mutate(year = as.numeric(year))


#interpolate data and select the A40/G1.5 trajectory

cca_intensities_A40_G1.5 <- cca_intensities %>%
  complete(scenario, year = seq(2025, 2050, by = 1)) %>%
  # Group by scenario to interpolate within each scenario
  group_by(scenario) %>%
  # Use approx to linearly interpolate missing values
  arrange(year) %>%
  mutate(mt_co2e_twh = approx(x = year[!is.na(mt_co2e_twh)], 
                              y = mt_co2e_twh[!is.na(mt_co2e_twh)], 
                              xout = year)$y) %>%
  ungroup() %>% 
  filter(scenario == "A40/G1.5")


##########################################
#Calculate emissions budget
##########################################
national_emissions_cca <- full_join(cca_intensities_A40_G1.5, aggregate_national_demand, by = join_by(year)) %>% 
  mutate(mt_co2e = underlying_demand_twh * mt_co2e_twh, #emissions = electricity demand * emissions intensity 
         scenario = "Model intensity")


emissions_budget_cca <- national_emissions_cca %>% 
  filter(!is.na(scenario)) %>% 
  group_by(scenario) %>% 
  summarise(budget = sum(mt_co2e))

#our total emissions budget is...
emissions_budget_cca



########################## **** QC Finished **** ################################


############################################
#Plot emissions intensity trajectory relative to ISP
############################################

#load ISP emissions step change data
step_change_ems <- read_excel('Data/2024 ISP generation and storage outlook/Core/2024 ISP - Step Change - Core.xlsx', 
                              sheet = 'Summary',
                              range = "H210:AK211") %>% 
  clean_names() %>% 
  pivot_longer(cols = contains('20'), 
               names_to = 'year',
               values_to = 'mt_co2e') %>% 
  rename(scenario = 1) %>% 
  mutate(scenario = '2024 ISP - Step Change',
         year = as.numeric(sub(".*_(\\d{2})", "20\\1", year))) %>% 
  select(- total)

#load step change generation data
step_change_gen <- read_excel('Data/2024 ISP generation and storage outlook/Core/2024 ISP - Step Change - Core.xlsx', 
                              sheet = 'Summary',
                              range = "H57:AJ72") %>% 
  clean_names() %>% 
  pivot_longer(cols = contains('20'), 
               names_to = 'year',
               values_to = 'twh_gen') %>% 
  mutate(year = as.numeric(sub(".*_(\\d{2})", "20\\1", year))) %>% 
  group_by(year) %>% 
  summarise(twh_gen = sum(twh_gen))


nem_intensity <- step_change_gen %>% 
  full_join(step_change_ems , join_by(year)) %>% 
  mutate(mt_co2e_twh = mt_co2e / twh_gen)


intensities <- bind_rows(nem_intensity, national_emissions_cca)

intensities %>% 
  ggplot(aes(x= year, y = mt_co2e_twh, colour = scenario)) +
  geom_line() +
  theme_grattan(legend = "bottom") 






#################################
#load NGER data
#################################

nger_23_24 <- read_excel("Data/greenhouse-and-energy-information-designated-generation-facility-2023-24.xlsx",
                         skip = 3) %>% 
  clean_names() %>% 
  select(facility_name, state, electricity_production_m_wh, total_scope_1_emissions_t_co2_e, grid_connected, grid)

nger_23_24_grid_agg <- nger_23_24 %>% 
  filter(facility_name != "Corporate Total",
         !is.na(facility_name)) %>% 
  group_by(grid, grid_connected) %>% 
  summarise(electricity_production_g_wh = sum(electricity_production_m_wh) / 1e3)



off_grid <- nger_23_24 %>% 
  filter(grid_connected != "On")









