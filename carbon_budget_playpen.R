# Load tidyverse packages
library(dplyr)
library(tidyr) 
library(readr)
library(janitor) 
library(ggplot2)
library(readabs)
library(fy)
library(fnmate)
library(lubridate)
library(readxl)
library(stringr)
library(purrr)
library(unpivotr)
library(tidyxl)
library(grattantheme)




years <- seq(2025, 2050, by = 5)
emissions_1.5 <- c(143.5656, 57.30034, 18.35925, 4.94271, 4.296637, 2.707967) * (525.8 / 767.25)

emissions_2 <- c(146.1781147,	72.40105949,	27.17687801,	26.03100873,	11.16146065,	3.593932279) * (525.8 / 657.9)


# Create a data frame 
emissions_df_1.5 <- data.frame(year = years, emission = emissions_1.5) %>% 
  mutate(scenario = "A40/G1.5")

emissions_df_2 <- data.frame(year = years, emission = emissions_2) %>% 
  mutate(scenario = "A50/G2")

emissions_df <- bind_rows(emissions_df_1.5, emissions_df_2)

interpolate_emissions <- function(scenario_name, start_year, end_year) {
  # Filter data for scenario, start and end years
  scenario_data <- emissions_df %>% filter(scenario == scenario_name)
  start_data <- scenario_data %>% filter(year == start_year)
  end_data <- scenario_data %>% filter(year == end_year)
  
  # Calculate the slope
  slope <- (end_data$emission - start_data$emission) / (end_year - start_year)
  
  # Generate yearly data points
  years_seq <- seq(start_year, end_year)
  emissions_seq <- start_data$emission + slope * (years_seq - start_year)
  
  return(data.frame(
    year = years_seq, 
    emission = emissions_seq,
    scenario = scenario_name
  ))
}

# Get unique scenarios and years
scenarios <- unique(emissions_df$scenario)
years <- unique(emissions_df$year)

# Generate interpolated data for all scenarios and year pairs
interpolated_data <- map_dfr(scenarios, function(s) {
  map_dfr(1:(length(years)-1), function(i) {
    interpolate_emissions(s, years[i], years[i+1])
  })
}) %>% 
  distinct() # Remove duplicated combinations

# Calculate area under the curve (cumulative emissions) for each scenario
cumulative_emissions <- interpolated_data %>%
  arrange(scenario, year) %>%
  group_by(scenario) %>%
  mutate(next_emission = lead(emission),
         next_year = lead(year),
         segment_area = 0.5 * (emission + next_emission) * (next_year - year)) %>%
  filter(!is.na(segment_area)) %>%
  summarise(total_emissions = sum(segment_area)) %>%
  ungroup()

interpolated_data %>% 
ggplot(aes(x = year, y = emission, fill = scenario))+
  geom_line()

################################################
#emissions intensity
################################################

cca_intensities <- read_excel("C:/Users/domijones/Downloads/20240911_EP2024-4366_Modelling_Sectoral_Pathways_to_Net_Zero_Emissions_Section 2_Sectors_Charts (1).xlsx", 
                              sheet = "Fig 13",
                              skip = 6) %>% 
  rename(scenario = 1) %>% 
  pivot_longer(cols = contains("20"), names_to = "year", values_to = "mt_c02e_twh") %>% 
  mutate(year = as.numeric(year))
  

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

step_change_gen <- read_excel('Data/2024 ISP generation and storage outlook/Core/2024 ISP - Step Change - Core.xlsx', 
                              sheet = 'Summary',
                              range = "H57:AJ72") %>% 
  clean_names() %>% 
  pivot_longer(cols = contains('20'), 
               names_to = 'year',
               values_to = 'twh_gen') %>% 
  rename(technology = 1) %>% 
  mutate(year = as.numeric(sub(".*_(\\d{2})", "20\\1", year)),
         type = case_when(str_detect(technology, 'coal')|
                            str_detect(technology, 'gas') ~ 'non-renewable',
                          str_detect(technology, '[w|W]ind')|
                            str_detect(technology, 'solar')|
                            str_detect(technology, 'PV')|
                            str_detect(technology, 'renewable')|
                            str_detect(technology, 'Hydro')|
                            str_detect(technology, 'DSP') ~ 'renewable',
                          .default = 'storage')) 

total_step_gen <- step_change_gen %>% 
  group_by(year) %>%
  summarise(twh_gen = sum(twh_gen))

step_change_intensity <- total_step_gen %>% 
  full_join(step_change_ems %>% select(-scenario), join_by(year)) %>% 
  mutate(mt_c02e_twh = (mt_co2e) / (twh_gen),
         scenario = "ISP - Step Change") %>% 
  select(scenario, year, mt_c02e_twh)

#green export...

green_exp_ems <- read_excel('Data/2024 ISP generation and storage outlook/Core/2024 ISP - Green Energy Exports - Core.xlsx', 
                              sheet = 'Summary',
                              range = "H210:AK211") %>% 
  clean_names() %>% 
  pivot_longer(cols = contains('20'), 
               names_to = 'year',
               values_to = 'mt_co2e') %>% 
  rename(scenario = 1) %>% 
  mutate(scenario = '2024 ISP - Green Exports',
         year = as.numeric(sub(".*_(\\d{2})", "20\\1", year))) %>% 
  select(- total)

green_exp_gen <- read_excel('Data/2024 ISP generation and storage outlook/Core/2024 ISP - Green Energy Exports - Core.xlsx', 
                              sheet = 'Summary',
                              range = "H57:AJ72") %>% 
  clean_names() %>% 
  pivot_longer(cols = contains('20'), 
               names_to = 'year',
               values_to = 'twh_gen') %>% 
  rename(technology = 1) %>% 
  mutate(year = as.numeric(sub(".*_(\\d{2})", "20\\1", year)),
         type = case_when(str_detect(technology, 'coal')|
                            str_detect(technology, 'gas') ~ 'non-renewable',
                          str_detect(technology, '[w|W]ind')|
                            str_detect(technology, 'solar')|
                            str_detect(technology, 'PV')|
                            str_detect(technology, 'renewable')|
                            str_detect(technology, 'Hydro')|
                            str_detect(technology, 'DSP') ~ 'renewable',
                          .default = 'storage')) 

total_green_exp_gen <- green_exp_gen %>% 
  group_by(year) %>%
  summarise(twh_gen = sum(twh_gen))

green_exp_intensity <- total_green_exp_gen %>% 
  full_join(step_change_ems %>% select(-scenario), join_by(year)) %>% 
  mutate(mt_c02e_twh = (mt_co2e) / (twh_gen),
         scenario = "ISP - Green Exports") %>% 
  select(scenario, year, mt_c02e_twh)

intensities <- bind_rows(green_exp_intensity, step_change_intensity, cca_intensities) 
  

intensities %>% 
  ggplot(aes(x = year, y = mt_c02e_twh, fill = scenario, colour = scenario)) +
  geom_line(size = 1) +
  geom_point() +
  scale_x_continuous(limits = c(2020, 2050)) +
  grattan_y_continuous(limits = c(0, 1.0)) +
  theme_grattan(legend = "top") +
  
  labs(title = "title",
       subtitle = "Emissions intensity of the electricy sector (Mt C02-e)")


grattan_save_pptx(p = ggplot2::last_plot(), "emissions_int_trajs.pptx", type = "fullslide")





