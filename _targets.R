# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline


##Libraries##

library(targets)
library(tarchetypes)
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
library(forcats)


# Set target options:
tar_option_set(
)

# Run the R scripts in the R/ folder with your custom functions:
tar_source('R/helpers.R')

tar_source('R/get_retail_data.R')
tar_source('R/get_petrol_data.R')
tar_source('R/project_petrol_data.R')


tar_source('R/get_gas_prices_data.R')
tar_source('R/get_gsoo_consumption_data.R')
tar_source('R/calc_gas_consumption_per_connection.R')
tar_source('R/project_gas_customers.R')
tar_source('R/get_household_connections_data.R')

tar_source('R/get_average_petrol_use_per_km.R')
tar_source('R/get_aemo_vehicle_data.R')
tar_source('R/get_vehicles_per_household.R')
tar_source('R/get_average_km_per_vehicle.R')
tar_source('R/get_residential_ev_consumption.R')


tar_source('R/get_average_residential_operational_demand.R')
tar_source('R/get_average_gas_consumption.R')
tar_source("R/calculate_average_petrol_consumption.R")



tar_source("R/create_esoo_demand_chart.R")

# Replace the target list below with your own:
tar_plan(
  
  start_year = 2025,
  end_year = 2030,
  
  ####################################################################
  #data files
  ####################################################################
  
  #retail electricity prices
  tar_file(retail_file, 'Data/AEMC price trends/nsw_25.csv'),
  
  #petrol prices
  tar_file(petrol_file, 'Data/accc_retail_fuel_04_24.csv'),
  
  #gas prices
  tar_file(gas_prices_file, 'Data/Gas/ACIL Allen Natural Gas Price Forecast.xlsx'),
  
  #aer gas customer data
  tar_file(connection_data_aer_file, 'Data/Gas/Schedule 2 - Quarter 3 2023-24 Retail Performance Data.xlsm'),
  
  #vic gas customer data
  tar_file(connection_data_vic_file, "Data/Gas/FY24-Annual-Overview-data.xlsx"),
  
  #GSOO gas consumption data
  tar_file(gsoo_consumption_data_file, 'Data/Gas/Gas GSOO 2024.xlsx'),
  
  #motorvehicle use survey data
  tar_file(mv_survey_data_file, 'Data/92080DO001_202006.xls'),
  
  #esoo_2024_assumptions_workbook_file
  tar_file(esoo_2024_assumptions_workbook_file, 'Data/2024 ESOO/2024 Forecasting Assumptions Update Workbook.xlsx'),
  
  #esoo 2024 operational demand file
  tar_file(esoo_2024_operational_file, 'Data/2024 ESOO/2024 ESOO operational (sent out).xlsx'),
  
  #2024 EV workbook
  tar_file(electric_vehicle_workbook_file, 'Data/2024 ESOO/2024 Electric Vehicle workbook.xlsx'),
  
  #RBS electricity consumption data
  tar_file(rbs_electricity_consumption_data_file, 'Data/power_demand_by_time_of_use_data.xlsx'),
  
  #rbs connections estimates and fuel use data
  tar_file(rbs_outputs_data_file, 'Data/2021 RBS_OutputTablesV1.9.2-AU.xlsx'),
  
  ####################################################################
  #load and clean price data
  ####################################################################
  
  #get retail electricity data
  tar_target(retail_price_data, get_retail_data(retail_file)),
  
  #get petrol price data
  tar_target(petrol_price_data, get_petrol_data(petrol_file)),
  
  #project out price data
  tar_target(petrol_price_projections, project_petrol_data(petrol_price_data, end_year)),
  
  #get gas volume prices
  tar_target(gas_volume_price_data, get_gas_prices_data(gas_prices_file)),
  
  #load gas consumption data
  tar_target(gsoo_consumption_data, get_gsoo_consumption_data(gsoo_consumption_data_file)),
  
  #calculate gas consumption per customer
  tar_target(gas_consumption_per_customer, calc_gas_consumption_per_connection(connection_data_aer_file, 
                                                                               connection_data_vic_file, 
                                                                               gsoo_consumption_data)),
  
  #get gas network costs data
  
  #project out estimated gas customers over time
  tar_target(gas_customer_projections, project_gas_customers(gas_consumption_per_customer, gsoo_consumption_data)),
  
  ##INSERT TARGET##
  #tar_target(gas_network_costs)
  
  #get current best standing offers data
  
  ##INSERT TARGET##
  #tar_target(gas_best_offers)
  
  #Estimate projected supply charges
  
  ##INSERT TARGET##
  #tar_target(gas_supply_charges_projections)
  
  
  ####################################################################
  #Calculate average consumer energy use over time
  ####################################################################
  
  #number of connections
  tar_target(household_connections, get_household_connections_data(esoo_2024_assumptions_workbook_file)),
  
  #Residential EV use
  tar_target(residential_ev_econsumption, get_residential_ev_consumption_data(electric_vehicle_workbook_file)),
  
  #Electricity use
  tar_target(average_residential_operational_demand, get_average_residential_operational_demand(esoo_2024_operational_file, 
                                                                                                residential_ev_econsumption, 
                                                                                                household_connections)),
  
  #Gas use - average over all households with an electricity connection
  tar_target(average_gas_consumption, get_average_gas_consumption(household_connections,
                                                               gsoo_consumption_data)),
  
  
  #Petrol use - per km
  tar_target(average_petrol_use_per_km, get_average_petrol_use_per_km(mv_survey_data_file)),
  
  #average km per vehicle
  tar_target(average_km_per_vehicle, get_average_km_per_vehicle(mv_survey_data_file)),
  
  #fuel efficiency over time?
  
  #AEMO vehicle fleet data
  tar_target(ev_fleet_data, get_aemo_vehicle_data(electric_vehicle_workbook_file)),
  
  #vehicles per household
  #vehicles_per_household <- 1.8,
  #source: https://www.abs.gov.au/statistics/industry/tourism-and-transport/transport-census/2021#data-downloads
  tar_target(vehicles_per_household, get_vehicles_per_household(ev_fleet_data, household_connections)),
  
  #average_petrol_consumption per household
  tar_target(average_petrol_consumption, calculate_average_petrol_consumption(ev_fleet_data, 
                                                                              average_petrol_use_per_km, 
                                                                              average_km_per_vehicle, 
                                                                              vehicles_per_household)),
 
  
  ####################################################################
  #Calculate average consumer energy costs over time
  #################################################################### 
  
  
  
  
  ####################################################################
  #Create charts
  ####################################################################
  tar_target(esoo_demand_chart, create_esoo_demand_chart(esoo_2024_operational_file))
  
)

