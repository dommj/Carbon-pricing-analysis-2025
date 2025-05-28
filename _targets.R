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
library(scales)
library(ggarchery)

# Set target options:
tar_option_set(
)

# Run the R scripts in the R/ folder with your custom functions:
tar_source('R/helpers.R')

#load price data
tar_source('R/get_retail_data.R')
tar_source('R/get_petrol_data.R')
tar_source('R/project_petrol_data.R')
tar_source('R/get_gas_prices_data.R')

#load residential baseline study data
tar_source('R/get_rbs_fuel_end_use.R')

#load jacobs demand data
tar_source('R/load_jacobs_demand_data.R')

#project gas prices and use
tar_source('R/get_gsoo_consumption_data.R')
tar_source('R/get_gas_connections_data.R')
tar_source('R/get_benchmark_gas_consumption.R')
tar_source('R/project_residential_gas_consumption.R')
tar_source('R/get_gas_standing_offers.R')
tar_source('R/calculate_gas_bill.R')
tar_source('R/calculate_total_gas_network_revenue.R')
tar_source('R/project_gas_retail_volumetric_price.R')
tar_source('R/project_gas_connection_charges.R')

#get average household level data
tar_source('R/get_household_connections_data.R')
tar_source('R/get_average_petrol_use_per_km.R')
tar_source('R/get_aemo_vehicle_data.R')
tar_source('R/get_vehicles_per_household.R')
tar_source('R/get_average_km_per_vehicle.R')
tar_source('R/get_residential_ev_consumption.R')


tar_source('R/get_average_residential_operational_demand.R')
tar_source('R/calculate_average_residential_gas_consumption.R')
tar_source("R/calculate_average_petrol_consumption.R")


#calculate average household costs
tar_source("R/calculate_average_household_costs.R")

#create cameo usage profiles
tar_source("R/load_and_deflate_rbs_households.R")
tar_source("R/get_fuel_conversion_coefficients.R")
tar_source("R/calculate_fuel_use_conversions.R")
tar_source("R/create_rbs_fuel_consumption_profiles.R")
tar_source("R/get_rbs_electricity_consumption_data.R")
tar_source("R/get_pv_profiles.R")
tar_source("R/get_ev_consumption_profiles.R")
tar_source("R/calculate_tou_consumer_profiles.R")

#create charts
tar_source("R/create_esoo_demand_chart.R")

# Replace the target list below with your own:
tar_plan(
  
  start_year = 2025,
  end_year = 2040,
  
  ####################################################################
  #data files
  ####################################################################
  
  #retail electricity prices
  tar_file(retail_file, 'Data/AEMC price trends/nsw_25.csv'),
  
  #petrol prices
  tar_file(petrol_file, 'Data/accc_retail_fuel_04_24.csv'),
  
  #gas prices
  tar_file(gas_prices_file, 'Data/Gas/ACIL Allen Natural Gas Price Forecast.xlsx'),
  
  #gas standing offers
  tar_file(gas_standing_offers_file, 'Data/gas_standing_offers_20250331.xlsx'),
  
  #aer gas customer data
  tar_file(connection_data_aer_file, 'Data/Gas/Schedule 2 - Quarter 3 2023-24 Retail Performance Data.xlsm'),
  
  #vic gas customer data
  tar_file(connection_data_vic_file, "Data/Gas/FY24-Annual-Overview-data.xlsx"),
  
  #GSOO gas consumption data
  tar_file(gsoo_consumption_data_file, 'Data/Gas/Gas GSOO 2024.xlsx'),
  
  #AER gas benchmarks file
  tar_file(aer_gas_benchmarks_file, 'Data/aer_residential_gas_consumption_benchmarks.xlsx'),
  
  #motorvehicle use survey data
  tar_file(mv_survey_data_file, 'Data/92080DO001_202006.xls'),
  
  #esoo_2024_assumptions_workbook_file
  tar_file(esoo_2024_assumptions_workbook_file, 'Data/2024 ESOO/2024 Forecasting Assumptions Update Workbook.xlsx'),
  
  #esoo 2024 operational demand file
  tar_file(esoo_2024_operational_file, 'Data/2024 ESOO/2024 ESOO operational (sent out).xlsx'),
  
  #jacobs demand file
  tar_file(jacobs_demand_data_file, 'Data/Jacobs/Consolidated Electricity Demand Forecasts (002).xlsx'),
  
  #2024 EV workbook
  tar_file(electric_vehicle_workbook_file, 'Data/2024 ESOO/2024 Electric Vehicle workbook.xlsx'),
  
  #RBS electricity consumption data
  tar_file(rbs_electricity_consumption_data_file, 'Data/power_demand_by_time_of_use_data.xlsx'),
  
  #rbs connections estimates and fuel use data
  tar_file(rbs_outputs_data_file, 'Data/2021 RBS_OutputTablesV1.9.2-AU.xlsx'),
  
  #electric to gas conversion coefficients file
  tar_file(electric_to_gas_coefficients_file, "Data/elec_to_gas_coefficients.xlsx"),
  
  #appliance efficiency file
  
  #temperature data folder
  tar_file(temp_data_folder, 'Data/temp_data/TMYWeatherFilesEpw_20240821'),
  
  #AER retail markets 2024 file
  tar_file(aer_retail_markets_file, 'Data/Data - State of the energy market 2024 - Chapter 6 - Retail energy markets.xlsx'),
  
  #PVWatts data folder
  tar_file(pv_data_path, 'Data/Pv'),
  
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
  
  #get gas connections data
  tar_target(gas_connections_data, get_gas_connections_data(connection_data_aer_file, connection_data_vic_file)),
  
  #get gas standing offers
  tar_target(gas_standing_offers, get_gas_standing_offers(gas_standing_offers_file)),
  
  #get AER benchmark gas use
  tar_target(benchmark_gas_consumption, get_benchmark_gas_consumption(aer_gas_benchmarks_file,                                                                      gas_connections_data)),
  
  #project residential gas consumption and connection projections
  tar_target(residential_gas_consumption_projections, project_residential_gas_consumption(gas_connections_data,
                                                                                          benchmark_gas_consumption,
                                                                                          gsoo_consumption_data)),
  #calculate gas bills from standing offers
  tar_target(standing_offer_bills, calculate_gas_bill(gas_standing_offers, benchmark_gas_consumption)),
  
  #calculate total supply charge revenue (we assume this is held constant in real terms for now) (this is in 2024 dollars)
  tar_target(gas_network_charge_revenue, 
             calculate_total_gas_network_revenue(standing_offer_bills, 
                                                 gas_standing_offers, 
                                                 gas_connections_data)),
  
  #average gas volumetric charge (indexed to our volumetric price series)
  tar_target(gas_retail_volumetric_price_projections, project_gas_retail_volumetric_price(standing_offer_bills,
                                                                                          benchmark_gas_consumption,
                                                                                          gas_volume_price_data)),
  
  
  #get actual changes to revenue requirements (rather than hold constant)
  #tar_target(gas_network_costs)
  
  
  
  #Estimate projected supply charges per connection
  
  tar_target(gas_connection_charge_projections, project_gas_connection_charges(gas_network_charge_revenue, residential_gas_consumption_projections)),
  
  
  ####################################################################
  #Calculate average consumer energy use over time
  ####################################################################
  
  #number of connections
  tar_target(household_connections, get_household_connections_data(esoo_2024_assumptions_workbook_file)),
  
  #Residential EV use
  tar_target(residential_ev_econsumption, get_residential_ev_consumption_data(electric_vehicle_workbook_file)),
  
  #Electricity use
  tar_target(average_residential_operational_demand, get_average_residential_operational_demand(esoo_2024_operational_file, residential_ev_econsumption, household_connections)),
  
  #jacobs electricity demand
  tar_target(jacobs_electricity_demand, load_jacobs_demand_data(jacobs_demand_data_file)),

  
  # Gas use - average over all households with an electricity connection
  tar_target(average_gas_consumption, calculate_average_residential_gas_consumption(household_connections,
                                                                                    residential_gas_consumption_projections)),

  
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
  
  #add average PV revenue?
  
  tar_target(average_household_costs, calculate_average_household_costs(retail_price_data, 
                                                                        gas_retail_volumetric_price_projections,
                                                                        gas_network_charge_revenue,
                                                                        petrol_price_projections,
                                                                        household_connections,
                                                                        average_residential_operational_demand,
                                                                        average_gas_consumption,
                                                                        average_petrol_consumption)),
  
  
  
  ####################################################################
  #Calculate cameo consumer energy usage
  #################################################################### 
  
  #load RBS household numbers and deflate to represent 10% vacancy rate.
  tar_target(rbs_households, load_and_deflate_rbs_households(rbs_outputs_data_file)),
  
  #residential baseline fuel use data
  tar_target(rbs_fuel_end_use_by_state, get_rbs_fuel_end_use(rbs_outputs_data_file)),
  
  #load in fuel efficiency coefficients
  #TO DO!! estimate efficiency now and over time, using stock... "Stock.EndUse.Cat.Grp-State" - Done. dead end, no substantial changes in stock distribution
  
  #also building coefficients? look at ESOO / ISP methodology for breakdown?
  tar_target(fuel_conversion_coefficients, get_fuel_conversion_coefficients(electric_to_gas_coefficients_file)),
  
  tar_target(integrated_fuel_use, calculate_fuel_use_conversions(fuel_conversion_coefficients,
                                                                 rbs_outputs_data_file,
                                                                 rbs_fuel_end_use_by_state)),
  
  tar_target(rbs_fuel_consumption_profiles, create_rbs_fuel_consumption_profiles(integrated_fuel_use,
                                                                                        rbs_households)),
  
  #TO DO !! apply energy efficiency to profiles
  
  #Next: convert electricity use to ToU profiles.
  
  #get rbs electricity consumption curves
  tar_target(rbs_tou_consumption_data, get_rbs_electricity_consumption_data(rbs_electricity_consumption_data_file)),
  
  #Next: Load EV consumption profiles
  tar_target(ev_consumption_profiles, get_ev_consumption_profiles(electric_vehicle_workbook_file,
                                                                  ev_fleet_data)),
  
  #Next: calculate PV generation for each state
  tar_target(pv_profiles, get_pv_profiles(pv_data_path, rbs_households)),
  
  
  #gas cooling is not really a thing. Currently, additional gas / electricity use isn't apportioned throughout seasons. this means summer will be too high (too much gas heating applied to the summer profile) and winter too low in e.g victoria
  
  #apply fuel profiles to generate loads for all customer classes, add in pv and evs
  tar_target(tou_consumer_profiles, calculate_tou_consumer_profiles(rbs_fuel_consumption_profiles,
                                                                    rbs_fuel_end_use_by_state,
                                                                    rbs_tou_consumption_data,
                                                                    ev_consumption_profiles,
                                                                    pv_profiles,
                                                                    rbs_households)),
  
  
  #time of use tariffs

  
  #calculate petrol costs
  
  
  
  #Next: simulate battery behaviour 
  
  
  #
  
  ####################################################################
  #Create charts
  ####################################################################
  tar_target(esoo_demand_chart, create_esoo_demand_chart(esoo_2024_operational_file))
  
)

