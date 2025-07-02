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
#library(fnmate)
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
library(ggtext)

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


#tar_source('R/get_average_residential_operational_demand.R')


#average consumption - archive
# tar_source('R/non_pipe/get_esoo_electrification_per_household.R')
# tar_source('R/get_rbs_baseline_consumption.R')
# tar_source('R/calculate_displaced_gas_consumption.R')
# tar_source("R/calculate_additional_electricity_consumption.R")
# tar_source("R/calculate_average_adj_tou_consumption.R")

#calculate average household costs
#tar_source("R/calculate_average_household_costs.R")

#create cameo usage profiles
tar_source("R/load_and_deflate_rbs_households.R")
tar_source("R/get_fuel_conversion_coefficients.R")
tar_source("R/calculate_fuel_use_conversions.R")
tar_source("R/load_temperature_data.R")
tar_source("R/create_rbs_fuel_consumption_profiles.R")
tar_source("R/calculate_space_heating_tou.R")
tar_source("R/get_rbs_electricity_consumption_data.R")
tar_source("R/get_pv_profiles.R")
tar_source("R/get_ev_consumption_profiles.R")
tar_source("R/calculate_tou_consumer_profiles.R")
tar_source("R/calculate_household_energy_efficiency.R")

tar_source("R/calculate_annual_electricity_consumption_profiles.R")

#calculate cameo costs
tar_source("R/calculate_cameo_petrol_costs.R")
tar_source("R/calculate_cameo_gas_costs.R")
tar_source("R/calculate_cameo_electricity_costs.R")

#average consumption - update
tar_source("R/get_esoo_average_underlying_demand.R")
tar_source("R/calculate_average_load_shapes.R")
tar_source("R/estimate_pv_system_stock.R")
tar_source("R/calculate_average_pv_profile.R")
tar_source("R/calculate_average_profiles.R")
tar_source("R/calculate_annual_electricity_consumption_averages.R")
tar_source('R/calculate_average_residential_gas_consumption.R')
tar_source("R/calculate_average_petrol_consumption.R")

#calculate average costs
tar_source("R/calculate_average_gas_costs.R")
tar_source("R/calculate_average_electricity_costs.R")
tar_source("R/calculate_average_petrol_costs.R")

#create charts
tar_source("R/create_esoo_demand_chart.R")

# Replace the target list below with your own:
tar_plan(
  
  start_year = 2025,
  end_year = 2050,
  
  ####################################################################
  #data files
  ####################################################################
  #jacobs files
  
  #jacobs demand file
  tar_file(jacobs_demand_data_file, 'Data/Jacobs/Consolidated Electricity Demand Forecasts - DJ0604.xlsx'),
  
  #jacobs retail model file
  tar_file(jacobs_retail_model_file, "Data/Jacobs/RetailPriceProjections_Base.xlsx"),
  
  #other data files
  
  #retail electricity prices
  tar_file(retail_file, 'Data/AEMC price trends/nsw_25.csv'),
  
  #petrol prices
  tar_file(petrol_file, 'Data/accc_retail_fuel_04_24_report.csv'),
  
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
  
  #abs dwelling completions file: https://dataexplorer.abs.gov.au/vis?tm=building%20activity&pg=0&df[ds]=ABS_ABS_TOPICS&df[id]=BUILDING_ACTIVITY&df[ag]=ABS&df[vs]=1.0.0&pd=2019-Q1%2C&dq=M7...TOT.9.100.10.Q&ly[cl]=TIME_PERIOD&ly[rw]=REGION&to[TIME_PERIOD]=false
  tar_file(abs_dwelling_completions_file, "Data/dwelling_completions_abs.xlsx"),
  
  #esoo 2024 operational demand file
  tar_file(esoo_2024_operational_file, 'Data/2024 ESOO/2024 ESOO operational (sent out).xlsx'),
  
  #esoo 2020 operational demand file - used for energy efficiency to 2024
  tar_file(esoo_2020_operational_file, 'Data/2020 ESOO.xlsx'),
  
  #2023 IASR assumptions file - essentially same as 2024 for efficiency
  tar_file(iasr_2023_file, 'Data/2024 ISP chart data.xlsx'),
  
  #2024 EV workbook
  tar_file(electric_vehicle_workbook_file, 'Data/2024 ESOO/2024 Electric Vehicle workbook.xlsx'),
  
  #RBS electricity consumption data
  tar_file(rbs_electricity_consumption_data_file, 'Data/power_demand_by_time_of_use_data.xlsx'),
  
  #rbs connections estimates and fuel use data
  tar_file(rbs_outputs_data_file, 'Data/2021 RBS_OutputTablesV1.9.2-AU.xlsx'),
  
  #electric to gas conversion coefficients file
  tar_file(electric_to_gas_coefficients_file, "Data/appliance_efficiencies.xlsx"),
  
  
  #temperature data folder
  tar_file(temp_data_folder, 'Data/temp_data/TMYWeatherFilesEpw_20240821'),
  
  #AER retail markets 2024 file
  tar_file(aer_retail_markets_file, 'Data/Data - State of the energy market 2024 - Chapter 6 - Retail energy markets.xlsx'),
  
  #PVWatts data folder
  tar_file(pv_data_path, 'Data/Pv'),
  
  #CSIRO solar projections 
  tar_file(csiro_pv_prevalance_file, 'Data/solar_prevalence_csiro_24.xlsx'),
  
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
  
  ####################################################################
  #calculate gas bill data
  ####################################################################
  
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
  tar_target(household_connections, get_household_connections_data(esoo_2024_assumptions_workbook_file,
                                                                   abs_dwelling_completions_file)),
  
  #Residential EV use
  tar_target(residential_ev_econsumption, get_residential_ev_consumption_data(electric_vehicle_workbook_file)),

  
  #Petrol use - per km
  
  tar_target(average_petrol_use_per_km, get_average_petrol_use_per_km(mv_survey_data_file)),
  
  #average km per vehicle
  tar_target(average_km_per_vehicle, get_average_km_per_vehicle(mv_survey_data_file)),
  
  #fuel efficiency over time?
  
  #AEMO vehicle fleet data
  tar_target(ev_fleet_data, get_aemo_vehicle_data(electric_vehicle_workbook_file)),
  
  # Load EV consumption profiles
  tar_target(ev_consumption_profiles, get_ev_consumption_profiles(electric_vehicle_workbook_file,
                                                                  ev_fleet_data)),
  
  #vehicles per household
  #vehicles_per_household <- 1.8,
  #source: https://www.abs.gov.au/statistics/industry/tourism-and-transport/transport-census/2021#data-downloads
  tar_target(vehicles_per_household, get_vehicles_per_household(ev_fleet_data, household_connections)),
  

 

  
  ####################################################################
  #Calculate consumer energy usage from residential baseline study
  #################################################################### 
  
  #load RBS household numbers and deflate to represent 10% vacancy rate.
  tar_target(rbs_households, load_and_deflate_rbs_households(rbs_outputs_data_file)),
  
  #residential baseline fuel use data
  tar_target(rbs_fuel_end_use_by_state, get_rbs_fuel_end_use(rbs_outputs_data_file)),
  
  #load in fuel efficiency coefficients
  #TO DO: CHECK fuel conversion coefficients and write up justification based on stock etc
  tar_target(fuel_conversion_coefficients, get_fuel_conversion_coefficients(electric_to_gas_coefficients_file)),
  
  
  #get rbs electricity consumption curves
  tar_target(rbs_tou_consumption_data, get_rbs_electricity_consumption_data(rbs_electricity_consumption_data_file)),
  
  #load temperature data
  tar_target(temperature_data, load_temperature_data(temp_data_folder)),
  
  #generate space heating tou profiles for heating and cooling
  tar_target(heating_cooling_profiles, calculate_space_heating_tou(rbs_tou_consumption_data, temperature_data, 
                                                                   comfort_temp_heating = 18, comfort_temp_cooling = 18)),
  
  
  
  tar_target(integrated_fuel_use, calculate_fuel_use_conversions(fuel_conversion_coefficients,
                                                                 rbs_outputs_data_file,
                                                                 rbs_fuel_end_use_by_state,
                                                                 heating_cooling_profiles)),
  
  #calculate energy efficiency multiplier for underlying demand
  tar_target(household_energy_efficiency, calculate_household_energy_efficiency(esoo_2024_operational_file,
                                                                                household_connections,
                                                                                esoo_2020_operational_file)),
  
  ##############################################
  #calculate average consumption profile - ARCHIVE
  ##############################################
  
  #Plan:
  #see if GSOO estimates for residential gas consumption per household (average gas consumption) in 2020 (need to get 2020 GSOO for this) roughly match RBS estimates (they mostly will I think)
  
  #calculate proportional decline in gas use per household from GSOO
  
  #scale down gas use in RBS proportionally to decline in GSOO
  
  #convert the usage that has been removed into electricity consumption
  
  #boom - check with alison if this logic sounds right.
  
  #define baseline rbs gas consumption per household
  # tar_target(rbs_baseline_consumption, get_rbs_baseline_consumption(integrated_fuel_use, 
  #                                                                   rbs_households)),
  # 
  # tar_target(rbs_displaced_gas_consumption, calculate_displaced_gas_consumption(average_gas_consumption,
  #                                                                               rbs_baseline_consumption,
  #                                                                               rbs_households)),
  # 
  # 
  # tar_target(additional_electricity_consumption, calculate_additional_electricity_consumption(fuel_conversion_coefficients,
  #                                                                                             rbs_displaced_gas_consumption,
  #                                                                                             rbs_baseline_consumption)),
  # 
  # 

  
  
  # #convert additonal electricity consumption from displaced gas into tou additional electricity consumption and scale total consumption by assumed efficiency gains, then add in ev tou demand
  # tar_target(average_adj_tou_consumption, calculate_average_adj_tou_consumption(rbs_baseline_consumption,
  #                                              additional_electricity_consumption,
  #                                              rbs_tou_consumption_data,
  #                                              heating_cooling_profiles,
  #                                              rbs_households,
  #                                              household_energy_efficiency,
  #                                              ev_consumption_profiles,
  #                                              household_connections,
  #                                              ev_fleet_data)),
  
  
  #calculate number and proportion of households in each state with PV
  
  
  
  
  #calculate average PV system size per household with PV
  
  #create profiles with PV added in.
  
  
  ###############################################
  #Calculate average energy costs
  ###############################################
  
  #add average PV feed in revenue
  
  
  #we're going to want to calculate costs without degassification and without changes in solar potentially, to show relevant savings  three scenarios, no-degassification-no solar increase, degassification but no solar increase, solar increase but no degasification, solar increase and degassification - ONLY IF TIME
  
  
  ##############################################
  #create cameo consumption profiles
  ##############################################
  
  
  #create fuel consumption totals for each profile
  tar_target(rbs_fuel_consumption_profiles, create_rbs_fuel_consumption_profiles(integrated_fuel_use,
                                                                                        rbs_households)),

  
  # Calculate PV generation for each state
  tar_target(pv_profiles, get_pv_profiles(pv_data_path, rbs_households, csiro_pv_prevalance_file)),
  
  
  #apply fuel profiles to generate loads for all customer classes, add in pv and evs, apply efficiency gains
  tar_target(tou_consumer_profiles, calculate_tou_consumer_profiles(rbs_fuel_consumption_profiles,
                                                                    integrated_fuel_use,
                                                                    rbs_tou_consumption_data,
                                                                    ev_consumption_profiles,
                                                                    pv_profiles,
                                                                    rbs_households,
                                                                    heating_cooling_profiles,
                                                                    household_energy_efficiency)),
  
  
  #calculate annual electricity consumption and exports for each year by aggregating ToU profiles
  tar_target(annual_electricity_consumption_profiles, calculate_annual_electricity_consumption_profiles(tou_consumer_profiles,
                                                                                                        rbs_fuel_consumption_profiles,
                                                                                                        rbs_households)),
  
  #calculate aggregate gas consumption
  
  
  ####################################################################
  #Calculate cameo consumer energy costs - flat rate
  #################################################################### 

  #calculate gas costs
  tar_target(cameo_gas_costs, calculate_cameo_gas_costs(gas_retail_volumetric_price_projections,
                                                        gas_connection_charge_projections,
                                                        rbs_fuel_consumption_profiles,
                                                        rbs_households)),
  
  #calculate electricity costs
  tar_target(cameo_electricity_costs, calculate_cameo_electricity_costs(annual_electricity_consumption_profiles,
                                                                        retail_price_data,
                                                                        jacobs_retail_model_file)),
  
    
  #calculate petrol costs
  tar_target(cameo_petrol_costs, calculate_cameo_petrol_costs(average_petrol_use_per_km, 
                                                              average_km_per_vehicle,
                                                              petrol_price_projections)),

  
  
  ##############################################
  #calculate average consumption profile 
  ##############################################
  
  #get the underlying electricity demand for consumers
  tar_target(esoo_average_underlying_demand, get_esoo_average_underlying_demand(esoo_2024_operational_file, 
                                                household_connections)),
  
  #calculate the load shape for baseline demand and electrified demand
  tar_target(average_load_shapes, calculate_average_load_shapes(rbs_tou_consumption_data,
                                                                 integrated_fuel_use,
                                                                 heating_cooling_profiles)),
  
  
  #PV load shape can just be taken from existing PV Watts and scaled to ESOO capacity
  
  #estimate number of pv systems in each state
  tar_target(pv_system_stock, estimate_pv_system_stock(rbs_outputs_data_file,
                                                       rbs_households,
                                                       household_connections,
                                                       csiro_pv_prevalance_file)),
  
  #calculate total PV generation per system by state (and sense check implied size of system)
  
  tar_target(average_pv_profile, calculate_average_pv_profile(pv_profiles,
                                                              esoo_2024_operational_file,
                                                              pv_system_stock)),
  
  
  #now add in EVs and PV to base consumption and calculate average profiles for Solar and non-solar owners
  tar_target(average_profiles, calculate_average_profiles(esoo_average_underlying_demand,
                                                          average_load_shapes,
                                                          average_pv_profile,
                                                          ev_consumption_profiles,
                                                          ev_fleet_data)),
  
  
  #calculate annual electricity consumption and exports for each year by aggregating ToU profiles
  tar_target(annual_electricity_consumption_averages, calculate_annual_electricity_consumption_averages(average_profiles)),
  
  
  # Gas use - average over all households with an electricity connection
  tar_target(average_gas_consumption, calculate_average_residential_gas_consumption(household_connections,
                                                                                    residential_gas_consumption_projections)),
  
  #average_petrol_consumption per household
  tar_target(average_petrol_consumption, calculate_average_petrol_consumption(ev_fleet_data,
                                                                              average_petrol_use_per_km,
                                                                              average_km_per_vehicle)),
  
  
  ####################################################################
  #Calculate average consumer energy costs - flat rate
  #################################################################### 
  
  #calculate gas costs
  tar_target(average_gas_costs, calculate_average_gas_costs(gas_retail_volumetric_price_projections,
                                                            gas_connection_charge_projections,
                                                            gas_network_charge_revenue,
                                                            household_connections,
                                                            average_gas_consumption)),
  
  #calculate electricity costs
  tar_target(average_electricity_costs, calculate_average_electricity_costs(annual_electricity_consumption_averages,
                                                                        retail_price_data,
                                                                        jacobs_retail_model_file,
                                                                        pv_system_stock)),
  
  tar_target(average_petrol_costs, calculate_average_petrol_costs(petrol_price_projections,
                                                                  average_petrol_consumption)),
  
  ####################################################################
  #Create charts
  ####################################################################
  
  #need to comment out overleaf paths before sending for QC
  
  #create esoo demand chart
  tar_target(esoo_demand_chart, create_esoo_demand_chart(esoo_2024_operational_file))
  
  
  #create victorian consumption charts to compare to AEMO
  #making_prelim_vic_charts_aemo_compare.R
  
  
  #early_charts_for_savings.R
  
)

