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
tar_source('R/get_retail_data_aemc.R')
tar_source('R/get_jacobs_retail_prices.R')
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
tar_source('R/get_gas_best_offers.R')
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
tar_source("R/generate_battery_profiles.R")

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
  tar_file(jacobs_demand_data_file, 'Data/Jacobs/Consolidated Electricity Demand Forecasts - DJ0704.xlsx'),
  
  #jacobs retail model file base
  tar_file(jacobs_retail_model_file_base, "Data/Jacobs/RetailPriceProjections_Base.xlsx"),
  
  #jacobs retail model file reference_case
  #tar_file(jacobs_retail_model_reference_case, "Data/Jacobs/..."),
  
  
  
  #other data files
  
  #retail electricity prices
  tar_file(retail_file, 'Data/AEMC price trends/nsw_25.csv'),
  
  #petrol prices
  tar_file(petrol_file, 'Data/accc_retail_fuel_04_24_report.csv'),
  
  #gas prices
  tar_file(gas_prices_file, 'Data/Gas/ACIL Allen Natural Gas Price Forecast.xlsx'),
  
  #wa_gas_prices_file 
  tar_file(wa_gas_prices_file, "Data/Gas/2024 Natural Gas Price Forecasts workbook - Western Australia - Expected case.xlsx"),
  
  #gas standing offers
  tar_file(gas_standing_offers_file, 'Data/gas_standing_offers_20250331.xlsx'),
  
  #aer gas customer data
  tar_file(connection_data_aer_file, 'Data/Gas/Schedule 2 - Quarter 3 2023-24 Retail Performance Data.xlsm'),
  
  #vic gas customer data
  tar_file(connection_data_vic_file, "Data/Gas/FY24-Annual-Overview-data.xlsx"),
  
  #WA gas customer data
  tar_file(connection_data_wa_file, "Data/Gas/energy-reports-retailer-data-2014-onwards.xlsx"),
  
  #GSOO gas consumption data
  tar_file(gsoo_consumption_data_file, 'Data/Gas/Gas GSOO 2024.xlsx'),
  
  #WA gsoo gas consumption data
  tar_file(wa_gsoo_consumption_data_file, 'Data/Gas/WA GSOO 2024.xlsx'),
  
  #AER gas benchmarks file
  tar_file(aer_gas_benchmarks_file, 'Data/Simple electricity and gas benchmarks - published December 2020. Updated June 2021 for postcodes.xlsx'),
  
  #motorvehicle use survey data
  tar_file(mv_survey_data_file, 'Data/92080DO001_202006.xls'),
  
  #abs dwelling completions file: https://dataexplorer.abs.gov.au/vis?tm=building%20activity&pg=0&df[ds]=ABS_ABS_TOPICS&df[id]=BUILDING_ACTIVITY&df[ag]=ABS&df[vs]=1.0.0&pd=2019-Q1%2C&dq=M7...TOT.9.100.10.Q&ly[cl]=TIME_PERIOD&ly[rw]=REGION&to[TIME_PERIOD]=false
  tar_file(abs_dwelling_completions_file, "Data/dwelling_completions_abs.xlsx"),
  
  #NEM esoo 2024 operational demand file
  tar_file(esoo_2024_operational_file, 'Data/2024 ESOO/2024 ESOO operational (sent out).xlsx'),
  
  #NEM esoo 2020 operational demand file - used for energy efficiency to 2024
  tar_file(esoo_2020_operational_file, 'Data/2020 ESOO.xlsx'),
  
  #esoo_2024_assumptions_workbook_file
  tar_file(esoo_2024_assumptions_workbook_file, 'Data/2024 ESOO/2024 Forecasting Assumptions Update Workbook.xlsx'),
  
  #NEM ESOO 2024 EV workbook
  tar_file(electric_vehicle_workbook_file, 'Data/2024 ESOO/2024 Electric Vehicle workbook.xlsx'),
  
  #NEM IASR 2023 EV workbook (to get 2024 numbers)
  tar_file(iasr_23_ev_workbook_file, 'Data/Detailed Electric Vehicle databook.xlsx'),
  
  #WEM ESOO 2024 operational demand file
  tar_file(wem_esoo_2024_operational_file, "Data/2024 ESOO/2024 WEM ESOO operational (sent out).xlsx"),
  
  #WEM ESOO 2024 Data Register
  tar_file(wem_esoo_2024_data_register_file, "Data/2024 ESOO/2024 WEM ESOO Data Register.xlsx"),
  
  #WEM ESOO 2024 electric vehicle projections
  tar_file(wem_esoo_2024_ev_projections_file, "Data/2024 ESOO/2024 WEM ESOO EV Projections.xlsx"),
  
  
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
  
  #get retail electricity data from AEMC price trends
  tar_target(retail_price_data_aemc, get_retail_data_aemc(retail_file)),
  
  #get retail prices from jacobs sheets
  tar_target(jacobs_retail_prices, get_jacobs_retail_prices(jacobs_retail_model_file_base,
                                                            household_connections)),
  
  
  #get petrol price data
  tar_target(petrol_price_data, get_petrol_data(petrol_file)),
  
  #project out price data
  tar_target(petrol_price_projections, project_petrol_data(petrol_price_data, end_year)),
  
  #get gas volume prices
  tar_target(gas_volume_price_data, get_gas_prices_data(gas_prices_file,
                                                        wa_gas_prices_file)),
  
  ####################################################################
  #calculate gas bill data
  ####################################################################
  
  #load gas consumption data
  tar_target(gsoo_consumption_data, get_gsoo_consumption_data(gsoo_consumption_data_file,
                                                              wa_gsoo_consumption_data_file)),
  
  #get gas connections data
  tar_target(gas_connections_data, get_gas_connections_data(connection_data_aer_file, connection_data_vic_file,
                                                            connection_data_wa_file)),
  
  #get gas standing offers
  tar_target(gas_standing_offers, get_gas_standing_offers(gas_standing_offers_file)),
  
  #get gas best offers
  tar_target(gas_best_offers, get_gas_best_offers(gas_standing_offers_file)),
  
  #get AER benchmark gas use
  tar_target(benchmark_gas_consumption, get_benchmark_gas_consumption(aer_gas_benchmarks_file,                                                                      gas_connections_data)),
  
  #project residential gas consumption and connection projections
  tar_target(residential_gas_consumption_projections, project_residential_gas_consumption(gas_connections_data,
                                                                                          benchmark_gas_consumption,
                                                                                          gsoo_consumption_data)),
  # #calculate gas bills from standing offers
  # tar_target(standing_offer_bills, calculate_gas_bill(gas_standing_offers, benchmark_gas_consumption)),
  
  #calculate gas bills from best offers
  tar_target(best_offer_bills, calculate_gas_bill(gas_best_offers, benchmark_gas_consumption)),
  
  #CHANGED to BEST OFFER BELOW
  
  #calculate total supply charge revenue (we assume this is held constant in real terms for now) (this is in 2025 dollars)
  tar_target(gas_network_charge_revenue, 
             calculate_total_gas_network_revenue(best_offer_bills, #CHANGED
                                                 gas_standing_offers, 
                                                 gas_connections_data)),
  
  #average gas volumetric charge (indexed to our volumetric price series)
  tar_target(gas_retail_volumetric_price_projections, project_gas_retail_volumetric_price(best_offer_bills, #CHANGED
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
                                                                   wem_esoo_2024_data_register_file,
                                                                   abs_dwelling_completions_file)),
  
  #Residential EV use
  tar_target(residential_ev_econsumption, get_residential_ev_consumption_data(electric_vehicle_workbook_file,
                                                                              wem_esoo_2024_ev_projections_file)),

  
  #Petrol use - per km
  
  tar_target(average_petrol_use_per_km, get_average_petrol_use_per_km(mv_survey_data_file)),
  
  #average km per vehicle
  tar_target(average_km_per_vehicle, get_average_km_per_vehicle(mv_survey_data_file)),
  
  #fuel efficiency over time?
  
  #AEMO vehicle fleet data
  tar_target(ev_fleet_data, get_aemo_vehicle_data(electric_vehicle_workbook_file,
                                                  iasr_23_ev_workbook_file,
                                                  wem_esoo_2024_ev_projections_file)),
  
  # Load EV consumption profiles
  tar_target(ev_consumption_profiles, get_ev_consumption_profiles(electric_vehicle_workbook_file,
                                                                  wem_esoo_2024_ev_projections_file,
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
                                                                                esoo_2020_operational_file,
                                                                                wem_esoo_2024_operational_file)),
  
  
  
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
  
  tar_target(tou_consumer_profiles_w_batteries, generate_battery_profiles(tou_consumer_profiles, 
                                                                          #trace definining characteristics
                                                                          c("cooking", "water_heating", "space_heating", "ev", "pv", "state", "year", "season"), 
                                                                          battery_capacity = 11)),
  
  tar_target(all_tou_consumer_profiles, bind_rows(tou_consumer_profiles %>% mutate(battery = F),
                                                  
                                                  #combine with profiles of consumers with PV and batteries
                                                  bind_rows(tou_consumer_profiles %>% 
                                                              filter(pv == T) %>% 
                                                              mutate(battery = T),
                                                            
                                                            #add in battery end_use
                                                            tou_consumer_profiles_w_batteries %>% 
                                                    mutate(power_kwh = battery_action) %>% 
                                                    select(-c(battery_action, battery_level, net_power))))),
  
  #calculate annual electricity consumption and exports for each year by aggregating ToU profiles
  tar_target(annual_electricity_consumption_profiles, calculate_annual_electricity_consumption_profiles(all_tou_consumer_profiles,
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
                                                                        jacobs_retail_prices)),
  
    
  #calculate petrol costs
  tar_target(cameo_petrol_costs, calculate_cameo_petrol_costs(average_petrol_use_per_km, 
                                                              average_km_per_vehicle,
                                                              petrol_price_projections)),

  
  
  ##############################################
  #calculate average consumption profile 
  ##############################################
  
  #get the underlying electricity demand for consumers
  tar_target(esoo_average_underlying_demand, get_esoo_average_underlying_demand(esoo_2024_operational_file, 
                                                                                wem_esoo_2024_operational_file,
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
  
  #proportion of pv systems with batteries
  
  
  
  #calculate total PV generation per system by state (and sense check implied size of system)
  
  tar_target(average_pv_profile, calculate_average_pv_profile(pv_profiles,
                                                              esoo_2024_operational_file,
                                                              wem_esoo_2024_operational_file,
                                                              pv_system_stock)),
  
  
  #now add in EVs and PV to base consumption and calculate average profiles for Solar and non-solar owners
  tar_target(average_profiles, calculate_average_profiles(esoo_average_underlying_demand,
                                                          average_load_shapes,
                                                          average_pv_profile,
                                                          ev_consumption_profiles,
                                                          ev_fleet_data)),
  
  tar_target(average_profiles_w_batteries, generate_battery_profiles(average_profiles, 
                                                                          #trace definining characteristics
                                                                          c("pv", "electrification", "state", "year", "season"), 
                                                                          battery_capacity = 11) %>% 
               rename(source = end_use)),
  
  
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
  
  #USE TAR_MAP TO CREATE RESULTS ACROSS INPUT RETAIL SPREADSHEETS
  
  #calculate electricity costs
  tar_target(average_electricity_costs, calculate_average_electricity_costs(annual_electricity_consumption_averages,
                                                                            jacobs_retail_prices,
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

