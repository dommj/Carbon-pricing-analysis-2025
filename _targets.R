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
tar_source('R/get_jacobs_retail_prices.R')
tar_source("R/get_electricity_tariffs.R")
tar_source('R/get_petrol_data.R')
#tar_source('R/project_petrol_data.R')
tar_source('R/get_gas_prices_data.R')

#load residential baseline study data
tar_source('R/get_rbs_fuel_end_use.R')


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
#tar_source('R/get_residential_ev_consumption.R')
tar_source("R/get_ev_consumption_profiles.R")
tar_source("R/project_ev_efficiency.R")
tar_source("R/calculate_scaled_ev_consumption_profiles.R")


#create cameo usage profiles
tar_source("R/load_and_deflate_rbs_households.R")
tar_source("R/get_fuel_conversion_coefficients.R")
tar_source("R/calculate_fuel_use_conversions.R")
tar_source("R/load_temperature_data.R")
tar_source("R/create_rbs_fuel_consumption_profiles.R")
tar_source("R/calculate_space_heating_tou.R")
tar_source("R/get_rbs_electricity_consumption_data.R")
tar_source("R/get_pv_profiles.R")
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
tar_source("R/estimate_battery_prevalence.R")
tar_source("R/calculate_average_pv_profile.R")
tar_source("R/calculate_average_profiles.R")
tar_source("R/calculate_annual_electricity_consumption_averages.R")
tar_source('R/calculate_average_residential_gas_consumption.R')
tar_source("R/calculate_average_petrol_consumption.R")
tar_source("R/get_jacobs_curtailment.R")

#calculate average costs
tar_source("R/calculate_average_gas_costs.R")
tar_source("R/calculate_average_electricity_costs.R")
tar_source("R/calculate_average_petrol_costs.R")

#create charts
tar_source("R/compile_average_net_costs.R")

tar_source("R/plot_energy_wallet.R")
tar_source("R/plot_gas_supply_charges.R")
tar_source("R/plot_average_profiles.R")

#tar_source("R/create_esoo_demand_chart.R")

# Replace the target list below with your own:
tar_plan(
  
  ####################################################################
  #data files
  ####################################################################

  ###############
  #Price files
  ###############
  
  tar_file(electricity_tariffs_file, "Data/electricity_tariffs_july_25.xlsx"),
  
  #petrol prices
  tar_file(petrol_file, 'Data/accc_petrol_prices_q1_2025.xlsx'),
  
  #gas prices
  tar_file(gas_prices_file, 'Data/Gas/ACIL Allen Natural Gas Price Forecast.xlsx'),
  
  #wa_gas_prices_file 
  tar_file(wa_gas_prices_file, "Data/Gas/2024 Natural Gas Price Forecasts workbook - Western Australia - Expected case.xlsx"),
  
  #gas standing offers
  tar_file(gas_standing_offers_file, 'Data/gas_offers.xlsx'),
  
  ######################################
  #Gas connections and consumption data
  ######################################
  
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
  
  #################################
  #ESOO projections and assumptions
  #################################
  
  #NEM esoo 2024 operational demand file
  tar_file(esoo_2024_operational_file, 'Data/2024 ESOO/2024 ESOO operational (sent out).xlsx'),
  
  #NEM esoo 2020 operational demand file - used for energy efficiency to 2024
  tar_file(esoo_2020_operational_file, 'Data/2020 ESOO.xlsx'),
  
  #esoo_2024_assumptions_workbook_file
  tar_file(esoo_2024_assumptions_workbook_file, 'Data/2024 ESOO/2024 Forecasting Assumptions Update Workbook.xlsx'),
  
  #NEM ESOO 2024 EV workbook
  tar_file(electric_vehicle_workbook_file, 'Data/2024 ESOO/2024 Electric Vehicle workbook.xlsx'),
  
  #CSIRO EV efficiency values page 30 Electric vehicle projections 2024, scraped with webplot digitiser
  tar_file(ev_efficiency_file, "Data/csiro_ev_efficiency_kw_km.csv"),
  
  #NEM IASR 2023 EV workbook (to get 2024 numbers)
  tar_file(iasr_23_ev_workbook_file, 'Data/Detailed Electric Vehicle databook.xlsx'),
  
  #WEM ESOO 2024 operational demand file
  tar_file(wem_esoo_2024_operational_file, "Data/2024 ESOO/2024 WEM ESOO operational (sent out).xlsx"),
  
  #WEM ESOO 2024 Data Register
  tar_file(wem_esoo_2024_data_register_file, "Data/2024 ESOO/2024 WEM ESOO Data Register.xlsx"),
  
  #WEM ESOO 2024 electric vehicle projections
  tar_file(wem_esoo_2024_ev_projections_file, "Data/2024 ESOO/2024 WEM ESOO EV Projections.xlsx"),
  
  
  #motorvehicle use survey data
  tar_file(mv_survey_data_file_20, 'Data/smvu_2020.xls'),
  tar_file(mv_survey_data_file_18, 'Data/smvu_2018.xls'),
  
  #abs dwelling completions file: https://dataexplorer.abs.gov.au/vis?tm=building%20activity&pg=0&df[ds]=ABS_ABS_TOPICS&df[id]=BUILDING_ACTIVITY&df[ag]=ABS&df[vs]=1.0.0&pd=2019-Q1%2C&dq=M7...TOT.9.100.10.Q&ly[cl]=TIME_PERIOD&ly[rw]=REGION&to[TIME_PERIOD]=false
  tar_file(abs_dwelling_completions_file, "Data/dwelling_completions_abs.xlsx"),
  
  ################################
  #Residential Baseline Study data
  ################################
  
  #RBS electricity consumption data
  tar_file(rbs_electricity_consumption_data_file, 'Data/power_demand_by_time_of_use_data.xlsx'),
  
  #rbs connections estimates and fuel use data
  tar_file(rbs_outputs_data_file, 'Data/2021 RBS_OutputTablesV1.9.2-AU.xlsx'),
  
  
  #############
  #MISC
  #############
  
  #electric to gas conversion coefficients file
  tar_file(electric_to_gas_coefficients_file, "Data/appliance_efficiencies.xlsx"),
  
  #temperature data folder
  tar_file(temp_data_folder, 'Data/temp_data/TMYWeatherFilesEpw_20240821'),
  
  #PVWatts data folder
  tar_file(pv_data_path, 'Data/Pv'),
  
  #CSIRO solar and battery projections 
  tar_file(csiro_pv_prevalance_file, 'Data/solar_prevalence_csiro_24.xlsx'),
  
  #ice vehicle efficiency file
  #tar_file(ice_efficiency_file, "Data/ICE fuel efficiency.csv"),
  
  ####################################################################
  #load and clean price data
  ####################################################################
  
  #MUST convert everything to 2025 Q2 dollars. (same as jacobs retail and scraped tarifs)

  
  #get retail prices from jacobs sheets
  
  #creates targets named:
  #jacobs_retail_model_reference_case
  #jacobs_retail_model_1_5_opt1
  #jacobs_retail_model_1_5_opt2
  
  #jacobs_retail_prices_reference_case
  #jacobs_retail_prices_1_5_opt1
  #jacobs_retail_prices_1_5_opt2
  
  
  tar_map(
    tibble(scenario = c("reference_case", "1_5_opt1", "1_5_opt2", "2_opt1", "2_opt2"),
           filepath = c("Data/Jacobs/RetailPriceProjections_Ref.xlsx",
                        "Data/Jacobs/RetailPriceProjections_1_5_Opt1.xlsx", 
                        "Data/Jacobs/RetailPriceProjections_1_5_Opt2.xlsx",
                        "Data/Jacobs/RetailPriceProjections_2_Opt1.xlsx", 
                        "Data/Jacobs/RetailPriceProjections_2_Opt2.xlsx")),
    names = scenario,
    tar_target(jacobs_retail_model, filepath, format = "file"),
    tar_target(jacobs_retail_prices, 
               get_jacobs_retail_prices(jacobs_retail_model, household_connections))
  ),
  
  #combine prices for all scenarios
  tar_target(jacobs_retail_prices, bind_rows(jacobs_retail_prices_reference_case,
                                             jacobs_retail_prices_1_5_opt1,
                                             jacobs_retail_prices_1_5_opt2,
                                             jacobs_retail_prices_2_opt1,
                                             jacobs_retail_prices_2_opt2,)),
  
  
  tar_target(retail_electricity_tariffs, get_electricity_tariffs(electricity_tariffs_file,
                                                                 jacobs_retail_prices,
                                                                 household_connections)),
  
  #get petrol price data
  tar_target(petrol_price_data, get_petrol_data(petrol_file)),
  
  # #project out price data
  # tar_target(petrol_price_projections, project_petrol_data(petrol_price_data, end_year)),
  
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
  #tar_target(gas_standing_offers, get_gas_standing_offers(gas_standing_offers_file)),
  
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
  
  #calculate gas bills from best offers - this script isn't really used to the full extent anymore, it used to calculate the best bill from a range of offers but we now simply take the best offer as calculated by government comparison sites. The connection charge costs are used to calculate total revenue but this can easily be derived by multiplying connection charges by 365 days and by connections.
  
  #NO REAL QC NEEDED (just to save you time), Happy to discuss.
  tar_target(best_offer_bills, calculate_gas_bill(gas_best_offers, benchmark_gas_consumption)),
  
  
  #calculate total supply charge revenue (we assume this is held constant in real terms for now) (this is in 2025 dollars)
  tar_target(gas_network_charge_revenue, 
             calculate_total_gas_network_revenue(best_offer_bills,  
                                                 gas_connections_data)),
  
  #average gas volumetric charge (indexed to our volumetric price series)
  tar_target(gas_retail_volumetric_price_projections, project_gas_retail_volumetric_price(best_offer_bills, 
                                                                                          benchmark_gas_consumption,
                                                                                          gas_volume_price_data)),
  
  
  #Estimate projected supply charges per connection
  
  tar_target(gas_connection_charge_projections, project_gas_connection_charges(gas_network_charge_revenue, residential_gas_consumption_projections)),
  
  
  ####################################################################
  #Load consumption data and connections forecast over time to be used
  #across modules
  ####################################################################
  
  #number of connections
  tar_target(household_connections, get_household_connections_data(esoo_2024_assumptions_workbook_file,
                                                                   wem_esoo_2024_data_register_file,
                                                                   abs_dwelling_completions_file)),
  
  #Residential EV use
  #tar_target(residential_ev_econsumption, get_residential_ev_consumption_data(electric_vehicle_workbook_file,
                                                                              #wem_esoo_2024_ev_projections_file)),

  #Petrol use - per km
  
  tar_target(average_petrol_use_per_km, get_average_petrol_use_per_km(mv_survey_data_file_20)),
  
  #average km per vehicle
  tar_target(average_km_per_vehicle, get_average_km_per_vehicle(mv_survey_data_file_18)),
  
  
  #AEMO vehicle fleet data
  tar_target(ev_fleet_data, get_aemo_vehicle_data(electric_vehicle_workbook_file,
                                                  iasr_23_ev_workbook_file,
                                                  wem_esoo_2024_ev_projections_file)),
  
  #calculate EV efficiency over time
  tar_target(ev_efficiency_projections, project_ev_efficiency(ev_efficiency_file)),
  
  # Load EV consumption profiles
  tar_target(ev_consumption_profiles, get_ev_consumption_profiles(electric_vehicle_workbook_file,
                                                                  wem_esoo_2024_ev_projections_file,
                                                                  ev_fleet_data,
                                                                  ev_efficiency_projections)),
  
  tar_target(scaled_ev_consumption_profiles, calculate_scaled_ev_consumption_profiles(average_km_per_vehicle,
                                                        ev_efficiency_projections,
                                                        ev_fleet_data,
                                                        ev_consumption_profiles)),

  #calculate scaled ev_tou_profiles according to smvu km per vehical data.
  # tar_target(scaled_ev_profiles, calculate_scaled_ev_profiles(ev_consumption_profiles,
  #                                                             ev_efficiency_projections,
  #                                                             average_km_per_vehicle,
  #                                                             ev_fleet_data)),
  # 
  
  #https://www.abs.gov.au/statistics/people/population/household-and-family-projections-australia/2021-2046
  tar_target(total_households_aus_21, 9993.9e3),
  
  #https://www.abs.gov.au/methodologies/motor-vehicle-census-australia-methodology/31-jan-2021
  tar_target(total_passenger_vehicles_21, 14850675),
  
  #vehicles per household
  #source: https://www.abs.gov.au/statistics/industry/tourism-and-transport/transport-census/2021
  tar_target(vehicles_per_household, total_passenger_vehicles_21 / total_households_aus_21),
  
  
  #Not used, average vehicle use from ABS taken instead
  #tar_target(vehicles_per_household, get_vehicles_per_household(ev_fleet_data, household_connections)),
  

 
  
  ####################################################################
  #Calculate consumer energy usage from residential baseline study
  #################################################################### 
  
  #load RBS household numbers and deflate to represent 10% vacancy rate.
  tar_target(rbs_households, load_and_deflate_rbs_households(rbs_outputs_data_file)),
  
  #residential baseline fuel use data
  tar_target(rbs_fuel_end_use_by_state, get_rbs_fuel_end_use(rbs_outputs_data_file)),
  
  #load in fuel efficiency coefficients
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
                                                                    scaled_ev_consumption_profiles,
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
                                                                                                        jacobs_curtailment,
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
                                                                        jacobs_retail_prices,
                                                                        retail_electricity_tariffs,
                                                                        rbs_households)),
  
    
  #calculate petrol costs
  tar_target(cameo_petrol_costs, calculate_cameo_petrol_costs(average_petrol_use_per_km, 
                                                              average_km_per_vehicle,
                                                              petrol_price_data)),

  
  
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
  
  #proportion of connections with pv **and** batteries
  tar_target(battery_n_pv_prop, estimate_battery_prevalence(esoo_2024_assumptions_workbook_file,
                                                            household_connections)),
  
  
  #calculate total PV generation per system by state (and sense check implied size of system)
  
  tar_target(average_pv_profile, calculate_average_pv_profile(pv_profiles,
                                                              esoo_2024_operational_file,
                                                              wem_esoo_2024_operational_file,
                                                              pv_system_stock)),
  
  
  #now add in EVs and PV to base consumption and calculate average profiles for Solar and non-solar owners
  tar_target(average_profiles, calculate_average_profiles(esoo_average_underlying_demand,
                                                          average_load_shapes,
                                                          average_pv_profile,
                                                          scaled_ev_consumption_profiles,
                                                          ev_fleet_data,
                                                          vehicles_per_household)),
  
  
  tar_target(average_profiles_w_batteries, generate_battery_profiles(average_profiles, 
                                                                          #trace definining characteristics
                                                                          c("pv", "electrification", "state", "year", "season"), 
                                                                          battery_capacity = 11) %>% 
               rename(source = end_use)),
  
  #add in average profiles with batteries 
  tar_target(all_average_profiles, bind_rows(average_profiles %>% mutate(battery = F),
                                                  
                                                  #combine with profiles of consumers with PV and batteries
                                                  bind_rows(average_profiles %>% 
                                                              filter(pv == 1) %>% 
                                                              mutate(battery = 1),
                                                            
                                                            #add in battery end_use
                                                            average_profiles_w_batteries %>% 
                                                              mutate(power_kwh = battery_action) %>% 
                                                              select(-c(battery_action, battery_level, net_power))))),
  
  
  
  #calculate annual electricity consumption and exports for each year by aggregating ToU profiles
  tar_target(annual_electricity_consumption_averages, calculate_annual_electricity_consumption_averages(all_average_profiles)),
  tar_target(annual_electricity_consumption_averages, calculate_annual_electricity_consumption_averages(all_average_profiles, jacobs_curtailment)),
  
  
  #creating a target for consumer type weights to call when making charts etc.
  tar_target(average_consumer_type_weights,  
             pv_system_stock %>% 
               filter(year >= 2025) %>% 
               left_join(battery_n_pv_prop) %>% 
               mutate(pv_only_prop = prop - battery_and_pv_prop,
                      no_pv_prop = 1 - prop) %>% 
               select(-c(prop, pv_stock)) %>% 
               pivot_longer(cols = contains('prop'), 
                            names_to = 'consumer_type',
                            values_to = 'prop') %>% 
               mutate(consumer_type = case_when(consumer_type == "battery_and_pv_prop" ~ "1_1",
                                                consumer_type == "pv_only_prop" ~ "1_0",
                                                consumer_type == "no_pv_prop" ~ "0_0"))),
  
  
  # Gas use - average over all households with an electricity connection
  tar_target(average_gas_consumption, calculate_average_residential_gas_consumption(household_connections,
                                                                                    residential_gas_consumption_projections)),
  
  #average_petrol_consumption per household
  tar_target(average_petrol_consumption, calculate_average_petrol_consumption(ev_fleet_data,
                                                                              vehicles_per_household,
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
                                                                            jacobs_retail_prices,
                                                                            retail_electricity_tariffs,
                                                                            rbs_households,
                                                                        pv_system_stock,
                                                                        battery_n_pv_prop)),
  #calculate the weighted average across consumer types
  tar_target(weighted_average_electricity_costs, average_electricity_costs %>% 
               mutate(average_cost_dollars = average_cost_dollars * prop) %>% 
               group_by(year, state, scenario, electrification, category) %>% 
               summarise(average_cost_dollars = sum(average_cost_dollars))),
  
  tar_target(average_petrol_costs, calculate_average_petrol_costs(petrol_price_data,
                                                                  average_petrol_consumption)),
  

  
  ####################################################################
  #Create charts - Charts to be QC'd by Ben 
  ####################################################################
  
  tar_target(average_net_costs, compile_average_net_costs(weighted_average_electricity_costs,
                                                           average_gas_costs,
                                                           average_petrol_costs)),
  
  
  ##################### Chapter 4 - Jacobs Results #######################
  
  tar_file(jacobs_results_summary, "Data/Jacobs/SummaryResultsV3U.xlsx"),
  
  tar_file(results_ref, "Data/Jacobs/ResultsRef_V2Rev.xlsm"),
  
  tar_file(results_1_5_Opt1, "Data/Jacobs/ResultsOption1_V1.xlsm"),
  
  tar_file(results_1_5_Opt2, "Data/Jacobs/ResultsOption2_V1.xlsm"),
  
  tar_file(results_2_Opt1, "Data/Jacobs/Results2DOption1_V1.xlsm"),
  
  tar_file(results_2_Opt2, "Data/Jacobs/Results2DOption2_V2U.xlsm"),
  

  tar_file(value_of_emissions_file, "Data/value_of_emissions_reductions_aer.xlsx"),
  
  
  
  #load_emissions data
  
  
  
  #load generation data
  
  
  #load capacity data
  
  
  tar_target(jacobs_curtailment, get_jacobs_curtailment(results_ref,
                                                        results_1_5_Opt1,
                                                        results_1_5_Opt2,
                                                        results_2_Opt1,
                                                        results_2_Opt2)),

  
  ########################
  #Chapter 5
  ########################
  
  #Electricity forms just part of total household costs
  tar_target(energy_wallet_chart, plot_energy_wallet(average_net_costs,
                                                     household_connections)),
  
  
  #calculate and plot 10/20 year CP cost burden and savings
  
  
  
  
  #average annual bill no solar, solar, solar and battery.
  
  
  
  ################
  #Appendix C
  ################
  
  #changing underlying demand
  tar_target(average_profiles_chart, plot_average_profiles(all_average_profiles)),
  
  
  #gas supply charges 
  
  tar_target(gas_supply_charge_chart, plot_gas_supply_charges(gas_connection_charge_projections, 
                                                              residential_gas_consumption_projections)),
  
  #do WA charts for everything we did for NEM.
  
  
  #####################
  #ARCHIVE
  #####################
  
  #create esoo demand chart
  #tar_target(esoo_demand_chart, create_esoo_demand_chart(esoo_2024_operational_file))
  
  
  #create victorian consumption charts to compare to AEMO
  #making_prelim_vic_charts_aemo_compare.R
  
  
  #early_charts_for_savings.R
  
  #report_charts.qmd
  
  #create_charts.qmd
  
)

