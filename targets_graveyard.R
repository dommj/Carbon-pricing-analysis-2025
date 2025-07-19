#targets graveyard


#tar_source('R/get_average_residential_operational_demand.R')


#average consumption - archive
# tar_source('R/non_pipe/get_esoo_electrification_per_household.R')
# tar_source('R/get_rbs_baseline_consumption.R')
# tar_source('R/calculate_displaced_gas_consumption.R')
# tar_source("R/calculate_additional_electricity_consumption.R")
# tar_source("R/calculate_average_adj_tou_consumption.R")

#calculate average household costs
#tar_source("R/calculate_average_household_costs.R")

#AER retail markets 2024 file
tar_file(aer_retail_markets_file, 'Data/Data - State of the energy market 2024 - Chapter 6 - Retail energy markets.xlsx'),


#retail electricity prices
tar_file(retail_file, 'Data/AEMC price trends/nsw_25.csv'),

#get retail electricity data from AEMC price trends
tar_target(retail_price_data_aemc, get_retail_data_aemc(retail_file)),

#jacobs retail model file base - this is the old outdated spreadsheet
tar_file(jacobs_retail_model_file_base, "Data/Jacobs/RetailPriceProjections_Base.xlsx"),


#jacobs demand file
tar_file(jacobs_demand_data_file, 'Data/Jacobs/Consolidated Electricity Demand Forecasts - DJ0704.xlsx'),

#Electricity use
tar_target(average_residential_operational_demand, get_average_residential_operational_demand(esoo_2024_operational_file, residential_ev_econsumption, household_connections)),

#jacobs electricity demand
tar_target(jacobs_electricity_demand, load_jacobs_demand_data(jacobs_demand_data_file)),




tar_target(average_household_costs, calculate_average_household_costs(retail_price_data, 
                                                                      gas_retail_volumetric_price_projections,
                                                                      gas_network_charge_revenue,
                                                                      petrol_price_projections,
                                                                      household_connections,
                                                                      average_residential_operational_demand,
                                                                      average_gas_consumption,
                                                                      average_petrol_consumption)),


#!!!! Test
tar_target(esoo_electrification_per_household, get_esoo_electrification_per_household(esoo_2024_operational_file,
                                                                                      household_connections,
                                                                                      rbs_baseline_consumption)),




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