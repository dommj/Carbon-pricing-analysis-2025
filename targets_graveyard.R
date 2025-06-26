#targets graveyard

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