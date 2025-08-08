#scenario results charts

plot_scenario_results <- function(jacobs_results_summary,
                                  results_ref,
                                  results_1_5_Opt1,
                                  results_1_5_Opt2,
                                  results_2_Opt1,
                                  results_2_Opt2,
                                  jacobs_retail_prices_reference_case,
                                  jacobs_retail_prices_1_5_opt1,
                                  jacobs_retail_prices_1_5_opt2,
                                  jacobs_retail_prices_2_opt1,
                                  jacobs_retail_prices_2_opt2,
                                  annual_electricity_consumption_averages,
                                  average_consumer_type_weights,
                                  household_connections,
                                  rbs_households,
                                  value_of_emissions_file){
  
  summary_cells <- xlsx_cells(jacobs_results_summary)
  
  summary_formats <- xlsx_formats(jacobs_results_summary)
  
  #plot emissions
  
  emissions_ag_ref <- summary_cells %>% 
    filter(sheet == "Emissions",
           row <= 4) %>% 
    behead("up-left",
           "temp") %>% 
    behead("up",
           "year") %>% 
    behead("left",
           "scenario") %>% 
    select(scenario, year, content) %>% 
    filter(!is.na(content)) %>% 
    rename(mt_co2_e = content)
  
  
  emissions_ag_options <- summary_cells %>% 
    filter(sheet == "Emissions",
           row <= 10,
           row %nin% c(1, 3, 4)) %>% 
    behead_if(summary_formats$local$alignment$indent[local_format_id] == 0,
              direction = "left-up",
              name = "temp") %>%
    behead("up",
           "year") %>% 
    behead("left",
           "scenario") %>% 
    select(scenario, year, temp, content) %>% 
    filter(!is.na(content)) %>% 
    rename(mt_co2_e = content)
  
  emissions_data <- emissions_ag_ref %>% 
    mutate(temp = "ref") %>% 
    bind_rows(emissions_ag_options) %>% 
    mutate(mt_co2_e = as.numeric(mt_co2_e),
           scenario = fct_case_when(scenario == "Reference case" ~ "No new policy",
                                    scenario == "Option One - LRET" & str_detect(temp, "1.5") ~ "RET 1.5 C",
                                    scenario == "Option Two - EIS" & str_detect(temp, "1.5") ~ "Safeguard 1.5 C",
                                    scenario == "Option One - LRET" & str_detect(temp, "2") ~ "RET < 2 C",
                                    scenario == "Option Two - EIS" & str_detect(temp, "2") ~ "Safeguard < 2 C"))
  
  
  chart_palette <- c("No new policy" = grattan_red,
                     "RET < 2 C" = grattan_yellow,
                     "Safeguard < 2 C" = grattan_orange)
  
  emissions_plot <- emissions_data %>% 
    filter(!str_detect(temp, "1.5")) %>% 
    ggplot(aes(x = year, y = mt_co2_e, colour = scenario)) +
    geom_line(size = 1) +
    grattan_label(data = . %>%  filter(year == 2040) %>% 
                mutate(x = year,
                       y = case_when(scenario == "No new policy" ~ 115,
                                     scenario == "RET < 2 C" ~ 100,
                                     scenario == "Safeguard < 2 C" ~ 85)), 
              aes(x = year, y = y, label = scenario, colour = scenario),
              hjust = 0) +
    grattan_y_continuous(limits = c(0, 120)) +
    theme_grattan() +
    scale_colour_manual(values = chart_palette) +
    labs(title = "Emissions fall quickly under the policy scenarios to meet climate goals",
         subtitle = "Electricity sector emissions (Mt CO2-e)",
         x = "",
         y = "") 
  
  
  grattan_save_all("C:/Users/domijones/Grattan Institute Dropbox/Dominic  Jones/Apps/Overleaf/energy-2025-carbon-pricing-for-electricity/atlas/2_degree_emissions_results.pdf",
                   object = emissions_plot)
  
  
  #################################
  #Generation mix
  #################################
  
  ref_cells <- xlsx_cells(results_ref)
  ret_2_cells <- xlsx_cells(results_2_Opt1)
  safeguard_2_cells <- xlsx_cells(results_2_Opt2)
  
  result_formats_ref <- xlsx_formats(results_ref)
  result_formats_ret <- xlsx_formats(results_2_Opt1)
  result_formats_safeguard <- xlsx_formats(results_2_Opt2)
  
  
  ref_generation <- ref_cells %>% 
    filter(sheet == "AustGeneration",
           row != 1,
           row <= 262) %>% 
    behead("up",
           "year") %>% 
    behead_if(result_formats_ref$local$font$bold[local_format_id] == T,
              direction = "left-up",
              name = "region") %>% 
    behead("left",
           "gen_type") %>% 
    mutate(generation_sent_out_gwh = content %>% as.numeric()) %>% 
    select(region, year, gen_type, generation_sent_out_gwh) %>% 
    filter(!is.na(generation_sent_out_gwh),
           !is.na(year),
           gen_type != "Total",
           #filter out aggregates that have sub-levels
           region %nin% c("Total NEM ", "Queensland")) %>% 
    mutate(year = as.numeric(year),
           scenario = "No new policy") 
  
  
  
  ret_2_generation <- ret_2_cells %>% 
    filter(sheet == "AustGeneration",
           row != 1,
           row <= 262) %>% 
    behead("up",
           "year") %>% 
    behead_if(result_formats_ret$local$font$bold[local_format_id] == T,
              direction = "left-up",
              name = "region") %>% 
    behead("left",
           "gen_type") %>% 
    mutate(generation_sent_out_gwh = content %>% as.numeric()) %>% 
    select(region, year, gen_type, generation_sent_out_gwh) %>% 
    filter(!is.na(generation_sent_out_gwh),
           !is.na(year),
           gen_type != "Total",
           #filter out aggregates that have sub-levels
           region %nin% c("Total NEM ", "Queensland")) %>% 
    mutate(year = as.numeric(year),
           scenario = "RET < 2 C") 

  safeguard_2_generation <- safeguard_2_cells %>% 
    filter(sheet == "AustGeneration",
           row != 1,
           row <= 262) %>% 
    behead("up",
           "year") %>% 
    behead_if(result_formats_safeguard$local$font$bold[local_format_id] == T,
              direction = "left-up",
              name = "region") %>% 
    behead("left",
           "gen_type") %>% 
    mutate(generation_sent_out_gwh = content %>% as.numeric()) %>% 
    select(region, year, gen_type, generation_sent_out_gwh) %>% 
    filter(!is.na(generation_sent_out_gwh),
           !is.na(year),
           gen_type != "Total",
           #filter out aggregates that have sub-levels
           region %nin% c("Total NEM ", "Queensland")) %>% 
    mutate(year = as.numeric(year),
           scenario = "Safeguard < 2 C") 
  
  
  scenario_generation <- bind_rows(safeguard_2_generation, 
                                   ret_2_generation,
                                   ref_generation) %>% 
    mutate(type = case_when(str_detect(gen_type, 'Coal') ~ "Coal",
                            str_detect(gen_type, 'Gas|Hydrogen') ~ 'Gas',
                            str_detect(gen_type, '[w|W]ind') ~ "Wind",
                            str_detect(gen_type, 'Solar') ~ "Utility solar",
                            str_detect(gen_type, 'PV') ~ "Rooftop PV",
                            .default = gen_type),
           renewable = case_when(gen_type %in% c("Hydro",
                                               "Wind",
                                               "Biomass",
                                               "Solar",
                                               "Solar + Storage",
                                               "Geothermal",
                                               "Rooftop PV") ~ "Renewable",
                                 
                                 gen_type %in% c("Pumped Hydro", 
                                 "Battery Storage",
                                 "Embedded Battery") ~ "Storage",
                                 .default = "Fossil fuel"))
  
  #renewable pct plot - replace/supplement with coal exits (capacity) - more interesting
  
  renewable_pct_plot <- scenario_generation %>% 
    filter(year <= 2050,
           year >=2025) %>% 
    group_by(year, renewable, scenario) %>% 
    summarise(generation_sent_out_gwh = sum(generation_sent_out_gwh)) %>% 
    filter(renewable != "Storage") %>% 
    group_by(year, scenario) %>% 
    mutate(pct_renewable = generation_sent_out_gwh / sum(generation_sent_out_gwh)) %>% 
    filter(renewable == "Renewable") %>% 
    ggplot(aes(x= year, y = pct_renewable, colour = scenario)) +
    geom_line(size = 1) +
    grattan_label(data = . %>%  filter(year == 2043) %>% 
                    mutate(x = year,
                           y = case_when(scenario == "No new policy" ~ 0.7,
                                         scenario == "RET < 2 C" ~ 0.75,
                                         scenario == "Safeguard < 2 C" ~ 0.8)), 
                  aes(x = year, y = y, label = scenario, colour = scenario),
                  hjust = 0) +
    grattan_y_continuous(limits = c(0, 1), labels = scales::percent_format()) +
    theme_grattan() +
    scale_colour_manual(values = chart_palette) +
    labs(title = "Whats the story here?",
         subtitle = "Renewable power percentage",
         x = "",
         y = "",
         caption = "Notes: Calculated as a proportion of sent out generation.") 
  
  
  grattan_save_all("C:/Users/domijones/Grattan Institute Dropbox/Dominic  Jones/Apps/Overleaf/energy-2025-carbon-pricing-for-electricity/atlas/2_degree_renewable_pct_results.pdf",
                   object = renewable_pct_plot)
  
  
  ##############################
  #wholesale prices
  ##############################
  
 #here we read in time-weighted wholesale prices for each 2 C scenario
  #wholesale prices
  ref_twp_2 <- read_excel(results_ref,
                          sheet = "Annual Black Price",
                          range = "A2:AN12") %>% 
    mutate(scenario = "No new policy")
  
  safeguard_twp_2 <- read_excel(results_2_Opt2,
                          sheet = "Annual Black Price",
                          range = "A2:AN12") %>% 
    mutate(scenario = "Safeguard < 2 C")
  

  ret_twp_2 <- read_excel(results_2_Opt1,
                          sheet = "Annual Black Price",
                          range = "A2:AN12") %>% 
    mutate(scenario = "RET < 2 C")
  
  time_weighted_prices <- bind_rows(ref_twp_2,
                                    safeguard_twp_2,
                                    ret_twp_2) %>% 
    rename(region = 1) %>% 
    filter(!is.na(`2022`)) %>% 
    pivot_longer(cols = contains('20'), names_to = "year", values_to = "dollars_mwh") %>% 
    mutate(year = as.numeric(year))
  
  #LGC prices
  ref_lgc_2 <- read_excel(results_ref,
                          sheet = "Bundled Price (Real)",
                          range = "A2:AN4") %>% 
    mutate(scenario = "No new policy")
  
  safeguard_lgc_2 <- read_excel(results_2_Opt2,
                                sheet = "Bundled Price (Real)",
                                range = "A2:AN4") %>% 
    mutate(scenario = "Safeguard < 2 C")
  
  
  ret_lgc_2 <- read_excel(results_2_Opt1,
                          sheet = "BundledPrice",
                          range = "A2:AN4") %>% 
    mutate(scenario = "RET < 2 C")
  
  lgc_prices <- bind_rows(ref_lgc_2,
                                    safeguard_lgc_2,
                                    ret_lgc_2) %>% 
    rename(cat = 1) %>% 
    filter(cat == "LGC Price") %>% 
    pivot_longer(cols = contains('20'), names_to = "year", values_to = "lgc_dollars_mwh") %>% 
    mutate(year = as.numeric(year)) %>% 
    select(-cat)
  
  #actually don't worry about adding in LGCs and just show wholesale prices without LGCs in the appendix and just retail prices in the main body (which includes LGCs)
  
  #lgc_impost <- 
  
  #national average (weight by generation sent out)
  
  time_weighted_prices %>% select(region) %>% unique()
  scenario_generation  %>% select(region) %>% unique()
  
  annual_price_chart_data <- time_weighted_prices %>% 
    left_join(scenario_generation %>% 
                #match region names
                mutate(region = case_when(str_detect(region, "Queensland") ~ "Queensland",
                                          region == "NW" ~ "NWIS (Dampier)",
                                          region == "Northern Territory" ~ "DKIS (Darwin)",
                                          .default = region)) %>% 
                group_by(region, year, scenario) %>% 
                summarise(generation_sent_out_gwh  = sum(generation_sent_out_gwh))
              ) %>% 
    group_by(year, scenario) %>% 
    summarise(dollars_mwh = weighted.mean(dollars_mwh, generation_sent_out_gwh)) 
  
  
  annual_price_chart <- annual_price_chart_data %>% 
    filter(year %in% seq(2025, 2050)) %>% 
    ggplot(aes(x = year, y = dollars_mwh, colour = scenario)) +
    geom_line(size = 1) +
    grattan_label(data = . %>%  filter(year == 2050) %>% 
                    mutate(x = c(2032, 2043, 2043),
                           y = case_when(scenario == "No new policy" ~ 110,
                                         scenario == "RET < 2 C" ~ 55,
                                         scenario == "Safeguard < 2 C" ~ 90)), 
                  aes(x = x, y = y, label = scenario, colour = scenario),
                  hjust = 0) +
    grattan_y_continuous(limits = c(0, 130), labels = scales::dollar_format()) +
    scale_x_continuous_grattan(breaks = seq(2025, 2050, by = 5)) +
    theme_grattan() +
    scale_colour_manual(values = chart_palette) +
    labs(title = "The Renewable Energy target keeps wholesale prices low, Safeguard is comparable with no new policy",
         subtitle = "Average time-weighted wholesale price, all Australian grids.",
         x = "",
         y = "",
         caption = "Notes: Average prices are calculated by weighting prices in each grid by total sent out generation.") 
  
  
  grattan_save_all("C:/Users/domijones/Grattan Institute Dropbox/Dominic  Jones/Apps/Overleaf/energy-2025-carbon-pricing-for-electricity/atlas/2_degree_wholesale_results.pdf",
                   object = annual_price_chart)
  
  #check_chart_aspect_ratio()
  
  ###################
  #Transmission
  ##################
  
  
  
  
  
  ########################################
  #retail prices
  ########################################
  
  retail_prices <- bind_rows(jacobs_retail_prices_reference_case,
                             jacobs_retail_prices_2_opt1,
                             jacobs_retail_prices_2_opt2) %>% 
    mutate(scenario = fct_case_when(scenario == "Ref" ~ "No new policy",
                                    scenario == "2_Opt1" ~ "RET < 2 C",
                                    scenario == "2_Opt2" ~ "Safeguard < 2 C")) 
  
  ###############average retail chart#############
  
  #Average retail prices across the NEM, weighted by residential retail billable volumes (total residential underlying minus PV-self consumption).
  
  #load residential consumption
  aggregate_consumption <- annual_electricity_consumption_averages %>% 
    filter(consumption_export == "Consumption",
           electrification == T) %>% 
    mutate(consumer_type = paste0(pv, "_", battery)) %>% 
    left_join(average_consumer_type_weights %>% 
                select(-connections)) %>% 
    left_join(household_connections) %>% 
    mutate(aggregate_consumption_mwh = annual_consumption_kwh * prop * connections / 1000) %>% 
    group_by(year, state) %>% 
    summarise(aggregate_consumption_mwh = sum(aggregate_consumption_mwh))
  
  retail_prices_nem_res <- retail_prices %>% 
    filter(market == "Residential")
  
  #weight nsw and act tariffs by household numbers in 2020 to get aggregate tariff (this aligns with our connection based weighting of consumption)
  
  retail_prices_nem_res_nsw_act <- retail_prices_nem_res %>% 
    filter(state %in% c("NSW", "ACT")) %>% 
    left_join(rbs_households %>% 
                filter(year == 2020) %>% 
                select(- year)) %>% 
    group_by(year, market, scenario, consumption_export) %>% 
    summarise(c_kwh = weighted.mean(c_kwh, occupied_households)) %>% 
    ungroup() %>% 
    mutate(state = "NSW and ACT")
  
  #add back in
  
  retail_prices_nem_res_ag <- retail_prices_nem_res %>%
    bind_rows(retail_prices_nem_res_nsw_act) %>% 
    filter(state %nin% c("NSW", "ACT"),
           #only looking at consumption tariffs
           consumption_export == "Consumption") 
    
  
  
  weighted_nem_average_retail_prices <- retail_prices_nem_res_ag %>% 
    left_join(aggregate_consumption) %>% 
    filter(state != "WA",
           state != "NT") %>% 
    group_by(year, scenario) %>% 
    summarise(c_kwh = weighted.mean(c_kwh, aggregate_consumption_mwh)) %>% 
    filter(year >= 2025)
  
  
  average_nem_retail_chart <- weighted_nem_average_retail_prices %>% 
    ggplot(aes(x = year, y = c_kwh * 1000/100,  #convert to dollars per mwh
               colour = scenario)) +
    geom_line(size = 1) +
    grattan_y_continuous(labels = scales::dollar_format(),
                         limits = c(0,500)) +
    grattan_label(data = . %>%  filter(year == 2050) %>% 
                    mutate(x = c(2045.5, 2042, 2036),
                           y = case_when(scenario == "No new policy" ~ 395,
                                         scenario == "RET < 2 C" ~ 470,
                                         scenario == "Safeguard < 2 C" ~ 350)), 
                  aes(x = x, y = y, label = scenario, colour = scenario),
                  hjust = 0) +
    scale_x_continuous_grattan() +
    scale_colour_manual(values = chart_palette) +
    theme_grattan() +
    labs(title = "Retail prices are similar across all scenarios, but the Safeguard outperforms the RET",
         subtitle = "Average NEM residential retail prices, dollars per MWh ($2025)",
         x = "",
         y = "")
  
  grattan_save_all("C:/Users/domijones/Grattan Institute Dropbox/Dominic  Jones/Apps/Overleaf/energy-2025-carbon-pricing-for-electricity/atlas/2_degree_average_nem_retail_line.pdf",
                   object = average_nem_retail_chart)
  
  
  
  # weighted_nem_average_retail_prices %>% 
  #   mutate(scenario = factor(scenario, levels = c("No new policy", "Safeguard < 2 C", "RET < 2 C"))) %>%
  #   filter(year %in% seq(2025,2050, by = 5)) %>% 
  #   ggplot(aes(x = year, 
  #              y = c_kwh * 1000/100,  #convert to dollars per mwh
  #              fill = scenario)) +
  #   geom_col(position = "dodge") +
  #   grattan_y_continuous(labels = scales::dollar_format(),
  #                        limits = c(0,500)) +
  #   grattan_label(data = . %>%  filter(year == 2025) %>% 
  #                   mutate(y = case_when(scenario == "No new policy" ~ 400,
  #                                        scenario == "RET < 2 C" ~ 460,
  #                                        scenario == "Safeguard < 2 C" ~ 430)), 
  #                 aes(x = year, y = y, label = scenario, colour = scenario),
  #                 hjust = 0,
  #                 nudge_x = -2) +
  #   scale_x_continuous_grattan(breaks = seq(2025,2050, by = 5)) +
  #   scale_colour_manual(values = chart_palette) +
  #   scale_fill_manual(values = chart_palette) +  # Add this line
  #   theme_grattan() +
  #   labs(title = "Retail prices are similar across all scenarios, but the Safeguard outperforms the RET",
  #        subtitle = "Average NEM residential retail prices, dollars per MWh ($2025)",
  #        x = "",
  #        y = "")
  
  
  ################## State retail chart ###############
  
  state_retail_chart <- retail_prices_nem_res_ag %>% 
    filter(market == "Residential",
           consumption_export == "Consumption",
           state %nin% c("NT"),
           year >= 2020) %>% 
    ggplot(aes(x = year, 
               y = (c_kwh / 100) * 1000, #convert to dollars per mwh
               colour = scenario)) +
    geom_line(size = 1) +
    facet_wrap(~state) +
    grattan_y_continuous(labels = scales::dollar_format(),
                         limits = c(0, 600)) +
    scale_x_continuous(breaks = c(2020, 2030, 2040, 2050)) +
    grattan_label(data = . %>%  filter(year == 2025,
                                       state == "SA") %>% 
                    mutate(y = case_when(scenario == "No new policy" ~ 50,
                                         scenario == "RET < 2 C" ~ 250,
                                         scenario == "Safeguard < 2 C" ~ 150)), 
                  aes(x = year, y = y, label = scenario, colour = scenario),
                  hjust = 0) +
    scale_colour_manual(values = chart_palette) +
    theme_grattan() +
    labs(title = "WA and Queensland consumers are likely to pay lower prices under a carbon price",
         subtitle = "Average residential retail prices, dollars per MWh ($2025)",
         x = "",
         y = "")
  
  
  grattan_save_all("C:/Users/domijones/Grattan Institute Dropbox/Dominic  Jones/Apps/Overleaf/energy-2025-carbon-pricing-for-electricity/atlas/2_degree_state_retail_line.pdf",
                   object = state_retail_chart)
  
  ############################
  #system cost over time:
  ############################
  
  #resource costs, annual
  resource_costs <-  read_excel(jacobs_results_summary, 
                                sheet = "ResourceCosts",
                                skip = 1) %>%
    #just capture ref case and 2 deg options
    slice(c(2, 7, 8)) %>% 
    rename(scenario = 1) %>% 
    mutate(scenario = case_when(str_detect(scenario, "Reference") ~ "No new policy",
                                str_detect(scenario, "One") ~ "RET < 2 C",
                                str_detect(scenario, "Two") ~ "Safeguard < 2 C")) %>% 
    pivot_longer(cols = contains("20"),
                 names_to = "year",
                 values_to = "million_dollars") %>% 
    mutate(category = "Resource costs",
           year = as.numeric(year)) %>% 
    select(scenario, year, category, million_dollars)
  
  #calculate emissions differences between scenarios
  ems_difference <- emissions_data %>% 
    filter(scenario %in% c("No new policy",
                           "RET < 2 C",
                           "Safeguard < 2 C")) %>% 
    select(-temp) %>% 
    group_by(year) %>% 
    mutate(mt_co2e_diff = mt_co2_e - mt_co2_e[scenario == "No new policy"])
  
  #value of emissions over time.
  #value of emissions source from AER guidance: https://www.aer.gov.au/system/files/2024-05/AER%20-%20Valuing%20emissions%20reduction%20-%20Final%20guidance%20and%20explanatory%20statement%20-%20May%202024.pdf
  
  value_of_emissions <- read_excel(value_of_emissions_file,
                                   skip = 1) %>% 
    rename(dollars_tonne_co2e = 2) %>% 
    #convert to 2025 dollars from 2023 dollars
    mutate(dollars_tonne_co2e = convert_to_2024_dollars(dollars_tonne_co2e, 2023)) %>% 
    clean_names() %>% 
    select(year, dollars_tonne_co2e) %>% 
    left_join(ems_difference) %>% 
    mutate(million_dollars = dollars_tonne_co2e * mt_co2e_diff,
           category = "Value of emissions reductions") %>% 
    select(year, scenario, category, million_dollars)
  
  #bind together and calculate NPV
  #discount rate = 7.4% (used by Jacobs in results summary, need to confirm in final report)
  
  discount_rate <- 0.074
  
  total_costs <- bind_rows(resource_costs,
                           value_of_emissions) %>% 
    mutate(discount_factor = 1 / (1 + discount_rate)^(year-2026), #check if Jacobs discount from 2026 or 2027
           present_value_billions = million_dollars * discount_factor / 1e3)
  
  
  npv_compare_chart <- total_costs %>% 
    filter(!is.na(scenario)) %>% 
    group_by(scenario, category) %>% 
    summarise(present_value = sum(present_value_billions)) %>% 
    ggplot(aes(x = scenario, y = present_value)) +
    geom_col(aes(fill = category, colour = category)) +
    geom_hline(aes(yintercept = 0),
               size = 1,
               colour = grattan_black)+
    geom_point(data = . %>% 
                 group_by(scenario) %>% 
                 summarise(present_value = sum(present_value)),
                 colour = grattan_black,
               size = 5) +
    grattan_y_continuous(labels = scales::dollar_format(suffix = "b"),
                         limits = c(-100, 450)) +
    scale_x_discrete(expand = expansion(add = c(0, 1.6))) +
    grattan_label(data = . %>%  filter(scenario == "Safeguard < 2 C") %>% 
                    mutate(y = present_value / 2), 
                  aes(x = scenario, y = y, 
                      label = str_wrap(category, 19),
                      colour = category),
                  hjust = 0,
                  nudge_x = 0.5) +
    grattan_label(data = . %>% 
                    group_by(scenario) %>% 
                    summarise(present_value = sum(present_value)) %>% 
                    filter(scenario == "Safeguard < 2 C") %>% 
                    mutate(y = present_value), 
                  aes(x = scenario, y = y, 
                      label = str_wrap("Net resource costs and value of emissions", 19)),
                  colour = grattan_black,
                  hjust = 0,
                  nudge_x = 0.5) +
    theme_grattan() +
    labs(title = "Implementing the Safeguard results in the lowest total system costs",
         subtitle = "Net present value of system costs and emissions reductions, in billions ($2025)",
         x = "",
         y = "",
         caption = "Note: Value of emissions reductions are calculated using AER guidance values. Net present value is calculated using a discount rate of 7.4%")
  
  
  grattan_save_all("C:/Users/domijones/Grattan Institute Dropbox/Dominic  Jones/Apps/Overleaf/energy-2025-carbon-pricing-for-electricity/atlas/2_degree_npv_results.pdf",
                   object = npv_compare_chart)
  
  
  #check_chart_aspect_ratio()
  
  #cost per tonne abatement
 
  cost_tonne_abatement <- total_costs %>% 
    filter(category == "Resource costs") %>% 
    group_by(scenario, category) %>% 
    summarise(present_value_billions = sum(present_value_billions)) %>% 
    ungroup() %>% 
    mutate(cost_diff = present_value_billions - present_value_billions[scenario == "No new policy"]) %>% 
    left_join(ems_difference %>% 
                #discount emissions reductions, this is standard for calc (see Jacobs)
                mutate(discount_factor = 1 / (1 + discount_rate)^(year-2026), #check if Jacobs discount from 2026 or 2027
                       mt_co2e_diff = mt_co2e_diff * discount_factor) %>% 
                group_by(scenario) %>% 
                summarise(mt_co2e_diff = sum(mt_co2e_diff))) %>% 
    mutate(dollars_per_ton = -(cost_diff * 1e9)/(mt_co2e_diff * 1e6))
    
  
  
  abatement_cost_chart <- cost_tonne_abatement %>% 
    filter(cost_diff != 0) %>% 
    ggplot(aes(x = scenario, y = dollars_per_ton, fill = scenario)) +
    geom_col() +
    grattan_y_continuous(labels = scales::dollar_format()) +
    theme_grattan() +
    scale_fill_manual(values = chart_palette) +
    labs(title = "The Safeguard is more efficient at decarbonising the grid",
         subtitle = "Abatement cost, dollars per tonne of CO2-e",
         x = "",
         y = "",
         caption = "Note: Abatement cost is calculated by dividing the present value of total resource costs by the present value of emissions reductions (in tonnes) relative to the 'No new policy' scenario.") 
    
  grattan_save_all("C:/Users/domijones/Grattan Institute Dropbox/Dominic  Jones/Apps/Overleaf/energy-2025-carbon-pricing-for-electricity/atlas/2_degree_abatement_cost.pdf",
                   object = abatement_cost_chart)
  
  
#plot coal capacity
  
  
#safeguard allows new tec to enter in the gas space (hydrogen ready)
    
  plot_list <- c("emissions_plot" = emissions_plot, 
                 "renewable_pct_plot" = renewable_pct_plot, 
                 "annual_price_chart" = annual_price_chart,
                 "average_nem_retail_chart" = average_nem_retail_chart, 
                 "state_retail_chart" = state_retail_chart, 
                 "npv_compare_chart" = npv_compare_chart,
                 "abatement_cost_chart" = abatement_cost_chart)
  
  return(plot_list)
}


# storage vs gas - do capacity instead?
# scenario_generation %>%
#   filter(year == 2045,
#          type == "Gas" | renewable == "Storage") %>%
#   mutate(type = if_else(type == "Gas", type, "storage")) %>%
#   group_by(year, type, scenario) %>%
#   summarise(generation_sent_out_gwh = sum(generation_sent_out_gwh)) %>%
#   ggplot(aes(x = type, y = generation_sent_out_gwh, fill = scenario)) +
#   geom_col(position = "dodge")



# scenario_generation %>% 
#   select(gen_type) %>% 
#   unique()
# 
# scenario_generation %>% 
#   #filter out storage
#   filter(type %nin% c("Pumped Hydro", "Battery Storage", "Embedded Battery"),
#          year %in% c(2030, 2032, 2034, 2036, 2038)) %>% 
#   group_by(year, type, scenario) %>% 
#   summarise(generation_sent_out_gwh = sum(generation_sent_out_gwh)) %>% 
#   group_by(year, scenario) %>% 
#   mutate(pct_mix = generation_sent_out_gwh / sum(generation_sent_out_gwh)) %>% 
#   ggplot(aes(x = scenario, y = pct_mix, fill = type)) +
#   geom_col() +
#   facet_wrap(~ year)
# 
# 
# scenario_generation %>% 
#   group_by(year, type, gen_type, scenario) %>% 
#   summarise(generation_sent_out_gwh = sum(generation_sent_out_gwh)) %>% 
#   group_by(year, scenario) %>% 
#   mutate(pct_mix = generation_sent_out_gwh / sum(generation_sent_out_gwh)) %>% 
#   filter(type %in% c("Coal", "Gas"),
#          year <=2050,
#          year >= 2025) %>% 
#   ggplot(aes(x = year, y = pct_mix, fill = gen_type)) +
#   geom_col() +
#   facet_wrap(~ scenario)


