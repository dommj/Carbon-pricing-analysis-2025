#create victorian consumption charts to compare with AEMO data
#cooking, water_heating, space_heating, ev, pv,
# 1 MWh = 3.6 GJ

#should turn this script or similar into one that can take a consumer type as an input and output a whole bunch of chart types e.g. intra-day profile, total generation, consumption. total consumption by end use, yearly variation ...


create_vic_cameo_monthly_consumption_charts <- function(tou_consumer_profiles,
                                                        rbs_fuel_consumption_profiles){
  
  consumer_type_1 <- c(cooking = "electric", water_heating = "gas", space_heating = "gas", ev = 0, pv = FALSE)
  
  consumer_type_1_code <- paste(consumer_type_1["cooking"], consumer_type_1["water_heating"],
                                consumer_type_1["space_heating"], consumer_type_1["ev"],
                                consumer_type_1["pv"], sep = "_")
  
  consumer_type_2 <- c(cooking = "gas", water_heating = "electric", space_heating = "electric", ev = 0, pv = TRUE)
  
  consumer_type_2_code <- paste(consumer_type_2["cooking"], consumer_type_2["water_heating"],
                                consumer_type_2["space_heating"], consumer_type_2["ev"],
                                consumer_type_2["pv"], sep = "_")
  
  consumer_type_3 <- c(cooking = "electric", water_heating = "electric", space_heating = "electric", ev = 1, pv = TRUE)
  
  
  elec_consumption_consumer_1 <- tou_consumer_profiles %>% 
    filter(cooking == consumer_type_1["cooking"], 
           water_heating == consumer_type_1["water_heating"],
           space_heating == consumer_type_1["space_heating"], 
           ev == consumer_type_1["ev"],
           pv == consumer_type_1["pv"],
           state == "Vic") %>% 
    group_by(state, season, hour) %>% 
    summarise(power_kwh = sum(power_kwh)) %>% 
    mutate(consumption_export = if_else(power_kwh < 0, "Exports", "Consumption")) %>%
    group_by(state, season, consumption_export) %>% 
    summarise(power_kwh = sum(power_kwh)) %>%  
    ungroup() %>% 
    mutate(total = power_kwh * (365.25 / 4),
           consumer_type = "Gas heating and hot water no PV")
  
  
  elec_consumption_consumer_2 <- tou_consumer_profiles %>% 
    filter(cooking == consumer_type_2["cooking"], 
           water_heating == consumer_type_2["water_heating"],
           space_heating == consumer_type_2["space_heating"], 
           ev == consumer_type_2["ev"],
           pv == consumer_type_2["pv"],
           state == "Vic") %>% 
    group_by(state, season, hour) %>% 
    summarise(power_kwh = sum(power_kwh)) %>% 
    mutate(consumption_export = if_else(power_kwh < 0, "Exports", "Consumption")) %>%
    group_by(state, season, consumption_export) %>% 
    summarise(power_kwh = sum(power_kwh)) %>% 
    ungroup() %>% 
    mutate(total = power_kwh * (365.25 / 4),
           consumer_type = "Electric heating and hot water + PV")
             
  elec_consumption_consumer_3 <- tou_consumer_profiles %>% 
    filter(cooking == consumer_type_3["cooking"], 
           water_heating == consumer_type_3["water_heating"],
           space_heating == consumer_type_3["space_heating"], 
           ev == consumer_type_3["ev"],
           pv == consumer_type_3["pv"],
           state == "Vic") %>% 
    group_by(state, season, hour) %>% 
    summarise(power_kwh = sum(power_kwh)) %>% 
    mutate(consumption_export = if_else(power_kwh < 0, "Exports", "Consumption")) %>%
    group_by(state, season, consumption_export) %>% 
    summarise(power_kwh = sum(power_kwh)) %>% 
    ungroup() %>% 
    mutate(total = power_kwh * (365.25 / 4),
           consumer_type = "Electric heating and hot water + PV + EV")
  
 
  ################################################
  #Sense checking
  ################################################  
    
  #consumer 1 - Mostly gas, no PV
  
  #annual consumption (4.0 MWh in comparison to 4.3 in AEMO sample ~ 7% lower demand)
  elec_consumption_consumer_1 %>% pull(total) %>% sum()
  
  gas_consumption_consumer_1 <- rbs_fuel_consumption_profiles %>% 
    filter(cooking == consumer_type_1["cooking"], 
           water_heating == consumer_type_1["water_heating"],
           space_heating == consumer_type_1["space_heating"], 
           ev == consumer_type_1["ev"],
           pv == consumer_type_1["pv"]) %>% 
    unnest(cols = c(output)) %>% 
    filter(state == "Vic",
           fuel == "Natural Gas")
  
  #annual consumption (49.42 GJ in comparison to 55 GJ in AEMO sample, ~ 10% demand)
  gas_consumption_consumer_1 %>% pull(annual_consumption_gj) %>% sum()
  
  
  #consumer 2 - ALL electric + PV
  
  #annual consumption 
  # 4.96 MWh consumption in comparison to 4.6 in AEMO sample ~ 8% higher consumption (would be higher with smaller PV too) could   # be due to efficiency of electric systems in VIC being particularly high given represents an active cohort of switchers / newer devices
  
  # 5.2 MWH exports in comparison to 3.72 in AEMO sample ~ 41% higher exports likely due to choice of PV system size
  
  elec_consumption_consumer_2 %>% 
    group_by(consumption_export) %>% 
    summarise(total = sum(total))
  
  gas_consumption_consumer_2 <- rbs_fuel_consumption_profiles %>% 
    filter(cooking == consumer_type_2["cooking"], 
           water_heating == consumer_type_2["water_heating"],
           space_heating == consumer_type_2["space_heating"], 
           ev == consumer_type_2["ev"],
           pv == consumer_type_2["pv"]) %>% 
    unnest(cols = c(output)) %>% 
    filter(state == "Vic",
           fuel == "Natural Gas")
  
  #annual consumption (3.7 GJ in comparison to 4.0 GJ in AEMO sample, ~ 8% lower consumption)
  gas_consumption_consumer_2 %>% pull(annual_consumption_gj) %>% sum()
  
  
  
  
  
  ###################################
  #Recreate AEMO-like charts
  ###################################
  
  months <- tibble(month = fct(c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                              "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")),
                   month_num = 1:12,
                   season = c("Summer", "Summer", "Autumn", "Autumn", 
                              "Autumn", "Winter", "Winter", "Winter", 
                              "Spring", "Spring", "Spring", "Summer"))
  
  consumer_data <- bind_rows(elec_consumption_consumer_1, elec_consumption_consumer_2, elec_consumption_consumer_3)
  
  
  monthly_elec_consumption <- full_join(consumer_data %>% select(-total), 
                                        months,
                                        relationship = "many-to-many")
  
  monthly_elec_consumption %>% 
    pivot_wider(names_from = consumption_export, values_from = power_kwh) %>%
    mutate(export_from_consumption = Consumption + Exports) %>% 
    ggplot(aes(x = month_num)) +
    # Area for consumption
    geom_ribbon(aes(ymin = 0, ymax = Consumption), 
                fill = grattan_darkred, 
                colour = grattan_darkred, 
                alpha = 0.6) +
    # Area for exports starting from consumption
    geom_ribbon(aes(ymin = Consumption, ymax = export_from_consumption), 
                fill = grattan_yellow, 
                colour = grattan_yellow, 
                alpha = 0.4) +
    scale_x_continuous(breaks = 1:12, labels = month.abb) +
    ylab("kWh") +
    theme_grattan() +
    facet_wrap(~consumer_type) +
    labs(title = 'Household consumption and exports - Victoria',
         subtitle = 'KWh',
         x = '',
         y = '')
  
  
  ####################################################
  #Daily charts
  ####################################################
  
  
  elec_consumption_consumer_1 <- tou_consumer_profiles %>% 
    filter(cooking == consumer_type_1["cooking"], 
           water_heating == consumer_type_1["water_heating"],
           space_heating == consumer_type_1["space_heating"], 
           ev == consumer_type_1["ev"],
           pv == consumer_type_1["pv"],
           state == "Vic",
           season == "Summer") %>% 
    mutate(consumption_export = if_else(power_kwh < 0, "Exports", "Consumption")) %>%
    group_by(hour, consumption_export) %>% 
    summarise(power_kwh = sum(power_kwh)) %>% 
    mutate(consumer_type = "Gas heating and hot water no PV") 
    
  elec_consumption_consumer_2 <- tou_consumer_profiles %>% 
    filter(cooking == consumer_type_2["cooking"], 
           water_heating == consumer_type_2["water_heating"],
           space_heating == consumer_type_2["space_heating"], 
           ev == consumer_type_2["ev"],
           pv == consumer_type_2["pv"],
           state == "Vic",
           season == "Summer") %>% 
    mutate(consumption_export = if_else(power_kwh < 0, "Exports", "Consumption")) %>%
    group_by(hour, consumption_export) %>% 
    summarise(power_kwh = sum(power_kwh)) %>% 
    mutate(consumer_type = "Electric heating and hot water + PV") 
  
consumers <- bind_rows(elec_consumption_consumer_1, elec_consumption_consumer_2)
  
  consumers %>% 
    pivot_wider(names_from = consumption_export, values_from = power_kwh) %>%
    mutate(export_from_consumption = Consumption + Exports) %>%
    ggplot(aes(x = hour)) +
    # Area for consumption
    geom_ribbon(aes(ymin = 0, ymax = Consumption), 
                fill = grattan_darkred, 
                colour = grattan_darkred, 
                alpha = 0.6) +
    # Area for exports starting from consumption
    geom_ribbon(aes(ymin = Consumption, ymax = export_from_consumption), 
                fill = grattan_yellow, 
                colour = grattan_yellow, 
                alpha = 0.4) +
    scale_x_continuous() +
    ylab("kWh") +
    theme_grattan() +
    facet_wrap(~consumer_type) +
    labs(title = 'Household consumption and exports - Victoria',
         subtitle = 'KWh',
         x = '',
         y = '')
  
  
  
  tou_consumer_profiles %>% 
    filter(cooking == consumer_type_2["cooking"], 
           water_heating == consumer_type_2["water_heating"],
           space_heating == consumer_type_2["space_heating"], 
           ev == consumer_type_2["ev"],
           pv == consumer_type_2["pv"],
           state == "Vic") %>% 
    ggplot(aes(x = hour, y = power_kwh, fill = end_use)) +
    geom_area()+
    facet_wrap(~season) 
  
  
  tou_consumer_profiles %>% 
    filter(cooking == consumer_type_2["cooking"], 
           water_heating == consumer_type_2["water_heating"],
           space_heating == consumer_type_2["space_heating"], 
           ev == consumer_type_2["ev"],
           pv == consumer_type_2["pv"],
           state == "NSW and ACT") %>% 
    ggplot(aes(x = hour, y = power_kwh, fill = end_use)) +
    geom_area()+
    facet_wrap(~season) 
  
  
  tou_consumer_profiles %>% 
    filter(cooking == "electric", 
           water_heating == "electric", 
           space_heating == "electric", 
           ev == 0,
           pv == 0,
           state == "Vic") %>% 
    ggplot(aes(x = hour, y = power_kwh, fill = end_use)) +
    geom_area()+
    facet_wrap(~season) 
  
}





