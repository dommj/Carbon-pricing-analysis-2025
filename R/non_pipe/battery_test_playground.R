#battery sim playground

average_adj_tou_consumption,
pv_profiles

states_with_data <-  average_adj_tou_consumption %>% select(state) %>% unique() %>% pull()

pv_profiles_unique <- pv_profiles %>% 
  filter(pv == TRUE,
         state %in% states_with_data) %>% 
  select(year, state, season, end_use, hour, power_kwh) %>% 
  unique() %>% 
  mutate(fuel = "Electricity",
         source = "PV",
         generation_consumption = "generation")

profile_w_pv_25 <- average_adj_tou_consumption %>% 
  mutate(generation_consumption = "consumption") %>% 
  bind_rows(pv_profiles_unique) %>% 
  filter(year == 2025) %>% 
  group_by(year, state, season, generation_consumption, hour) %>% 
  summarise(power_kwh = sum(power_kwh))


########################
#Claude code
########################

# Battery optimization function
optimize_battery_strategy <- function(data, battery_capacity_kwh) {
  
  # Prepare the data: calculate net consumption for each hour
  net_consumption <- data %>%
    pivot_wider(names_from = generation_consumption, values_from = power_kwh) %>%
    mutate(
      # Net consumption = consumption - generation (generation is negative, so we add it)
      net_consumption = consumption + generation,
      battery_charge = 0,
      battery_discharge = 0,
      battery_level = 0,
      net_import_after_battery = net_consumption
    ) %>%
    arrange(hour)
  
  # Initialize battery level
  current_battery_level <- 0
  
  # First pass: Charge battery during excess generation (negative net consumption)
  for (i in 1:nrow(net_consumption)) {
    if (net_consumption$net_consumption[i] < 0) {  # Excess generation
      # Available excess energy to charge battery
      excess_energy <- abs(net_consumption$net_consumption[i])
      
      # Maximum charge possible (limited by battery capacity and available energy)
      max_charge <- min(excess_energy, battery_capacity_kwh - current_battery_level)
      
      if (max_charge > 0) {
        net_consumption$battery_charge[i] <- max_charge
        current_battery_level <- current_battery_level + max_charge
        net_consumption$battery_level[i] <- current_battery_level
        net_consumption$net_import_after_battery[i] <- net_consumption$net_consumption[i] + max_charge
      } else {
        net_consumption$battery_level[i] <- current_battery_level
      }
    } else {
      net_consumption$battery_level[i] <- current_battery_level
    }
  }
  
  # Second pass: Discharge battery during high consumption periods
  # Sort hours by net consumption (highest first) to prioritize discharge
  discharge_priority <- net_consumption %>%
    filter(net_consumption > 0) %>%  # Only consider periods with net consumption
    arrange(desc(net_consumption)) %>%
    pull(hour)
  
  # Reset battery level for second pass
  current_battery_level <- max(net_consumption$battery_level)
  
  # Apply discharges in priority order
  for (hour_to_discharge in discharge_priority) {
    row_idx <- which(net_consumption$hour == hour_to_discharge)
    
    if (current_battery_level > 0) {
      # Maximum discharge possible (limited by battery level and consumption need)
      consumption_need <- net_consumption$net_consumption[row_idx]
      max_discharge <- min(current_battery_level, consumption_need)
      
      if (max_discharge > 0) {
        net_consumption$battery_discharge[row_idx] <- max_discharge
        current_battery_level <- current_battery_level - max_discharge
        net_consumption$net_import_after_battery[row_idx] <- 
          net_consumption$net_consumption[row_idx] - max_discharge
      }
    }
  }
  
  # Recalculate battery levels chronologically after discharge optimization
  current_battery_level <- 0
  for (i in 1:nrow(net_consumption)) {
    current_battery_level <- current_battery_level + 
      net_consumption$battery_charge[i] - net_consumption$battery_discharge[i]
    net_consumption$battery_level[i] <- current_battery_level
  }
  
  # Calculate results
  total_imports_without_battery <- sum(pmax(0, net_consumption$net_consumption))
  total_imports_with_battery <- sum(pmax(0, net_consumption$net_import_after_battery))
  import_reduction <- total_imports_without_battery - total_imports_with_battery
  
  # Return results
  list(
    strategy = net_consumption %>%
      select(hour, net_consumption, battery_charge, battery_discharge, 
             battery_level, net_import_after_battery),
    summary = tibble(
      battery_capacity_kwh = battery_capacity_kwh,
      total_imports_without_battery = total_imports_without_battery,
      total_imports_with_battery = total_imports_with_battery,
      import_reduction_kwh = import_reduction,
      import_reduction_percent = (import_reduction / total_imports_without_battery) * 100,
      max_battery_utilization = max(net_consumption$battery_level),
      battery_utilization_percent = (max(net_consumption$battery_level) / battery_capacity_kwh) * 100
    )
  )
}


x <- optimize_battery_strategy(profile_w_pv_25 %>% ungroup() %>% 
                                 filter(state == "Qld",
                                        season == "Autumn"), 15)

x$strategy %>% 
  pivot_longer(cols = c(net_consumption,
                        battery_charge,
                        battery_discharge,
                        net_import_after_battery),
               names_to = "demand_type",
               values_to = "power_kwh") %>% 
  ggplot(aes(x= hour, y = power_kwh, colour = demand_type)) +
  geom_line()

