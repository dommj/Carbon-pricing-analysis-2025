# tou_consumer_profiles <- average_profiles
# grouping <- c("pv", "electrification", "state", "year", "season")

# tou_consumer_profiles <- tou_consumer_profiles
# grouping <- c("cooking", "water_heating", "space_heating", "ev", "pv", "state", "year", "season")

generate_battery_profiles <- function(tou_consumer_profiles, grouping, battery_capacity = 11){
  
  average_consumption_export <- tou_consumer_profiles %>% 
    filter(pv == TRUE,
           year > 2024) %>% 
    group_by(!!!syms(grouping), hour) %>% 
    summarise(power_kwh = sum(power_kwh)) %>% 
    ungroup() %>% 
    group_split(!!!syms(grouping)) 
    
  
  

  # Simple optimisation function
  optimise_battery_simple <- function(power_data = power_kwh_col, #average_consumption_export[[1]]$power_kwh, 
                                      battery_capacity_kwh = battery_capacity, 
                                      max_usable_capacity = 0.85,
                                      efficiency_factor = 0.85) {
    
    max_charge_rate_kw <- battery_capacity_kwh / 2.1 #CSIRO assumption
    
    max_storage <- battery_capacity_kwh * max_usable_capacity
    min_storage <- battery_capacity_kwh * (1 - max_usable_capacity)
    
    
    
    # Find starting battery level that creates a continuous cycle
    
    #run the battery and return the difference between start and finish
    battery_residual <- function(target_start) {
      
      battery_level <- target_start
      for (hour in 1:24) {
        power <- power_data[hour]
        
        if (power < 0) {  # Excess generation - charge battery
          charge_amount <- min(abs(power), max_charge_rate_kw, (max_storage - battery_level)/ sqrt(efficiency_factor))
          battery_level <- battery_level + charge_amount * sqrt(efficiency_factor)
        } else {  # Consumption - discharge if beneficial
          discharge_amount <- min(power, max_charge_rate_kw, battery_level - min_storage)
          battery_level <- battery_level - discharge_amount 
        }
      }
      return(abs(battery_level - target_start))  # Return the imbalance
    }
    
    #search for the start level
    
    optim_start_level <- optimise(battery_residual, 
                                  c(min_storage, max_storage))
    
    start_level <- optim_start_level$minimum
    battery_levels <- numeric(24)
    battery_actions <- numeric(24)
    
    #run the battery from the start level we have found
    for (hour in 1:24) {
      power <- power_data[hour]
      
      if (hour == 1){
        battery_level = start_level
      }
      
      if (power < 0) {  # Excess generation - charge battery
        charge_amount <- min(abs(power), max_charge_rate_kw, (max_storage - battery_level)/ sqrt(efficiency_factor))
        battery_actions[hour] <- charge_amount #consumes charge amount with out losses
        battery_level <- battery_level + charge_amount * sqrt(efficiency_factor)
        battery_levels[hour] <- battery_level
        
      } else {  # Consumption - discharge if beneficial
        discharge_amount <- min(power, max_charge_rate_kw, battery_level - min_storage)
        battery_actions[hour] <- -discharge_amount * sqrt(efficiency_factor) #outputs charge amount minus losses
        battery_level <- battery_level - discharge_amount #battery drops by output + losses
        battery_levels[hour] <- battery_level
      }
    }
  
    
    results <- tibble(
      hour = 0:23,
      battery_action = battery_actions,
      battery_level = battery_levels,
      net_power = power_data + battery_actions,
    )
    
    return(results)
  }
  

  all_results <- imap(average_consumption_export,
                      ~ left_join(.x, 
                                  optimise_battery_simple(power_data = .x$power_kwh, 
                                                          battery_capacity_kwh = battery_capacity, 
                                                          max_usable_capacity = 0.85),
                                  by = join_by(hour))
                      )
  
  
  all_results_comb <- bind_rows(all_results) %>% 
    mutate(battery = T,
           end_use = "battery")
  
  return(all_results_comb)
  
}
  
  
  
  
# left_join(average_consumption_export[[1]], 
#           optimise_battery_simple(power_data = average_consumption_export[[1]]$power_kwh, 
#                                   battery_capacity_kwh = 11, 
#                                   max_usable_capacity = 0.85))
  
  