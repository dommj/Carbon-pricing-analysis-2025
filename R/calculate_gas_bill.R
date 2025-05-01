
calculate_gas_bill <- function(offers,
                               gas_consumption){
  
  # Calculate annual cost function 
  calculate_gas_seasonal_usage_costs <- function(seasonal_consumption, company_name, customer_class_name, state_name, tariffs) {
    # Get tariffs for the specific company and state
    plan_tariffs <- tariffs %>%
      filter(company == company_name, 
             state == state_name,
             customer_class == customer_class_name)
    
    # Calculate daily average consumption
    daily_consumption <- seasonal_consumption / (365 / 4)
    
    # Extract usage tiers
    usage_tiers <- plan_tariffs %>%
      filter(price_type != "supply charge") %>%
      arrange(min_vol)
    
    # print(paste("Number of rows in usage_tiers:", nrow(usage_tiers)))
    # print(usage_tiers)
    
    usage_cost <- 0
    
    
    for (i in 1:nrow(usage_tiers)) {
      tier_min <- usage_tiers$min_vol[i]
      tier_max <- usage_tiers$max_vol[i]
      tier_price <- usage_tiers$price[i]
      
      # If consumption is below current tier minimum, no more calculation needed
      if (daily_consumption <= tier_min) {
        break
      }
      
      # Calculate volume in this tier
      tier_volume <- min(daily_consumption - tier_min, tier_max - tier_min)
      
      # Calculate cost for this tier
      tier_cost <- tier_volume * tier_price
      
      # Add to total cost
      usage_cost <- usage_cost + tier_cost
      
      
      # If consumption is fully accounted for, exit loop
      if (daily_consumption <= tier_max) {
        break
      }
    }
    
    
    
    # seasonal costs
    
    seasonal_usage_cost <- usage_cost * (365 / 4)   #multiply by days in a season
    
    return(seasonal_usage_cost / 100) #(convert to dollars)
  }

  #calculate seasonal supply costs
  calculate_gas_seasonal_supply_costs <- function(company_name, customer_class, state_name, tariffs) {
    
    # Get tariffs for the specific company and state
    plan_tariffs <- tariffs %>%
      filter(company == company_name, 
             state == state_name,
             customer_class == customer_class)
    
    # Get supply charge (cents per day)
    supply_charge <- plan_tariffs %>% 
      filter(price_type == "supply charge") %>% 
      pull(price) %>%
      first()
    
    # seasonal supply costs
    seasonal_supply_cost <- (supply_charge ) * (365 / 4)  #multiply by days in a season

    seasonal_supply_cost / 100 #(convert to dollars)
  }
  
  
  seasonal_costs <- gas_consumption %>% 
    select(- benchmark_use_mj) %>% 
    pivot_longer(cols = -state, names_to = "season", values_to = "benchmark_consumption_mj") %>% 
    full_join(offers %>% 
                select(state, company) %>% 
                unique()) %>% 
    rowwise() %>% 
    mutate(network_cost = calculate_gas_seasonal_supply_costs(company_name = company,
                                                              customer_class = "Residential",
                                                              state_name = state,
                                                              tariffs = offers),
           usage_cost = calculate_gas_seasonal_usage_costs(seasonal_consumption = benchmark_consumption_mj,
                                                           company_name = company,
                                                           customer_class = "Residential",
                                                           state_name = state,
                                                           tariffs = offers)) %>% 
    ungroup()
  
  
  annual_costs <- seasonal_costs %>% 
    group_by(state, company) %>% 
    summarise(network_cost = sum(network_cost),
              usage_cost = sum(usage_cost)) %>% 
    mutate(total_cost = network_cost + usage_cost)
  
   #checked results as comparable to https://www.energyaustralia.com.au/epfs-documents/VIC_GAS_RSOT_T2_TRU567154SR.pdf
  
  
  annual_costs
  
}


