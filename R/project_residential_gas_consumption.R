#project residential gas consumption

project_residential_gas_consumption <- function(gas_connections_data,
                                                benchmark_gas_consumption,
                                                gsoo_consumption_data){

  #####################################
  #project GSOO consumption data to 2050
  #####################################
  
  #plot to see traj is linear
  # gsoo_consumption_data %>% 
  #   ggplot(aes(x= year, y = annual_consumption_gj, colour = state)) +
  #   geom_line()
  
  #create linear models
  models_by_state <- gsoo_consumption_data %>%
    filter(year >= 2030 & year <= 2043) %>%
    group_by(state) %>%
    nest() %>%
    mutate(model = map(data, ~lm(annual_consumption_gj ~ year, data = .x))) %>% 
    select(state, model)
  
  #complete data to 2050 - but remove WA for beyond 2034
  gas_consumption_projections_complete <- gsoo_consumption_data %>% 
    complete(year = 2023:2050, state) %>%
    left_join(models_by_state, by = "state") %>%
    mutate(pred = map2_dbl(model, year, ~ predict(.x, newdata = data.frame(year = .y))),
           annual_consumption_gj = if_else(is.na(annual_consumption_gj), pred, annual_consumption_gj)) %>% 
    #vic goes negative in 2050, let 2050 = 2049 usage for now (same as customers)
    mutate(annual_consumption_gj = if_else(year == 2050 & state == "Vic",
                                           annual_consumption_gj[year == 2049 & state == "Vic"],
                                           annual_consumption_gj)) %>% 
    filter(!(year > 2034 & state == "WA")) %>% 
    select(year, state, annual_consumption_gj)
  
  #plot trajectory
  gas_consumption_projections_complete %>%
    ggplot(aes(x= year, y = annual_consumption_gj, colour = state)) +
    geom_line()
  
  #aggregate nsw and act connections
  gas_connections_data <- gas_connections_data %>% 
    bind_rows(
      gas_connections_data %>%
        filter(state %in% c("NSW", "ACT")) %>%
        summarise(
          state = "NSW and ACT",
          residential = sum(residential),
          small_business = sum(small_business),
          total_customers = sum(total_customers),
          year = first(year))) %>%
    # Remove the original NSW and ACT rows 
    filter(!state %in% c("NSW", "ACT")) %>% 
    mutate(pct_residential = residential / total_customers) 
  
  #work out the current proportion of gas use that is residential
  residential_consumption_pct <- full_join(gas_connections_data,
                                       benchmark_gas_consumption) %>% 
    mutate(residential_consumption = residential * benchmark_use_mj) %>% 
    select(year, state, residential_consumption) %>% 
    #we don't have 2023 data for WA but we can assume the residential consumption is essentially constant (as we're only looking at the decline trend here)
    left_join(gsoo_consumption_data %>% 
                #change 2024 data to 2023 to match with consumption data
                mutate(year = if_else(year == 2024 & state == "WA", 2023, year)), 
              by = join_by(year, state)) %>% 
    mutate(pct_residential_consumption = (residential_consumption / 1000) / annual_consumption_gj) %>% 
    select(state, pct_residential_consumption)

 residential_consumption <- gas_consumption_projections_complete %>%
  full_join(residential_consumption_pct) %>% 
  full_join(benchmark_gas_consumption %>% 
              select(state, benchmark_use_mj)) %>% 
  mutate(residential_consumption_gj = (annual_consumption_gj * pct_residential_consumption),
         residential_gas_connections = residential_consumption_gj / (benchmark_use_mj / 1000),
         category = 'gas') %>% 
   select(year, state, residential_consumption_gj, residential_gas_connections, category) %>% 
   filter(!is.na(residential_consumption_gj))

 return(residential_consumption)
}


