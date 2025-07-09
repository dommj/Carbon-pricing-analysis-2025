#this script uses AER benchmark data and connections data to produce the average gas consumption per connection for each state

get_benchmark_gas_consumption <- function(aer_gas_benchmarks_file,
                                          gas_connections_data){
  
  #average number of persons per household is 2.5 australia-wide and essentially constant by state
  #gas use is non linear with household number, we take 3 person household as best benchmark
  
  act_gas_benchmarks <- read_excel(aer_gas_benchmarks_file,
                                   sheet = 'ACT',
                                   skip = 7) %>% 
    clean_names() %>% 
    select(-state) %>% 
    rename(state = region, household_size = household_size_3) %>% 
    select(state, season, household_size, benchmark_mj) %>% 
    filter(household_size == "3") %>% 
    select(-household_size) %>% 
    pivot_wider(names_from = season, values_from = benchmark_mj)
  

  tas_gas_benchmarks <- read_excel(aer_gas_benchmarks_file,
                                   sheet = 'TAS',
                                   skip = 7) %>% 
    clean_names() %>% 
    select(-state) %>% 
    rename(state = region, household_size = household_size_3) %>% 
    select(state, season, household_size, benchmark_mj) %>% 
    filter(household_size == "3") %>% 
    select(-household_size) %>% 
    pivot_wider(names_from = season, values_from = benchmark_mj)
  
  
  qld_gas_benchmarks <- read_excel(aer_gas_benchmarks_file,
                                   sheet = 'QLD',
                                   skip = 7) %>% 
    clean_names() %>% 
    select(-state) %>% 
    rename(state = region, household_size = household_size_3) %>% 
    select(state, season, household_size, benchmark_mj) %>% 
    filter(household_size == "3") %>% 
    select(-household_size) %>% 
    pivot_wider(names_from = season, values_from = benchmark_mj)
  
  
  nsw_gas_benchmarks <- read_excel(aer_gas_benchmarks_file,
                                   sheet = 'NSW',
                                   skip = 7) %>% 
    clean_names() %>% 
    select(-state) %>% 
    rename(state = region, household_size = household_size_3) %>% 
    select(state, season, household_size, benchmark_mj) %>% 
    filter(household_size == "3") %>% 
    select(-household_size) %>% 
    pivot_wider(names_from = season, values_from = benchmark_mj)
  
  sa_gas_benchmarks <- read_excel(aer_gas_benchmarks_file,
                                   sheet = 'SA',
                                   skip = 7) %>% 
    clean_names() %>% 
    select(-state) %>% 
    rename(state = region, household_size = household_size_3) %>% 
    select(state, season, household_size, benchmark_mj) %>% 
    filter(household_size == "3") %>% 
    select(-household_size) %>% 
    pivot_wider(names_from = season, values_from = benchmark_mj)
  
  vic_gas_benchmarks <- read_excel(aer_gas_benchmarks_file,
                                   sheet = 'VIC',
                                   skip = 7) %>% 
    clean_names() %>% 
    select(-state) %>% 
    rename(state = region, household_size = household_size_3) %>% 
    select(state, season, household_size, benchmark_mj) %>% 
    filter(household_size == "3") %>% 
    select(-household_size) %>% 
    pivot_wider(names_from = season, values_from = benchmark_mj)
  
  
  #WA gas consumption

  #Indicator D 8 https://gas.atco.com/content/dam/atco-gas-website/en-au/assets/documents/ATCO-Gas-Australia-23-24-ERA-Performance-Report.pdf
  wa_gas_residential_consumption_total <- 10357570
  wa_gas_residential_connections <- gas_connections_data %>% 
    filter(state == "WA") %>% pull(residential)
  
  #this figure is equal to that reported by EnergyNetworks: 
  #https://www.energynetworks.com.au/resources/fact-sheets/reliable-and-clean-gas-for-australian-homes-2/#:~:text=ACT%20NSW%20QLD%20SA%20TAS,1 
  #(National statistics by region)
  Wa_consumption_per_customer <- wa_gas_residential_consumption_total / wa_gas_residential_connections
  
  #Using data reported by EnergyNetworks:
  #https://www.energynetworks.com.au/resources/fact-sheets/reliable-and-clean-gas-for-australian-homes-2/#:~:text=ACT%20NSW%20QLD%20SA%20TAS,1 
  #(National gas network statistics)
  wa_winter_consumption <- 3 * 1.6
  wa_summer_consumption <- 3 * 0.7
  wa_autumn_spring_consumption <-  (Wa_consumption_per_customer - wa_winter_consumption - wa_summer_consumption)/2
  
  wa_gas_benchmarks <- tibble(state = "WA", 
                              Autumn = wa_autumn_spring_consumption * 1000,
                              Spring = wa_autumn_spring_consumption * 1000,
                              Summer = wa_summer_consumption * 1000,
                              Winter = wa_winter_consumption * 1000)
  
  gas_benchmarks <- bind_rows(act_gas_benchmarks, 
                              tas_gas_benchmarks, 
                              qld_gas_benchmarks, 
                              nsw_gas_benchmarks, 
                              sa_gas_benchmarks, 
                              vic_gas_benchmarks,
                              wa_gas_benchmarks) %>%
    clean_names() %>% 
    mutate(state = if_else(str_detect(state, "ACT"), "ACT", state),
           state = convert_states(state),
           benchmark_use_mj = summer + winter + autumn + spring) %>% 
    left_join(gas_connections_data %>% 
                select(state, residential),
              by = "state") 

  
  #aggregate NSW and ACT
  gas_benchmarks_weighted <- gas_benchmarks %>% 
    bind_rows(
      gas_benchmarks %>%
        filter(state %in% c("NSW", "ACT")) %>%
        summarise(
          state = "NSW and ACT",
          benchmark_use_mj = weighted.mean(benchmark_use_mj, residential),
          summer = weighted.mean(summer, residential),
          autumn = weighted.mean(autumn, residential),
          winter = weighted.mean(winter, residential),
          spring = weighted.mean(spring, residential),
          residential = sum(residential)
        )
    ) %>%
    # Remove the original NSW and ACT rows 
    filter(!state %in% c("NSW", "ACT")) %>% 
    select(-residential)
  
  gas_benchmarks_weighted 
  
}
