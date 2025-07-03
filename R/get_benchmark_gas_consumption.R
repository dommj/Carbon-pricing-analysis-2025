#this script uses AER benchmark data and connections data to produce the average gas consumption per connection for each state

get_benchmark_gas_consumption <- function(aer_gas_benchmarks_file,
                                          gas_connections_data){
  
  #average number of persons per household is 2.5 australia-wide and essentially constant by state
  #gas use is non linear with household number but so we take 3 person household as best benchmark
  
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
  
  
  
  gas_benchmarks <- bind_rows(act_gas_benchmarks, 
                              tas_gas_benchmarks, 
                              qld_gas_benchmarks, 
                              nsw_gas_benchmarks, 
                              sa_gas_benchmarks, 
                              vic_gas_benchmarks) %>%
    clean_names() %>% 
    mutate(state = if_else(str_detect(state, "ACT"), "ACT", state),
           state = convert_states(state),
           benchmark_use_mj = summer + winter + autumn + spring) %>% 
    left_join(gas_connections_data %>% 
                select(state, residential),
              by = "state") %>% 
    #we don't have gas data for Tas so we filter out for now
    filter(state != "Tas")

  
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
