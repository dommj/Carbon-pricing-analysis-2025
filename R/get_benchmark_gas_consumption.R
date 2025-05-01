#this script uses AER benchmark data and connections data to produce the average gas consumption per connection for each state

get_benchmark_gas_consumption <- function(aer_gas_benchmarks_file,
                                          gas_connections_data){
  
  gas_benchmarks <- read_excel(aer_gas_benchmarks_file,
                               skip = 2) %>% 
    clean_names() %>% 
    rename(benchmark_use_mj = annual) %>% 
    select(-count) %>% 
    mutate(state = if_else(str_detect(state, "ACT"), "ACT", state),
           state = convert_states(state)) %>% 
    left_join(gas_connections_data %>% 
                select(state, residential),
              by = "state") 

  
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
