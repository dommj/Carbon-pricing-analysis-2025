#get household connections data

get_household_connections_data <- function(esoo_2024_assumptions_workbook_file, 
                                           abs_dwelling_completions_file){
  
  aemo_projections <- read_excel(esoo_2024_assumptions_workbook_file, 
             sheet = 'Connections Forecasts',
             range = 'B21:AG26') %>% 
    rename(state = 1) %>% 
    clean_names() %>% 
    pivot_longer(cols = -state, 
                 names_to = 'year', 
                 values_to = 'connections') %>%
    mutate(state = convert_states(state),
           state = if_else(state == 'NSW', 'NSW and ACT', state),
           year = paste0('20', 
                                str_match(year, 'x\\d\\d\\d\\d_(\\d\\d)')[,2]) %>% 
             as.numeric()) 
          
  
  #back calculate to 2020 using abs dwelling completions
  
  abs_completions <- read_excel(abs_dwelling_completions_file, 
                                sheet = "Table",
                                range = "B10:AA20") %>% 
    rename(state = 1) %>% 
    clean_names() %>% 
    select(- 2) %>% 
    pivot_longer(cols = contains('20'), names_to = "quarter", values_to = "completions") %>% 
    mutate(fy = if_else(str_detect(quarter, "q1|q2"), 
                        paste0(str_extract(quarter, "\\d\\d\\d\\d") %>% as.numeric() - 1, "-", str_extract(quarter, "\\d\\d\\d\\d")),
                        paste0(str_extract(quarter, "\\d\\d\\d\\d"), "-", str_extract(quarter, "\\d\\d\\d\\d")%>% as.numeric() + 1) ),
           year = str_extract_all(fy, "\\d\\d\\d\\d"),
           year = map_chr(year, ~ .x[2]) %>% as.numeric(),
           state = str_remove(state, "·  "),
           state = convert_states(state),
           state = if_else(state == "NSW" | state == "ACT", "NSW and ACT", state)) %>% 
    group_by(state, year) %>% 
    summarise(completions = sum(completions)) %>% 
    filter(!is.na(completions),
           state != "Aus")
  
  #this is only approximate, but early years are only needed to estimate changes in consumption per household, which should be fairly steady over the 4 years from 2020 to 2024
  
  back_calculated <- expand_grid(
    state = unique(aemo_projections$state),
    year = 2019:2023 ) %>%  # Years to backfill: 2019-2020 through 2022-2023
  bind_rows(aemo_projections) %>% 
    left_join(abs_completions, by = join_by(state, year)) %>% 
    group_by(state) %>% 
    arrange(state, desc(year)) %>% 
    group_split() %>%
    map_dfr(~{
      # For each state, calculate iteratively
      for(i in 2:nrow(.x)) {
        if(is.na(.x$connections[i])) {
          .x$connections[i] <- .x$connections[i-1] - .x$completions[i]
        }
      }
      return(.x)
    }) %>% 
    select(- completions)
  
  #overall trend / changes over time are whats important, so can simply map fy to calendar year without attempting some sort of averaging I think
  
  
  back_calculated
}


