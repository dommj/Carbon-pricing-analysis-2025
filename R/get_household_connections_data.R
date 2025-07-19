#get household connections data

get_household_connections_data <- function(esoo_2024_assumptions_workbook_file,
                                           wem_esoo_2024_data_register_file,
                                           abs_dwelling_completions_file){
  
  ##########################
  #NEM
  ##########################
  
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
  
  #look at shape of aemo projections to see if linear projection suitable for WEM projection - yes
  aemo_projections %>% 
    ggplot(aes(x = year, y = connections, colour = state)) +
    geom_line()
  
  
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
  
  nem_back_calculated <- expand_grid(
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
  
  ##########################
  #WEM
  ##########################
  
  #We take the start number of residential customers in the SWIS as those at june 30 2023
  #Source:Synergy regulatory data
  
  #https://www.synergy.net.au/-/media/Files/PDF-Library/Synergy-electricity-retail-licence-ERL1-performance-report-datasheet-2022_2023.pdf
  swis_residential_connections_23 <- 1062521
  
  #https://www.synergy.net.au/-/media/Documents/Policies/2022-Synergy-Electricity-Retail-Licence-Performance-Reporting-Datasheet.pdf
  swis_residential_connections_22 <- 1039188
  
  #https://www.synergy.net.au/-/media/Documents/Policies/2021-Electricity-Retail-Licence-Performance-Reporting-Datasheet---Synergy.pdf
  swis_residential_connections_21 <- 1023854
  
  #https://www.synergy.net.au/-/media/Documents/Policies/2020-Electricity-performance-reporting.pdf
  swis_residential_connections_20 <- 1013561
  
  wem_connections_20_23 <- tibble(year = c("2019-20", "2020-21", "2021-22", "2022-23"),
                                  connections = c(swis_residential_connections_20,
                                                  swis_residential_connections_21,
                                                  swis_residential_connections_22,
                                                  swis_residential_connections_23),
                                  state = "WA")
  
  #load new connections data and add existing 23-24  total connections to get connection projections
  wem_connections <- read_excel(wem_esoo_2024_data_register_file,
                                    sheet = "A1_F.4",
                                    range = "B23:M25") %>% 
    filter(Scenario == "2024 Expected") %>% 
    pivot_longer(cols = contains("20"), names_to = 'year', values_to = "new_connections") %>% 
    mutate(state = "WA",
           new_connections_cum = cumsum(new_connections),
           connections = swis_residential_connections_23 + new_connections_cum) %>% 
    select(-c(Scenario, new_connections)) %>% 
    bind_rows(wem_connections_20_23) %>% 
    mutate(year = paste0('20', 
                  str_match(year, '\\d\\d\\d\\d-(\\d\\d)')[,2]) %>% 
    as.numeric())
  
  #extrapolate forwards linearly. This projection is not actually used as we have decided to only report WA data up to 2034 due to lack of data
  
  # Fit linear model
  linear_model <- lm(connections ~ year, data = wem_connections)
  
  wem_connections_projected <- wem_connections %>% 
    complete(year = seq(2020,2050)) %>% 
    mutate(state = if_else(is.na(state), "WA", state),
           connections = if_else(is.na(connections), 
                        predict(linear_model, newdata = data.frame(year = year)), 
                        connections)) %>% 
    select(-new_connections_cum)
  
  #check projection
  wem_connections_projected %>% 
    ggplot(aes(x = year, y = connections, colour = state)) +
    geom_line()
  
  

  wem_nem_connections_projections <- bind_rows(nem_back_calculated, wem_connections_projected)
  
  
  #sense_check
  wem_nem_connections_projections %>% 
    ggplot(aes(x = year, y = connections, colour = state)) +
    geom_line()
  
  return(wem_nem_connections_projections)
  
}


