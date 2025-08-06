
get_jacobs_curtailment <- function(results_ref,
                                   results_1_5_Opt1,
                                   results_1_5_Opt2,
                                   results_2_Opt1,
                                   results_2_Opt2) {
  
  reference <- read_excel(results_ref,
                          sheet = 'Annual Black Price',
                          range = 'A68:AN74') %>%
    rename('state' = 'VRE Regional Economic Curtailment (%)') %>%
    pivot_longer(-state, names_to = 'year', values_to = 'curtailment') %>%
    clean_names() %>% 
    mutate(state = recode(state, 
           "Tasmania" = "Tas",
           "WEM" = "WA",
           "Victoria" = "Vic", 
           "New South Wales" = "NSW and ACT", 
           "Queensland" = "Qld",
           "South Australia" = "SA")) %>%
    mutate(scenario = 'reference')
  
  opt_1_1_5d <- read_excel(results_1_5_Opt1,
                           sheet = 'Annual Black Price',
                           range = 'A68:AN74') %>%
    rename('state' = 'VRE Regional Economic Curtailment (%)') %>%
    pivot_longer(-state, names_to = 'year', values_to = 'curtailment') %>%
    clean_names() %>% 
    mutate(state = recode(state, 
                          "Tasmania" = "Tas",
                          "WEM" = "WA",
                          "Victoria" = "Vic", 
                          "New South Wales" = "NSW and ACT", 
                          "Queensland" = "Qld",
                          "South Australia" = "SA")) %>%
    mutate(scenario = '1_5_Opt1')
  
  opt_2_1_5d <- read_excel(results_1_5_Opt2,
                           sheet = 'Annual Black Price',
                           range = 'A68:AN74') %>%
    rename('state' = 'VRE Regional Economic Curtailment (%)') %>%
    pivot_longer(-state, names_to = 'year', values_to = 'curtailment') %>%
    clean_names() %>% 
    mutate(state = recode(state, 
                          "Tasmania" = "Tas",
                          "WEM" = "WA",
                          "Victoria" = "Vic", 
                          "New South Wales" = "NSW and ACT", 
                          "Queensland" = "Qld",
                          "South Australia" = "SA")) %>%
    mutate(scenario = '1_5_Opt2')
  
  opt_1_2d <- read_excel(results_2_Opt1,
                           sheet = 'Annual Black Price',
                           range = 'A68:AN74') %>%
    rename('state' = 'VRE Regional Economic Curtailment (%)') %>%
    pivot_longer(-state, names_to = 'year', values_to = 'curtailment') %>%
    clean_names() %>% 
    mutate(state = recode(state, 
                          "Tasmania" = "Tas",
                          "WEM" = "WA",
                          "Victoria" = "Vic", 
                          "New South Wales" = "NSW and ACT", 
                          "Queensland" = "Qld", 
                          "South Australia" = "SA")) %>%
    mutate(scenario = '2_Opt1')
  
  opt_2_2d <- read_excel(results_2_Opt2,
                         sheet = 'Annual Black Price',
                         range = 'A68:AN74') %>%
    rename('state' = 'VRE Regional Economic Curtailment (%)') %>%
    pivot_longer(-state, names_to = 'year', values_to = 'curtailment') %>%
    clean_names() %>% 
    mutate(state = recode(state, 
                          "Tasmania" = "Tas",
                          "WEM" = "WA",
                          "Victoria" = "Vic", 
                          "New South Wales" = "NSW and ACT", 
                          "Queensland" = "Qld",
                          "South Australia" = "SA")) %>%
    mutate(scenario = '2_Opt2')
  
  jacobs_curtailment <- bind_rows(reference,
                                  opt_1_1_5d,
                                  opt_2_1_5d,
                                  opt_1_2d,
                                  opt_2_2d) %>%
    mutate(year = as.double(year))
  
  return(jacobs_curtailment)
  
}
