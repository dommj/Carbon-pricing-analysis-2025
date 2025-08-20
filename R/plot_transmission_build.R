plot_transmission_build <- function(results_ref,
                                    results_1_5_Opt1,
                                    results_1_5_Opt2,
                                    results_2_Opt1,
                                    results_2_Opt2) {
  
  # Define scenarios and their corresponding file paths
  transmission_scenarios <- data.frame(
    scenario = c('No new policy', 'RET < 2 C', 'Safeguard < 2 C', 'RET < 1.5 C', 'Safeguard < 1.5 C'),
    file_path = c(results_ref, results_2_Opt1, results_2_Opt2, results_1_5_Opt1, results_1_5_Opt2),
    plot_name = c('transmission_plot_ref', 'transmission_plot_ret_2', 'transmission_plot_safeguard_2', 'transmission_plot_ret_1_5', 'transmission_plot_safeguard_1_5'),
    file_name = c('reference_transmission_plot.pdf', '2_degree_ret_transmission_plot.pdf', '2_degree_safeguard_transmission_plot.pdf', '1_5_degree_ret_transmission_plot.pdf', '1_5_degree_safeguard_transmission_plot.pdf'),
    stringsAsFactors = FALSE
  )
  
  # Create empty list to store transmission plots
  transmission_plot_list <- list()
  
  # Loop through each scenario to create transmission plots
  for (i in 1:nrow(transmission_scenarios)) {
    current_scenario <- transmission_scenarios$scenario[i]
    file_path <- transmission_scenarios$file_path[i]
    plot_name <- transmission_scenarios$plot_name[i]
    file_name <- transmission_scenarios$file_name[i]
    
    # Read and process data for current scenario
    transmission_data <- read_excel(file_path,
                                    sheet = 'NEMInterconnector',
                                    range = 'A2:AO81') %>%
      clean_names() %>%
      filter(x2 == 'MW') %>%
      mutate(financial_year = str_remove(financial_year, " Firm Capacity$")) %>%
      select(-x2) %>%
      pivot_longer(-financial_year,
                   values_to = 'mw',
                   names_to = 'year') %>%
      mutate(year = as.numeric(str_remove(year, 'x'))) %>%
      filter(year >= 2025, year <= 2050) %>%
      rename(connection = financial_year) %>%
      mutate(connection = forcats::fct_recode(connection,
                                              'NSW-SA' = "NSW- SA",
                                              'NSW-Snowy' = 'NSW-SNOWY',
                                              'QLD Central-\nQLD South' = 'QLDCEN-QLDSTH',
                                              'QLD North-\nQLD Central' = 'QLDNTH-QLDCEN',
                                              'QLD South-NSW' = 'QLDSTH-NSW',
                                              'Redcliff-VIC' = 'REDCLIFF-VIC',
                                              'SA-Redcliff' = 'SA-REDCLIFF',
                                              'SA-VIC' = 'SA-VIC',
                                              'Snowy-VIC' = 'SNOWY-VIC',
                                              'VIC-TAS' = 'VIC to TAS'))
    
    # Create plot
    current_transmission_plot <- ggplot(transmission_data) + 
      geom_col(aes(x = year, y = mw, colour = grattan_orange, fill = grattan_orange)) +
      facet_wrap(~connection) +
      scale_x_continuous(breaks = seq(2025, 2050, by = 10)) +
      scale_y_continuous(labels = function(x) paste0(comma(x/1000), "k MW"),
                         expand = expansion(mult = c(0, -0.05))) + 
      xlab('') +
      theme_grattan()
    
    # Save plot
    grattan_save_all(paste0("/Users/bjjefferson/Grattan Institute Dropbox/Ben Jefferson/Apps/Overleaf/energy-2025-carbon-pricing-for-electricity/atlas/", file_name),
                     object = current_transmission_plot)
    
    # Add to plot list
    transmission_plot_list[[plot_name]] <- current_transmission_plot
  }
  
  # Return the list of plots
  return(transmission_plot_list)
}