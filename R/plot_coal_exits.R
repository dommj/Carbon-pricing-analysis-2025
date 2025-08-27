#Plotting coal exits 
plot_coal_exits <- function(results_ref,
                            results_1_5_Opt1,
                            results_1_5_Opt2,
                            results_2_Opt1,
                            results_2_Opt2) {
  
  # Define scenarios and their corresponding file paths
  coal_scenarios <- data.frame(
    scenario = c('No new policy', 'RET < 2 C', 'Safeguard < 2 C', 'RET < 1.5 C', 'Safeguard < 1.5 C'),
    file_path = c(results_ref, results_2_Opt1, results_2_Opt2, results_1_5_Opt1, results_1_5_Opt2),
    plot_name = c('coal_exit_plot_ref', 'coal_exit_plot_ret_2', 'coal_exit_plot_safeguard_2', 'coal_exit_plot_ret_1_5', 'coal_exit_plot_safeguard_1_5'),
    file_name_grid = c('reference_coal_exit_grid_plot.pdf', '2_degree_ret_coal_exit_grid_plot.pdf', '2_degree_safeguard_coal_exit_grid_plot.pdf', '1_5_degree_ret_coal_exit_grid_plot.pdf', '1_5_degree_safeguard_coal_exit_grid_plot.pdf'),
    file_name_state = c('reference_coal_exit_state_plot.pdf', '2_degree_ret_coal_exit_state_plot.pdf', '2_degree_safeguard_coal_exit_state_plot.pdf', '1_5_degree_ret_coal_exit_state_plot.pdf', '1_5_degree_safeguard_coal_exit_state_plot.pdf'),
    stringsAsFactors = FALSE
  )
  
  # Plant names mapping
  plant_names <- c(
    "LIDDELL" = "Liddell",
    "ERARING" = "Eraring", 
    "YALL" = "Yallourn",
    "CALLIDEB" = "Callide B",
    "BAYSWATR" = "Bayswater",
    "VALESPNT" = "Vales Point",
    "GPS" = "Gladstone",
    "LYA" = "Loy Yang A",
    "TARONG" = "Tarong",
    "TARONGN" = "Tarong North", 
    "MTPIPER" = "Mt Piper",
    "KoganCrk" = "Kogan Creek",
    "STANWELL" = "Stanwell",
    "LYB" = "Loy Yang B",
    "CALLIDEC" = "Callide C",
    "MILLMERN" = "Millmerran",
    "MujaC" = "Muja C",
    "Collie" = "Collie",
    "MujaD" = "Muja D"
  )
  
  # Create empty list to store coal exit plots
  coal_plot_list <- list()
  
  # Loop through each scenario to create coal exit plots
  for (i in 1:nrow(coal_scenarios)) {
    current_scenario <- coal_scenarios$scenario[i]
    file_path <- coal_scenarios$file_path[i]
    plot_name <- coal_scenarios$plot_name[i]
    file_name_grid <- coal_scenarios$file_name_grid[i]
    file_name_state <- coal_scenarios$file_name_state[i]
    
    # Read and process data for current scenario
    coal_exit <- read_excel(file_path,
                            sheet = 'CoalRetirements', 
                            range = 'A2:AP22') %>%
      clean_names() %>%
      pivot_longer(-c(plant, type, state), 
                   values_to = 'mw',
                   names_to = 'year') %>%
      mutate(year = as.numeric(str_remove(year, 'x'))) %>%
      mutate(mw = replace_na(mw, 0)) %>%
      mutate(grid = fct_collapse(state,
                                 'NSW' = c("New South Wales"),
                                 'VIC' = c("Victoria"),
                                 'QLD' = c("Queensland Central",
                                           "Queensland South"),
                                 'WA' = c("WEM"))) %>%
      mutate(eraring_yallourn = ifelse(plant == "YALL", yes = "Yallourn",
                                       ifelse(plant == "ERARING", yes = "Eraring",
                                              no = "Other")),
             no = "Other") %>%
      mutate(nem = fct_collapse(grid,
                                'NEM' = c('NSW', 'VIC', 'QLD'),
                                'SWIS' = c('WA'))) %>%
      mutate(eraring_yallourn = fct_relevel(eraring_yallourn, "Other", "Eraring", "Yallourn")) %>%
      group_by(grid) %>%
      arrange(-mw)
    
    # Calculate totals for grid plot
    totals <- coal_exit %>%
      group_by(nem, year) %>%
      summarise(total = sum(mw), .groups = 'drop')
    
    # Calculate exit events
    exit_events <- coal_exit %>%
      arrange(plant, nem, year) %>%
      group_by(plant, nem) %>%
      mutate(prev = dplyr::lag(mw),
             exit = (mw == 0 & dplyr::coalesce(prev, 0) > 0)) %>%
      filter(exit) %>%
      summarise(exit_year = first(year), .groups = 'drop')
    
    # Create label frame for grid plot
    label_frame <- exit_events %>%
      mutate(plant_name = ifelse(plant %in% names(plant_names),
                                 plant_names[plant],
                                 plant)) %>%
      group_by(nem, exit_year) %>%
      summarise(plant = paste(plant_name, collapse = ',\n'), .groups = 'drop') %>%
      left_join(totals, by = c('nem', 'exit_year' = 'year')) %>%
      left_join(
        totals %>%
          group_by(nem) %>%
          summarise(max_total = max(total), .groups = 'drop'),
        by = 'nem'
      ) %>%
      mutate(y = total + 0.01 * max_total) %>%
      transmute(nem, year = exit_year-0.2, y, label = plant)
    
    # Create grid plot
    coal_exit_plot_by_grid <- ggplot(totals) +
      geom_col(aes(x = year, y = total),
               colour = grattan_orange, fill = grattan_orange) +
      facet_wrap(~nem, ncol = 2, scales = "free_y") +
      geom_text(data = label_frame,
                aes(x = year, y = y, label = label),
                hjust = 0, vjust = 0, size = 3, lineheight = 0.7,
                colour = grattan_black) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.05)),
                         labels = function(x) paste0(scales::comma(x/1000), "k MW")) +
      xlab("") +
      labs(title = paste0(current_scenario, " - coal capacity with exiting plants labelled"),
           subtitle = "Total coal power capacity",
           x = '',
           y = '') +
      coord_cartesian(clip = "off") +
      theme_grattan() +
      theme(plot.margin = margin(5.5, 30, 5.5, 5.5))
    
    # Save plots
    grattan_save_all(paste0("/Users/bjjefferson/Grattan Institute Dropbox/Ben Jefferson/Apps/Overleaf/energy-2025-carbon-pricing-for-electricity/atlas/Backups/", 
                            current_scenario, " backups/", plot_name),
                     object = coal_exit_plot_by_grid)
    
    # Add to plot list
    coal_plot_list[[paste0(plot_name, "_grid")]] <- coal_exit_plot_by_grid
  }
  
  # Return the list of plots
  grattan_save_pptx(p = coal_plot_list, filename = '/Users/bjjefferson/Grattan Institute Dropbox/Ben Jefferson/Apps/Overleaf/energy-2025-carbon-pricing-for-electricity/atlas/Backups/Backup Compendia/coal_exit_plots.pptx')
  return(coal_plot_list)
}
