####### Generation plots by scenario ######


plot_capacity_charts <- function(results_ref,
                            results_1_5_Opt1,
                            results_1_5_Opt2,
                            results_2_Opt1,
                            results_2_Opt2) {
  
  ####### Reading in and formatting data #####
  
  ##Load results sheets from all scenarios, and formats
  ref_cells <- xlsx_cells(results_ref)
  ret_2_cells <- xlsx_cells(results_2_Opt1)
  safeguard_2_cells <- xlsx_cells(results_2_Opt2)
  ret_1_5_cells <- xlsx_cells(results_1_5_Opt1)
  safeguard_1_5_cells <- xlsx_cells(results_1_5_Opt2)
  
  result_formats_ref <- xlsx_formats(results_ref)
  result_formats_ret_2 <- xlsx_formats(results_2_Opt1)
  result_formats_safeguard_2 <- xlsx_formats(results_2_Opt2)
  result_formats_ret_1_5 <- xlsx_formats(results_1_5_Opt1)
  result_formats_safeguard_1_5 <- xlsx_formats(results_1_5_Opt2)
  
  ##Gathering relevant data from sheets
  
  #Reference case
  ref_capacity <- ref_cells %>% 
    filter(sheet == "AustCapacity",
           row != 1,
           row <= 262) %>% 
    behead("up",
           "year") %>% 
    behead_if(result_formats_ref$local$font$bold[local_format_id] == T,
              direction = "left-up",
              name = "region") %>% 
    behead("left",
           "cap_type") %>% 
    mutate(capacity_mw = content %>% as.numeric()) %>% 
    select(region, year, cap_type, capacity_mw) %>% 
    filter(!is.na(capacity_mw),
           !is.na(year),
           cap_type != "Total",
           #filter out aggregates that have sub-levels
           region %nin% c("Total NEM ", "Queensland")) %>% 
    mutate(year = as.numeric(year),
           scenario = "No new policy") 
  
  #2 degrees, RET policy
  ret_2_capacity <- ret_2_cells %>% 
    filter(sheet == "AustCapacity",
           row != 1,
           row <= 262) %>% 
    behead("up",
           "year") %>% 
    behead_if(result_formats_ret_2$local$font$bold[local_format_id] == T,
              direction = "left-up",
              name = "region") %>% 
    behead("left",
           "cap_type") %>% 
    mutate(capacity_mw = content %>% as.numeric()) %>% 
    select(region, year, cap_type, capacity_mw) %>% 
    filter(!is.na(capacity_mw),
           !is.na(year),
           cap_type != "Total",
           #filter out aggregates that have sub-levels
           region %nin% c("Total NEM ", "Queensland")) %>% 
    mutate(year = as.numeric(year),
           scenario = "RET < 2 C") 
  
  #1 degrees, Safeguard policy
  safeguard_2_capacity <- safeguard_2_cells %>% 
    filter(sheet == "AustCapacity",
           row != 1,
           row <= 262) %>% 
    behead("up",
           "year") %>% 
    behead_if(result_formats_safeguard_2$local$font$bold[local_format_id] == T,
              direction = "left-up",
              name = "region") %>% 
    behead("left",
           "cap_type") %>% 
    mutate(capacity_mw = content %>% as.numeric()) %>% 
    select(region, year, cap_type, capacity_mw) %>% 
    filter(!is.na(capacity_mw),
           !is.na(year),
           cap_type != "Total",
           #filter out aggregates that have sub-levels
           region %nin% c("Total NEM ", "Queensland")) %>% 
    mutate(year = as.numeric(year),
           scenario = "Safeguard < 2 C") 
  
  #1.5 degrees, RET policy
  ret_1_5_capacity <- ret_1_5_cells %>% 
    filter(sheet == "AustCapacity",
           row != 1,
           row <= 262) %>% 
    behead("up",
           "year") %>% 
    behead_if(result_formats_ret_1_5$local$font$bold[local_format_id] == T,
              direction = "left-up",
              name = "region") %>% 
    behead("left",
           "cap_type") %>% 
    mutate(capacity_mw = content %>% as.numeric()) %>% 
    select(region, year, cap_type, capacity_mw) %>% 
    filter(!is.na(capacity_mw),
           !is.na(year),
           cap_type != "Total",
           #filter out aggregates that have sub-levels
           region %nin% c("Total NEM ", "Queensland")) %>% 
    mutate(year = as.numeric(year),
           scenario = "RET < 1.5 C") 
  
  #1.5 degrees, Safeguard policy
  safeguard_1_5_capacity <- safeguard_1_5_cells %>% 
    filter(sheet == "AustCapacity",
           row != 1,
           row <= 262) %>% 
    behead("up",
           "year") %>% 
    behead_if(result_formats_safeguard_1_5$local$font$bold[local_format_id] == T,
              direction = "left-up",
              name = "region") %>% 
    behead("left",
           "cap_type") %>% 
    mutate(capacity_mw = content %>% as.numeric()) %>% 
    select(region, year, cap_type, capacity_mw) %>% 
    filter(!is.na(capacity_mw),
           !is.na(year),
           cap_type != "Total",
           #filter out aggregates that have sub-levels
           region %nin% c("Total NEM ", "Queensland")) %>% 
    mutate(year = as.numeric(year),
           scenario = "Safeguard < 1.5 C") 
  
  ##Combining datasets
  scenario_capacity <- bind_rows(safeguard_1_5_capacity,
                                   ret_1_5_capacity,
                                   safeguard_2_capacity, 
                                   ret_2_capacity,
                                   ref_capacity) %>% 
    mutate(type = case_when(str_detect(cap_type, 'Coal') ~ "Coal",
                            str_detect(cap_type, 'Gas|Hydrogen') ~ 'Gas',
                            str_detect(cap_type, '[w|W]ind') ~ "Wind",
                            str_detect(cap_type, 'Solar') ~ "Utility solar",
                            str_detect(cap_type, 'PV') ~ "Rooftop PV",
                            .default = cap_type),
           renewable = case_when(cap_type %in% c("Hydro",
                                                 "Wind",
                                                 "Biomass",
                                                 "Solar",
                                                 "Solar + Storage",
                                                 "Geothermal",
                                                 "Rooftop PV") ~ "Renewable",
                                 cap_type %in% c("Pumped Hydro", 
                                                 "Battery Storage",
                                                 "Embedded Battery") ~ "Storage",
                                 .default = "Fossil fuel")) %>%
    mutate(state = fct_collapse(region,
                                'NSW & ACT' = c("New South Wales"),
                                'VIC' = c("Victoria"),
                                'QLD' = c("Queensland Central", "Queensland North", "Queensland South",
                                          "Mt Isa"),
                                'WA' = c("WEM", "NW"),
                                'NT' = c("Northern Territory"),
                                'SA' = c("South Australia"),
                                'TAS' = c("Tasmania"))) %>% 
    mutate(grid = fct_collapse(region,
                               'NEM' = c("New South Wales", "Victoria", "Queensland South",
                                         "Queensland North", "Queensland Central", "Tasmania",
                                         "South Australia"),
                               'SWIS' = c("WEM"),
                               'DKIS' = c("Northern Territory"),
                               'Mt Isa' = c("Mt Isa"),
                               'NWIS' = c('NW'))) %>%
    mutate(source = fct_collapse(cap_type,
                                 'Wind' = c("Wind"),
                                 'Solar' = c("Rooftop PV", "Solar", "Solar + Storage"),
                                 'Hydro' = c("Pumped Hydro", "Hydro"),
                                 'Coal' = c("Black Coal", "Brown Coal"),
                                 'Gas' = c("Gas CC CCS", "Gas CCGT & Cogen",
                                           "Gas GT", "Gas Steam"),
                                 'Battery' = c("Battery Storage", "Embedded Battery"),
                                 'Other' = c("Biomass", "Distillate", 
                                             "Geothermal", "Hydrogen ready"))) %>%
    mutate(source = fct_relevel(source, "Wind", "Solar", "Hydro", "Battery", 
                                "Coal", "Gas", "Other"))
  
  ####### Plotting generation charts #######
  
  #Create generation palette
  cap_pal <- c("Solar" = grattan_yellow,
               "Wind" = grattan_orange,
               "Hydro" = grattan_lightblue,
               "Battery" = grattan_lightorange2,
               "Gas" = grattan_red,
               "Coal" = grattan_black,
               "Other" = grattan_lightblue6)
  
  # show_col(gen_pal)
  
  ## Plotting 2 degrees, RET policy chart
  
  #Creating chart data
  cap_plot_data <- scenario_capacity %>%
    group_by(grid, scenario, year, source) %>%
    summarise(capacity = sum(capacity_mw)) %>%
    filter(grid == "NEM" | grid == "SWIS")
  
  source_levels <- levels(fct_rev(scenario_capacity$source))
  
  source_labels <- data.frame(
    labels = source_levels,
    cols = cap_pal,
    x = c(2022.5, 2022.5, 2022.5, 2022.5, 2030, 2030, 2030),
    y = c(seq(270000,370000, length.out = 4), seq(320000, 370000, length.out = 3)),
    stringsAsFactors = FALSE,
    grid = "NEM")
  
 
  ## Plotting by scenario
  
  scenarios <- data.frame(
    scenario = c('No new policy', 'RET < 2 C', 'Safeguard < 2 C', 
                 'RET < 1.5 C', 'Safeguard < 1.5 C'),
    plot_name = c('cap_plot_ref', 'cap_plot_ret_2', 'cap_plot_safeguard_2', 
                  'cap_plot_ret_1_5', 'cap_plot_safeguard_1_5'),
    file_name = c('reference.pdf', '2_degree_ret.pdf', 
                  '2_degree_safeguard.pdf', '1_5_degree_ret.pdf',
                  '1_5_degree_safeguard.pdf'),
    stringsAsFactors = FALSE
  )
  
  plot_list <- list()
  
  # Loop through each scenario to create plots
  for (i in 1:nrow(scenarios)) {
    current_scenario <- scenarios$scenario[i]
    plot_name <- scenarios$plot_name[i]
    file_name <- scenarios$file_name[i]
    
    # Create plot
    current_plot <- ggplot(cap_plot_data %>% filter(scenario == current_scenario)) +
      geom_col(aes(x = year, y = capacity, fill = source, colour = source)) +
      facet_wrap(~grid,
                 ncol = 2,
                 scales = 'free_y') +
      scale_fill_manual(values = cap_pal) + 
      scale_colour_manual(values = cap_pal) + 
      scale_y_continuous(labels = function(x) paste0(x/1000, ' gw'),
                         expand = expansion(mult = c(0, 0.15)),
                         breaks = scales::breaks_pretty(n = 3)) +
      xlab('') +
      scale_x_continuous(expand = expansion(mult = c(0, 0.1))) +
      geom_text(data = source_labels,
                aes(x = x, y = y, label = labels, colour = labels),
                inherit.aes = F,
                hjust = 0, 
                size = 6) + 
      labs(title = paste0(current_scenario, " - capacity mix"),
           subtitle = "Capacity in GW by grid",
           x = '',
           y = '') +
      theme_grattan()

    # Add to plot list
    plot_list[[plot_name]] <- current_plot
  }
  current_plot
  check_chart_aspect_ratio(type = 'fullslide') 
  grattan_save_pptx(p = plot_list, filename = '/Users/bjjefferson/Grattan Institute Dropbox/Ben Jefferson/Apps/Overleaf/energy-2025-carbon-pricing-for-electricity/atlas/Backups/Backup Compendia/capacity_plots.pptx')
  return(plot_list)
  
}


