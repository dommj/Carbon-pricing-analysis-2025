####### Generation plots by scenario ######

plot_nem_generation_2035 <- function(results_ref,
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
  ref_generation <- ref_cells %>% 
    filter(sheet == "AustGeneration",
           row != 1,
           row <= 262) %>% 
    behead("up",
           "year") %>% 
    behead_if(result_formats_ref$local$font$bold[local_format_id] == T,
              direction = "left-up",
              name = "region") %>% 
    behead("left",
           "gen_type") %>% 
    mutate(generation_sent_out_gwh = content %>% as.numeric()) %>% 
    select(region, year, gen_type, generation_sent_out_gwh) %>% 
    filter(!is.na(generation_sent_out_gwh),
           !is.na(year),
           gen_type != "Total",
           #filter out aggregates that have sub-levels
           region %nin% c("Total NEM ", "Queensland")) %>% 
    mutate(year = as.numeric(year),
           scenario = "No new policy") 
  
  #2 degrees, RET policy
  ret_2_generation <- ret_2_cells %>% 
    filter(sheet == "AustGeneration",
           row != 1,
           row <= 262) %>% 
    behead("up",
           "year") %>% 
    behead_if(result_formats_ret_2$local$font$bold[local_format_id] == T,
              direction = "left-up",
              name = "region") %>% 
    behead("left",
           "gen_type") %>% 
    mutate(generation_sent_out_gwh = content %>% as.numeric()) %>% 
    select(region, year, gen_type, generation_sent_out_gwh) %>% 
    filter(!is.na(generation_sent_out_gwh),
           !is.na(year),
           gen_type != "Total",
           #filter out aggregates that have sub-levels
           region %nin% c("Total NEM ", "Queensland")) %>% 
    mutate(year = as.numeric(year),
           scenario = "RET < 2 C") 
  
  #1 degrees, Safeguard policy
  safeguard_2_generation <- safeguard_2_cells %>% 
    filter(sheet == "AustGeneration",
           row != 1,
           row <= 262) %>% 
    behead("up",
           "year") %>% 
    behead_if(result_formats_safeguard_2$local$font$bold[local_format_id] == T,
              direction = "left-up",
              name = "region") %>% 
    behead("left",
           "gen_type") %>% 
    mutate(generation_sent_out_gwh = content %>% as.numeric()) %>% 
    select(region, year, gen_type, generation_sent_out_gwh) %>% 
    filter(!is.na(generation_sent_out_gwh),
           !is.na(year),
           gen_type != "Total",
           #filter out aggregates that have sub-levels
           region %nin% c("Total NEM ", "Queensland")) %>% 
    mutate(year = as.numeric(year),
           scenario = "Safeguard < 2 C") 
  
  #1.5 degrees, RET policy
  ret_1_5_generation <- ret_1_5_cells %>% 
    filter(sheet == "AustGeneration",
           row != 1,
           row <= 262) %>% 
    behead("up",
           "year") %>% 
    behead_if(result_formats_ret_1_5$local$font$bold[local_format_id] == T,
              direction = "left-up",
              name = "region") %>% 
    behead("left",
           "gen_type") %>% 
    mutate(generation_sent_out_gwh = content %>% as.numeric()) %>% 
    select(region, year, gen_type, generation_sent_out_gwh) %>% 
    filter(!is.na(generation_sent_out_gwh),
           !is.na(year),
           gen_type != "Total",
           #filter out aggregates that have sub-levels
           region %nin% c("Total NEM ", "Queensland")) %>% 
    mutate(year = as.numeric(year),
           scenario = "RET < 1.5 C") 
  
  #1.5 degrees, Safeguard policy
  safeguard_1_5_generation <- safeguard_1_5_cells %>% 
    filter(sheet == "AustGeneration",
           row != 1,
           row <= 262) %>% 
    behead("up",
           "year") %>% 
    behead_if(result_formats_safeguard_1_5$local$font$bold[local_format_id] == T,
              direction = "left-up",
              name = "region") %>% 
    behead("left",
           "gen_type") %>% 
    mutate(generation_sent_out_gwh = content %>% as.numeric()) %>% 
    select(region, year, gen_type, generation_sent_out_gwh) %>% 
    filter(!is.na(generation_sent_out_gwh),
           !is.na(year),
           gen_type != "Total",
           #filter out aggregates that have sub-levels
           region %nin% c("Total NEM ", "Queensland")) %>% 
    mutate(year = as.numeric(year),
           scenario = "Safeguard < 1.5 C") 
  
  ##Combining datasets
  scenario_generation <- bind_rows(safeguard_1_5_generation,
                                   ret_1_5_generation,
                                   safeguard_2_generation, 
                                   ret_2_generation,
                                   ref_generation) %>% 
    mutate(type = case_when(str_detect(gen_type, 'Coal') ~ "Coal",
                            str_detect(gen_type, 'Gas|Hydrogen') ~ 'Gas',
                            str_detect(gen_type, '[w|W]ind') ~ "Wind",
                            str_detect(gen_type, 'Solar') ~ "Utility solar",
                            str_detect(gen_type, 'PV') ~ "Rooftop PV",
                            .default = gen_type),
           renewable = case_when(gen_type %in% c("Hydro",
                                                 "Wind",
                                                 "Biomass",
                                                 "Solar",
                                                 "Solar + Storage",
                                                 "Geothermal",
                                                 "Rooftop PV") ~ "Renewable",
                                 gen_type %in% c("Pumped Hydro", 
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
    mutate(source = fct_collapse(gen_type,
                                 'Wind' = c("Wind"),
                                 'Solar' = c("Rooftop PV", "Solar", "Solar + Storage"),
                                 'Hydro' = c("Hydro"),
                                 'Coal' = c("Black Coal", "Brown Coal"),
                                 'Gas' = c("Gas CC CCS", "Gas CCGT & Cogen",
                                           "Gas GT", "Gas Steam", "Hydrogen ready"),
                                 'Storage' = c("Battery Storage", "Embedded Battery", "Pumped Hydro"),
                                 'Other' = c("Biomass", "Distillate", 
                                             "Geothermal"))) %>%
    mutate(source = fct_relevel(source, "Wind", "Solar", "Hydro", "Storage", 
                                "Coal", "Gas", "Other"))
  
  ####### Plotting generation charts #######
  
  #Create generation palette
  gen_pal <- c("Solar" = grattan_yellow,
               "Wind" = grattan_orange,
               "Hydro" = grattan_lightblue,
               "Storage" = grattan_lightorange2,
               "Gas" = grattan_red,
               "Coal" = grattan_black,
               "Other" = grattan_lightblue6)
  
  # show_col(gen_pal)
  
  ## Plotting 2 degrees, RET policy chart
  
  #Creating chart data
  gen_plot_data <- scenario_generation %>%
    group_by(grid, scenario, year, source) %>%
    summarise(generation = sum(generation_sent_out_gwh)) %>%
    filter(grid == "NEM") %>%
    filter(year >= 2030, year <= 2035)
  
  source_levels <- levels(fct_rev(scenario_generation$source))
  
  source_labels <- data.frame(
    labels = source_levels,
    cols = gen_pal,
    x = c(2030, 2030, 2030, 2030, 2031, 2031, 2031),
    y = c(seq(280000,350000, length.out = 4), seq(300000, 350000, length.out = 3)),
    stringsAsFactors = FALSE,
    grid = "NEM")
  
 
  ## Plotting by scenario
  
  scenarios <- data.frame(
    scenario = c('No new policy', 'RET < 2 C', 'Safeguard < 2 C', 
                 'RET < 1.5 C', 'Safeguard < 1.5 C'),
    plot_name = c('gen_plot_ref', 'gen_plot_ret_2', 'gen_plot_safeguard_2', 
                  'gen_plot_ret_1_5', 'gen_plot_safeguard_1_5'),
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
    current_plot <- ggplot(gen_plot_data %>% filter(scenario == current_scenario)) +
      geom_col(aes(x = year, y = generation, fill = source, colour = source)) +
      scale_fill_manual(values = gen_pal) + 
      scale_colour_manual(values = gen_pal) + 
      scale_y_continuous(labels = function(x) paste0(x/1000, 'k GWH'),
                         expand = expansion(mult = c(0, 0.1)),
                         breaks = scales::breaks_pretty(n = 3)) +
      xlab('') +
      geom_text(data = source_labels,
                aes(x = x, y = y, label = labels, colour = labels),
                inherit.aes = F,
                hjust = 0, 
                nudge_x = -0.4,
                size = 6) + 
      labs(title = paste0(current_scenario, " - generation mix"),
           subtitle = "Generation in GWH, NEM",
           x = '',
           y = '') +
      theme_grattan()

    # Add to plot list
    plot_list[[plot_name]] <- current_plot
  }
  
  grattan_save_pptx(p = plot_list, filename = '/Users/bjjefferson/Grattan Institute Dropbox/Ben Jefferson/Apps/Overleaf/energy-2025-carbon-pricing-for-electricity/atlas/Backups/Backup Compendia/nem_generation_2035_1.pptx')
  return(plot_list)
  
}

