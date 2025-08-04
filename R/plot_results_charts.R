#scenario results charts

plot_scenario_results <- function(jacobs_results_summary,
                                  results_ref,
                                  results_1_5_Opt1,
                                  results_1_5_Opt2,
                                  results_2_Opt1,
                                  results_2_Opt2){
  
  summary_cells <- xlsx_cells(jacobs_results_summary)
  
  summary_formats <- xlsx_formats(jacobs_results_summary)
  
  #plot emissions
  
  emissions_ag_ref <- summary_cells %>% 
    filter(sheet == "Emissions",
           row <= 4) %>% 
    behead("up-left",
           "temp") %>% 
    behead("up",
           "year") %>% 
    behead("left",
           "scenario") %>% 
    select(scenario, year, content) %>% 
    filter(!is.na(content)) %>% 
    rename(mt_co2_e = content)
  
  
  emissions_ag_options <- summary_cells %>% 
    filter(sheet == "Emissions",
           row <= 10,
           row %nin% c(1, 3, 4)) %>% 
    behead_if(summary_formats$local$alignment$indent[local_format_id] == 0,
              direction = "left-up",
              name = "temp") %>%
    behead("up",
           "year") %>% 
    behead("left",
           "scenario") %>% 
    select(scenario, year, temp, content) %>% 
    filter(!is.na(content)) %>% 
    rename(mt_co2_e = content)
  
  emissions_data <- emissions_ag_ref %>% 
    mutate(temp = "ref") %>% 
    bind_rows(emissions_ag_options) %>% 
    mutate(mt_co2_e = as.numeric(mt_co2_e),
           scenario = fct_case_when(scenario == "Reference case" ~ "Reference case",
                                    scenario == "Option One - LRET" & str_detect(temp, "1.5") ~ "RET 1.5 C",
                                    scenario == "Option Two - EIS" & str_detect(temp, "1.5") ~ "Safeguard 1.5 C",
                                    scenario == "Option One - LRET" & str_detect(temp, "2") ~ "RET < 2 C",
                                    scenario == "Option Two - EIS" & str_detect(temp, "2") ~ "Safeguard < 2 C"))
  
  
  chart_palette <- c("Reference case" = grattan_red,
                     "RET < 2 C" = grattan_yellow,
                     "Safeguard < 2 C" = grattan_orange)
  
  emissions_plot <- emissions_data %>% 
    filter(!str_detect(temp, "1.5")) %>% 
    ggplot(aes(x = year, y = mt_co2_e, colour = scenario)) +
    geom_line(size = 1) +
    grattan_label(data = . %>%  filter(year == 2040) %>% 
                mutate(x = year,
                       y = case_when(scenario == "Reference case" ~ 115,
                                     scenario == "RET < 2 C" ~ 100,
                                     scenario == "Safeguard < 2 C" ~ 85)), 
              aes(x = year, y = y, label = scenario, colour = scenario),
              hjust = 0) +
    grattan_y_continuous(limits = c(0, 120)) +
    theme_grattan() +
    scale_colour_manual(values = chart_palette) +
    labs(title = "Emissions fall quickly under the policy scenarios to meet climate goals",
         subtitle = "Electricity sector emissions (Mt CO2-e)",
         x = "",
         y = "") 
  
  
  grattan_save_all("C:/Users/domijones/Grattan Institute Dropbox/Dominic  Jones/Apps/Overleaf/energy-2025-carbon-pricing-for-electricity/atlas/2_degree_emissions_results.pdf",
                   object = emissions_plot)
  
  
  
  #Generation mix
  
  ref_cells <- xlsx_cells(results_ref)
  ret_2_cells <- xlsx_cells(results_2_Opt1)
  safeguard_2_cells <- xlsx_cells(results_2_Opt2)
  
  result_formats <- xlsx_formats(results_ref)
  
  
  ref_generation <- ref_cells %>% 
    filter(sheet == "AustGeneration",
           row != 1,
           row <= 262) %>% 
    behead("up",
           "year") %>% 
    behead_if(result_formats$local$font$bold[local_format_id] == T,
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
           type = case_when(str_detect(gen_type, 'Coal') ~ "Coal",
                              str_detect(gen_type, 'Gas|Hydrogen') ~ 'Gas',
                            str_detect(gen_type, '[w|W]ind') ~ "Wind",
                              str_detect(gen_type, 'Solar') ~ "Utility solar",
                              str_detect(gen_type, 'PV') ~ "Rooftop PV",
                            .default = gen_type)) 
  
  
  ret_2_generation <- ref_cells %>% 
    filter(sheet == "AustGeneration",
           row != 1,
           row <= 262) %>% 
    behead("up",
           "year") %>% 
    behead_if(result_formats$local$font$bold[local_format_id] == T,
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
           type = case_when(str_detect(gen_type, 'Coal') ~ "Coal",
                            str_detect(gen_type, 'Gas|Hydrogen') ~ 'Gas',
                            str_detect(gen_type, '[w|W]ind') ~ "Wind",
                            str_detect(gen_type, 'Solar') ~ "Utility solar",
                            str_detect(gen_type, 'PV') ~ "Rooftop PV",
                            .default = gen_type)) 
  
    
  
  
  ref_generation %>% 
    #filter out storage
    filter(type %nin% c("Pumped Hydro", "Battery Storage", "Embedded Battery")) %>% 
    group_by(year, type) %>% 
    summarise(generation_sent_out_gwh = sum(generation_sent_out_gwh)) %>% 
    ggplot(aes(x = year, y = generation_sent_out_gwh, fill = type)) +
    geom_col()
  
  

  

  
    
  
  
}

