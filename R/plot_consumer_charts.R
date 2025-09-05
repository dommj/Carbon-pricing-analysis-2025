#consumer charts


plot_consumer_charts <- function(average_net_costs,
                                 household_connections,
                                 cameo_electricity_costs,
                                 cameo_gas_costs,
                                 cameo_petrol_costs){
  
  
  chart_palette_fuels <- c(
    "Petrol" = grattan_red,
    "Gas" = grattan_orange, 
    "Electricity" = grattan_yellow
  )
  
  
  chart_palette_scenarios <- c("No new policy" = grattan_red,
                     "RET < 2 C" = grattan_yellow,
                     "Safeguard < 2 C" = grattan_orange,
                     "RET < 1.5 C" = grattan_yellow2,
                     "Safeguard < 1.5 C" = grattan_orange2)
  
  
  #average electricity bills 2C 
  chart_data_2c <- average_net_costs %>% 
    left_join(household_connections) %>% 
    filter(state != "WA",
           scenario == "Ref" | scenario == "2_Opt2" | scenario == "2_Opt1", #Have to come back here to get all the scenarios in by degrees
           #electrification == T is the default scenario that we use for all, showing the expected electrification
           electrification == T) %>% 
    group_by(year, scenario, category) %>% 
    summarise(average_cost_dollars = weighted.mean(average_cost_dollars, connections)) %>% 
    mutate(category = fct(category, levels = c("Petrol", "Electricity", "Gas")),
           scenario = fct_case_when(scenario == "Ref" ~ "No new policy",
                                    scenario == "1_5_Opt1" ~ "RET < 1.5 C",
                                    scenario == "1_5_Opt2"  ~ "Safeguard < 1.5 C",
                                    scenario == "2_Opt1"  ~ "RET < 2 C",
                                    scenario == "2_Opt2" ~ "Safeguard < 2 C")) %>% 
    ungroup() 
  
  
  annual_chart_data_2c <- chart_data_2c %>% 
    filter(category == "Electricity") %>% 
    group_by(scenario) %>% 
    summarise(annualised_cost = sum(average_cost_dollars) / n()) %>% 
    mutate(facet = "Total bills") %>% 
    bind_rows(
      
      chart_data_2c %>% 
        filter(category == "Electricity") %>% 
        group_by(scenario) %>% 
        summarise(annualised_cost = sum(average_cost_dollars) / n()) %>% 
        ungroup() %>% 
        mutate(annualised_cost = annualised_cost - annualised_cost[scenario == "No new policy"],
               facet = "Difference in bills")
      
    )
  
  p1_2c <- annual_chart_data_2c %>% 
    filter(facet == "Total bills") %>% 
    ggplot(aes(reorder(x = scenario, annualised_cost), y = annualised_cost, fill = scenario)) +
    facet_wrap(~facet, ncol = 1) +
    geom_col() +
    grattan_y_continuous(labels = scales::dollar_format()) +
    theme_grattan() +
    labs(x = "") +
    scale_fill_manual(values = chart_palette_scenarios)
  
  p2_2c <- annual_chart_data_2c %>% 
    filter(facet == "Difference in bills") %>% 
    #set zero value to NA so we don't get a white box
    mutate(annualised_cost = if_else(annualised_cost ==0, NA, annualised_cost)) %>% 
    ggplot(aes(x = scenario, y = annualised_cost, fill = scenario)) +
    facet_wrap(~facet, ncol = 1) +
    geom_col() +
    grattan_y_continuous(labels = scales::dollar_format()) +
    scale_x_discrete(limits = c("No new policy", "RET < 2 C", "Safeguard < 2 C")) +
    scale_fill_manual(values = chart_palette_scenarios) +
    theme_grattan() +
    labs(x = "")
  
  # Combine plots vertically using patchwork
  bill_diff_plot_1_2c <- p1_2c + p2_2c
  bill_diff_plot_2_2c <- p1_2c / p2_2c
  
  bill_diff_plot_1_2c
  
  grattan_save_all("/Users/bjjefferson/Grattan Institute Dropbox/Ben Jefferson/Apps/Overleaf/energy-2025-carbon-pricing-for-electricity/atlas/Energy Bills/2_degree_dif_in_bills_by_side.pdf",
                   object = bill_diff_plot_1_2c)   

  grattan_save_all("/Users/bjjefferson/Grattan Institute Dropbox/Ben Jefferson/Apps/Overleaf/energy-2025-carbon-pricing-for-electricity/atlas/Energy Bills/2_degree_dif_in_bills_stack.pdf",
                   object = bill_diff_plot_2_2c) 
  
  #average electricity bills 1.5C
  chart_data_15c <- average_net_costs %>% 
    left_join(household_connections) %>% 
    filter(state != "WA",
           scenario == "Ref" | scenario == "1_5_Opt2" | scenario == "1_5_Opt1", #Have to come back here to get all the scenarios in by degrees
           #electrification == T is the default scenario that we use for all, showing the expected electrification
           electrification == T) %>% 
    group_by(year, scenario, category) %>% 
    summarise(average_cost_dollars = weighted.mean(average_cost_dollars, connections)) %>% 
    mutate(category = fct(category, levels = c("Petrol", "Electricity", "Gas")),
           scenario = fct_case_when(scenario == "Ref" ~ "No new policy",
                                    scenario == "1_5_Opt1" ~ "RET < 1.5 C",
                                    scenario == "1_5_Opt2"  ~ "Safeguard < 1.5 C",
                                    scenario == "2_Opt1"  ~ "RET < 2 C",
                                    scenario == "2_Opt2" ~ "Safeguard < 2 C")) %>% 
    ungroup() 
  
  
  annual_chart_data_15c <- chart_data_15c %>% 
    filter(category == "Electricity") %>% 
    group_by(scenario) %>% 
    summarise(annualised_cost = sum(average_cost_dollars) / n()) %>% 
    mutate(facet = "Total bills") %>% 
    bind_rows(
      
      chart_data_15c %>% 
        filter(category == "Electricity") %>% 
        group_by(scenario) %>% 
        summarise(annualised_cost = sum(average_cost_dollars) / n()) %>% 
        ungroup() %>% 
        mutate(annualised_cost = annualised_cost - annualised_cost[scenario == "No new policy"],
               facet = "Difference in bills")
      
    )
  
  p1_15c <- annual_chart_data_15c %>% 
    filter(facet == "Total bills") %>% 
    ggplot(aes(reorder(x = scenario, annualised_cost), y = annualised_cost, fill = scenario)) +
    facet_wrap(~facet, ncol = 1) +
    geom_col() +
    grattan_y_continuous(labels = scales::dollar_format()) +
    theme_grattan() +
    labs(x = "") +
    scale_fill_manual(values = chart_palette_scenarios)
  
  p2_15c <- annual_chart_data_15c %>% 
    filter(facet == "Difference in bills") %>% 
    #set zero value to NA so we don't get a white box
    mutate(annualised_cost = if_else(annualised_cost == 0, NA, annualised_cost)) %>% 
    ggplot(aes(x = scenario, y = annualised_cost, fill = scenario)) +
    facet_wrap(~facet, ncol = 1) +
    geom_col() +
    grattan_y_continuous(labels = scales::dollar_format()) +
    scale_x_discrete(limits = c("No new policy", "RET < 1.5 C", "Safeguard < 1.5 C")) +
    scale_fill_manual(values = chart_palette_scenarios) +
    theme_grattan() +
    labs(x = "")
  
  # Combine plots vertically using patchwork
  bill_diff_plot_1_15c <- p1_15c + p2_15c
  bill_diff_plot_2_15c <- p1_15c / p2_15c
  
  grattan_save_all("/Users/bjjefferson/Grattan Institute Dropbox/Ben Jefferson/Apps/Overleaf/energy-2025-carbon-pricing-for-electricity/atlas/1.5C/Energy Bills/1_5_degree_dif_in_bills_by_side.pdf",
                   object = bill_diff_plot_1_15c)   
  
  grattan_save_all("/Users/bjjefferson/Grattan Institute Dropbox/Ben Jefferson/Apps/Overleaf/energy-2025-carbon-pricing-for-electricity/atlas/1.5C/Energy Bills/1_5_degree_dif_in_bills_stack.pdf",
                   object = bill_diff_plot_2_15c)   
  
  
  
  ####################################
  #total energy Wallet over time - 2C
  ####################################
  label_data_2c <- chart_data_2c %>% 
    filter(scenario == "Safeguard < 2 C") %>% 
    # Calculate the y positions for the labels at the midpoints of each stack
    group_by(year) %>% 
    arrange(category) %>% 
    mutate(y_position = cumsum(average_cost_dollars),
           y_position = y_position - 0.5 * average_cost_dollars) %>% 
    ungroup()
  
  gap_data_2c <- chart_data_2c %>% 
    filter(scenario == "Safeguard < 2 C",
           year >= 2025) %>%
    group_by(year) %>% 
    summarise(average_cost_dollars = sum(average_cost_dollars)) %>% 
    ungroup() %>% 
    mutate(gap = average_cost_dollars - average_cost_dollars[year==2025])

  energy_wallet_over_time_plot_2c <- chart_data_2c %>% 
    filter(scenario == "Safeguard < 2 C",
           year >= 2025) %>% 
    ggplot(aes(x = year, y = average_cost_dollars)) +
    geom_col(aes(x = year, y = average_cost_dollars, 
                 colour = fct_rev(category), fill = fct_rev(category))) +
    grattan_label(data = label_data_2c %>% 
                    filter(year == 2050), 
                  aes(x = year, 
                      y = y_position, 
                      label = str_wrap(category, 13),
                      colour = category),
                  hjust = 0,
                  nudge_x = 0.6) +
    
    grattan_y_continuous(labels = scales::dollar_format(), expand_top = 0.1) +
    scale_x_continuous_grattan(expand_right = 0.17,
                               breaks = seq(2025,2050, by = 5)) +
    scale_colour_manual(values = chart_palette_fuels) +
    scale_fill_manual(values = chart_palette_fuels) +
    theme_grattan() +
    labs(title = paste0("Households are set to save on energy costs throughout the transition"),
         subtitle = 'Average NEM household energy costs in the Safeguard < 2 C scenario, ($2025)',
         x = '',
         y = '',
         caption = "Notes: \nSource: Grattan Institute analaysis see app X")
  
    grattan_save_all("/Users/bjjefferson/Grattan Institute Dropbox/Ben Jefferson/Apps/Overleaf/energy-2025-carbon-pricing-for-electricity/atlas/Energy Bills/2_degree_energy_cost_over_time_safeguard.pdf",
                   object = energy_wallet_over_time_plot_2c)
  
  
    ######################################
    #total energy Wallet over time - 1.5C
    ######################################
    label_data_15c <- chart_data_15c %>% 
      filter(scenario == "Safeguard < 1.5 C") %>% 
      # Calculate the y positions for the labels at the midpoints of each stack
      group_by(year) %>% 
      arrange(category) %>% 
      mutate(y_position = cumsum(average_cost_dollars),
             y_position = y_position - 0.5 * average_cost_dollars) %>% 
      ungroup()
    
    gap_data_15c <- chart_data_15c %>% 
      filter(scenario == "Safeguard < 1.5 C",
             year >= 2025) %>%
      group_by(year) %>% 
      summarise(average_cost_dollars = sum(average_cost_dollars)) %>% 
      ungroup() %>% 
      mutate(gap = average_cost_dollars - average_cost_dollars[year==2025])
    
    energy_wallet_over_time_plot_15c <- chart_data_15c %>% 
      filter(scenario == "Safeguard < 1.5 C",
             year >= 2025) %>% 
      ggplot(aes(x = year, y = average_cost_dollars)) +
      geom_col(aes(x = year, y = average_cost_dollars, 
                   colour = fct_rev(category), fill = fct_rev(category))) +
      grattan_label(data = label_data_15c %>% 
                      filter(year == 2050), 
                    aes(x = year, 
                        y = y_position, 
                        label = str_wrap(category, 13),
                        colour = category),
                    hjust = 0,
                    nudge_x = 0.6) +
      
      grattan_y_continuous(labels = scales::dollar_format(), expand_top = 0.1) +
      scale_x_continuous_grattan(expand_right = 0.17,
                                 breaks = seq(2025,2050, by = 5)) +
      scale_colour_manual(values = chart_palette_fuels) +
      scale_fill_manual(values = chart_palette_fuels) +
      theme_grattan() +
      labs(title = paste0("Households are set to save on energy costs throughout the transition"),
           subtitle = 'Average NEM household energy costs in the Safeguard < 1.5 C scenario, ($2025)',
           x = '',
           y = '',
           caption = "Notes: \nSource: Grattan Institute analaysis see app X")
    
    grattan_save_all("/Users/bjjefferson/Grattan Institute Dropbox/Ben Jefferson/Apps/Overleaf/energy-2025-carbon-pricing-for-electricity/atlas/1.5C/Energy Bills/1_5_degree_energy_cost_over_time_safeguard.pdf",
                     object = energy_wallet_over_time_plot_15c)
  
  #### Creating waterfall plotting function #####
  
  create_plot <- function(data, label){
    
    
    bar_text <- data %>% 
      mutate(chart_value = if_else(chart_value >=0, 
                                   paste0('$',signif(chart_value, 3)),
                                   paste0('-$',-signif(chart_value, 3)))) %>% 
      pull(chart_value)
    
    total_text <- paste0("$", data %>% 
                           pull(chart_value) %>% 
                           sum() %>% 
                           signif(3))
    
    data %>% 
      select(-state) %>% 
      waterfall(calc_total= TRUE, 
                rect_text_labels = bar_text,
                total_rect_text = total_text,
                total_rect_border_color = NA,
                rect_text_size = 2,
                rect_border = NA,
                theme_text_family = "theme_grattan",
                fill_colours = c(grattan_red,
                                 grattan_orange,
                                 grattan_orange,
                                 grattan_orange,
                                 grattan_orange),
                total_rect_color = grattan_yellow,
                fill_by_sign = F,
                total_axis_text = "Fully electrified home") +
      theme_grattan() +
      grattan_label(data = chart_label_data %>% 
                      filter(state == label,
                             consumer_name != "Fossil fuel home"),
                    aes(x = x_value + 1, y = y_value,
                        label = consumer_name),
                    hjust = 0,
                    nudge_x = -0.5) +
      grattan_y_continuous(limits = c(0, 6200), labels = scales::dollar_format()) +
      scale_x_discrete(breaks = c("Electrify gas","Switch to an EV"), labels = axis_labels,
                       expand = expansion(c(0,0.2)) ) +
      ggtitle(label) +
      theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold")) +
      labs(x = "")
    
  }
  
  
  #####################################
  #Switch 'n' Save - 2C Safeguard
  #####################################
  
  #this plot shows the difference in annual bills for a household in 2025 depending on their consumer type
  #Create cameo plot data for 2C Safeguard
  
  cameo_cost_data_2c_safeguard <- bind_rows(cameo_electricity_costs, cameo_gas_costs %>% 
                                              cross_join(tibble(scenario = cameo_electricity_costs %>% 
                                                                  ungroup() %>% 
                                                                  select(scenario) %>% 
                                                                  unique() %>% 
                                                                  pull()))) %>% 
    cross_join(tibble(ice = c(0,1,2))) %>% 
    bind_rows(cameo_petrol_costs %>% 
                cross_join(tibble(scenario = cameo_electricity_costs %>% 
                                    ungroup() %>% 
                                    select(scenario) %>% 
                                    unique() %>% 
                                    pull()))) %>% 
    filter(scenario == "2_Opt2")
  
  
  #create chart data for 2C Safeguard analysis 
  electrification_data_2c_safeguard <- cameo_cost_data_2c_safeguard %>% 
    filter(year == 2025) %>%
    group_by(year, cooking, water_heating, space_heating, ev, pv, battery, ice, state) %>% 
    summarise(total_cost_dollars = sum(annual_cost_dollars)) %>% 
    mutate(consumer_type = paste(cooking, water_heating, space_heating, ev, pv, battery, ice, sep = "_")) %>% 
    filter(consumer_type %in% c("gas_gas_gas_0_FALSE_FALSE_1", # all gas with an ICE vehicle
                                "gas_gas_gas_1_FALSE_FALSE_0",
                                "gas_gas_gas_1_TRUE_FALSE_0",
                                "gas_gas_gas_1_TRUE_TRUE_0",
                                "electric_electric_electric_1_TRUE_TRUE_0")) %>% 
    mutate(consumer_name = fct_case_when(consumer_type == "gas_gas_gas_0_FALSE_FALSE_1" ~ "Fossil fuel home",
                                         consumer_type == "gas_gas_gas_1_FALSE_FALSE_0" ~ "Switch to an EV",
                                         consumer_type == "gas_gas_gas_1_TRUE_FALSE_0" ~ "Install solar",
                                         consumer_type == "gas_gas_gas_1_TRUE_TRUE_0" ~ "Install a battery",
                                         consumer_type == "electric_electric_electric_1_TRUE_TRUE_0" ~ "Electrify gas") %>% fct_rev()) %>% 
    filter(state %nin% c("WA", "NT", "Aus")) %>% 
    ungroup()
  
  
  # average_electrification_data <- electrification_data %>% 
  #   left_join(household_connections) %>% 
  #   filter(!is.na(connections)) %>% 
  #   group_by(consumer_name) %>% 
  #   summarise(total_cost_dollars = weighted.mean(total_cost_dollars, connections))
  
  
  fall_chart_data_2c_safeguard <- electrification_data_2c_safeguard %>% 
    select(state, consumer_name, total_cost_dollars) %>% 
    group_by(state) %>% 
    arrange(consumer_name) %>% 
    mutate(chart_value = if_else(consumer_name %in% c("Fossil fuel home"), total_cost_dollars, total_cost_dollars - lead(total_cost_dollars))) %>% 
    ungroup() %>% 
    select(state, consumer_name, chart_value) %>% 
    arrange(fct_rev(consumer_name))
  
  
  # Get the category names for axis labels
  axis_labels <- c("Fossil fuel home",  "Fully electrified home")
  chart_label_data <- fall_chart_data_2c_safeguard %>% 
    group_by(state) %>% 
    mutate(y_value = cumsum(chart_value),
           y_value = y_value - chart_value /2,
           x_value = row_number())
  
  # Split data by faceting variable
  split_data_2c_safeguard <- fall_chart_data_2c_safeguard %>% 
    #filter chart for no Tas for now
    filter(state!= "Tas") %>% 
    #just doing two states for now all in appendix
    filter(state == "NSW and ACT"| state == "Vic") %>% 
    group_split(!!sym("state"))
  
  # Get the unique values for titles
  facet_labels <- fall_chart_data_2c_safeguard %>% 
    filter(state!= "Tas") %>% 
    #just doing two states for now all in appendix
    filter(state == "NSW and ACT"| state == "Vic") %>% 
    distinct(!!sym("state")) %>% 
    pull(!!sym("state"))
  
  ncol <- 1
  
  plots_2c_safeguard <- map2(split_data_2c_safeguard, facet_labels, ~{
    base_plot <- create_plot( .x, .y)
    
    # Determine if this is a leftmost plot (first in each row)
    plot_index <- which(facet_labels == .y)
    is_leftmost <- (plot_index - 1) %% ncol == 0
    is_bottom <- plot_index > length(facet_labels) - ncol 
    
    base_plot <- base_plot + 
      labs(title = .y)
    
    if (!is_leftmost) {
      base_plot <- base_plot +
        theme(axis.text.y = element_blank(),
              axis.ticks.y = element_blank(),
              axis.text.x = element_blank(),
              axis.ticks.x = element_blank())
    }
    
    # if (!is_bottom) {
    #   base_plot <- base_plot +
    #     theme(axis.text.x = element_blank(),
    #           axis.ticks.x = element_blank())
    # }
    # 
    base_plot
    
  })
  
  #####################################
  #Switch 'n' Save - 2C RET
  #####################################
  
  #this plot shows the difference in annual bills for a household in 2025 depending on their consumer type
  #Create cameo plot data for 2C RET
  
  cameo_cost_data_2c_ret <- bind_rows(cameo_electricity_costs, cameo_gas_costs %>% 
                                              cross_join(tibble(scenario = cameo_electricity_costs %>% 
                                                                  ungroup() %>% 
                                                                  select(scenario) %>% 
                                                                  unique() %>% 
                                                                  pull()))) %>% 
    cross_join(tibble(ice = c(0,1,2))) %>% 
    bind_rows(cameo_petrol_costs %>% 
                cross_join(tibble(scenario = cameo_electricity_costs %>% 
                                    ungroup() %>% 
                                    select(scenario) %>% 
                                    unique() %>% 
                                    pull()))) %>% 
    filter(scenario == "2_Opt1")
  
  
  #create chart data for 2C Safeguard analysis 
  electrification_data_2c_ret <- cameo_cost_data_2c_ret %>% 
    filter(year == 2025) %>%
    group_by(year, cooking, water_heating, space_heating, ev, pv, battery, ice, state) %>% 
    summarise(total_cost_dollars = sum(annual_cost_dollars)) %>% 
    mutate(consumer_type = paste(cooking, water_heating, space_heating, ev, pv, battery, ice, sep = "_")) %>% 
    filter(consumer_type %in% c("gas_gas_gas_0_FALSE_FALSE_1", # all gas with an ICE vehicle
                                "gas_gas_gas_1_FALSE_FALSE_0",
                                "gas_gas_gas_1_TRUE_FALSE_0",
                                "gas_gas_gas_1_TRUE_TRUE_0",
                                "electric_electric_electric_1_TRUE_TRUE_0")) %>% 
    mutate(consumer_name = fct_case_when(consumer_type == "gas_gas_gas_0_FALSE_FALSE_1" ~ "Fossil fuel home",
                                         consumer_type == "gas_gas_gas_1_FALSE_FALSE_0" ~ "Switch to an EV",
                                         consumer_type == "gas_gas_gas_1_TRUE_FALSE_0" ~ "Install solar",
                                         consumer_type == "gas_gas_gas_1_TRUE_TRUE_0" ~ "Install a battery",
                                         consumer_type == "electric_electric_electric_1_TRUE_TRUE_0" ~ "Electrify gas") %>% fct_rev()) %>% 
    filter(state %nin% c("WA", "NT", "Aus")) %>% 
    ungroup()
  
  
  # average_electrification_data <- electrification_data %>% 
  #   left_join(household_connections) %>% 
  #   filter(!is.na(connections)) %>% 
  #   group_by(consumer_name) %>% 
  #   summarise(total_cost_dollars = weighted.mean(total_cost_dollars, connections))
  
  
  fall_chart_data_2c_ret <- electrification_data_2c_ret %>% 
    select(state, consumer_name, total_cost_dollars) %>% 
    group_by(state) %>% 
    arrange(consumer_name) %>% 
    mutate(chart_value = if_else(consumer_name %in% c("Fossil fuel home"), total_cost_dollars, total_cost_dollars - lead(total_cost_dollars))) %>% 
    ungroup() %>% 
    select(state, consumer_name, chart_value) %>% 
    arrange(fct_rev(consumer_name))
  
  
  # Get the category names for axis labels
  axis_labels <- c("Fossil fuel home",  "Fully electrified home")
  chart_label_data <- fall_chart_data_2c_ret %>% 
    group_by(state) %>% 
    mutate(y_value = cumsum(chart_value),
           y_value = y_value - chart_value /2,
           x_value = row_number())
  
  # Split data by faceting variable
  split_data_2c_ret <- fall_chart_data_2c_ret %>% 
    #filter chart for no Tas for now
    filter(state!= "Tas") %>% 
    #just doing two states for now all in appendix
    filter(state == "NSW and ACT"| state == "Vic") %>% 
    group_split(!!sym("state"))
  
  # Get the unique values for titles
  facet_labels <- fall_chart_data_2c_ret%>% 
    filter(state!= "Tas") %>% 
    #just doing two states for now all in appendix
    filter(state == "NSW and ACT"| state == "Vic") %>% 
    distinct(!!sym("state")) %>% 
    pull(!!sym("state"))
  
  ncol <- 1
  
  plots_2c_ret <- map2(split_data_2c_ret, facet_labels, ~{
    base_plot <- create_plot( .x, .y)
    
    # Determine if this is a leftmost plot (first in each row)
    plot_index <- which(facet_labels == .y)
    is_leftmost <- (plot_index - 1) %% ncol == 0
    is_bottom <- plot_index > length(facet_labels) - ncol 
    
    if (!is_leftmost) {
      base_plot <- base_plot +
        theme(axis.text.y = element_blank(),
              axis.ticks.y = element_blank(),
              axis.text.x = element_blank(),
              axis.ticks.x = element_blank())
    }
    
    # if (!is_bottom) {
    #   base_plot <- base_plot +
    #     theme(axis.text.x = element_blank(),
    #           axis.ticks.x = element_blank())
    # }
    # 
    base_plot
    
  })
  
  # Combine plots
  p1 <- nsw_vic_waterfall_plot_2c_safeguard <- wrap_plots(plots_2c_safeguard, ncol = ncol) 
  p2 <- nsw_vic_waterfall_plot_2c_ret <- wrap_plots(plots_2c_ret, ncol = ncol)
    
  grattan_save_all("/Users/bjjefferson/Grattan Institute Dropbox/Ben Jefferson/Apps/Overleaf/energy-2025-carbon-pricing-for-electricity/atlas/Waterfall/2_degree_waterfall_nsw_vic_2c_safeguard.pdf",
                   object = nsw_vic_waterfall_plot_2c_safeguard)   
  grattan_save_all("/Users/bjjefferson/Grattan Institute Dropbox/Ben Jefferson/Apps/Overleaf/energy-2025-carbon-pricing-for-electricity/atlas/Waterfall/2_degree_waterfall_nsw_vic_2c_ret.pdf",
                   object = nsw_vic_waterfall_plot_2c_ret)   
  
  
  
  #################################
  #Some customers are in trouble
  #################################
  
  electrification_data_2c_safeguard <- cameo_cost_data_2c_safeguard %>% 
    filter(year <= 2045,
           year >= 2025) %>% 
    mutate(consumer_type = paste(cooking, water_heating, space_heating, ev, pv, battery, ice, sep = "_")) %>% 
    filter(consumer_type %in% c("gas_gas_gas_0_FALSE_FALSE_0", # all gas
                                "electric_electric_electric_0_FALSE_FALSE_0")) %>%  #all electric
    mutate(consumer_name = fct_case_when(consumer_type == "gas_gas_gas_0_FALSE_FALSE_0" ~ "Mostly gas household",
                                         consumer_type == "electric_electric_electric_0_FALSE_FALSE_0" ~ "All electric household") %>% fct_rev(),
           category = case_when(category == "Electricity consumption" ~ "Electricity", 
                                category == "Gas volumetric" ~ "Gas consumption",
                                .default = category),
           category = fct(category, 
                          levels = c("Electricity", 
                                     "Gas consumption", 
                                     "Gas connection",
                                     "Petrol")) %>% fct_rev()) %>% 
    select(year, consumer_name, state, category, annual_cost_dollars) %>%
    filter(state %in% c("Vic")) %>% 
    ungroup()
  
  chart_fill_palette <- c(
    "Petrol" = grattan_red,
    "Gas consumption" = grattan_orange, 
    "Gas connection" = grattan_darkorange,
    "Electricity" = grattan_yellow
  )
  
  # label_data <- electrification_data %>% 
  #   # Calculate the y positions for the labels at the midpoints of each stack
  #   group_by(year) %>% 
  #   arrange(category) %>% 
  #   mutate(y_position = cumsum(annual_cost_dollars),
  #          y_position = y_position - 0.5 * annual_cost_dollars) %>% 
  #   ungroup()
  
  label_data <- tibble(consumer_name = c("Mostly gas household", 
                                         "Mostly gas household",
                                         "All electric household"),
                       year = c(2025, 2025, 2025),
                       y = c(4500, 4000, 3000),
                       category = c("Gas connection",
                                    "Gas consumption",
                                    "Electricity"))
  
  gas_v_electric_household_plot_2c_safeguard <- electrification_data_2c_safeguard %>% 
    bind_rows(tibble(year = seq(2045, 2060),
                     consumer_name = "Mostly gas household")) %>% 
    ggplot(aes(x = year, y = annual_cost_dollars, fill = category)) +
    geom_col() +
    facet_wrap(~consumer_name) +
    grattan_y_continuous(limits = c(0, 5000),
                         labels = scales::dollar_format()) +
    grattan_label(data = label_data, 
                  aes(x = year, y = y, label = category, colour = category),
                  hjust = 0) +
    theme_grattan() +
    scale_fill_manual(values = chart_fill_palette) +
    scale_colour_manual(values = chart_fill_palette) +
    labs(title = "As connection charges grow, households that are stuck on gas will pay far more than they need to",
         subtitle = "Annual gas and electricity costs by household type, Vic ($2025)",
         x = "",
         y= "") 
  
  grattan_save_all("/Users/bjjefferson/Grattan Institute Dropbox/Ben Jefferson/Apps/Overleaf/energy-2025-carbon-pricing-for-electricity/atlas/Gas Connections/2_degree_gas_v_electric_household.pdf",
                   object = gas_v_electric_household_plot_2c_safeguard)   
  
  
  return(c("bill_diff_plot_1_2c" = bill_diff_plot_1_2c,
           "bill_diff_plot_2_2c" = bill_diff_plot_2_2c,
           "bill_diff_plot_1_15c" = bill_diff_plot_1_15c,
           "bill_diff_plot_2_15c" = bill_diff_plot_2_15c,
         "energy_wallet_over_time_plot_2c" = energy_wallet_over_time_plot_2c,
         "energy_wallet_over_time_plot_15c" = energy_wallet_over_time_plot_15c,
         "nsw_vic_waterfall_plot_2c_safeguard" = nsw_vic_waterfall_plot_2c_safeguard,
         "nsw_vic_waterfall_plot_2c_ret" = nsw_vic_waterfall_plot_2c_ret,
         "gas_v_electric_household_plot_2c_safeguard" = gas_v_electric_household_plot_2c_safeguard
         ))
}

#Calcs for report

# bill_pct_delta <- annual_chart_data %>%
#   group_by(scenario) %>%
#   mutate(pct_delta = annualised_cost[facet == 'Difference in bills'] / annualised_cost[facet == 'Total bills'])
# 
# bill_pct_delta
