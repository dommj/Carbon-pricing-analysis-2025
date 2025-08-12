#plot changes in time of use profiles 

plot_average_profiles <- function(all_average_profiles){

  
  chart_palette <- c("Electrification" = grattan_red,
                     "Electric vehicles" = grattan_yellow,
                     "Baseline" = grattan_orange)
  
  
  plot <- all_average_profiles %>% 
    mutate(source = fct_case_when(source == "baseline" ~ "Baseline",
                                  source == "electrification" ~ "Electrification",
                                  source == "Electric vehicles" ~ "Electric vehicles"),
           facet = paste0(state, " ", season, " - ", year )) %>% 
    filter(facet %in% c("Vic Winter - 2025", "Qld Summer - 2025",
                        "Vic Winter - 2050", "Qld Summer - 2050"),
           pv == 0,
           battery == F,
           electrification == T,
           year %in% c(2025, 2050)) %>% 
    ggplot(aes(x = hour, y = power_kwh, fill = fct_rev(source), colour = fct_rev(source))) +
    geom_area() +
    grattan_label(data = . %>%  filter(str_detect(facet, "2050"),
                                   hour == 0) %>% 
                mutate(x = 0,
                       y = case_when(source == "Baseline" ~ 1.3,
                                     source == "Electrification" ~ 1.55,
                                     source == "Electric vehicles" ~ 1.8)), 
              aes(x = x, y = y, label = source, colour = source),
              hjust = 0) +
    facet_wrap(~facet) +
    grattan_y_continuous(expand_top = 0.1) +
    scale_x_continuous_grattan(breaks = seq(0, 23, 8), 
                               labels = c("12am",  "8am",  "4pm")) +
    scale_fill_manual(values = chart_palette) +
    scale_colour_manual(values = chart_palette) +
    theme_grattan() +
    labs(title = "Underlying electricity demand will change substantially over time",
         subtitle = "Power (kilowatts)",
         x = "",
         y = "")
  
  check_chart_aspect_ratio()
  
  grattan_save_all("/Users/bjjefferson/Grattan Institute Dropbox/Ben Jefferson/Apps/Overleaf/energy-2025-carbon-pricing-for-electricity/atlas/average_demand_shape_over_time.pdf",
                   object = plot)
  
  
}


# 
# all_average_profiles %>% 
#   mutate(source = fct_case_when(source == "baseline" ~ "Baseline",
#                                 source == "electrification" ~ "Electrification",
#                                 source == "Electric vehicles" ~ "Electric vehicles")) %>% 
#   filter(state == "Vic",
#          season == "Winter",
#          pv == 0,
#          battery == F,
#          electrification == T,
#          year %in% c(2025, 2040)) %>% 
#   ggplot(aes(x = hour, y = power_kwh, fill = fct_rev(source), colour = fct_rev(source))) +
#   geom_text(data = tibble(source = c("Baseline", "Electrification", "Electric vehicles"),
#                           x = c(4, 4, 4), 
#                           y = c(1 +0.3, 1 + 0.4, 1+ 0.5),
#                           year = 2040), 
#             aes(x = x, y = y, label = source, colour = source),
#             hjust = 0) +
#   geom_area() +
#   facet_wrap(~year) +
#   grattan_y_continuous() +
#   scale_x_continuous_grattan(breaks = seq(0, 23, 8), 
#                              labels = c("12am",  "8am",  "4pm")) +
#   scale_fill_manual(values = chart_palette) +
#   scale_colour_manual(values = chart_palette) +
#   theme_grattan() +
#   labs(title = "Underlying electricity demand will change substantially over time",
#        subtitle = "Power (kilowatts)",
#        x = "",
#        y = "")
# 
# 
# all_average_profiles %>% 
#   mutate(source = fct_case_when(source == "baseline" ~ "Baseline",
#                                 source == "electrification" ~ "Electrification",
#                                 source == "Electric vehicles" ~ "Electric vehicles")) %>% 
#   filter(state == "Qld",
#          season == "Summer",
#          pv == 0,
#          battery == F,
#          electrification == T,
#          year %in% c(2025, 2040)) %>% 
#   ggplot(aes(x = hour, y = power_kwh, fill = fct_rev(source), colour = fct_rev(source))) +
#   geom_area() +
#   facet_wrap(~year) +
#   geom_text(data = tibble(source = c("Baseline", "Electrification", "Electric vehicles"),
#                           x = c(4, 4, 4), 
#                           y = c(1 +0.3, 1 + 0.4, 1+ 0.5),
#                           year = 2040), 
#             aes(x = x, y = y, label = source, colour = source),
#             hjust = 0) +
#   grattan_y_continuous() +
#   scale_x_continuous_grattan(breaks = seq(0, 23, 8), 
#                              labels = c("12am",  "8am",  "4pm")) +
#   scale_fill_manual(values = chart_palette) +
#   scale_colour_manual(values = chart_palette) +
#   theme_grattan() +
#   labs(title = "Underlying electricity demand will change substantially over time",
#        subtitle = "Power (kilowatts)",
#        x = "",
#        y = "")





# 
# heating_cooling_profiles %>% 
#   filter(state == "Vic",
#          season == "Winter",
#          day_type == "WD",
#          end_use_category == "Space conditioning - heating",
#          year %in% c(2020)) %>% 
#   ggplot(aes(x = hour, y = power)) +
#   geom_area() 