
create_esoo_demand_chart <- function(esoo_2024_operational_file){
  
  operational_sent_out <- read_excel(esoo_2024_operational_file) %>% 
    clean_names() %>% 
    filter(scenario %in% c('Actual', 'Central'),
           parent_category == 'Operational (Sent Out)',
           #only include explicitly residential categories. residential EV will be added on top.
           category != 'Operational (Sent Out)',
           category != 'Energy Efficiency',
           region == 'NEM') %>% 
    mutate(type = if_else(category == "Electric Vehicles" | category == "Electrification", category, "All other consumption"),
           type = str_to_sentence(type)) %>% 
    group_by(year, type) %>% 
    summarise(annual_consumption_t_wh = sum(annual_consumption_t_wh)) 
  
levels <- operational_sent_out %>% 
  filter(year == 2050) %>% 
  arrange(annual_consumption_t_wh) %>% 
  pull(type)

operational_sent_out <- operational_sent_out %>% 
  mutate(type = factor(type, levels = levels))
  
plot <- operational_sent_out %>% 
    group_by(year) %>% 
    filter(year >= 2025) %>%
    arrange(type, .by_group = T) %>% 
    ggplot(aes(x = year, y = annual_consumption_t_wh, fill = type)) +
    geom_col()+
    grattan_y_continuous() +
    scale_x_discrete(limits = c(2025, 2030, 2035, 2040, 2045, 2050), expand = c(0,0,0,7)) +
    grattan_label(data = . %>%
                    filter(year == 2054) %>%
                    mutate(annual_consumption_t_wh_cum = rev(cumsum(rev(annual_consumption_t_wh))),
                           annual_consumption_t_wh_cum = annual_consumption_t_wh_cum - annual_consumption_t_wh / 2),
                  aes(x = year, y = annual_consumption_t_wh_cum,
                      label = str_wrap(type, width = 15),
                      colour = type,
                      fill = NA),
                  hjust = 0,
                  label.size = NA,
                  nudge_x = 1) +
    theme_grattan() +
    scale_fill_manual(values = c("All other consumption" = grattan_orange, "Electric vehicles" = grattan_red, "Electrification" = grattan_yellow)) +
  scale_colour_manual(values = c("All other consumption" = grattan_orange, "Electric vehicles" = grattan_red, "Electrification" = grattan_yellow)) +
    labs(title = 'Electric vehicles and electrification will drive much of the growth in electricity consumption',
         subtitle = 'Electricity consumption (TWh) in the NEM',
         x = '',
         y = '')
  
plot

grattan_save("C:/Users/domijones/Grattan Institute Dropbox/Dominic  Jones/Apps/Overleaf/energy-2025-carbon-pricing-for-electricity/atlas/evs_electrification_demand_esoo24.pdf",
            object = plot,
            no_new_folder = TRUE)

grattan_save_all("Output/Atlas/evs_electrification_demand_esoo24.pdf",
             object = plot)

return(plot)

}



