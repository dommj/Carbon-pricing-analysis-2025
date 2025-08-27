#New file

chart_palette_scenarios <- c("No new policy" = grattan_red,
                             "RET < 2 C" = grattan_yellow,
                             "Safeguard < 2 C" = grattan_orange)

chart_data <- transmission_build %>% 
  filter(scenario == "No new policy" | scenario == "RET < 2 C" 
         | scenario == "Safeguard < 2 C") %>% 
  group_by(year, scenario) %>%
  summarise(capacity = sum(mw))

transmission_total <- ggplot(chart_data) + 
  geom_line(aes(x = year, y = capacity, colour = scenario)) + 
  grattan_y_continuous(labels = function(x) paste0(comma(x/1000), "k mw")) +
  scale_colour_manual(values = chart_palette_scenarios) + 
  xlab('') +
  labs(title = "Safeguard scenario tracks just below RET scenario in transmission profile",
       subtitle = "Total NEM transmission capacity, MW") +
  theme_grattan()

check_chart_aspect_ratio(type = 'fullslide')

grattan_save_pptx(p = transmission_total, filename = '/Users/bjjefferson/Grattan Institute Dropbox/Ben Jefferson/Apps/Overleaf/energy-2025-carbon-pricing-for-electricity/atlas/Backups/transmission_total.pptx')

