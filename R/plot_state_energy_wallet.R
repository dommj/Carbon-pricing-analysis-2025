#Reconstruct plot of energy wallet 

chart <- average_net_costs %>% 
  mutate(category = factor(category, levels = c('Gas', 'Electricity', 'Petrol'))) %>%
  filter(electrification == T, scenario == 'Ref',
         year >= 2025,
         state != 'WA') %>%
  ggplot() + 
  geom_col(aes(x = year, y = average_cost_dollars,
                fill = category, colour = category)) +
  facet_wrap(~state, ncol = 3) +
  xlab('') +
  theme_grattan() +
  scale_y_continuous(labels = function(x) paste0("$", comma(x)),
                     expand = expansion(mult = c(0,0.05)))

chart  
check_chart_aspect_ratio()  
  