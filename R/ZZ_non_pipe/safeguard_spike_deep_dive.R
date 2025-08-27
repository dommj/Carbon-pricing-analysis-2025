chart_palette_fuels <- c(
  "Petrol" = grattan_red,
  "Gas" = grattan_orange, 
  "Electricity" = grattan_yellow
)


chart_palette_scenarios <- c("No new policy" = grattan_red,
                             "RET < 2 C" = grattan_yellow,
                             "Safeguard < 2 C" = grattan_orange)


plot_list <- list()

#Deep dive on 2035 Safeguard wholesale price spike
#Generation mix

data_2035 <- scenario_generation %>%
  filter(grid == "NEM") %>%
  group_by(source, year, scenario) %>%
  summarise(gen = sum(generation_sent_out_gwh)) %>%
  filter(year == 2030 | year == 2035) %>%
  arrange(desc(gen)) 

gen_2035_plot <- ggplot(data_2035) + 
  geom_col(aes(x = source, y = gen, colour = scenario, fill = scenario),
           position = 'dodge') +
  facet_wrap(~year, ncol = 2) + 
  xlab('') +
  scale_y_continuous(labels = function(x) paste0(x/1000, "k gwh"),
                     expand = expansion(mult = c(0, 0))) +
  labs(title = 'Coal displaces wind under no new policy scenario in 2035',
       subtitle = 'Energy generation mix in 2030 and 2035 under each modelled scenario') + 
  theme_grattan(legend = 'bottom') 

plot_list['generation_mix_2035'] <- list(gen_2035_plot)

#LGC cost
#Reading in LGC costs
ref_lgc_2 <- read_excel(results_ref,
                        sheet = "Bundled Price (Real)",
                        range = "A2:AN4") %>% 
  mutate(scenario = "No new policy")

safeguard_lgc_2 <- read_excel(results_2_Opt2,
                              sheet = "Bundled Price (Real)",
                              range = "A2:AN4") %>% 
  mutate(scenario = "Safeguard < 2 C")


ret_lgc_2 <- read_excel(results_2_Opt1,
                        sheet = "Bundled Price (Real)",
                        range = "A2:AN4") %>% 
  mutate(scenario = "RET < 2 C")

lgc_prices <- bind_rows(ref_lgc_2,
                        safeguard_lgc_2,
                        ret_lgc_2) %>% 
  rename(cat = 1) %>% 
  filter(cat == "LGC Price") %>% 
  pivot_longer(cols = contains('20'), names_to = "year", values_to = "lgc_dollars_mwh") %>% 
  mutate(year = as.numeric(year)) %>% 
  select(-cat)

#Visualising
lgc_plot <- ggplot(lgc_prices) +
  geom_line(aes(x = year, y = lgc_dollars_mwh, colour = scenario),
            size = 1) +
  xlab('') + 
  scale_y_continuous(labels = function(x) paste0("$", x)) +
  labs(title = "LGC prices go to zero in 2031 under both scenarios bar the RET",
       subtitle = "Renewable energy target Large-scale Generation Certificate price") +
  scale_colour_manual(values = chart_palette_scenarios) +
  theme_grattan(legend = 'bottom')

plot_list['lgc_plot'] <- list(lgc_plot)

grattan_save_pptx(p = plot_list, filename = "/Users/bjjefferson/Grattan Institute Dropbox/Ben Jefferson/Apps/Overleaf/energy-2025-carbon-pricing-for-electricity/atlas/Backups/Safeguard_Spike_Deep_Dive.pptx")


