#Coal exit plot

coal_exit <- read_excel('Data/Jacobs/Update/Results_RefV3.xlsx',
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
  arrange(-mw) %>%
  ungroup()

# label_data <- data.frame(
#   label = c("Eraring exits\nin 2028", "Yallourn exits\nin 2028"),
#   grid = c("NSW", "VIC"),
#   x = c(2035, 2030),
#   y = c(3350, 4250),
#   cols = c(grattan_red, grattan_darkred)
# )

#Creating labels for coal plant exits

totals <- coal_exit %>%
  group_by(nem, year) %>%
  summarise(total = sum(mw), .groups = 'drop')

exit_events <- coal_exit %>%
  arrange(plant, nem, year) %>%
  group_by(plant, nem) %>%
  mutate(prev = dplyr::lag(mw),
         exit = (mw == 0 & dplyr::coalesce(prev, 0) > 0)) %>%
  filter(exit) %>%
  summarise(exit_year = first(year), .groups = 'drop')

pad_tbl <- totals %>%
  group_by(nem) %>%
  summarise(pad = 0.04 * max(total), .groups = 'drop')

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


coal_exit_plot_by_grid <- ggplot(totals) +
  geom_col(aes(x = year, y = total),
           colour = grattan_orange, fill = grattan_orange) +
  facet_wrap(~nem, ncol = 2, scales = "free_y") +
  geom_text(data = label_frame,
            aes(x = year, y = y, label = label),
            hjust = 0, vjust = 0, size = 3, lineheight = 0.7,
            colour = grattan_orange) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)),
                     labels = function(x) paste0(scales::comma(x/1000), "k MW")) +
  xlab("") +
  coord_cartesian(clip = "off") +
  theme_grattan() +
  theme(plot.margin = margin(5.5, 30, 5.5, 5.5))   

coal_exit_plot_by_grid  
check_chart_aspect_ratio()


#Facet by state plot 

coal_exit_plot_by_state <- ggplot(coal_exit) + 
  geom_col(aes(x = year, y = mw, fill = eraring_yallourn, colour = eraring_yallourn)) + 
  facet_wrap(~grid, 
             ncol = 2) +
  scale_colour_manual(values = make_grattan_pal(palette = 'graph')(29)) +
  scale_fill_manual(values = make_grattan_pal(palette = 'graph')(29)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0)),
                     labels = function(x) paste0(comma(x/1000), "k MW")) +
  xlab('') +
  geom_text(data = label_data,
            aes(x = x, y = y, label = label),
            colour = label_data$cols,  
            inherit.aes = T,
            size = 6, hjust = 0, lineheight = 0.8) +
  theme_grattan() 

#NEM and non-NEM coal 

coal_exit <- coal_exit %>%
  mutate(nem = fct_collapse(grid,
                            'NEM' = c('NSW', 'VIC', 'QLD'),
                            'SWIS' = c('WA')))


