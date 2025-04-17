#appliance stock, by end use

rbs_output_cells <- xlsx_cells(rbs_outputs_data_file)

formats <- xlsx_formats(rbs_outputs_data_file)

indent <- formats$local$alignment$indent


rbs_stock_by_state <- rbs_output_cells %>% 
  filter(row > 3,
         sheet == "Stock.EndUse.Cat.Grp-State") %>% 
  behead("up-left", "state") %>% 
  behead("up", "year") %>% 
  behead_if(indent[local_format_id] == 0,
            direction = "left-up",
            name = "end_use") %>% 
  behead_if(indent[local_format_id] == 1,
            direction = "left-up",
            name = "category") %>% 
  behead("left", "appliance") %>% 
  select(year, state, end_use, category, appliance, content) %>% 
  rename(stock = content) %>%
  filter(!is.na(appliance)) %>% 
  mutate(stock = as.numeric(stock))

aggregates_hw <- rbs_stock_by_state %>% 
  filter(year == 2020,
         end_use %in% c("Water heating")) %>% 
  group_by(appliance) %>% 
  summarise(stock = sum(stock)) %>% 
  ungroup() %>% 
  mutate(pct = stock / sum(stock))
