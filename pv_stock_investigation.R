rbs_outputs_data_file,
rbs_households


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


pv_stock <- rbs_stock_by_state %>% 
  filter(category == "PV")


pv_stock %>% 
  ungroup() %>% 
  mutate(year = as.numeric(year),
         state = convert_states(state)) %>% 
  filter(year == 2020) %>% 
  group_by(year) %>% 
  summarise(stock = sum(stock)) %>% 
  left_join(rbs_households %>% 
              filter(state != "Aus") %>% 
              group_by(year) %>% 
              summarise(occupied_households = sum(occupied_households))) %>% 
  mutate(prop = stock / occupied_households)



