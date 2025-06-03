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


#Assume water is between gas instant and electric storage there arent that many heat pumps even in 2040, electric storage stays dominant
aggregates_hw <- rbs_stock_by_state %>% 
  filter(year == 2040,
         end_use %in% c("Water heating")) %>% 
  group_by(appliance) %>% 
  summarise(stock = sum(stock)) %>% 
  ungroup() %>% 
  mutate(pct = stock / sum(stock),
         appliance_cat = case_when(str_detect(appliance,"Electric")~ "Electric storage",
                                   str_detect(appliance,"Gas instant")~ "Gas instant",
                                   str_detect(appliance,"Gas storage")~ "Gas storage",
                                   str_detect(appliance,"Heat pump")~ "Heat pump",
                                   .default = "other")
         ) %>% 
  group_by(appliance_cat) %>% 
  summarise(pct = sum(pct))



#assume space conditioning is converted between RCAC and ducted gas. Need to write up justification but its chill

aggregates_sc <- rbs_stock_by_state %>%
  filter(year == 2040,
         end_use %in% c("Space conditioning")) %>%
  group_by(appliance) %>%
  summarise(stock = sum(stock)) %>%
  ungroup() %>%
  mutate(pct = stock / sum(stock),
         appliance_cat = case_when(str_detect(appliance,"AC")~ "AC",
                                   str_detect(appliance,"Gas instant")~ "Gas instant",
                                   str_detect(appliance,"Gas storage")~ "Gas storage",
                                   str_detect(appliance,"Heat pump")~ "Heat pump",
                                   .default = "other")
  ) %>%
  group_by(appliance_cat) %>%
  summarise(pct = sum(pct))


#look at hot water

aggregates_cookin <- rbs_stock_by_state %>%
  filter(year == 2040,
         end_use %in% c("Cooking")) %>%
  group_by(appliance) %>%
  summarise(stock = sum(stock)) %>%
  ungroup() %>%
  mutate(pct = stock / sum(stock),
         appliance_cat = case_when(str_detect(appliance,"AC")~ "AC",
                                   str_detect(appliance,"Gas instant")~ "Gas instant",
                                   str_detect(appliance,"Gas storage")~ "Gas storage",
                                   str_detect(appliance,"Heat pump")~ "Heat pump",
                                   .default = "other")
  ) %>%
  group_by(appliance_cat) %>%
  summarise(pct = sum(pct))






