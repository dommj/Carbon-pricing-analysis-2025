#read in guys  model for gas heating loads

raw_gas_data_vic <- read_excel("C:/Users/domijones/Grattan Institute Dropbox/Dominic  Jones/Energy/1. Reports/2102 Gas as a transition fuel/2b. Analysis/Electrifying gas load - effect on peak demand - 2019 model.xlsx", 
                           sheet = "Vic regression",
                           skip = 5) %>% 
  clean_names() %>% 
  select(day, day_no, hour_3, morning, afternoon, mw_load_19)


raw_gas_data %>% 
  group_by(day_no) %>% 
  summarise(mw_load = sum(mw_load_19)) %>% 
  ggplot(aes(x = day_no, y = mw_load)) +
  geom_line()


raw_gas_data %>% 
  mutate(month = month(day),
         season = fct_case_when(month %in% c(12,1,2) ~ "Summer",
                            month %in% c(3,4,5) ~ "Autumn",
                            month %in% c(6,7,8) ~ "Winter",
                            month %in% c(9,10,11) ~ "Spring")) %>% 
  group_by(season) %>% 
  summarise(mw_load = sum(mw_load_19)) %>% 
  ungroup() %>% 
  #normalise   
  ggplot(aes(x = season, y = mw_load)) +
  geom_col()
  

rbs_tou_consumption_data