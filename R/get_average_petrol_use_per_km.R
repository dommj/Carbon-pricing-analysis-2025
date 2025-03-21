#calculate average petrol use per vehicle for households 

get_average_petrol_use_per_km <- function(mv_survey_data_file){
  
  
  #get just petrol value for now
  ice <- read_excel(mv_survey_data_file,
                                 sheet = "Table_6",
                                 range = "A79:D86") %>% 
    rename(vehicle = 1, petrol = 2, diesel = 4) %>% 
    filter(vehicle == 'Passenger vehicles') %>% 
    pivot_longer(cols = c(petrol, diesel), names_to = 'fuel_type', values_to = 'fuel_l_km') %>% 
    select(fuel_type, fuel_l_km) %>% 
    filter(fuel_type == 'petrol') %>% 
    mutate(fuel_type = 'ICE',
           fuel_l_km = fuel_l_km /100)
  
  phev <- 0.03 #need to put in real PHEV value here using https://www.greenvehicleguide.gov.au/Vehicle/Search
    
  bev <- 0
  
  average_petrol_use_per_vehicle <- tibble(fuel_type = c('PHEV', 'BEV'), fuel_l_km = c( phev, bev)) %>% 
    bind_rows(ice)
  
  average_petrol_use_per_vehicle
}

get_average_petrol_use_per_km('Data/92080DO001_202006.xls')






################################


function(){
  
get_average_petrol_use_per_vehicle('Data/92080DO001_202006.xls')


read_excel('Data/92080DO001_202006.xls',
           sheet = "Table_4",
           range = "A5:J12") %>% 
  clean_names() %>% 
  rename(year = 1) %>% 
  filter(year == '2020') %>% 
  mutate(average_kilometres_travelled =  as.numeric(average_kilometres_travelled) * 1000,
         #divide rate of consumption by 100 to get rate per km
         average_petrol_consumption_per_vehicle = average_kilometres_travelled * as.numeric(rate_of_fuel_consumption) / 100) %>% 
  pull(average_petrol_consumption_per_vehicle)

}