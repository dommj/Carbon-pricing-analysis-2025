# load retail electricity prices by jurisdiction

# In this script, we are simply taking values scraped from the AEMC 2024 price trends report.
#prices are in real FY25 dollars

#source('R/setup.R')

get_retail_data <- function(retail_file) {
  read_csv(retail_file, col_types = cols()) %>% 
    clean_names() %>% 
    mutate(across(c_kwh, 
                  ~convert_to_2024_dollars(., "2024-25", financial = TRUE)))
}



#nsw_25 <- read.csv('data/AEMC price trends/nsw_25.csv')
# library(readabs)
# library(fy)
# 
# cpi <- read_cpi() %>% 
#   filter(year(date)>2000) %>% 
#   mutate(fy = date2fy(date)) %>% 
#   group_by(fy) %>% 
#   summarise(cpi=mean(cpi)) %>% 
#   ungroup() %>% 
#   mutate(cpi_25=cpi[25]) %>% 
#   mutate(cpi_use=cpi/cpi_25) %>% 
#   select(fy, cpi_use)
