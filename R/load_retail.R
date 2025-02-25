# load retail electricity prices by jurisdiction

# In this script, we are simply taking values scraped from the AEMC 2024 price trends report.

#source('R/setup.R')

get_retail_data <- function(retail_file) {
  read_csv(retail_file, col_types = cols()) %>% 
    clean_names()
}


#nsw_25 <- read.csv('data/AEMC price trends/nsw_25.csv')


