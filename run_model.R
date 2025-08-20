# script to run the model

#install all necessary packages
required_packages <- required_packages <- c(
  "targets",
  "tarchetypes", 
  "dplyr",
  "tidyr",
  "readr",
  "janitor",
  "ggplot2",
  "readabs",
  "fy",
  #"fnmate",
  "lubridate",
  "readxl",
  "stringr",
  "purrr",
  "unpivotr",
  "tidyxl",
  "grattantheme",
  "forcats",
  "scales",
  "ggarchery"
)

missing_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]

if (length(missing_packages) > 0) {
  cat("Installing missing packages:", paste(missing_packages, collapse = ", "), "\n")
  install.packages(missing_packages, dependencies = TRUE)
} else {
  cat("All required packages are already installed.\n")
}

#run model

library(targets)
library(tarchetypes)

tar_make()

tar_visnetwork()
# tar_read(standing_offer_bills)

# average_household_costs
# household_connections
# 
# x <- average_household_costs %>% 
#   left_join(household_connections) %>% 
#   group_by(year, category) %>% 
#   summarise(average_cost_dollars = weighted.mean(average_cost_dollars, connections)) %>% 
#   filter(!is.na(average_cost_dollars))

