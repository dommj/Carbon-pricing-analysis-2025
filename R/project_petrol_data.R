#project petrol data using linear fit from 2004 to 2024

project_petrol_data <- function(petrol_data, end_year) {
  
  # Calculate the linear interpolation parameters
  year_1 <- min(petrol_data$year)
  year_n <- max(petrol_data$year)
  c_litre_1 <- petrol_data$c_litre[petrol_data$year == year_1]
  c_litre_n <- petrol_data$c_litre[petrol_data$year == year_n]
  
  # Calculate slope
  slope <- (c_litre_n - c_litre_1) / (year_n - year_1)
  
  # Create a sequence of all years
  all_years <- tibble(
    year = seq(year_1, end_year)
  )
  
  # Calculate interpolated values using the linear formula: y = y1 + slope * (x - x1)
  interpolated_data <- all_years %>%
    mutate(c_litre = c_litre_1 + slope * (year - year_1),
           category = "petrol")
  
  interpolated_data
}

# project_petrol_data(petrol_data, 2050) %>% 
#   ggplot(aes(x = year, y = c_litre)) +
#   geom_line()
  