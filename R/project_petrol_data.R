#project real petrol prices data linear fit from 2004 to 2024

project_petrol_data <- function(petrol_price_data, end_year) {
  
  # #calculate linear fit for petrol price data
  # model <- lm(c_litre ~ year, data = petrol_price_data)
  # 
  # # Create extended year sequence for projection
  # projection_years <- tibble(
  #   year = (max(petrol_price_data$year) + 1):end_year
  # )
  # 
  # #predict
  # projected_petrol_price <- projection_years %>%
  #   mutate(c_litre = predict(model, newdata = .),
  #          category = "Petrol")
  
  #keep petrol prices constant (update with latest data)
  
  projected_petrol_price <- petrol_price_data %>% 
    filter(year == 2024) %>% 
    select(- year) %>% 
    cross_join(tibble(year = seq(2025,2050)))
  
  

  return(bind_rows(petrol_price_data, projected_petrol_price))
}

# project_petrol_data(petrol_data, 2050) %>% 
#   ggplot(aes(x = year, y = c_litre)) +
#   geom_line()
  