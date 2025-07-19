#calculate typical temperature data

temp_data_new <- temp_data %>%
  mutate(date = as.Date(date),
         year = year(date),
         month = month(date),
         tmean = (min + max) / 2)

# Function to compute the FS statistic
fs_stat <- function(x, ref) {
  x_cdf <- ecdf(x)
  ref_cdf <- ecdf(ref)
  grid <- sort(unique(c(x, ref)))
  max(abs(x_cdf(grid) - ref_cdf(grid)))
}

# Initialize a list to store selected months
selected_months <- list()

# Loop through each month
for (m in 1:12) {
  # Filter data for the current month
  month_data <- df %>% filter(month == m)
  
  # Compute the reference CDF for the month
  ref_cdf_data <- month_data$tmean
  
  # Compute FS statistic for each year
  fs_values <- month_data %>%
    group_by(year) %>%
    summarise(fs = fs_stat(tmean, ref_cdf_data), .groups = 'drop')
  
  # Identify the year with the minimum FS statistic
  best_year <- fs_values %>% filter(fs == min(fs)) %>% pull(year)
  
  # Extract data for the selected month and year
  selected_month <- month_data %>% filter(year == best_year)
  
  # Store the selected month
  selected_months[[m]] <- selected_month
}

# Combine the selected months to form the TMY
tmy <- bind_rows(selected_months) %>%
  arrange(date)