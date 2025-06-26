library(tidyverse)
library(lubridate)

# Generate oscillating data that sums to 1
generate_oscillating_data <- function() {
  
  # Create date sequence from Jan 2023 to June 2025
  dates <- seq(from = as.Date("2023-01-01"), 
               to = as.Date("2025-06-30"), 
               by = "day")
  
  # Key transition points
  start_date <- as.Date("2023-01-01")
  transition_date <- as.Date("2023-09-01")  # When values become equal
  end_date <- as.Date("2025-06-30")
  
  # Calculate days from start for each phase
  transition_days <- as.numeric(transition_date - start_date)
  total_days <- as.numeric(end_date - start_date)
  
  data <- tibble(date = dates) %>%
    mutate(
      days_from_start = as.numeric(date - start_date),
      
      # Create the two phases
      Nick = case_when(
        # Phase 1: Decay from 1 to 0.5 (Jan to Sep 2023)
        date <= transition_date ~ 0.5 + 0.5 * cos(pi * (days_from_start / transition_days)/2 ),
        
        # Phase 2: Oscillate around 0.5 with amplitude 0.1 (Sep 2023 to June 2025)
        TRUE ~ 0.5 + 0.05 * sin(2 * pi * (days_from_start - transition_days) / 365.25 -pi)
      ),
      
      Dom = case_when(
        # Phase 1: Rise from 0 to 0.5 (Jan to Sep 2023)
        date <= transition_date ~ 0.5 - 0.5 * cos(pi * (days_from_start / transition_days)/2),
        
        # Phase 2: Oscillate around 0.5, 180 degrees out of phase (Sep 2023 to June 2025)
        TRUE ~ 0.5 - 0.05 * sin(2 * pi * (days_from_start - transition_days) / 365.25 - pi)
      ),
      
      
      # Add month-year for grouping
      month_year = floor_date(date, "month")
    )
  
  return(data)
}

# Generate the data
oscillating_data <- generate_oscillating_data()



# Create visualization
p1 <- oscillating_data %>%
  select(date, Nick, Dom) %>%
  pivot_longer(cols = c(Nick, Dom), 
               names_to = "series", 
               values_to = "value") %>%
  ggplot(aes(x = date, y = value, colour = series)) +
  geom_line(alpha = 0.8, size = 1.5) +
  grattan_y_continuous(labels = scales::percent_format()) +
  grattan_label(data = . %>% filter(date == as.Date('2023-01-04')),
                aes(label = series)) +
  theme_grattan() +
  labs(
    title = "In his time at Grattan, Nick taught Dom the essential skills to succeed in the associate role",
    subtitle = "Proportion of games won in Nick v Dom TT matches, weekly rolling average",
    x = "",
    y = "",
  ) 

print(p1)

grattan_save_pptx("nick_tt.pptx")

# Show monthly averages for key periods
monthly_summary <- oscillating_data %>%
  group_by(month_year) %>%
  summarise(
    avg_value_a = mean(value_a),
    avg_value_b = mean(value_b),
    avg_total = mean(total),
    .groups = "drop"
  ) %>%
  filter(month_year %in% c(as.Date("2023-01-01"), 
                           as.Date("2023-09-01"), 
                           as.Date("2024-03-01"),
                           as.Date("2025-06-01")))

print("Monthly averages for key periods:")
print(monthly_summary)
