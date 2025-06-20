# plot retail

plot_retail_data <- function(retail_data) {
  ggplot(retail_data, aes(x = year, y = c_kwh)) +
    geom_line() +
    labs(title = 'title', x = "Date", y = "Retail") +
    theme_minimal()
}
