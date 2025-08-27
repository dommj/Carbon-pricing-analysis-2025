#Reconstruct_plot function

reconstruct_plot <- function(plot_name, chart_list) {
  components <- paste0(plot_name, c(".data", ".layers", ".scales", ".guides", 
                                    ".mapping", ".theme", ".coordinates", ".facet", 
                                    ".plot_env", ".layout", ".labels"))
  
  plot_list <- setNames(chart_list[components], 
                        c("data", "layers", "scales", "guides", "mapping", 
                          "theme", "coordinates", "facet", "plot_env", "layout", "labels"))
  
  structure(plot_list, class = c("gg", "ggplot"))
}

# Use it to display any plot
# emissions_plot <- reconstruct_plot("emissions_plot", jacobs_results_charts)
# renewable_plot <- reconstruct_plot("renewable_pct_plot", jacobs_results_charts)
# 
# # Display
# emissions_plot
