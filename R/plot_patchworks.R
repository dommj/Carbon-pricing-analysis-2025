##Generating patchworks

plot_patchworks <- function(jacobs_results_charts) {

  #Initiate empty plot list
  plot_list = list()
  
  #Reconstruct emissions and renewables plots
  emissions_plot <- reconstruct_plot("emissions_plot_2c", jacobs_results_charts) + 
    theme(plot.title = element_blank(),
          plot.subtitle = element_blank(),
          plot.caption = element_blank())
  renewables_plot <- reconstruct_plot("renewable_pct_plot_2c", jacobs_results_charts) + 
    theme(plot.title = element_blank(),
          plot.subtitle = element_blank(),
          plot.caption = element_blank())
  
  emission_renewables <- (emissions_plot + renewables_plot) + 
    plot_annotation(
      title = "Emissions fall quickly under policy scenarios and renewable use reaches peak penetration by 2035",
      subtitle = "Electricity sector emissions (Mt CO2-e) (LHS) and renewable power percentage of generation (RHS)"
    )
  
  plot_list['emission_renewables'] <- list(emission_renewables)
  
  #Reconstruct pricing charts 
  annual_price_chart <- reconstruct_plot("annual_price_chart_2c", jacobs_results_charts) + 
    theme(plot.title = element_blank(), 
          plot.subtitle = element_blank(),
          plot.caption = element_blank())
  average_nem_retail_chart <- reconstruct_plot("average_nem_retail_chart_2c", jacobs_results_charts) +
    theme(plot.title = element_blank(),
          plot.subtitle = element_blank(),
          plot.caption = element_blank())
  
  prices_charts <- (annual_price_chart + average_nem_retail_chart) + 
    plot_annotation(
      title = "The Renewable Energy Target keeps wholesale prices low, Safeguard comparable with no new policy",
      subtitle = "$/mwh, average time-weighted wholesale price (LHS), average NEM residential retail price (RHS)",
      caption = "Note: average prices are calculated by weighting prices in each grid by total sent out generation"
    )
  
  
  plot_list['prices_charts'] <- list(prices_charts)
  
  grattan_save_pptx(p = plot_list, filename = "/Users/bjjefferson/Grattan Institute Dropbox/Ben Jefferson/Apps/Overleaf/energy-2025-carbon-pricing-for-electricity/atlas/Patchworks/summary_slides_patchworks.pptx")

  return(plot_list)  
  
}

