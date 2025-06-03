# this script creates an household fuel use profile depending on the variables cooking, water_heating, and space_heating using the converted RBS data

create_rbs_fuel_consumption_profiles <- function(integrated_fuel_use,
                                                 rbs_households){

  #write function to generate a profile for a particular set of our parameters
  generate_fuel_consumption_profile <- function(integrated_fuel_use,
                                                   rbs_households,
                               cooking = "electric",
                               water_heating = "electric",
                               space_heating = "electric"){
   #cooking
    
    if (cooking == "electric"){
      
      cooking_fuel_use <- integrated_fuel_use %>% 
        filter(end_use == "Cooking",
               fuel == "Electricity",
               conversion %in% c("unconverted",
                                 "gas_to_electric_converted")) %>% 
        group_by(year, state, fuel, end_use) %>% 
        summarise(pj = sum(pj))
      
    }
    
    if (cooking == "gas"){
      
      cooking_fuel_use <- integrated_fuel_use %>% 
        filter(end_use == "Cooking",
               fuel == "Natural Gas",
               conversion %in% c("unconverted",
                                 "electric_to_gas_converted")) %>% 
        group_by(year, state, fuel, end_use) %>% 
        summarise(pj = sum(pj))
    }
   
  #water heating 
    if (water_heating == "electric"){
      
      water_heating_fuel_use <- integrated_fuel_use %>% 
        filter(end_use == "Water heating",
               fuel == "Electricity",
               conversion %in% c("unconverted",
                                 "gas_to_electric_converted")) %>% 
        group_by(year, state, fuel, end_use) %>% 
        summarise(pj = sum(pj))
      
    }
    
    if (water_heating == "gas"){
      
      water_heating_fuel_use <- integrated_fuel_use %>% 
        filter(end_use == "Water heating",
               fuel == "Natural Gas",
               conversion %in% c("unconverted",
                                 "electric_to_gas_converted")) %>% 
        group_by(year, state, fuel, end_use) %>% 
        summarise(pj = sum(pj))
      
    }
   
    
  #space heating
    
    if (space_heating == "electric"){
      
      space_heating_fuel_use <- integrated_fuel_use %>% 
        filter(end_use == "Space conditioning - heating",
               fuel == "Electricity",
               conversion %in% c("unconverted",
                                 "gas_to_electric_converted")) %>% 
        group_by(year, state, fuel, end_use) %>% 
        summarise(pj = sum(pj))
      
    } 
    
    if (space_heating == "gas"){
      
      space_heating_fuel_use <- integrated_fuel_use %>% 
        filter(end_use == "Space conditioning - heating",
               fuel == "Natural Gas",
               conversion %in% c("unconverted",
                                 "electric_to_gas_converted")) %>% 
        group_by(year, state, fuel, end_use) %>% 
        summarise(pj = sum(pj))
      
    }
    
    #space_conditioning - cooling is constant and not converted
  
    fuel_use <-  integrated_fuel_use %>% 
      filter(end_use %nin% c("Cooking",
                             "Water heating",
                             "Space conditioning - heating")) %>% 
      select(-conversion) %>% 
      bind_rows(cooking_fuel_use,
                water_heating_fuel_use,
                space_heating_fuel_use)
    
    household_fuel_use <- fuel_use %>% 
      left_join(rbs_households) %>% 
      mutate(annual_consumption_gj = (pj / occupied_households) * 1e6,
             consumer_type = paste0(cooking, " cooking, ", water_heating, " water heating, and ", space_heating, " heating and cooling")) %>% 
      select(-c(pj, occupied_households)) %>% 
      #add microwave to cooking
      mutate(end_use = if_else(end_use == "Microwave", "Cooking", end_use)) %>% 
      group_by(consumer_type, year, fuel, end_use, state) %>% 
      summarise(annual_consumption_gj = sum(annual_consumption_gj))
      
    return(household_fuel_use)
  }

  
  #evaluate the function over all of the possible customer configurations
  
  params <- expand_grid(
    cooking = c("gas", "electric"),
    water_heating = c("gas", "electric"),
    space_heating = c("gas", "electric"),
    pv = c(TRUE, FALSE),
    ev = c(0, 1, 2)
  )
  
  
  results <- params %>%
    mutate(
      output = pmap(
        list(cooking, water_heating, space_heating),
        ~ generate_fuel_consumption_profile(
          integrated_fuel_use,
          rbs_households,
          cooking = ..1,
          water_heating = ..2,
          space_heating = ..3
        )
      )
    )
  
}

# x <- generate_fuel_consumption_profile(integrated_fuel_use,
#                                           rbs_households,
#                                                  cooking = "electric",
#                                                  water_heating = "electric",
#                                                  space_heating = "electric")
# 
# 
# x %>%
#   ggplot(aes(x = state, y = annual_consumption_gj, fill = end_use)) +
#   geom_col()
# 
# 
# y <- results %>% 
#   filter(cooking == "electric",
#          water_heating == "electric",
#          space_heating == "gas") %>% 
#   pull(output)
# 
# y[[1]] %>%
#   ggplot(aes(x = state, y = annual_consumption_gj, fill = end_use)) +
#   geom_col()
# 
# results %>% 
#   unnest()
