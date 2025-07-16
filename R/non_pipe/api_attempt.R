# 1. Load required libraries
library(httr)      # for making HTTP requests
library(jsonlite)  # for parsing JSON responses
library(dplyr)     # for data manipulation (tidyverse)
library(tidyr)     # for tidying nested data structures

# 2. Define API base and retailer list
# The base URL for Energy Made Easy CDR API:
base_api <- "https://cdr.energymadeeasy.gov.au"

# List of retailer codes (paths) to query.
# In practice, you would obtain this list from the AER's published data:contentReference[oaicite:22]{index=22}.
# For example purposes, we'll use a small subset of known retailers.
retailers <- c("agl", "origin", "energyaustralia", "actewagl")  # etc... (AER lists ~77 retailers)

# # (Optional) Alternatively, you could fetch the retailer list programmatically from AER's reference data:
# ref_data <- fromJSON("https://api.energymadeeasy.gov.au/refdata2?keys=organisations")
# 
# # Ensure that ref_data$organisations is a data frame before flattening
# organisations_df <- bind_rows(ref_data$data$organisations)
# 
# # Now you can access the cdrCode column directly
# retailers <- organisations_df$cdrCode 


# 3. Fetch all plan summaries for each retailer
all_plans_list <- list()  # to collect results

for (retailer in retailers) {
  # Construct the plans endpoint URL for this retailer
  plans_url <- paste0(base_api, "/", retailer, "/cds-au/v1/energy/plans")
  
  # Make GET request to fetch all current electricity and gas plans (both residential and business)
  res <- GET(plans_url, 
             query = list(
               type = "ALL",        # include both Market and Standing offers:contentReference[oaicite:23]{index=23}
               fuelType = "ALL",    # include both electricity and gas plans:contentReference[oaicite:24]{index=24}
               effective = "CURRENT"  # only currently effective plans (the latest tariffs)
             ),
             add_headers(
               "x-v" = "1",       # API version 1 for Get Generic Plans:contentReference[oaicite:25]{index=25}
               "x-min-v" = "1",   # minimum version 1
               "Accept" = "application/json"  # expecting JSON response
             ))
  
  # Parse the JSON response
  content_txt <- content(res, "text", encoding = "UTF-8")
  plans_data <- fromJSON(content_txt)
  
  # Extract the list of plans from the response
  plans <- plans_data$data$plans  # each element is a plan summary (with planId, name, etc.)
  
  # Add a column for retailer to identify which retailer the plan belongs to
  plans$retailerCode <- retailer
  
  # Store in our list
  all_plans_list[[retailer]] <- plans
}

# Combine all retailers' plans into one data frame
all_plans_df <- bind_rows(all_plans_list)

# Quick check: number of plans retrieved
nrow(all_plans_df)


# 4. Fetch detailed tariff information for each plan
plan_ids <- all_plans_df$planId  # vector of all plan IDs
plan_details_list <- list()

for (pid in plan_ids) {
  # Each planId is unique across retailers. We need to call the plan detail endpoint.
  # We also need to include the retailer's base path in the URL. We can derive it from the retailerCode column.
  retailer_code <- all_plans_df$retailerCode[ all_plans_df$planId == pid ]
  retailer_code <- unique(retailer_code)[[1]]  # get the retailer code for this plan (should be one)
  
  # Construct the plan detail URL for this plan
  detail_url <- paste0(base_api, "/", retailer_code, "/cds-au/v1/energy/plans/", pid)
  
  # GET request to fetch plan detail
  res <- GET(detail_url,
             add_headers(
               "x-v" = "3",     # use version 3 for plan detail (latest as of 2025):contentReference[oaicite:28]{index=28}
               "x-min-v" = "3",
               "Accept" = "application/json"
             ))
  
  # Parse JSON content
  detail_txt <- content(res, "text", encoding = "UTF-8")
  detail_data <- fromJSON(detail_txt, flatten = TRUE)  # flatten=TRUE to simplify nested structures
  
  # Extract the plan detail object (which includes contracts, pricing, etc.)
  plan_detail <- detail_data$data
  
  # Store it in the list
  plan_details_list[[pid]] <- plan_detail
  
  # (Optional) Pause briefly to avoid overwhelming the server, especially if many plans
  # Sys.sleep(0.2)
}

plan_details_list[[1]]


################################################################################

library(tidyverse)
library(purrr)

# Function to safely extract nested values
safe_extract <- function(x, ...) {
  tryCatch({
    result <- x[[...]]
    if (is.null(result)) NA else result
  }, error = function(e) NA)
}

# Helper function to check if something is effectively empty
is_empty <- function(x) {
  is.null(x) || (length(x) == 1 && is.na(x)) || (is.data.frame(x) && nrow(x) == 0)
}

# Function to extract basic plan information
extract_plan_basics <- function(plan) {
  tibble(
    plan_id = safe_extract(plan, "planId"),
    brand = safe_extract(plan, "brand"),
    brand_name = safe_extract(plan, "brandName"),
    display_name = safe_extract(plan, "displayName"),
    fuel_type = safe_extract(plan, "fuelType"),
    type = safe_extract(plan, "type"),
    customer_type = safe_extract(plan, "customerType"),
    last_updated = safe_extract(plan, "lastUpdated"),
    effective_from = safe_extract(plan, "effectiveFrom"),
    distributor = safe_extract(plan, "geography", "distributors"),
    postcode_count = length(safe_extract(plan, "geography", "includedPostcodes"))
  )
}

# Function to extract electricity contract basics
extract_contract_basics <- function(plan) {
  contract <- safe_extract(plan, "electricityContract")
  if (is_empty(contract)) {
    return(tibble(
      plan_id = safe_extract(plan, "planId"),
      is_fixed = NA,
      time_zone = NA,
      pricing_model = NA,
      daily_supply_charge = NA,
      cooling_off_days = NA,
      terms = NA,
      variation = NA
    ))
  }
  
  tibble(
    plan_id = safe_extract(plan, "planId"),
    is_fixed = safe_extract(contract, "isFixed"),
    time_zone = safe_extract(contract, "timeZone"),
    pricing_model = safe_extract(contract, "pricingModel"),
    daily_supply_charge = as.numeric(safe_extract(contract, "tariffPeriod", 1, "dailySupplyCharge")),
    cooling_off_days = safe_extract(contract, "coolingOffDays"),
    terms = safe_extract(contract, "terms"),
    variation = safe_extract(contract, "variation")
  )
}

# Function to extract time of use rates
extract_tou_rates <- function(plan) {
  contract <- safe_extract(plan, "electricityContract")
  if (is_empty(contract)) return(tibble())
  
  tariff_period <- safe_extract(contract, "tariffPeriod", 1)
  if (is_empty(tariff_period)) return(tibble())
  
  tou_rates <- safe_extract(tariff_period, "timeOfUseRates", 1)
  if (is_empty(tou_rates)) return(tibble())
  
  tou_rates %>%
    mutate(
      plan_id = safe_extract(plan, "planId"),
      unit_price = map_dbl(rates, ~ as.numeric(safe_extract(.x, 1, "unitPrice"))),
      measure_unit = map_chr(rates, ~ safe_extract(.x, 1, "measureUnit")),
      time_start = map_chr(timeOfUse, ~ safe_extract(.x, 1, "startTime")),
      time_end = map_chr(timeOfUse, ~ safe_extract(.x, 1, "endTime"))
    ) %>%
    select(plan_id, rate_type = type, unit_price, measure_unit, 
           time_start, time_end, description, display_name = displayName)
}

# Function to extract fees
extract_fees <- function(plan) {
  contract <- safe_extract(plan, "electricityContract")
  if (is_empty(contract)) return(tibble())
  
  fees <- safe_extract(contract, "fees")
  if (is_empty(fees)) return(tibble())
  
  fees %>%
    mutate(
      plan_id = safe_extract(plan, "planId"),
      amount = as.numeric(amount),
      rate = if("rate" %in% names(fees)) rate else NA_character_
    ) %>%
    select(plan_id, fee_term = term, fee_type = type, amount, description, rate)
}

# Function to extract solar feed-in tariffs
extract_solar_tariffs <- function(plan) {
  contract <- safe_extract(plan, "electricityContract")
  if (is_empty(contract)) return(tibble())
  
  solar_tariffs <- safe_extract(contract, "solarFeedInTariff")
  if (is_empty(solar_tariffs)) return(tibble())
  
  solar_tariffs %>%
    mutate(
      plan_id = safe_extract(plan, "planId"),
      unit_price = map_dbl(singleTariff.rates, ~ as.numeric(safe_extract(.x, 1, "unitPrice"))),
      measure_unit = map_chr(singleTariff.rates, ~ safe_extract(.x, 1, "measureUnit"))
    ) %>%
    select(plan_id, scheme, payer_type = payerType, unit_price, measure_unit, 
           description, display_name = displayName)
}

# Function to extract controlled load information
extract_controlled_load <- function(plan) {
  contract <- safe_extract(plan, "electricityContract")
  if (is_empty(contract)) return(tibble())
  
  controlled_load <- safe_extract(contract, "controlledLoad")
  if (is_empty(controlled_load)) return(tibble())
  
  controlled_load %>%
    mutate(
      plan_id = safe_extract(plan, "planId"),
      unit_price = map_dbl(singleRate.rates, ~ as.numeric(safe_extract(.x, 1, "unitPrice"))),
      measure_unit = map_chr(singleRate.rates, ~ safe_extract(.x, 1, "measureUnit"))
    ) %>%
    select(plan_id, display_name = displayName, unit_price, measure_unit,
           description = singleRate.description, 
           daily_supply_charge = singleRate.dailySupplyCharge)
}

# Function to extract incentives
extract_incentives <- function(plan) {
  contract <- safe_extract(plan, "electricityContract")
  if (is_empty(contract)) return(tibble())
  
  incentives <- safe_extract(contract, "incentives")
  if (is_empty(incentives)) return(tibble())
  
  incentives %>%
    mutate(plan_id = safe_extract(plan, "planId")) %>%
    select(plan_id, category, description, display_name = displayName)
}

# Main function to process all plans
tidy_energy_plans <- function(plan_details_list) {
  
  # Extract basic plan information
  plan_basics <- map_dfr(plan_details_list, extract_plan_basics)
  
  # Extract contract basics
  contract_basics <- map_dfr(plan_details_list, extract_contract_basics)
  
  # Extract time of use rates
  #tou_rates <- map_dfr(plan_details_list, extract_tou_rates)
  
  # Extract fees
  fees <- map_dfr(plan_details_list, extract_fees)
  
  # Extract solar tariffs
  #solar_tariffs <- map_dfr(plan_details_list, extract_solar_tariffs)
  
  # Extract controlled load
  #controlled_load <- map_dfr(plan_details_list, extract_controlled_load)
  
  # Extract incentives
  #incentives <- map_dfr(plan_details_list, extract_incentives)
  
  # Combine basic plan and contract information
  main_table <- plan_basics %>%
    left_join(contract_basics, by = "plan_id")
  
  # Return a list of tidy tables
  list(
    plans = main_table,
    #time_of_use_rates = tou_rates,
    fees = fees
    #solar_tariffs = solar_tariffs,
    #controlled_load = controlled_load,
    #incentives = incentives
  )
}

# Usage example:
# Assuming your data is in plan_details_list
tidy_data <- tidy_energy_plans(plan_details_list)

plan_fees <- tidy_data$fees

plan_basics <- tidy_data$plans

# Access individual tables:
# tidy_data$plans              # Main plan information
# tidy_data$time_of_use_rates  # Time of use pricing
# tidy_data$fees               # Various fees
# tidy_data$solar_tariffs      # Solar feed-in tariffs
# tidy_data$controlled_load    # Controlled load information
# tidy_data$incentives         # Plan incentives

# If you want everything in one large table (though this will create many duplicate rows):
# create_single_table <- function(tidy_data) {
#   tidy_data$plans %>%
#     left_join(tidy_data$time_of_use_rates, by = "plan_id") %>%
#     left_join(tidy_data$fees, by = "plan_id") %>%
#     left_join(tidy_data$solar_tariffs, by = "plan_id") %>%
#     left_join(tidy_data$controlled_load, by = "plan_id") %>%
#     left_join(tidy_data$incentives, by = "plan_id")
# }
extract_contract_basics(plan_details_list[[1]])


#############################################################################################


