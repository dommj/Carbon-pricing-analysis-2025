
#convert states to Grattan style

`%nin%` = Negate(`%in%`)

convert_states <- function(state){
  
  state <- str_to_upper(state)  
  
  case_when(state == 'TAS' | state == 'TASMANIA' ~ 'Tas',
            state == 'QLD' | state == 'QUEENSLAND' ~ 'Qld',
            state == 'VIC'| state == "VICTORIA" ~ 'Vic',
            state == 'NSW'| state == "NEW SOUTH WALES" ~ 'NSW',
            state == 'SA'| state == "SOUTH AUSTRALIA" ~ 'SA',
            state == 'NT'| state == "NORTHERN TERRITORY" ~ 'NT',
            state == 'WA'| state == "WESTERN AUSTRALIA" ~ 'WA',
            state == 'ACT'| state == "AUSTRALIAN CAPITAL TERRITORY" ~ 'ACT',
            state == 'Aus' | state == 'AUSTRALIA' ~ "Aus",
            .default = NA)
  
}


convert_to_2024_dollars <- function(dollars, current_year, financial = F){
  
  cpi_fy <- read_cpi() %>%
    filter(year(date)>2000) %>%
    mutate(fy = date2fy(date)) %>%
    group_by(fy) %>%
    summarise(cpi=mean(cpi)) %>%
    ungroup() 
  
  cpi_calendar <- read_cpi() %>%
    filter(year(date)>2000) %>%
    mutate(year = year(date)) %>%
    group_by(year) %>%
    summarise(cpi=mean(cpi)) %>%
    ungroup()
  
  if (financial == F){
    
    dollars_24 <- dollars_24 <- dollars * 
      (cpi_calendar %>% filter(year == 2024) %>% pull(cpi)) / 
      (cpi_calendar %>% filter(year == current_year) %>% pull(cpi))
    
  }
  
  if (financial == T){
    
    dollars_24 <- dollars_24 <- dollars * 
      (cpi_calendar %>% filter(year == 2024) %>% pull(cpi)) / 
      (cpi_fy %>% filter(fy == current_year) %>% pull(cpi))
    
  }
 
  dollars_24 
}


write_numbers_in_text <- function(number, type = c("percent", "number")) {
  library(english)
  library(dplyr)
  library(scales)
  
  type <- match.arg(type)
  
  if (type == "percent") {
    output <- case_when(
      dplyr::between(number, 0.46, 0.54) ~ "half",
      dplyr::between(number, 0.30, 0.36) ~ "a third",
      dplyr::between(number, 0.64, 0.70) ~ "two thirds",
      number < 0.1 ~ paste0(as.character(english::as.english(number * 100)), " per cent"),  # Convert to character
      TRUE ~ scales::label_percent(accuracy = 1, suffix = " per cent")(number) 
    )
  } else {
    output <- case_when(
      number < 10 ~ as.character(english::as.english(number)),  # Convert to character
      TRUE ~ scales::comma(number) 
    )
  }
  
  # we do this for copying into Latex, so it comments out the extra line breaks and doesn't muck up the para
  output <- paste0(output, "%")
  return(output)
}

