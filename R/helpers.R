#===============================================
# helpers.R
# Ebola 2026 Project
# This code is written by Koissi Savi
#===============================================

library(dplyr)

calculate_summary <- function(df){
  
  tibble(
    
    suspected =
      sum(df$suspected_cases, na.rm = TRUE),
    
    confirmed =
      sum(df$confirmed_cases, na.rm = TRUE),
    
    deaths =
      sum(df$deaths, na.rm = TRUE),
    
    recoveries =
      sum(df$recoveries, na.rm = TRUE)
  )
}

get_latest_snapshot <- function(df){
  
  latest <- max(df$date)
  
  df %>% 
    dplyr::filter(date == latest)
}

country_ranking <- function(df){
  
  latest <- max(df$date)
  
  df %>% 
    dplyr::filter(date == latest) |>
    arrange(desc(confirmed_cases))
}