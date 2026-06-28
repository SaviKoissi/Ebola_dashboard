#===============================================
# data_loader.R
# Ebola 2026 Project
# This code is written by Koissi Savi
#===============================================

library(shiny)
library(readr)
library(dplyr)
library(stringr)
library(lubridate)

#=============================================================================
# R/data_loader.R
# Dynamic Rolling File Ingestion Engine (Enhanced Production Version)
#=============================================================================

load_ebola_data <- function() {
  # Reactive poll re-checks the data folder every 60,000 ms (1 minute)
  reactivePoll(60000, session = NULL,
               checkFunc = function() {
                 # Guard check against missing or empty data directories
                 if (!dir.exists("data/incoming/")) return(NULL)
                 
                 data_files <- list.files("data/incoming/", pattern = "\\.csv$", full.names = TRUE)
                 if (length(data_files) == 0) return(NULL)
                 
                 # Vectorized state verification using files modification timestamps
                 max(file.info(data_files)$mtime, na.rm = TRUE)
               },
               valueFunc = function() {
                 data_files <- list.files("data/incoming/", pattern = "\\.csv$", full.names = TRUE)
                 
                 if (length(data_files) == 0) {
                   stop("Critical Ingestion Failure: No tracking data logs found in data/incoming/")
                 }
                 
                 # Identify and isolate the absolute latest file tracking state
                 latest_file <- data_files[which.max(file.info(data_files)$mtime)]
                 cat(sprintf("[INGEST] >>> Loading live backend asset: %s\n", basename(latest_file)))
                 
                 # Enforce explicit column parsing schema configurations at read-time
                 raw_data <- tryCatch({
                   read_csv(latest_file, show_col_types = FALSE, col_types = cols(
                     date                   = col_character(), 
                     country                = col_character(),
                     suspected_cases        = col_double(),
                     confirmed_cases        = col_double(),
                     deaths                 = col_double(),
                     recoveries             = col_double(),
                     case_fatality_rate_pct = col_double(),
                     source_url             = col_character()
                   ))
                 }, error = function(e) {
                   warning("Failed parsing raw csv layout structure: ", e$message)
                   return(tibble())
                 })
                 
                 if (nrow(raw_data) == 0) return(raw_data)
                 
                 #-----------------------------------------------------------
                 # TYPE RESOLUTION ENGINE
                 #-----------------------------------------------------------
                 clean_dates <- tryCatch({
                   if (inherits(raw_data$date, "Date")) {
                     raw_data$date
                   } else if (inherits(raw_data$date, "POSIXt")) {
                     as.Date(raw_data$date)
                   } else {
                     # Standardize character vector strings
                     date_chars <- str_trim(as.character(raw_data$date))
                     
                     # Match historical year-only text patterns (e.g., "1976")
                     year_only_indices <- str_detect(date_chars, "^\\d{4}$")
                     if (any(year_only_indices, na.rm = TRUE)) {
                       date_chars[year_only_indices] <- paste0(date_chars[year_only_indices], "-01-01")
                     }
                     
                     # Explicitly try 2-digit year structures first (mdy handles '6/22/26' natively as 2026)
                     parsed <- lubridate::parse_date_time(date_chars, orders = c("mdy", "Ymd", "mdY", "dmy"))
                     as.Date(parsed)
                   }
                 }, error = function(e) {
                   warning("Date transformation failure inside data loader context: ", e$message)
                   return(rep(as.Date(NA), nrow(raw_data)))
                 })
                 
                 #-----------------------------------------------------------
                 # SCHEMA SYNCHRONIZATION & CONVERSION DEFENSE
                 #-----------------------------------------------------------
                 processed_data <- raw_data %>%
                   mutate(
                     date                   = clean_dates,
                     country                = str_trim(as.character(country)),
                     suspected_cases        = coalesce(as.double(suspected_cases), NA_real_),
                     confirmed_cases        = coalesce(as.double(confirmed_cases), NA_real_),
                     deaths                 = coalesce(as.double(deaths), NA_real_),
                     recoveries             = coalesce(as.double(recoveries), NA_real_),
                     case_fatality_rate_pct = coalesce(as.double(case_fatality_rate_pct), NA_real_),
                     source_url             = if ("source_url" %in% colnames(raw_data)) as.character(source_url) else "Unknown Source"
                   ) %>%
                   # Structural row validation: Drop completely unparseable records safely
                   filter(!is.na(date), !is.na(country), country != "")
                 
                 return(processed_data)
               }
  )
}