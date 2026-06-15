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
# Dynamic Rolling File Ingestion Engine (Type-Enforced Patch)
#=============================================================================

load_ebola_data <- function() {
  # Reactive poll re-checks the data folder every 60,000 ms (1 minute)
  reactivePoll(60000, session = NULL,
               checkFunc = function() {
                 data_files <- list.files("data/incoming/", pattern = "\\.csv$", full.names = TRUE)
                 if (length(data_files) == 0) return(NULL)
                 
                 # Return modification times to evaluate if files changed on disk
                 max(file.info(data_files)$mtime)
               },
               valueFunc = function() {
                 data_files <- list.files("data/incoming/", pattern = "\\.csv$", full.names = TRUE)
                 
                 if (length(data_files) == 0) {
                   stop("Critical Ingestion Failure: No tracking data logs found in data/incoming/")
                 }
                 
                 # Identify and ingest ONLY the absolute latest file by modification time
                 latest_file <- data_files[which.max(file.info(data_files)$mtime)]
                 
                 cat(">>> Live Ingesting Backend Data Asset:", basename(latest_file), "\n")
                 
                 # Force columns directly to target double arrays via standard read_csv configurations
                 raw_data <- read_csv(latest_file, col_types = cols(
                   date = col_character(), # Read as text first to handle variable formats safely
                   country = col_character(),
                   suspected_cases = col_double(),
                   confirmed_cases = col_double(),
                   deaths = col_double(),
                   recoveries = col_double(),
                   case_fatality_rate_pct = col_double()
                 ))
                 
                 # Format-agnostic date standardization
                 raw_data %>%
                   mutate(
                     date = case_when(
                       str_detect(date, "^\\d{4}-\\d{2}-\\d{2}$") ~ lubridate::as_date(date),
                       str_detect(date, "/") ~ lubridate::mdy(date),
                       TRUE ~ lubridate::as_date(date)
                     ),
                     country = as.character(country)
                   ) %>%
                   filter(!is.na(date))
               }
  )
}