#!/usr/bin/env r
#=============================================================================
# cron/fetch_ebola_data_aut.R
# Ebola 2026 Project - Fault-Tolerant Automated Ingestion Engine 
#=============================================================================

library(readr)
library(dplyr)
library(stringr)
library(lubridate)

# Ensure absolute execution context path matches your deployment workspace
#setwd("/Ebola/Ebola_dashboard") 

target_dir <- "data/incoming"
if (!dir.exists(target_dir)) dir.create(target_dir, recursive = TRUE)

timestamp_suffix <- format(Sys.time(), "%Y%m%d_%H%M%S")
destination_path <- file.path(target_dir, sprintf("ebola_snapshot_%s.csv", timestamp_suffix))

# Define your source target (Ensure this domain is live or whitelisted in your server firewall)
api_endpoint <- "https://api.surveillance-hub.org/v1/ebola/delta.csv"

# Function to check host connectivity safely before downloading
is_host_resolvable <- function(url, timeout_secs = 5) {
  tryCatch({
    con <- url(url)
    withTimeout <- setTimeLimit(elapsed = timeout_secs, transient = TRUE)
    on.exit(setTimeLimit(elapsed = Inf, transient = Inf))
    # Test read a minimal chunk
    suppressWarnings(readLines(con, n = 1))
    close(con)
    return(TRUE)
  }, error = function(e) {
    return(FALSE)
  })
}

tryCatch({
  cat(sprintf("[%s] Starting scheduled data fetch...\n", Sys.time()))
  
  # Check if network route is up and DNS handles the domain target mapping
  host_ok <- is_host_resolvable(api_endpoint, timeout_secs = 6)
  
  if (!host_ok) {
    stop(sprintf("DNS Resolution Failure: Could not resolve host target network route for '%s'", api_endpoint))
  }
  
  # Fetch live incoming delta data safely
  incoming_delta <- read_csv(api_endpoint, show_col_types = FALSE)
  
  if (nrow(incoming_delta) == 0) {
    stop("Fetch completed but downstream record collection was completely empty.")
  }
  
  # Enforce strict type conversions & century definitions
  processed_delta <- incoming_delta %>%
    mutate(
      date_str = str_trim(as.character(date)),
      parsed_date = as.Date(parse_date_time(date_str, orders = c("mdy", "Ymd", "mdY", "dmy"))),
      country = str_trim(as.character(country))
    ) %>%
    filter(!is.na(parsed_date), !is.na(country), country != "") %>%
    mutate(date = as.character(parsed_date)) %>%
    select(-date_str, -parsed_date)
  
  # Handle structural persistence merge
  existing_files <- list.files(target_dir, pattern = "\\.csv$", full.names = TRUE)
  
  if (length(existing_files) > 0) {
    latest_existing_file <- existing_files[which.max(file.info(existing_files)$mtime)]
    base_data <- read_csv(latest_existing_file, show_col_types = FALSE, col_types = cols(.default = col_character()))
    
    final_output <- bind_rows(base_data, processed_delta %>% mutate(across(everything(), as.character))) %>%
      distinct(date, country, .keep_all = TRUE)
  } else {
    final_output = processed_delta
  }
  
  write_csv(final_output, destination_path)
  cat(sprintf("[%s] SUCCESS: New tracker log asset generated at: %s\n", Sys.time(), basename(destination_path)))
  
}, error = function(e) {
  #---------------------------------------------------------------------------
  # FALLBACK MODE: Handle Network Outages Gracefully
  #---------------------------------------------------------------------------
  cat(sprintf("[%s] WARNING/ERROR: %s\n", Sys.time(), e$message), file = stderr())
  cat(sprintf("[%s] Initiating local fallback loop sequence...\n", Sys.time()))
  
  existing_files <- list.files(target_dir, pattern = "\\.csv$", full.names = TRUE)
  
  if (length(existing_files) > 0) {
    # Isolate the most recent operational snapshot file path
    # Filter out any files that might have been touched during this specific cron execution minute
    clean_history <- existing_files[!str_detect(basename(existing_files), timestamp_suffix)]
    
    if(length(clean_history) > 0) {
      latest_valid_snapshot <- clean_history[which.max(file.info(clean_history)$mtime)]
      
      # Touch/copy the last valid file into a new timestamp to trigger the Shiny reactivePoll update safely
      file.copy(latest_valid_snapshot, destination_path, overwrite = TRUE)
      cat(sprintf("[%s] FALLBACK SUCCESS: Redundant asset link generated from historical state: %s\n", 
                  Sys.time(), basename(latest_valid_snapshot)))
    } else {
      cat(sprintf("[%s] CRITICAL: No historical fallback array logs exist to copy.\n", Sys.time()), file = stderr())
    }
  } else {
    cat(sprintf("[%s] CRITICAL: Pipeline folder completely empty. Standby for network restoration.\n", Sys.time()), file = stderr())
  }
})