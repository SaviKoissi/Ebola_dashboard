#=============================================================================
# scripts/flexible_append_engine.R
# General-Purpose Ledger Append & Deduplication Utility
#=============================================================================

library(tidyverse)
library(lubridate)

#' Append New Streaming Ingests to an Existing Master Archive File
#'
#' @param archive_path Path to your complete historic master dataset (the old data).
#' @param incoming_path Path to the new, updated file (e.g., "data/incoming/ebola2.csv").
#' @export
merge_and_append_ledgers <- function(archive_path, incoming_path) {
  cat("-> Initializing flexible merge sequence...\n")
  
  # Structural validation checking tracking source files
  if (!file.exists(incoming_path)) {
    stop("Error: The incoming update file could not be found at: ", incoming_path)
  }
  
  # Read incoming data frame
  incoming_df <- read_csv(incoming_path, show_col_types = FALSE)
  if (nrow(incoming_df) == 0) {
    cat("ℹ Incoming update ledger contains zero records. Merge suspended.\n")
    return(invisible(FALSE))
  }
  
  # Enforce rigorous schema standards for incoming files
  incoming_clean <- incoming_df %>%
    mutate(across(c(where(is.character), -any_of("country"), -any_of("source_url")), as.character)) %>% 
    mutate(
      date = as.Date(date),
      country = as.character(country),
      suspected_cases = as.double(suspected_cases),
      confirmed_cases = as.double(confirmed_cases),
      deaths = as.double(deaths),
      recoveries = as.double(recoveries),
      case_fatality_rate_pct = as.double(case_fatality_rate_pct),
      source_url = if("source_url" %in% colnames(.)) as.character(source_url) else "Direct Ingest"
    ) %>%
    filter(!is.na(country) & !is.na(date))
  
  # If master archive exists, perform deduplication step; otherwise initialize it
  if (file.exists(archive_path)) {
    cat("✔ Master archive located. Reading historical tracking blocks...\n")
    archive_df <- read_csv(archive_path, show_col_types = FALSE)
    
    # Back-fill legacy tracking columns if absent from archive
    if (!"source_url" %in% colnames(archive_df)) {
      archive_df <- archive_df %>% mutate(source_url = "Direct Ingest")
    }
    
    archive_clean <- archive_df %>%
      mutate(
        date = as.Date(date), country = as.character(country),
        suspected_cases = as.double(suspected_cases), confirmed_cases = as.double(confirmed_cases),
        deaths = as.double(deaths), recoveries = as.double(recoveries),
        case_fatality_rate_pct = as.double(case_fatality_rate_pct), source_url = as.character(source_url)
      )
    
    # Remove rows from the old database that are updated inside the new file
    deduplicated_archive <- archive_clean %>%
      filter(!paste0(date, country, source_url) %in% 
               paste0(incoming_clean$date, incoming_clean$country, incoming_clean$source_url))
    
    # Append the backfilled updates directly to the remaining old blocks
    final_output <- deduplicated_archive %>%
      bind_rows(incoming_clean) %>%
      arrange(desc(date), country)
    
    cat("✔ Verification complete. Appended", nrow(incoming_clean), "rows to master ledger matrix.\n")
    
  } else {
    cat("ℹ Targeted master archive file path not found. Creating a new master file directly from incoming entries.\n")
    final_output <- incoming_clean %>% arrange(desc(date), country)
    dir.create(dirname(archive_path), showWarnings = FALSE, recursive = TRUE)
  }
  
  # Commit changes to your master destination file
  write_csv(final_output, archive_path)
  cat("✔ Master file successfully synchronized at:", archive_path, "\n")
  return(invisible(TRUE))
}

# =============================================================================
# EXAMPLES OF FLEXIBLE USAGE
# =============================================================================
# # Example 1: Standard Sync
# merge_and_append_ledgers(
#   archive_path  = "data/historical/master_archive_complete.csv", 
#   incoming_path = "data/incoming/ebola2.csv"
# )
#
# # Example 2: Running with a totally different historical file name
# merge_and_append_ledgers(
#   archive_path  = "data/backups/global_ebola_tracker_2026.csv", 
#   incoming_path = "data/incoming/ebola2.csv"
# )