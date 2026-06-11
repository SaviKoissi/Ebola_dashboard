#===============================================
# data_loader.R
# Ebola 2026 Project
# This code is written by Koissi Savi
#===============================================


library(shiny)
library(readr)
library(dplyr)

#=============================================================================
# R/data_loader.R
# Dynamic Rolling File Ingestion Engine
#=============================================================================

load_ebola_data <- function() {
  # Reactive poll re-checks the data folder every 60,000 ms (1 minute)
  # If a new file appears, it automatically updates the dashboard globally
  reactivePoll(60000, session = NULL,
               checkFunc = function() {
                 # Target file tracking directory
                 data_files <- list.files("data/incoming/", pattern = "\\.csv$", full.names = TRUE)
                 if (length(data_files) == 0) return(NULL)
                 
                 # Return the composite modification times of the directory to check for updates
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
                 
                 read_csv(latest_file) %>%
                   mutate(
                     date = as.Date(date),
                     country = as.character(country)
                   )
               }
  )
}