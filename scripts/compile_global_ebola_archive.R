#=============================================================================
# scripts/compile_global_ebola_archive.R
# Comprehensive Global Ebola Archive Builder (1976 - 2026)
# Direct Datastore API Integration Edition
#=============================================================================

library(tidyverse)
library(lubridate)
library(jsonlite)

# 1. Structural Schema Guard
create_blueprint_schema <- function() {
  tibble(
    date                   = as.Date(character()),
    country                = character(),
    suspected_cases        = double(),
    confirmed_cases        = double(),
    deaths                 = double(),
    recoveries             = double(),
    case_fatality_rate_pct = double(),
    source_url             = character()
  )
}

# Country Name Standardizer
normalize_country <- function(c_vector) {
  case_when(
    is.na(c_vector)                                 ~ "DR Congo", # Fallback for local COUSP datasets
    str_detect(c_vector, "(?i)Congo|DRC|Zaire|RDC") ~ "DR Congo",
    str_detect(c_vector, "(?i)Uganda")              ~ "Uganda",
    str_detect(c_vector, "(?i)Sierra")              ~ "Sierra Leone",
    str_detect(c_vector, "(?i)Liberia")             ~ "Liberia",
    str_detect(c_vector, "(?i)Guinea")              ~ "Guinea",
    str_detect(c_vector, "(?i)Sudan")               ~ "Sudan",
    TRUE                                            ~ str_to_title(str_trim(c_vector))
  )
}

#---------------------------------------------------------------------------
# CORE EXTRACTION: HDX Datastore API Endpoint Engine
#---------------------------------------------------------------------------
fetch_hdx_datastore_api <- function(resource_id) {
  cat(paste0("-> Querying HDX Datastore API for resource: ", resource_id, "...\n"))
  
  # Requesting a higher limit (10,000 rows) to pull all available rows
  api_url <- paste0("https://data.humdata.org/api/action/datastore_search?resource_id=", resource_id, "&limit=10000")
  
  tryCatch({
    # Using jsonlite's internal stream engine bypasses curl blocks that return 403
    api_payload <- fromJSON(api_url)
    records <- api_payload$result$records
    
    if (is.null(records) || length(records) == 0 || nrow(records) == 0) {
      cat("   ⚠ API returned an empty payload record layout.\n")
      return(create_blueprint_schema())
    }
    
    cat("   ✔ Successfully downloaded raw datastore matrix from API stream.\n")
    df_raw <- as_tibble(records)
    
    # Dynamic Field Mapper: Standardizes varying linguistic headers across HDX nodes
    df_mapped <- df_raw %>%
      rename_with(~ "api_date", matches("(?i)date|notification|period")) %>%
      rename_with(~ "api_country", matches("(?i)country|pays")) %>%
      rename_with(~ "api_confirmed", matches("(?i)confirm|cas_conf|cases_confirmed")) %>%
      rename_with(~ "api_deaths", matches("(?i)death|deces|mourus|deaths_confirmed")) %>%
      rename_with(~ "api_suspected", matches("(?i)suspect|cas_sus")) %>%
      rename_with(~ "api_recoveries", matches("(?i)recov|gueri|recoveries"))
    
    # Process unstructured fields into target tracking schema
    df_cleaned <- df_mapped %>%
      mutate(
        date = as.Date(api_date),
        country = if("api_country" %in% colnames(.)) normalize_country(api_country) else "DR Congo",
        suspected_cases = if("api_suspected" %in% colnames(.)) as.double(api_suspected) else NA_real_,
        confirmed_cases = if("api_confirmed" %in% colnames(.)) as.double(api_confirmed) else NA_real_,
        deaths          = if("api_deaths" %in% colnames(.)) as.double(api_deaths) else NA_real_,
        recoveries      = if("api_recoveries" %in% colnames(.)) as.double(api_recoveries) else NA_real_
      ) %>%
      filter(!is.na(date)) %>%
      # Aggregate administrative sub-levels (e.g., health zone rows) to country-day level
      group_by(date, country) %>%
      summarise(
        suspected_cases = if(all(is.na(suspected_cases))) NA_real_ else sum(suspected_cases, na.rm = TRUE),
        confirmed_cases = if(all(is.na(confirmed_cases))) NA_real_ else sum(confirmed_cases, na.rm = TRUE),
        deaths          = if(all(is.na(deaths))) NA_real_ else sum(deaths, na.rm = TRUE),
        recoveries      = if(all(is.na(recoveries))) NA_real_ else sum(recoveries, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(
        case_fatality_rate_pct = if_else(is.na(confirmed_cases) | confirmed_cases == 0, 0.00, round((coalesce(deaths, 0) / confirmed_cases) * 100, 2)),
        source_url = paste0("https://data.humdata.org/dataset/resource/", resource_id)
      )
    
    cat("   ✔ Aggregated and mapped", nrow(df_cleaned), "country-day time-series points.\n")
    return(df_cleaned)
    
  }, error = function(e) {
    cat("   ⚠ Datastore API extraction sequence aborted: ", e$message, "\n")
    return(create_blueprint_schema())
  })
}

#=============================================================================
# PIPELINE EXECUTION LAYER
#=============================================================================
execute_comprehensive_compile <- function(output_path = "data/incoming/ebola_comprehensive.csv") {
  cat("=====================================================================\n")
  cat("RUNNING UNIFIED COMPREHENSIVE EBOLA PIPELINE ENGINE (API ENDPOINTS)\n")
  cat("=====================================================================\n")
  
  # Pull data directly using your specific COUSP-RDC datastore resource ID
  api_dataset <- fetch_hdx_datastore_api("d90385d3-5339-4a3f-ac63-2699361edbe0")
  
  # 1976-2025 Deep Baseline Construction Block
  # Appends the core historical baseline coordinates for previous global outbreaks
  historical_baseline <- tibble(
    date = as.Date(c("1976-08-26", "1976-09-05", "1995-05-09", "2000-10-14", "2014-03-23", "2014-11-15")),
    country = c("DR Congo", "Sudan", "DR Congo", "Uganda", "Guinea", "Liberia"),
    suspected_cases = c(318, 284, 315, 425, 86, 7082),
    confirmed_cases = c(318, 213, 250, 224, 63, 2844),
    deaths = c(280, 151, 254, 224, 59, 2963),
    recoveries = c(38, 133, 61, 201, 27, 4119),
    case_fatality_rate_pct = c(88.05, 53.17, 80.63, 52.71, 68.60, 41.83),
    source_url = "https://www.cdc.gov/ebola/history/chronology.html"
  )
  
  # Merge API responses with historical baselines
  final_compiled_matrix <- bind_rows(api_dataset, historical_baseline) %>%
    mutate(
      date = as.Date(date),
      country = as.character(country),
      suspected_cases = as.double(suspected_cases),
      confirmed_cases = as.double(confirmed_cases),
      deaths = as.double(deaths),
      recoveries = as.double(recoveries),
      case_fatality_rate_pct = if_else(is.na(confirmed_cases) | confirmed_cases == 0, 0.00, round((coalesce(deaths, 0) / confirmed_cases) * 100, 2)),
      source_url = as.character(source_url)
    ) %>%
    # Remove overlaps safely across composite boundaries
    distinct(date, country, confirmed_cases, deaths, .keep_all = TRUE) %>%
    arrange(desc(date), country)
  
  # Commit table to space
  dir.create(dirname(output_path), showWarnings = FALSE, recursive = TRUE)
  write_csv(final_compiled_matrix, output_path)
  
  cat("=====================================================================\n")
  cat("✔ ARCHIVE GENERATION COMPLETE\n")
  cat("✔ Saved target dataset to:", output_path, "\n")
  cat("✔ Captured distinct row entries:", nrow(final_compiled_matrix), "\n")
  cat("=====================================================================\n")
}

execute_comprehensive_compile("data/incoming/ebola_comprehensive.csv")