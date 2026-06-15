#=============================================================================
# scripts/fetch_ebola_data.R
# Type-Enforced Data Pipeline targeting data/incoming/ebola.csv
#=============================================================================

library(rvest)
library(tidyverse)
library(lubridate)

# 1. Initialize Essential Local Directories Safely
if(!dir.exists("data")) dir.create("data", recursive = TRUE)
if(!dir.exists("logs")) dir.create("logs")

# 2. Define Explicit Target URL & Storage Constants
live_url     <- "https://www.cdc.gov/ebola/situation-summary/index.html"
historic_url <- "https://www.cdc.gov/ebola/outbreaks/index.html"
output_file  <- "data/incoming/ebola_hist.csv"

# 3. Helper Function: Force Extraction explicitly into a Vectorized Double <dbl>
clean_val_dbl <- function(x) {
  if (is.null(x) || is.na(x) || tolower(x) == "na") return(NA_real_)
  val <- as.numeric(str_remove_all(x, "[^0-9]"))
  return(if_else(is.na(val), NA_real_, as.double(val)))
}

cat("[", as.character(Sys.time()), "] Initializing type-enforced ingestion cascade...\n")

tryCatch({
  
  #---------------------------------------------------------------------------
  # PART 1: Block-Isolated Live Text Mining
  #---------------------------------------------------------------------------
  cat("-> Extracting active live outbreak streams...\n")
  live_html <- read_html(live_url)
  live_text <- live_html %>% html_text()
  
  # Isolate precise country text blocks to eliminate menu interference
  drc_start <- str_split(live_text, "DRC")[[1]]
  
  if (length(drc_start) > 1) {
    drc_block <- str_split(drc_start[2], "Uganda")[[1]][1]
    uga_block <- str_split(drc_start[2], "Uganda")[[1]][2]
  } else {
    drc_block <- live_text
    uga_block <- live_text
  }
  
  # Structural multi-line extraction matching
  drc_c <- str_match(drc_block, "(\\d+)\\s*\\r?\\n?\\s*confirmed cases")[,2]
  drc_d <- str_match(drc_block, "(\\d+)\\s*\\r?\\n?\\s*confirmed deaths")[,2]
  drc_s <- str_match(drc_block, "(\\d+)\\s*\\r?\\n?\\s*suspected cases")[,2]
  
  uga_c <- str_match(uga_block, "(\\d+)\\s*\\r?\\n?\\s*confirmed cases")[,2]
  uga_d <- str_match(uga_block, "(\\d+)\\s*\\r?\\n?\\s*confirmed deaths")[,2]
  uga_s <- str_match(uga_block, "(\\d+)\\s*\\r?\\n?\\s*probable case")[,2] 
  
  live_snapshot <- tibble(
    date            = c(as.Date("2026-06-13"), as.Date("2026-06-14")),
    country         = c("DR Congo", "Uganda"),
    suspected_cases = c(clean_val_dbl(drc_s), clean_val_dbl(uga_s)),
    confirmed_cases = c(clean_val_dbl(drc_c), clean_val_dbl(uga_c)),
    deaths          = c(clean_val_dbl(drc_d), clean_val_dbl(uga_d)),
    recoveries      = c(NA_real_, NA_real_)
  )
  
  #---------------------------------------------------------------------------
  # PART 2: Global Historical Text Mining (1976 - Present Day)
  #---------------------------------------------------------------------------
  cat("-> Text mining historical multi-country archives (1976 - Present)...\n")
  hist_html <- read_html(historic_url)
  dom_blocks <- hist_html %>% html_nodes("div, li, p") %>% html_text()
  
  historical_records <- list()
  record_idx <- 1
  
  target_countries <- "Democratic Republic of the Congo|Uganda|Sudan|Guinea|Sierra Leone|Liberia|Gabon|Congo|South Africa"
  
  for (block in dom_blocks) {
    if (str_detect(block, "Reported number of cases:")) {
      
      country_match <- str_extract(block, target_countries)
      if (is.na(country_match)) next
      
      country_clean <- case_when(
        str_detect(country_match, "Congo|DRC") ~ "DR Congo",
        TRUE ~ country_match
      )
      
      year_match <- str_extract(block, "\\b(1976|19[7-9]\\d|20[0-2]\\d)\\b")
      
      if (!is.na(year_match)) {
        event_date <- as.Date(paste0(year_match, "-01-01"))
      } else {
        event_date <- as.Date("2025-01-01")
      }
      
      cases_match  <- str_match(block, "Reported number of cases:\\s*([0-9,]+)")[,2]
      deaths_match <- str_match(block, "Reported number of deaths[^:]*:\\s*([0-9,]+)")[,2]
      
      if (!is.na(cases_match) || !is.na(deaths_match)) {
        historical_records[[record_idx]] <- tibble(
          date            = event_date,
          country         = country_clean,
          suspected_cases = NA_real_,
          confirmed_cases = clean_val_dbl(cases_match),
          deaths          = clean_val_dbl(deaths_match),
          recoveries      = NA_real_
        )
        record_idx <- record_idx + 1
      }
    }
  }
  
  historical_snapshot <- bind_rows(historical_records)
  
  #---------------------------------------------------------------------------
  # PART 3: Merging, Strict Variable Type Locking & Output Serialization
  #---------------------------------------------------------------------------
  processed_dataset <- bind_rows(live_snapshot, historical_snapshot) %>%
    filter(!is.na(country)) %>%
    distinct(date, country, .keep_all = TRUE) %>%
    mutate(
      suspected_cases = as.double(suspected_cases),
      confirmed_cases = as.double(confirmed_cases),
      deaths          = as.double(deaths),
      recoveries      = as.double(recoveries)
    ) %>%
    mutate(
      case_fatality_rate_pct = if_else(
        is.na(confirmed_cases) | confirmed_cases == 0,
        0.00,
        round((coalesce(deaths, 0.00) / confirmed_cases) * 100, 2)
      )
    ) %>%
    mutate(case_fatality_rate_pct = as.double(if_else(case_fatality_rate_pct > 100, 100, case_fatality_rate_pct))) %>%
    select(date, country, suspected_cases, confirmed_cases, deaths, recoveries, case_fatality_rate_pct) %>%
    arrange(desc(date), country)
  
  # Synchronize data directly inside the project output path location
  if (file.exists(output_file)) {
    old_data <- read_csv(output_file, col_types = cols(
      date = col_date(),
      country = col_character(),
      suspected_cases = col_double(),
      confirmed_cases = col_double(),
      deaths = col_double(),
      recoveries = col_double(),
      case_fatality_rate_pct = col_double()
    ))
    
    final_output <- old_data %>%
      filter(!paste0(date, country) %in% paste0(processed_dataset$date, processed_dataset$country)) %>%
      bind_rows(processed_dataset) %>%
      arrange(desc(date), country)
    
    write_csv(final_output, output_file)
    cat("✔ Pipeline file successfully updated at data/ebola_hist.csv\n")
  } else {
    write_csv(processed_dataset, output_file)
    cat("✔ Base data framework initialized successfully at data/ebola_hist.csv\n")
  }
  
}, error = function(e) {
  cat("[CRITICAL DATA INGESTION ERROR]:", e$message, "\n", 
      file = "logs/scraper_failures.log", append = TRUE)
})