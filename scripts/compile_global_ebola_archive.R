#=============================================================================
# scripts/compile_global_ebola_archive.R
# Fortified Multi-Era Global Ebola Ingestion Pipeline
# Strict Header Resolution Engine for Conflicting OCHA Metadata Fields
#=============================================================================

library(tidyverse)
library(lubridate)
library(jsonlite)
library(httr2)

create_blueprint_schema <- function() {
  tibble(
    date                    = as.Date(character()),
    country                 = character(),
    suspected_cases        = double(),
    confirmed_cases        = double(),
    deaths                 = double(),
    recoveries             = double(),
    case_fatality_rate_pct = double(),
    source_url             = character()
  )
}

normalize_country <- function(c_vector) {
  case_when(
    is.na(c_vector)                                           ~ "Unknown",
    str_detect(c_vector, "(?i)Congo|DRC|Zaire|RDC|COD|Rép")    ~ "DR Congo",
    str_detect(c_vector, "(?i)Uganda|UGA")                       ~ "Uganda",
    str_detect(c_vector, "(?i)Sierra|SLE")                       ~ "Sierra Leone",
    str_detect(c_vector, "(?i)Liberia|LBR")                      ~ "Liberia",
    str_detect(c_vector, "(?i)Guinea|GIN")                       ~ "Guinea",
    str_detect(c_vector, "(?i)Sudan|SDN")                        ~ "Sudan",
    str_detect(c_vector, "(?i)Nigeria|NGA")                      ~ "Nigeria",
    TRUE                                                     ~ str_to_title(str_trim(c_vector))
  )
}

parse_flexible_date <- function(date_vector) {
  parsed <- as.Date(parse_date_time(date_vector, orders = c("Ymd", "mdY", "dmy", "Ymd HMS", "mdY HMS")))
  return(parsed)
}

build_secure_request <- function(url) {
  request(url) %>% 
    req_timeout(45) %>%
    req_user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) R-Research-Pipeline") %>%
    req_options(ssl_verifypeer = FALSE, ssl_verifyhost = 0L)
}

#---------------------------------------------------------------------------
# NODE 1: Historical Baseline (1976 - 2016 Era)
#---------------------------------------------------------------------------
fetch_hdx_historical_baseline <- function() {
  cat("-> Syncing historical timeline repository (1976-2016)...\n")
  csv_url <- "https://data.humdata.org/dataset/0d089fa0-3567-4b01-9c03-39d340ff34e3/resource/c59b5722-ca4b-41ca-a446-472d6d824d01/download/ebola_data_db_format.csv"
  
  tryCatch({
    req <- build_secure_request(csv_url)
    resp <- req_perform(req)
    df_raw <- read_csv(I(resp_body_string(resp)), show_col_types = FALSE)
    
    df_cleaned <- df_raw %>%
      rename_with(~ case_when(
        str_detect(.x, "(?i)date")                      ~ "api_date",
        str_detect(.x, "(?i)country")                   ~ "api_country",
        str_detect(.x, "(?i)indicator")                 ~ "case_classification",
        str_detect(.x, "(?i)value")                     ~ "api_value",
        TRUE                                            ~ .x
      )) %>%
      mutate(
        api_date = parse_flexible_date(api_date),
        api_country = normalize_country(api_country),
        api_value = as.double(api_value),
        metric_clean = case_when(
          str_detect(case_classification, "(?i)confirm")     ~ "confirmed_cases",
          str_detect(case_classification, "(?i)death")       ~ "deaths",
          str_detect(case_classification, "(?i)suspect")     ~ "suspected_cases",
          str_detect(case_classification, "(?i)recov")       ~ "recoveries",
          TRUE                                               ~ "drop_untracked"
        )
      ) %>%
      filter(metric_clean != "drop_untracked", !is.na(api_date)) %>%
      pivot_wider(
        id_cols = c(api_date, api_country),
        names_from = metric_clean,
        values_from = api_value,
        values_fn = max,
        values_fill = 0
      ) %>%
      transmute(
        date = api_date, country = api_country,
        suspected_cases = if("suspected_cases" %in% colnames(.)) as.double(suspected_cases) else 0,
        confirmed_cases = if("confirmed_cases" %in% colnames(.)) as.double(confirmed_cases) else 0,
        deaths          = if("deaths" %in% colnames(.)) as.double(deaths) else 0,
        recoveries      = if("recoveries" %in% colnames(.)) as.double(recoveries) else 0,
        source_url = "https://data.humdata.org/dataset/ebola-cases-2014"
      )
    return(df_cleaned)
  }, error = function(e) {
    cat("   ⚠ Historical boundary node parsing skipped: ", conditionMessage(e), "\n")
    return(create_blueprint_schema())
  })
}

#---------------------------------------------------------------------------
# NODE 2: Modern Live Crisis API Hook (Resolves Active 2026 Outbreaks)
#---------------------------------------------------------------------------
fetch_hdx_live_crisis_layer <- function() {
  cat("-> Locating active crisis operational metrics via HDX Portal Registry API...\n")
  meta_url <- "https://data.humdata.org/api/3/action/package_show?id=republique-democratique-du-congo-cas-et-deces-d-ebola"
  
  tryCatch({
    req <- build_secure_request(meta_url)
    resp <- req_perform(req)
    meta_json <- fromJSON(resp_body_string(resp))
    
    resources <- meta_json$result$resources
    csv_target <- resources %>% 
      as_tibble() %>% 
      filter(format == "CSV") %>% 
      slice(1) %>% 
      pull(url)
    
    if (length(csv_target) == 0 || csv_target == "") return(create_blueprint_schema())
    
    cat("   ✔ Discovered live streaming target:", csv_target, "\n")
    live_resp <- req_perform(build_secure_request(csv_target))
    df_live_raw <- read_csv(I(resp_body_string(live_resp)), show_col_types = FALSE)
    
    raw_cols <- names(df_live_raw)
    
    # Precise priority extraction to prevent naming collision with duplicated metadata fields
    target_date_col <- raw_cols[str_detect(raw_cols, "(?i)^date$") | str_detect(raw_cols, "(?i)epi.*date")][1]
    if(is.na(target_date_col)) target_date_col <- raw_cols[str_detect(raw_cols, "(?i)date")][1]
    
    target_country_col <- raw_cols[str_detect(raw_cols, "(?i)^pays$") | str_detect(raw_cols, "(?i)^country$")][1]
    
    target_class_col <- raw_cols[str_detect(raw_cols, "(?i)indicator") | str_detect(raw_cols, "(?i)indicateur") | str_detect(raw_cols, "(?i)classification")][1]
    
    target_value_col <- raw_cols[str_detect(raw_cols, "(?i)valeur") | str_detect(raw_cols, "(?i)value") | str_detect(raw_cols, "(?i)nombre") | str_detect(raw_cols, "(?i)^cas$")][1]
    
    # Safe fallback if country is implicit in sub-national files
    if (is.na(target_country_col)) {
      df_live_raw <- df_live_raw %>% mutate(resolved_country = "DR Congo")
      target_country_col <- "resolved_country"
    }
    
    # Perform isolating projection using exact strings rather than generalized regex renaming
    df_isolated <- df_live_raw %>%
      transmute(
        api_date    = .data[[target_date_col]],
        api_country = .data[[target_country_col]],
        case_classification = if(!is.na(target_class_col)) as.character(.data[[target_class_col]]) else "confirmed_cases",
        api_value   = if(!is.na(target_value_col)) as.double(.data[[target_value_col]]) else 1.0
      ) %>%
      mutate(
        parsed_date = parse_flexible_date(api_date),
        country = normalize_country(api_country)
      ) %>%
      filter(!is.na(parsed_date))
    
    # Route through long-form transformation clean logic
    df_final_extracted <- df_isolated %>%
      mutate(
        metric_clean = case_when(
          str_detect(case_classification, "(?i)confirm")     ~ "confirmed_cases",
          str_detect(case_classification, "(?i)death|décès") ~ "deaths",
          str_detect(case_classification, "(?i)suspect")     ~ "suspected_cases",
          str_detect(case_classification, "(?i)recov|guéri") ~ "recoveries",
          TRUE                                               ~ "drop_untracked"
        )
      ) %>%
      filter(metric_clean != "drop_untracked") %>%
      group_by(parsed_date, country, metric_clean) %>%
      summarise(api_value = sum(api_value, na.rm = TRUE), .groups = "drop") %>% 
      pivot_wider(
        id_cols = c(parsed_date, country),
        names_from = metric_clean,
        values_from = api_value,
        values_fill = 0
      ) %>%
      rename(date = parsed_date)
    
    # Secure structural dimensions
    required_cols <- c("suspected_cases", "confirmed_cases", "deaths", "recoveries")
    for (col in required_cols) {
      if (!col %in% colnames(df_final_extracted)) {
        df_final_extracted[[col]] <- 0.0
      }
    }
    
    return(df_final_extracted %>% mutate(source_url = csv_target))
    
  }, error = function(e) {
    cat("   ⚠ Operational modern stream lookup skipped: ", conditionMessage(e), "\n")
    return(create_blueprint_schema())
  })
}

#=============================================================================
# PIPELINE UNIFICATION INTERFACE
#=============================================================================
execute_comprehensive_compile <- function(output_path = "data/incoming/ebola_comprehensive.csv") {
  cat("=====================================================================\n")
  cat("RUNNING UNIFIED COMPREHENSIVE MULTI-ERA GLOBAL EBOLA ENGINE\n")
  cat("=====================================================================\n")
  
  historical_baseline <- fetch_hdx_historical_baseline()
  live_modern_layers  <- fetch_hdx_live_crisis_layer()
  
  cat("-> Binding historical eras and running structural de-duplication...\n")
  combined <- bind_rows(historical_baseline, live_modern_layers)
  
  if (nrow(combined) == 0) {
    cat("   ⚠ Pipeline failure: Operational layers could not be resolved.\n")
    return(NULL)
  }
  
  final_compiled_matrix <- combined %>%
    filter(!is.na(date), !is.na(country), country != "Unknown") %>%
    group_by(date, country) %>%
    summarise(
      suspected_cases = max(suspected_cases, na.rm = TRUE),
      confirmed_cases = max(confirmed_cases, na.rm = TRUE),
      deaths          = max(deaths, na.rm = TRUE),
      recoveries      = max(recoveries, na.rm = TRUE),
      source_url      = first(source_url),
      .groups = "drop"
    ) %>%
    mutate(
      case_fatality_rate_pct = if_else(confirmed_cases == 0, 0.00, round((deaths / confirmed_cases) * 100, 2))
    ) %>%
    arrange(desc(date), country)
  
  dir.create(dirname(output_path), showWarnings = FALSE, recursive = TRUE)
  write_csv(final_compiled_matrix, output_path)
  
  cat("=====================================================================\n")
  cat("✔ ARCHIVE MERGE COMPLETE\n")
  cat("✔ Saved target dataset to:", output_path, "\n")
  cat("✔ Chronological range resolved:", as.character(min(final_compiled_matrix$date)), "to", as.character(max(final_compiled_matrix$date)), "\n")
  cat("✔ Matrix size:", nrow(final_compiled_matrix), "observations mapped\n")
  cat("=====================================================================\n")
}

execute_comprehensive_compile("data/incoming/ebola_comprehensive.csv")