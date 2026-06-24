#=============================================================================
# app.R
# Ebola 2026 Project - Robust Multi-Country Pipeline
# Fixed Reactivity Tracking & Strict 2-Digit Year Century Alignment
#=============================================================================

library(shiny)
library(bslib)
library(dplyr)
library(lubridate) 

source("R/data_loader.R")
source("R/mod_dashboard.R")
source("R/mod_map.R")
source("R/mod_timeseries.R")
source("R/mod_country.R")
source("R/mod_download.R")

app_theme <- function(){
  bs_theme(
    version = 5,
    bootswatch = "lux",
    primary =   "#e74c3c",
    secondary = "#2c3e50",  
    bg = "#F8F9FA",
    fg = "#2C3E50"
  )
}

ui <- page_navbar(
  title = "🦠 Ebola Surveillance",
  theme = app_theme(),
  
  tags$head(
    includeCSS("www/css/style.css"),
    includeScript("www/js/custom.js")
  ),
  
  sidebar = sidebar(
    title = "Surveillance Filters",
    
    div(
      style = "display: flex; gap: 10px; margin-bottom: 5px;",
      actionButton("btn_reset", "🔄 Reset", class = "btn-secondary btn-sm", style = "flex: 1;"),
      actionButton("btn_lang", "🌐 FR / EN", class = "btn-outline-primary btn-sm", style = "flex: 1;")
    ),
    
    # Dynamic Live Metadata Update Tracker Block
    uiOutput("live_update_badge"),
    hr(style = "margin-top: 10px; margin-bottom: 15px;"),
    
    dateRangeInput(
      "date_range",
      "Reporting Window",
      start = "2014-01-01",  
      end   = "2027-12-31"
    ),
    selectizeInput(
      "countries",
      "Target Countries (All if Blank)",
      choices = NULL,         
      multiple = TRUE,       
      options = list(placeholder = 'Select Country...')
    ),
    selectInput(
      "metric",
      "Primary Metric Matrix",
      choices = c(
        "Confirmed Cases" = "confirmed_cases",
        "Suspected Cases" = "suspected_cases",
        "Fatalities" = "deaths",
        "Recoveries" = "recoveries"
      ),
      selected = "confirmed_cases"
    )
  ),
  
  nav_panel("Explorer", mod_dashboard_ui("dashboard")),
  nav_panel("Spatiotemporal Map", mod_map_ui("map")),
  nav_panel("Epi Trends", mod_timeseries_ui("trend")),
  nav_panel("Profiler", mod_country_ui("country")),
  nav_panel("Data Hub", mod_download_ui("download")),
  
  nav_spacer(),
  nav_item(
    tags$img(
      src = "ASB.png", 
      style = "height: 38px; width: auto; object-fit: contain; padding-bottom: 2px;"
    )
  )
)

server <- function(input, output, session){
  
  current_lang <- reactiveVal("EN")
  
  observeEvent(input$btn_lang, {
    if (current_lang() == "EN") current_lang("FR") else current_lang("EN")
  })
  
  observe({
    lang <- current_lang()
    if (lang == "FR") {
      updateDateRangeInput(session, "date_range", label = "Fenêtre de rapport")
      updateSelectizeInput(session, "countries", label = "Pays Cibles (Tous si vide)", 
                           options = list(placeholder = 'Choisir un pays...'))
      updateSelectInput(session, "metric", label = "Matrice des métriques principales",
                        choices = c("Cas Confirmés" = "confirmed_cases", "Cas Suspects" = "suspected_cases", "Décès" = "deaths", "Guérisons" = "recoveries"),
                        selected = input$metric)
    } else {
      updateDateRangeInput(session, "date_range", label = "Reporting Window")
      updateSelectizeInput(session, "countries", label = "Target Countries (All if Blank)", 
                           options = list(placeholder = 'Select Country...'))
      updateSelectInput(session, "metric", label = "Primary Metric Matrix",
                        choices = c("Confirmed Cases" = "confirmed_cases", "Suspected Cases" = "suspected_cases", "Fatalities" = "deaths", "Recoveries" = "recoveries"),
                        selected = input$metric)
    }
  })
  
  raw_ebola_data <- load_ebola_data()
  
  # Only load country choices ONCE when data is ready to break infinite reactive loop
  observeEvent(raw_ebola_data(), {
    df <- raw_ebola_data()
    if (!is.null(df) && nrow(df) > 0) {
      available_countries <- sort(unique(df$country))
      updateSelectizeInput(session, "countries", choices = available_countries, server = TRUE)
    }
  })
  
  # Render the File-Inbound Modification Timestamp Badge dynamically
  output$live_update_badge <- renderUI({
    req(raw_ebola_data())
    df <- raw_ebola_data()
    
    # Calculate the latest date string found within the data rows
    max_data_date <- max(as.Date(parse_date_time(df$date, orders = c("mdy", "Ymd", "mdY", "dmy"))), na.rm = TRUE)
    formatted_date <- format(max_data_date, "%Y-%m-%d")
    
    if (current_lang() == "FR") {
      tags$div(
        style = "font-size: 11px; color: #7F8C8D; text-align: center; margin-top: 5px; font-style: italic;",
        span(paste(" Dernière mise à jour des données :", formatted_date))
      )
    } else {
      tags$div(
        style = "font-size: 11px; color: #7F8C8D; text-align: center; margin-top: 5px; font-style: italic;",
        span(paste(" Last Data Update:", formatted_date))
      )
    }
  })
  
  # Reset button action logic
  observeEvent(input$btn_reset, {
    req(raw_ebola_data())
    df <- raw_ebola_data()
    dates <- as.Date(parse_date_time(df$date, orders = c("mdy", "Ymd", "mdY", "dmy")))
    valid_dates <- dates[!is.na(dates)]
    
    updateDateRangeInput(session, "date_range", 
                         start = min(valid_dates, default = "1976-01-01"), 
                         end = max(valid_dates, default = "2027-12-31"))
    updateSelectizeInput(session, "countries", selected = character(0))
    updateSelectInput(session, "metric", selected = "confirmed_cases")
  })
  
  filtered_data <- reactive({
    req(raw_ebola_data())
    
    df <- raw_ebola_data() %>%
      mutate(
        safe_date = as.Date(parse_date_time(date, orders = c("mdy", "Ymd", "mdY", "dmy")))
      ) %>%
      filter(!is.na(safe_date))
    
    if (!is.null(input$date_range)) {
      df <- df %>% filter(safe_date >= as.Date(input$date_range[1]) & safe_date <= as.Date(input$date_range[2]))
    }
    
    if (!is.null(input$countries) && length(input$countries) > 0) {
      df <- df %>% filter(country %in% input$countries)
    }
    
    df <- df %>% mutate(date = safe_date) %>% select(-safe_date)
    return(df)
  })
  
  chosen_metric <- reactive({ input$metric })
  
  mod_dashboard_server("dashboard", filtered_data, chosen_metric, current_lang)
  mod_map_server("map", filtered_data, chosen_metric)
  mod_timeseries_server("trend", filtered_data, chosen_metric)
  mod_country_server("country", filtered_data)
  mod_download_server("download", filtered_data)
}

shinyApp(ui, server)