#===========================================================
# app.R
# Ebola 2026 Project - Robust Multi-Country Pipeline
#===========================================================

library(shiny)
library(bslib)
library(dplyr)

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
    dateRangeInput(
      "date_range",
      "Reporting Window",
      start = "1976-01-01",  # Matches your glimpse data timeline
      end   = "2027-12-31"
    ),
    selectizeInput(
      "countries",
      "Target Countries (All if Blank)",
      choices = NULL,        # Filled dynamically by server
      multiple = TRUE,       # ALLOWS MULTIPLE SELECTIONS
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
  nav_panel("Epidemiological Trends", mod_timeseries_ui("trend")),
  nav_panel("Country Profiler", mod_country_ui("country")),
  nav_panel("Data Hub", mod_download_ui("download")),
  
  # FIX: Pushes all tabs to the left and cleanly hosts the logo on the far right of the same line
  nav_spacer(),
  nav_item(
    tags$img(
      src = "ASB.png", # Fixed 'scr' typo to 'src'
      style = "height: 38px; width: auto; object-fit: contain; padding-bottom: 2px;"
    )
  )
)

server <- function(input, output, session){
  
  # 1. Ingest base data stream cleanly
  raw_ebola_data <- load_ebola_data()
  
  # 2. Dynamically populate the selectize list from data categories
  observe({
    req(raw_ebola_data())
    available_countries <- unique(raw_ebola_data()$country)
    updateSelectizeInput(session, "countries", choices = available_countries, server = TRUE)
  })
  
  # 3. FIX: Non-blocking central filter engine
  filtered_data <- reactive({
    req(raw_ebola_data())
    df <- raw_ebola_data()
    
    # Filter by dates safely
    if (!is.null(input$date_range)) {
      df <- df %>% filter(date >= input$date_range[1] & date <= input$date_range[2])
    }
    
    # FIX: If countries are selected, filter by the vector. If empty, keep all data.
    if (!is.null(input$countries) && length(input$countries) > 0) {
      df <- df %>% filter(country %in% input$countries)
    }
    
    return(df)
  })
  
  # Track selected metric string
  chosen_metric <- reactive({ input$metric })
  
  # Cascade clean reactive modules forward
  mod_dashboard_server("dashboard", filtered_data, chosen_metric)
  mod_map_server("map", filtered_data, chosen_metric)
  mod_timeseries_server("trend", filtered_data, chosen_metric)
  mod_country_server("country", filtered_data)
  mod_download_server("download", filtered_data)
}

shinyApp(ui, server)