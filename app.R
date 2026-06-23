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
    
    # Language Toggle & Reset Row
    div(
      style = "display: flex; gap: 10px; margin-bottom: 15px;",
      actionButton("btn_reset", "🔄 Reset", class = "btn-secondary btn-sm", style = "flex: 1;"),
      actionButton("btn_lang", "🌐 FR / EN", class = "btn-outline-primary btn-sm", style = "flex: 1;")
    ),
    
    dateRangeInput(
      "date_range",
      "Reporting Window",
      start = "1976-01-01",  
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
  
  # Reactive translation layer baseline
  current_lang <- reactiveVal("EN")
  
  observeEvent(input$btn_lang, {
    if (current_lang() == "EN") current_lang("FR") else current_lang("EN")
  })
  
  # Translate UI Elements dynamically based on selection state
  observe({
    lang <- current_lang()
    if (lang == "FR") {
      updateDateRangeInput(session, "date_range", label = "Fenêtre de rapport")
      updateSelectInput(session, "metric", label = "Matrice des métriques principales",
                        choices = c("Cas Confirmés" = "confirmed_cases", "Cas Suspects" = "suspected_cases", "Décès" = "deaths", "Guérisons" = "recoveries"))
    } else {
      updateDateRangeInput(session, "date_range", label = "Reporting Window")
      updateSelectInput(session, "metric", label = "Primary Metric Matrix",
                        choices = c("Confirmed Cases" = "confirmed_cases", "Suspected Cases" = "suspected_cases", "Fatalities" = "deaths", "Recoveries" = "recoveries"))
    }
  })
  
  # Reset button action logic
  observeEvent(input$btn_reset, {
    updateDateRangeInput(session, "date_range", start = "1976-01-01", end = "2027-12-31")
    updateSelectizeInput(session, "countries", selected = "")
    updateSelectInput(session, "metric", selected = "confirmed_cases")
  })
  
  raw_ebola_data <- load_ebola_data()
  
  observe({
    req(raw_ebola_data())
    available_countries <- unique(raw_ebola_data()$country)
    updateSelectizeInput(session, "countries", choices = available_countries, server = TRUE)
  })
  
  filtered_data <- reactive({
    req(raw_ebola_data())
    df <- raw_ebola_data()
    
    if (!is.null(input$date_range)) {
      # Make sure R safely treats the dataset date layout as Date vector objects
      df <- df %>% filter(as.Date(date) >= as.Date(input$date_range[1]) & as.Date(date) <= as.Date(input$date_range[2]))
    }
    
    if (!is.null(input$countries) && length(input$countries) > 0) {
      df <- df %>% filter(country %in% input$countries)
    }
    
    return(df)
  })
  
  chosen_metric <- reactive({ input$metric })
  
  # Pass language state down to individual rendering modules
  mod_dashboard_server("dashboard", filtered_data, chosen_metric, current_lang)
  mod_map_server("map", filtered_data, chosen_metric)
  mod_timeseries_server("trend", filtered_data, chosen_metric)
  mod_country_server("country", filtered_data)
  mod_download_server("download", filtered_data)
}

shinyApp(ui, server)