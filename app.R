#=============================================================================
# app.R
# Ebola 2026 Project - Robust Multi-Country Pipeline
# Production-Grade Theme Deployment, Trilingual Selector & Creator Attribution
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

# Base design palette boundaries leveraging bslib's native dark/light utility
app_theme <- bs_theme(
  version = 5,
  bootswatch = "lux",
  primary = "#e74c3c", 
  secondary = "#2c3e50",  
  bg = "#F8F9FA", 
  fg = "#2C3E50"
)

ui <- page_navbar(
  id = "nav_bar_tracker",
  title = div(
    div(
      style = "display: inline-flex; align-items: center; gap: 8px;",
      HTML('<svg width="24" height="24" viewBox="0 0 24 24" fill="none" stroke="#e74c3c" stroke-width="2.5" stroke-linecap="round" stroke-linejoin="round" style="transform: rotate(-15deg);"><path d="M2 17c3-1 6-4 5-8s-3-5 1-7 6 1 6 5-1 7 4 8 4-2 4-2"/></svg>'),
      span("Ebola Surveillance", style = "font-weight: bold;")
    ),
    div(
      "African Society for Biomathematics (ASB)", 
      style = "font-size: 11px; font-weight: normal; color: #7F8C8D; margin-top: -2px; padding-left: 32px;"
    )
  ),
  theme = app_theme, 
  
  tags$head(
    includeCSS("www/css/style.css"),
    includeScript("www/js/custom.js")
  ),
  
  sidebar = sidebar(
    title = "Surveillance Filters",
    
    div(
      style = "margin-bottom: 15px; display: flex; flex-direction: column; gap: 8px;",
      selectInput(
        "ui_lang", 
        "Language / Langue / Língua", 
        choices = c("English" = "EN", "Français" = "FR", "Português" = "PT"),
        selected = "EN",
        width = "100%"
      ),
      div(
        style = "display: flex; gap: 8px; align-items: center;",
        actionButton("btn_reset", "🔄 Reset", class = "btn-secondary btn-sm", style = "flex: 2;"),
        input_dark_mode(id = "dark_mode_toggle", mode = "light")
      )
    ),
    
    uiOutput("live_update_badge"),
    hr(style = "margin-top: 10px; margin-bottom: 15px;"),
    
    uiOutput("sidebar_date_ui"),
    uiOutput("sidebar_country_ui"),
    uiOutput("sidebar_metric_ui")
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
  ),
  
  #---------------------------------------------------------------------------
  # Dynamic Layout Footer Section (Attribution & Copyright Notice)
  #---------------------------------------------------------------------------
  footer = tags$footer(
    style = "border-top: 1px solid rgba(0,0,0,0.08); padding: 15px 20px; margin-top: 20px; font-size: 12px; color: #7F8C8D;",
    div(
      style = "display: flex; justify-content: space-between; align-items: center; flex-wrap: wrap; gap: 10px;",
      div(
        HTML(paste0("&copy; ", format(Sys.Date(), "%Y"), " <strong>African Society for Biomathematics</strong>. All rights reserved."))
      ),
      div(
        style = "text-align: right;",
        "Designed & Developed by ",
        tags$a(
          href = "https://scholar.google.com/citations?user=-BCB6_0AAAAJ&hl=en", 
          target = "_blank", 
          style = "color: #e74c3c; font-weight: bold; text-decoration: none; border-bottom: 1px dotted #e74c3c;",
          "Koissi Savi"
        )
      )
    )
  )
)

server <- function(input, output, session){
  
  current_lang <- reactive({ req(input$ui_lang); input$ui_lang })
  
  is_dark_mode <- reactive({
    req(input$dark_mode_toggle)
    input$dark_mode_toggle == "dark"
  })
  
  raw_ebola_data <- load_ebola_data()
  
  available_countries <- reactive({
    df <- raw_ebola_data()
    if (!is.null(df) && nrow(df) > 0) return(sort(unique(df$country)))
    return(character(0))
  })
  
  output$sidebar_date_ui <- renderUI({
    label_text <- switch(current_lang(), "FR" = "Fenêtre de rapport", "PT" = "Intervalo de Relatórios", "Reporting Window")
    dateRangeInput("date_range", label_text, start = "2014-01-01", end = "2027-12-31")
  })
  
  output$sidebar_country_ui <- renderUI({
    label_text <- switch(current_lang(), "FR" = "Pays Cibles (Tous si vide)", "PT" = "Países Alvo (Todos se em branco)", "Target Countries (All if Blank)")
    placeholder_text <- switch(current_lang(), "FR" = "Choisir un pays...", "PT" = "Selecionar País...", "Select Country...")
    selectizeInput("countries", label_text, choices = available_countries(), multiple = TRUE, options = list(placeholder = placeholder_text))
  })
  
  output$sidebar_metric_ui <- renderUI({
    label_text <- switch(current_lang(), "FR" = "Matrice des métriques principales", "PT" = "Matriz de Métricas Primárias", "Primary Metric Matrix")
    
    choices_vec <- switch(current_lang(),
                          "FR" = c("Cas Confirmés" = "confirmed_cases", "Cas Suspects" = "suspected_cases", "Décès" = "deaths"),
                          "PT" = c("Casos Confirmados" = "confirmed_cases", "Casos Suspeitos" = "suspected_cases", "Óbitos" = "deaths"),
                          c("Confirmed Cases" = "confirmed_cases", "Suspected Cases" = "suspected_cases", "Fatalities" = "deaths")
    )
    selectInput("metric", label_text, choices = choices_vec, selected = "confirmed_cases")
  })
  
  output$live_update_badge <- renderUI({
    req(raw_ebola_data())
    df <- raw_ebola_data()
    max_data_date <- max(as.Date(parse_date_time(df$date, orders = c("mdy", "Ymd", "mdY", "dmy"))), na.rm = TRUE)
    formatted_date <- format(max_data_date, "%Y-%m-%d")
    
    label_text <- switch(current_lang(),
                         "FR" = paste(" Dernière mise à jour des données :", formatted_date),
                         "PT" = paste(" Última atualização de dados:", formatted_date),
                         paste(" Last Data Update:", formatted_date)
    )
    tags$div(style = "font-size: 11px; color: #7F8C8D; text-align: center; margin-top: 5px; font-style: italic;", span(label_text))
  })
  
  observeEvent(input$btn_reset, {
    req(raw_ebola_data())
    df <- raw_ebola_data()
    dates <- as.Date(parse_date_time(df$date, orders = c("mdy", "Ymd", "mdY", "dmy")))
    valid_dates <- dates[!is.na(dates)]
    updateDateRangeInput(session, "date_range", start = min(valid_dates, default = "1976-01-01"), end = max(valid_dates, default = "2027-12-31"))
    updateSelectizeInput(session, "countries", selected = character(0))
    updateSelectInput(session, "metric", selected = "confirmed_cases")
  })
  
  filtered_data <- reactive({
    req(raw_ebola_data())
    df <- raw_ebola_data() %>%
      mutate(safe_date = as.Date(parse_date_time(date, orders = c("mdy", "Ymd", "mdY", "dmy")))) %>%
      filter(!is.na(safe_date))
    if (!is.null(input$date_range)) df <- df %>% filter(safe_date >= as.Date(input$date_range[1]) & safe_date <= as.Date(input$date_range[2]))
    if (!is.null(input$countries) && length(input$countries) > 0) df <- df %>% filter(country %in% input$countries)
    df %>% mutate(date = safe_date) %>% select(-safe_date)
  })
  
  chosen_metric <- reactive({ req(input$metric); input$metric })
  
  mod_dashboard_server("dashboard", filtered_data, chosen_metric, current_lang)
  mod_map_server("map", filtered_data, chosen_metric, is_dark_mode)
  mod_timeseries_server("trend", filtered_data, chosen_metric)
  mod_country_server("country", filtered_data)
  mod_download_server("download", filtered_data)
}

shinyApp(ui, server)