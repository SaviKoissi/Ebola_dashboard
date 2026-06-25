#=============================================================================
# mod_dashboard.R
# Ebola 2026 Project
# This code is written by Koissi Savi
# Production-Grade Dashboard Module - Recoveries Stripped & Trilingual Ready
#=============================================================================

library(shiny)
library(bslib)
library(plotly)
library(dplyr)

mod_dashboard_ui <- function(id){
  ns <- NS(id)
  fluidPage(
    br(),
    # Top Row: Clean Summary Value Cards with Dynamic Language Rendering
    uiOutput(ns("value_boxes_ui")),
    br(),
    # Bottom Row: Distribution Visualizations
    layout_column_wrap(
      width = 1/2,
      card(
        card_header(uiOutput(ns("header_pie"))),
        plotlyOutput(ns("pie_chart"))
      ),
      card(
        card_header(uiOutput(ns("header_hist"))),
        plotlyOutput(ns("histogram"))
      )
    )
  )
}

mod_dashboard_server <- function(id, data, metric, current_lang){
  moduleServer(id, function(input, output, session){
    
    # 1. Dynamically Render Value Boxes (Recoveries Removed, Re-balanced Grid)
    output$value_boxes_ui <- renderUI({
      req(data())
      lang <- current_lang()
      
      # Calculate metrics reactively
      conf_val <- format(sum(data()$confirmed_cases, na.rm=TRUE), big.mark=",")
      dead_val <- format(sum(data()$deaths, na.rm=TRUE), big.mark=",")
      
      denom <- sum(data()$confirmed_cases, na.rm=TRUE)
      cfr_val <- if(denom == 0) "0%" else paste0(round((sum(data()$deaths, na.rm=TRUE) / denom) * 100, 1), "%")
      
      # Three-stage language map vectors
      labels <- if(lang == "FR") {
        list(conf = "Cumul Confirmé", dead = "Décès Signalés", cfr = "Taux de Létalité Moyen")
      } else if(lang == "PT") {
        list(conf = "Casos Confirmados", dead = "Óbitos Registrados", cfr = "Taxa de Letalidade Média")
      } else {
        list(conf = "Cumulative Confirmed", dead = "Reported Deaths", cfr = "Avg Case Fatality Rate")
      }
      
      # Restructured grid split into 1/3 spaces across the dashboard panel row
      layout_column_wrap(
        width = 1/3,
        value_box(title = labels$conf, value = conf_val, theme = "danger"),
        value_box(title = labels$dead, value = dead_val, theme = "dark"),
        value_box(title = labels$cfr,  value = cfr_val,  theme = "info")
      )
    })
    
    # Dynamic Headers for plots
    output$header_pie <- renderText({
      if(current_lang() == "FR") {
        "Distribution de la Répartition des Cas"
      } else if(current_lang() == "PT") {
        "Distribuição Detalhada dos Casos"
      } else {
        "Case Breakdown Distribution"
      }
    })
    
    output$header_hist <- renderText({
      if(current_lang() == "FR") {
        "Étalement de la Fréquence Métrique Géographique"
      } else if(current_lang() == "PT") {
        "Distribuição da Frequência Métrica Geográfica"
      } else {
        "Geographic Metric Frequency Spread"
      }
    })
    
    # 2. Interactive Pie Chart (Recoveries Stripped)
    output$pie_chart <- renderPlotly({
      req(data())
      lang <- current_lang()
      
      categories <- if(lang == "FR") {
        c("Confirmés", "Suspects", "Décès")
      } else if(lang == "PT") {
        c("Confirmados", "Suspeitos", "Óbitos")
      } else {
        c("Confirmed", "Suspected", "Deaths")
      }
      
      summary_df <- data.frame(
        Category = categories,
        Count = c(
          sum(data()$confirmed_cases, na.rm=TRUE),
          sum(data()$suspected_cases, na.rm=TRUE),
          sum(data()$deaths, na.rm=TRUE)
        )
      )
      
      plot_ly(summary_df, labels = ~Category, values = ~Count, type = 'pie',
              textinfo = 'label+percent', insidetextorientation = 'radial',
              marker = list(colors = c('#e74c3c', '#f39c12', '#2c3e50'))) %>%  
        layout(margin = list(l=20, r=20, t=20, b=20))
    })
    
    # 3. Interactive Histogram
    output$histogram <- renderPlotly({
      req(data(), metric())
      lang <- current_lang()
      
      x_title <- if(lang == "FR") {
        paste("Magnitude de", metric())
      } else if(lang == "PT") {
        paste("Magnitude de", metric())
      } else {
        paste("Magnitude of", metric())
      }
      
      y_title <- if(lang == "FR") {
        "Matrice de Nombre de Grilles d'Observation"
      } else if(lang == "PT") {
        "Matriz de Contagem de Observações"
      } else {
        "Observation Grid Count Matrix"
      }
      
      plot_ly(data(), x = ~get(metric()), type = "histogram",
              marker = list(color = '#F4F6F7', line = list(color = '#2C3E50', width = 0.5))) %>%
        layout(
          xaxis = list(title = x_title),
          yaxis = list(title = y_title),
          margin = list(l=40, r=20, t=20, b=40)
        )
    })
  })
}