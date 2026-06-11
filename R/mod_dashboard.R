#===============================================
# mod_dashboard.R
# Ebola 2026 Project
# This code is written by Koissi Savi
#===============================================

library(shiny)
library(bslib)
library(plotly)
library(dplyr)

mod_dashboard_ui <- function(id){
  ns <- NS(id)
  fluidPage(
    br(),
    # Top Row: Clean Summary Value Cards
    layout_column_wrap(
      width = 1/4,
      value_box(
        title = "Cumulative Confirmed",
        value = textOutput(ns("vbox_confirmed")),
        theme = "danger"
      ),
      value_box(
        title = "Reported Deaths",
        value = textOutput(ns("vbox_deaths")),
        theme = "dark"
      ),
      value_box(
        title = "Documented Recoveries",
        value = textOutput(ns("vbox_recoveries")),
        theme = "success"
      ),
      value_box(
        title = "Avg Case Fatality Rate",
        value = textOutput(ns("vbox_cfr")),
        theme = "info"
      )
    ),
    br(),
    # Bottom Row: Distribution Visualizations
    layout_column_wrap(
      width = 1/2,
      card(
        card_header("Case Breakdown Distribution"),
        plotlyOutput(ns("pie_chart"))
      ),
      card(
        card_header("Geographic Metric Frequency Spread"),
        plotlyOutput(ns("histogram"))
      )
    )
  )
}

mod_dashboard_server <- function(id, data, metric){
  moduleServer(id, function(input, output, session){
    
    output$vbox_confirmed  <- renderText({ sum(data()$confirmed_cases, na.rm=TRUE) })
    output$vbox_deaths     <- renderText({ sum(data()$deaths, na.rm=TRUE) })
    output$vbox_recoveries <- renderText({ sum(data()$recoveries, na.rm=TRUE) })
    output$vbox_cfr        <- renderText({
      denom <- sum(data()$confirmed_cases, na.rm=TRUE)
      if(denom == 0) return("0%")
      paste0(round((sum(data()$deaths, na.rm=TRUE) / denom) * 100, 1), "%")
    })
    
    # 1. Interactive Pie Chart
    output$pie_chart <- renderPlotly({
      req(data())
      summary_df <- data.frame(
        Category = c("Confirmed", "Suspected", "Deaths", "Recoveries"),
        Count = c(
          sum(data()$confirmed_cases, na.rm=TRUE),
          sum(data()$suspected_cases, na.rm=TRUE),
          sum(data()$deaths, na.rm=TRUE),
          sum(data()$recoveries, na.rm=TRUE)
        )
      )
      
      plot_ly(summary_df, labels = ~Category, values = ~Count, type = 'pie',
              textinfo = 'label+percent', insidetextorientation = 'radial',
              marker = list(colors = c('#e74c3c', '#f39c12', '#2c3e50', '#2ecc71'))) %>%  
        layout(margin = list(l=20, r=20, t=20, b=20))
    })
    
    # 2. Interactive Histogram
    output$histogram <- renderPlotly({
      req(data(), metric())
      
      plot_ly(data(), x = ~get(metric()), type = "histogram",
              marker = list(color = '#FFFFF0', line = list(color = '#black', width = 0.5))) %>%
        layout(
          xaxis = list(title = paste("Magnitude of", metric())),
          yaxis = list(title = "Observation Grid Count Matrix"),
          margin = list(l=40, r=20, t=20, b=40)
        )
    })
  })
}