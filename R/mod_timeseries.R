#===============================================
# mod_timeseries.R
# Ebola 2026 Project
# This code is written by Koissi Savi
#===============================================

library(shiny)
library(echarts4r)
library(dplyr)

mod_timeseries_ui <- function(id){
  ns <- NS(id)
  fluidPage(
    br(),
    bslib::card(
      bslib::card_header(class = "bg-dark text-white", "Epidemiological Curve & Aggregated Trajectories"),
      echarts4rOutput(ns("trend"), height = "600px")
    )
  )
}

mod_timeseries_server <- function(id, data, metric){
  moduleServer(id, function(input, output, session){
    
    output$trend <- renderEcharts4r({
      req(data(), metric())
      
      active_metric_str <- metric()
      
      # Grouping timelines based on criteria filters
      timeline_df <- data() %>%
        group_by(date) %>%
        summarise(
          display_val = sum(as.numeric(.data[[active_metric_str]]), na.rm = TRUE), 
          .groups = "drop"
        ) %>%
        arrange(date)
      
      # FIX 1: Enforce explicit shiny package namespace resolution 
      # This prevents echarts4r from intercepting the validation call
      shiny::validate(
        shiny::need(nrow(timeline_df) > 0, "No case data available within selected filter bounds.")
      )
      
      # Formatting clean text display flags for the series header
      clean_label <- gsub("_", " ", toupper(active_metric_str))
      
      timeline_df |>
        e_charts(date) |>
        e_line_(
          "display_val", 
          name = clean_label, 
          symbol = "circle", 
          symbolSize = 6,
          smooth = TRUE,
          itemStyle = list(color = "#e74c3c")
        ) |>
        e_area_(
          "display_val", 
          opacity = 0.2, 
          color = "#e74c3c"
        ) |>
        e_theme("minimal") |> 
        e_tooltip(trigger = "axis") |>
        e_datazoom(type = "slider", color = "#2c3e50") |>
        e_legend(show = TRUE, bottom = 0)
    })
  })
}