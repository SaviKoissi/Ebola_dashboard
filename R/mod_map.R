#===============================================
# mod_map.R
# Ebola 2026 Project
# This code is written by Koissi Savi
#===============================================


library(shiny)
library(leaflet)
library(sf)
library(dplyr)
library(rnaturalearth)

mod_map_ui <- function(id){
  ns <- NS(id)
  card(
    full_screen = TRUE,
    card_header("Global Outbreak Dispersal Footprint"),
    leafletOutput(ns("map"), height = "750px")
  )
}

mod_map_server <- function(id, data, metric){
  moduleServer(id, function(input, output, session){
    
    output$map <- renderLeaflet({
      req(data(), metric())
      
      # Group the data by country to aggregate metrics safely
      aggregated_data <- data() %>%
        group_by(country) %>%
        summarise(metric_total = sum(get(metric()), na.rm = TRUE), .groups = "drop")
      
      world <- ne_countries(scale = "medium", returnclass = "sf")
      
      map_df <- world %>%
        left_join(aggregated_data, by = c("name" = "country")) %>%
        filter(!is.na(metric_total)) # Focus view strictly on active regions
      
      if(nrow(map_df) == 0) {
        return(leaflet() %>% addTiles() %>% addMiniMap())
      }
      
      pal <- colorNumeric("YlOrRd", domain = map_df$metric_total, na.color = "transparent")
      
      leaflet(map_df) %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(
          fillColor = ~pal(metric_total),
          fillOpacity = 0.75,
          weight = 1.5,
          color = "#ffffff",
          highlightOptions = highlightOptions(weight = 3, color = "#e74c3c", bringToFront = TRUE),
          label = ~paste0("<strong>", name, "</strong><br/>Value: ", metric_total) %>% lapply(htmltools::HTML)
        ) %>%
        addLegend(pal = pal, values = ~metric_total, title = "Scale", position = "bottomright")
    })
  })
}