# #===============================================
# # mod_map.R
# # Ebola 2026 Project
# # This code is written by Koissi Savi
# #===============================================
# 
# 
# library(shiny)
# library(leaflet)
# library(sf)
# library(dplyr)
# library(rnaturalearth)
# 
# mod_map_ui <- function(id){
#   ns <- NS(id)
#   card(
#     full_screen = TRUE,
#     card_header("Global Outbreak Dispersal Footprint"),
#     leafletOutput(ns("map"), height = "750px")
#   )
# }
# 
# mod_map_server <- function(id, data, metric){
#   moduleServer(id, function(input, output, session){
#     
#     output$map <- renderLeaflet({
#       req(data(), metric())
#       
#       # Group the data by country to aggregate metrics safely
#       aggregated_data <- data() %>%
#         group_by(country) %>%
#         summarise(metric_total = sum(get(metric()), na.rm = TRUE), .groups = "drop")
#       
#       world <- ne_countries(scale = "medium", returnclass = "sf")
#       
#       map_df <- world %>%
#         left_join(aggregated_data, by = c("name" = "country")) %>%
#         filter(!is.na(metric_total)) # Focus view strictly on active regions
#       
#       if(nrow(map_df) == 0) {
#         return(leaflet() %>% addTiles() %>% addMiniMap())
#       }
#       
#       pal <- colorNumeric("YlOrRd", domain = map_df$metric_total, na.color = "transparent")
#       
#       leaflet(map_df) %>%
#         addProviderTiles(providers$CartoDB.Positron) %>%
#         addPolygons(
#           fillColor = ~pal(metric_total),
#           fillOpacity = 0.75,
#           weight = 1.5,
#           color = "#ffffff",
#           highlightOptions = highlightOptions(weight = 3, color = "#e74c3c", bringToFront = TRUE),
#           label = ~paste0("<strong>", name, "</strong><br/>Value: ", metric_total) %>% lapply(htmltools::HTML)
#         ) %>%
#         addLegend(pal = pal, values = ~metric_total, title = "Scale", position = "bottomright")
#     })
#   })
# }

#=============================================================================
# mod_map.R
# Ebola 2026 Project
# Enhanced Mapping Engine with Dynamic Multi-Metric Tooltips & Name Syncing
#=============================================================================

library(shiny)
library(leaflet)
library(sf)
library(dplyr)
library(rnaturalearth)
library(htmltools)

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
      
      # 1. Aggregate ALL metrics by country simultaneously to build rich summaries
      # Aggregate ALL metrics by country simultaneously to build rich summaries
      aggregated_data <- data() %>%
        group_by(country) %>%
        summarise(
          selected_metric_total = sum(get(metric()), na.rm = TRUE),
          confirmed_total       = sum(confirmed_cases, na.rm = TRUE),
          suspected_total       = sum(suspected_cases, na.rm = TRUE),
          deaths_total          = sum(deaths, na.rm = TRUE),
          recoveries_total      = sum(recoveries, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        # Normalize the country names to match rnaturalearth's "name" column value properties
        mutate(
          join_country = case_when(
            country %in% c("DR Congo", "DRC", "Congo-Kinshasa", "Democratic Republic of the Congo") ~ "Dem. Rep. Congo",
            country %in% c("Congo-Brazzaville", "Republic of Congo") ~ "Congo",
            TRUE ~ country
          )
        )
      
      # 2. Fetch spatial geometry map layers
      world <- ne_countries(scale = "medium", returnclass = "sf")
      
      # 3. Join spatial layers with the normalized dataset keys
      map_df <- world %>%
        left_join(aggregated_data, by = c("name" = "join_country")) %>%
        filter(!is.na(selected_metric_total)) # Focus strictly on active reporting countries
      
      # Fallback display engine if subsetting creates an empty collection
      if(nrow(map_df) == 0) {
        return(leaflet() %>% addProviderTiles(providers$CartoDB.Positron) %>% setView(0, 0, 2))
      }
      
      # 4. Generate dynamic color palette scales
      pal <- colorNumeric("YlOrRd", domain = map_df$selected_metric_total, na.color = "transparent")
      
      # 5. Build HTML popups dynamically containing full country dashboard snapshots
      popup_labels <- lapply(seq_len(nrow(map_df)), function(i) {
        HTML(sprintf(
          "<div style='font-family: Arial, sans-serif; padding: 5px; min-width: 180px;'>
            <h5 style='margin: 0 0 8px 0; color: #2C3E50; border-bottom: 2px solid #e74c3c; padding-bottom: 4px;'>🦠 %s</h5>
            <table style='width: 100%%; font-size: 12px; border-collapse: collapse;'>
              <tr><td style='padding: 2px 0; color: #7F8C8D;'>Confirmed:</td><td style='text-align: right; font-weight: bold; color: #e74c3c;'>%s</td></tr>
              <tr><td style='padding: 2px 0; color: #7F8C8D;'>Suspected:</td><td style='text-align: right; font-weight: bold; color: #f39c12;'>%s</td></tr>
              <tr><td style='padding: 2px 0; color: #7F8C8D;'>Fatalities:</td><td style='text-align: right; font-weight: bold; color: #2c3e50;'>%s</td></tr>
              <tr><td style='padding: 2px 0; color: #7F8C8D;'>Recoveries:</td><td style='text-align: right; font-weight: bold; color: #2ecc71;'>%s</td></tr>
            </table>
          </div>",
          map_df$country[i],
          format(map_df$confirmed_total[i], big.mark=","),
          format(map_df$suspected_total[i], big.mark=","),
          format(map_df$deaths_total[i], big.mark=","),
          format(map_df$recoveries_total[i], big.mark=",")
        ))
      })
      
      # Clean label metric label strings for primary hover text indicators
      metric_label_clean <- stringr::str_to_title(gsub("_", " ", metric()))
      
      # 6. Render Leaflet Canvas Layout
      leaflet(map_df) %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(
          fillColor = ~pal(selected_metric_total),
          fillOpacity = 0.75,
          weight = 1.5,
          color = "#ffffff",
          highlightOptions = highlightOptions(
            weight = 3, 
            color = "#e74c3c", 
            fillOpacity = 0.9,
            bringToFront = TRUE
          ),
          # Hover tooltip text shows basic contextual totals
          label = ~paste0("<strong>", country, "</strong><br/>", metric_label_clean, ": ", format(selected_metric_total, big.mark=",")) %>% lapply(HTML),
          # Clicking the country polygon launches the comprehensive HTML summary modal popover
          popup = popup_labels
        ) %>%
        addLegend(
          pal = pal, 
          values = ~selected_metric_total, 
          title = metric_label_clean, 
          position = "bottomright"
        )
    })
  })
}