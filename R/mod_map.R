#=============================================================================
# mod_map.R
# Ebola 2026 Project
# Harmonized Mapping Engine with Filovirus Elements & Adaptive Canvas Basemaps
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

mod_map_server <- function(id, data, metric, is_dark){
  moduleServer(id, function(input, output, session){
    
    output$map <- renderLeaflet({
      req(data(), metric())
      
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
        mutate(
          join_country = case_when(
            country %in% c("DR Congo", "DRC", "Congo-Kinshasa", "Democratic Republic of the Congo") ~ "Dem. Rep. Congo",
            country %in% c("Congo-Brazzaville", "Republic of Congo") ~ "Congo",
            TRUE ~ country
          )
        )
      
      world <- ne_countries(scale = "medium", returnclass = "sf")
      
      map_df <- world %>%
        left_join(aggregated_data, by = c("name" = "join_country")) %>%
        filter(!is.na(selected_metric_total))
      
      tile_provider <- if(is_dark()) providers$CartoDB.DarkMatter else providers$CartoDB.Positron
      
      if(nrow(map_df) == 0) {
        return(leaflet() %>% addProviderTiles(tile_provider) %>% setView(0, 0, 2))
      }
      
      pal <- colorNumeric("YlOrRd", domain = map_df$selected_metric_total, na.color = "transparent")
      
      popup_labels <- lapply(seq_len(nrow(map_df)), function(i) {
        HTML(sprintf(
          "<div style='font-family: Arial, sans-serif; padding: 5px; min-width: 180px;'>
            <h5 style='margin: 0 0 8px 0; color: #2C3E50; border-bottom: 2px solid #e74c3c; padding-bottom: 4px; display: flex; align-items: center; gap: 6px;'>
              <svg width='18' height='18' viewBox='0 0 24 24' fill='none' stroke='#e74c3c' stroke-width='2.5' stroke-linecap='round' stroke-linejoin='round' style='transform: rotate(-15deg);'><path d='M2 17c3-1 6-4 5-8s-3-5 1-7 6 1 6 5-1 7 4 8 4-2 4-2'/></svg>
              %s
            </h5>
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
      
      metric_label_clean <- stringr::str_to_title(gsub("_", " ", metric()))
      
      leaflet(map_df) %>%
        addProviderTiles(tile_provider) %>%
        addPolygons(
          fillColor = ~pal(selected_metric_total),
          fillOpacity = 0.75,
          weight = 1.5,
          color = if(is_dark()) "#333333" else "#ffffff",
          highlightOptions = highlightOptions(
            weight = 3, color = "#e74c3c", fillOpacity = 0.9, bringToFront = TRUE
          ),
          label = ~paste0("<strong>", country, "</strong><br/>", metric_label_clean, ": ", format(selected_metric_total, big.mark=",")) %>% lapply(HTML),
          popup = popup_labels
        ) %>%
        addLegend(
          pal = pal, values = ~selected_metric_total, title = metric_label_clean, position = "bottomright"
        )
    })
  })
}