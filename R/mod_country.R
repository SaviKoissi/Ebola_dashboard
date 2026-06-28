#===============================================
# mod_country.R
# Ebola 2026 Project
# This code is written by Koissi Savi
#===============================================

library(shiny)
library(DT)

mod_country_ui <- function(id){
  ns <- NS(id)
  fluidPage(
    selectInput(
      ns("country"),
      "Country",
      NULL
    ),
    DTOutput(
      ns("table")
    )
  )
}

mod_country_server <- function(id, data){
  moduleServer(id, function(input, output, session){
    observe({
      updateSelectInput(
        session,
        "country",
        choices = unique(data()$country)
      )
    })
    
    output$table <- renderDT({
      req(input$country)
      data() |>
        dplyr::filter(
          country == input$country
        )
    })
  })
}