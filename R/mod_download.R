#===============================================
# mod_download.R
# Ebola 2026 Project
# This code is written by Koissi Savi
#===============================================


library(shiny)

mod_download_ui <- function(id){
  
  ns <- NS(id)
  
  fluidPage(
    
    h3("Download Data"),
    
    downloadButton(
      ns("csv"),
      "Download CSV"
    )
  )
}

mod_download_server <- function(id,data){
  
  moduleServer(id,function(input,output,session){
    
    output$csv <- downloadHandler(
      
      filename = function(){
        
        paste0(
          "ebola_data_",
          Sys.Date(),
          ".csv"
        )
      },
      
      content = function(file){
        
        write.csv(
          data(),
          file,
          row.names = FALSE
        )
      }
    )
  })
}