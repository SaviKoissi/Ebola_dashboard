#===============================================
# mod_timeseries.R
#===============================================

library(shiny)
library(echarts4r)
library(dplyr)

mod_timeseries_ui <- function(id){
  ns <- NS(id)
  fluidPage(
    br(),
    bslib::card(
      bslib::card_header(
        class = "bg-dark text-white d-flex justify-content-between align-items-center",
        div("Epidemiological Curve & Aggregated Trajectories"),
        uiOutput(ns("predict_btn_container"))
      ),
      echarts4rOutput(ns("trend"), height = "600px")
    )
  )
}

mod_timeseries_server <- function(id, data, metric, current_lang) {
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # Track projection toggle internally within the module instance
    is_predicting <- reactiveVal(FALSE)
    
    output$predict_btn_container <- renderUI({
      lang <- current_lang()
      btn_label <- switch(lang, "FR" = "🔮 Projections Lineaires", "PT" = "🔮 Executar Projeção", "🔮 Run Projections")
      info_label <- "ℹ"
      
      div(
        class = "d-flex align-items-center gap-2",
        actionButton(ns("toggle_predict"), btn_label, class = "btn-danger btn-sm"),
        actionButton(ns("show_help"), info_label, class = "btn-outline-light btn-sm", style = "border-radius: 50%; width: 28px; height: 28px; padding: 0;")
      )
    })
    
    observeEvent(input$toggle_predict, {
      is_predicting(!is_predicting())
      lang <- current_lang()
      # Dynamic visual feedback on active toggle state
      if(is_predicting()) {
        updateActionButton(session, "toggle_predict", label = switch(lang, "FR" = "📉 Masquer Projections", "PT" = "📉 Ocultar Projeção", "📉 Hide Projections"))
      } else {
        updateActionButton(session, "toggle_predict", label = switch(lang, "FR" = "🔮 Projections Lineaires", "PT" = "🔮 Executar Projeção", "🔮 Run Projections"))
      }
    })
    
    # Model Helper and Disclaimer Modal
    observeEvent(input$show_help, {
      lang <- current_lang()
      
      modal_title <- switch(lang, "FR" = "Spécifications du Modèle Prédictif", "PT" = "Especificações do Modelo Preditivo", "Predictive Model Specifications")
      
      modal_body <- switch(lang,
                           "FR" = div(
                             h6("Architecture du modèle :"),
                             p("Cet outil utilise un modèle autoregressif de tendance à nœud unique (Simulated Feed-Forward Neural Layer). Il capture la vélocité et l'accélération récentes des données sélectionnées pour projeter la trajectoire de l'épidémie sur les 12 prochains mois."),
                             hr(),
                             tags$b("AVERTISSEMENT CLAIR ET CLAUSE DE NON-RESPONSABILITÉ :"),
                             p("Ces projections sont générées à des fins purement indicatives et académiques. Elles ne prennent pas en compte les interventions de santé publique, les dynamiques de transmission complexes (modèles SIR/SEIR) ou les contraintes écologiques. Ne pas utiliser pour la prise de décision clinique ou stratégique réelle.")
                           ),
                           "PT" = div(
                             h6("Arquitetura do Modelo:"),
                             p("Esta ferramenta utiliza um modelo autorregressivo de tendência de nó único estrutural (Simulated Feed-Forward Neural Layer). Ele captura a velocidade e aceleração recentes dos dados para projetar uma trajetória dos próximos 12 meses."),
                             hr(),
                             tags$b("AVISO LEGAL E ISENÇÃO DE RESPONSABILIDADE:"),
                             p("Estas projeções são geradas apenas para fins indicativos e acadêmicos. Não consideram intervenções de saúde pública em tempo real ou dinâmicas biológicas complexas de transmissão. Não devem ser usadas para tomadas de decisão estratégica ou clínica.")
                           ),
                           div(
                             h6("Model Architecture:"),
                             p("This feature utilizes a structural single-node autoregressive trend projection network (Simulated Feed-Forward Neural Layer). It isolates the localized speed, slope, and momentum parameters of the currently filtered dataset to estimate trajectory coordinates across a 12-month future horizon."),
                             hr(),
                             tags$b("CRITICAL DISCLAIMER & NOTICE:"),
                             p("These calculations are generated strictly for academic demonstration and simulation profiling. They do not account for active non-linear field interventions, biological host constraints, or complex mechanistic compartmental parameters (e.g., SEIR variations). They must not be utilized as definitive clinical guidance or policy deployment frameworks.")
                           )
      )
      
      showModal(modalDialog(
        title = modal_title,
        modal_body,
        easyClose = TRUE,
        footer = modalButton(switch(lang, "FR" = "Fermer", "PT" = "Fechar", "Close"))
      ))
    })
    
    output$trend <- renderEcharts4r({
      req(data(), metric())
      active_metric_str <- metric()
      
      # Prepare historical series
      timeline_df <- data() %>%
        group_by(date) %>%
        summarise(display_val = sum(as.numeric(.data[[active_metric_str]]), na.rm = TRUE), .groups = "drop") %>%
        arrange(date) %>%
        mutate(type = "Historical")
      
      shiny::validate(shiny::need(nrow(timeline_df) > 0, "No case data available within selected filter bounds."))
      
      clean_label <- gsub("_", " ", toupper(active_metric_str))
      
      # Construct Chart base
      if (!is_predicting()) {
        timeline_df |>
          e_charts(date) |>
          e_line(display_val, name = paste(clean_label, "(Observed)"), symbol = "circle", symbolSize = 6, smooth = TRUE, itemStyle = list(color = "#e74c3c")) |>
          e_area(display_val, opacity = 0.2, color = "#e74c3c") |>
          e_theme("minimal") |> e_tooltip(trigger = "axis") |> e_datazoom(type = "slider", color = "#2c3e50") |> e_legend(show = TRUE, bottom = 0)
      } else {
        # Combine Historical with generated Neural Projections
        source("R/prediction_engine.R", local = TRUE)
        pred_df <- generate_surveillance_predictions(data(), active_metric_str)
        
        # Merge datasets into sequential time streams
        combined_df <- bind_rows(timeline_df, pred_df)
        
        combined_df |>
          group_by(type) |>
          e_charts(date) |>
          e_line(display_val, symbol = "circle", symbolSize = 6, smooth = TRUE) |>
          e_theme("minimal") |> 
          e_tooltip(trigger = "axis") |> 
          e_datazoom(type = "slider", color = "#2c3e50") |> 
          e_legend(show = TRUE, bottom = 0) |>
          e_color(c("#e74c3c", "#3498db")) # Red for historical, Blue for future projections
      }
    })
  })
}