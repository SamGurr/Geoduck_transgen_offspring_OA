#rm(list = ls())
library(shiny)

ui <- shinyUI(fluidPage(
  mainPanel(
    numericInput("tank_volume", "tank_volume (L)", 250 ,min = 0, max = 5000),
    numericInput("how_many_tanks", "how_many_tanks of the same volume", 4,min = 1, max = 10),
    numericInput("flow_rate", "flow_rate (LPM)", 0.8,min = 0.00, max = 5.00),
    numericInput("target_cell_density", "target_cell_density (cells per mL)", 20000,min = 0, max = 1000000),
    numericInput("feed_conical", "target mL per hour continuous feed", 1000 ,min = 0, max = 1000),
    numericInput("algae_1", "Algae #1 cells mL-1 (pav, iso, tet, nano, etc.)", 3500000,min = 0, max = 2000000),
    numericInput("algae_2", "Algae #2 cells mL-1  (pav, iso, tet, nano, etc.)", 3500000,min = 0, max = 2000000),
    numericInput("algae_3", "Algae #3 cells mL-1  (pav, iso, tet, nano, etc.)", 3500000,min = 0, max = 2000000),
    textOutput("text_calc"))
))

server <- shinyServer(function(input, output,session){
  
  vals <- reactiveValues()
  observe({
    vals$algae_mix_conc      <- (input$algae_1 + input$algae_2 + input$algae_3)/3 # total cell concentration of mixed diet
    vals$x                   <- input$tank_volume*1000 # convert to cells per milliliter
    vals$t                   <- input$target_cell_density # in cells per milliliter
    vals$q                   <- input$how_many_tanks
    vals$f                   <- input$flow_rate*1000 # convert to milliliters
    vals$total_cells         <- vals$x*vals$t
    vals$vol                 <- input$feed_conical
    vals$assumed_hourly_loss <- (vals$f*60)*vals$t  # in cells per milliliter
    vals$percent_loss_hourly <- (vals$assumed_hourly_loss/vals$total_cells)*100 
    vals$Liters_algae_initial <- (vals$total_cells/vals$algae_mix_conc)/1000
    vals$LPH_feed <- ((vals$percent_loss_hourly/100)*vals$Liters_algae_initial)
    
  }) 
  
  output$text_calc <- renderText({
    
    paste("Initial batch feed per tank (L) =", 
          vals$Liters_algae_initial,
          "Hourly loss from flow through (cells mL) =",
          vals$assumed_hourly_loss,
          "Volume to replenish conical-1  hour-1 (L) =",
          (vals$assumed_hourly_loss/vals$algae_mix_conc)/1000, 
          "Daily volume:  MIXED ALGAE DIET (L) =", 
          ((vals$percent_loss_hourly/100)*vals$Liters_algae_initial)*24*vals$q,
          "Daily Volume: FILTERED SEAWATER (L) =", 
          ((1000-(((((vals$percent_loss_hourly/100)*vals$Liters_algae_initial)*24*vals$q)/24/vals$q)*input$feed_conical))*(vals$q)*(24))/1000,
          "How much algae (mL) conical-1 hour-1? =", 
          ((((vals$percent_loss_hourly/100)*vals$Liters_algae_initial)*24*vals$q)/24/vals$q)*1000)
    
  })
})


shinyApp(ui = ui, server = server)
