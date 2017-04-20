update_sim_new_modelsel <- function(rv, input, output, session){
  
  models <- names(rv$models)
  updateSelectInput(session,"sim_new_model",choices = models)
  
}

listener_sim_new_run <- function(rv, input, output, session){
  
  # Get params
  nsims <- input$sim_new_nsims
  volume <- input$sim_new_volume
  manual <- input$sim_new_manual
  if (manual == 'Manual') {
    model <- NA
    lambda <- input$sim_new_lambda *1E-9
    ml <- input$sim_new_ml
    sdl <- input$sim_new_sdl
  }
  else{
    model <- input$sim_new_model
    if (is.null(model)) return(NULL)
    lambda <- rv$models[[model]]$lambda
    ml <- rv$models[[model]]$feret$ml
    sdl <- rv$models[[model]]$feret$sdl
  }
  if ( is.null(lambda) || is.null(ml)  || is.null(sdl) ) return(NULL)
  
  # Run simulations
  currSims <- nrow(rv$simulations$df)
  sims <- nRSASimulations(nsims, lambda, volume * 1E9, mu = ml, sigma = sdl )
  
  # Archive Sims
  lapply(sims, function(x){
    rv$simulations$data[[ nrow(rv$simulations$df) + 1 ]] <- x
    rv$simulations$df[ nrow(rv$simulations$df) + 1 , ] <- list( volume = volume,
                                                          model = model,
                                                          lambda = lambda,
                                                          ml = ml,
                                                          sdl = sdl)
  })
  
  # Log
  output$sim_new_log <- renderUI({
    tagList(
      tags$p( sprintf(" %d simulations created and saved successfully. Creations tagged from %d to %d", nsims, currSims+1 , currSims+nsims)),
      tags$p( sprintf(" You can go to Dashboard - 3d Preview tab to display created simulations or to Data loading - view tables
                      to see raw data tables.")),
      tags$p( sprintf(" Simulations can be also downloaded in CSV format from Simulation main tab."))
    )
  })
}