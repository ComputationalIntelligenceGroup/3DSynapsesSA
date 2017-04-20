#' Create reactive values storage for synapses SA
#'
synapsesSA_setRV <- function(){
  
  # Global (NON REACTIVE) element
  rv <- list()
  
  # General sample data
  rv$samples.data <- reactiveValues()
  
  # data
  rv$synapses.data <- reactiveValues()
  
  # models
  rv$models <- reactiveValues()
  rv$currentModel <- reactiveValues()
  
  # simulations
  rv$simulations <- reactiveValues()
  rv$simulations$data <- list()
  rv$simulations$df <- data.frame(
    "volume" = numeric(0),
    "model" = character(0),
    "lambda" = numeric(0),
    "ml" = numeric(0),
    "sdl" = numeric(0),
    stringsAsFactors = F
  )
  
  # Simulation data
  rv$sim.data <- reactiveValues()
  
  # Initialize samples.data
  rv$samples.data$df <- data.frame(
    "sample" = character(0),
    "samplenum" = character(0),
    "filename" = character(0),
    "extension" = character(0),
    stringsAsFactors = F
  )
  
  ## Initialize synapses.data
  rv$synapses.data$df <- data.frame(
    "sample" = character(0),
    "samplenum" = numeric(0),
    "layer" = character(0),
    "type" = character(0),
    "x" = numeric(0),
    "y" = numeric(0),
    "z" = numeric(0),
    "feret" = numeric(0),
    stringsAsFactors = F
  )
  rv$synapses.data$layers <- c()
  rv$synapses.data$nsamples <- 0
  rv$synapses.data$npoints <- 0
  rv$synapses.data$avgVolume <- 0

  return(rv)  
}