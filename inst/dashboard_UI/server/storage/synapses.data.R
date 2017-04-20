#' Add new synapses data to current and recompute auxiliar quantities
#'
synapsesSA_addSynapses <- function(rv, toAdd){
  
  # shinyLogger$entering()
  
  # Rbind ( Match colnames )
  rv$synapses.data$df <- rbind(rv$synapses.data$df, toAdd[, colnames(rv$synapses.data$df ) ] )
  
  # Recompute
  synapsesSA_update_syndatasum(rv)
  
  #shinyLogger$exiting()
}

# Update summary stats
synapsesSA_update_syndatasum <- function(rv){
  
  # shinyLogger$entering()
  
  rv$synapses.data$layers <- unique(rv$synapses.data$df$layer)
  rv$synapses.data$nsamples <- nrow(rv$samples.data$df)
  rv$synapses.data$npoints <- nrow(rv$synapses.data$df)
  
  #shinyLogger$exiting()
}