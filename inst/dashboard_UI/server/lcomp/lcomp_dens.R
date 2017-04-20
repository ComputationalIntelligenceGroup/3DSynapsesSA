#' Update lcomp_dens and dist selector
update_lcomp_selector <- function(rv, input, output, session){
  
  # shinyLogger$entering() # Add function calling
  
  data <- rv$synapses.data$df # Get data + dependency
  
  # Ghost calls (init)
  if ( is.null(data) || nrow(data) == 0) return()
  
  # TODO: SIMULATION
  boxdata <- unique(data[,c("sample","layer","samplenum")])
  str <- create_hcbox_selector(boxdata, bySample = F)
  updateHCBox(session, "lcomp_dens_selector", structure = str )
  updateHCBox(session, "lcomp_dist_selector", structure = str )
  
  #shinyLogger$exiting()
}

#' Update table (based on selector)
lcomp_dens_button_enable <- function(rv, input, output, session){
  
  # shinyLogger$entering() # Add function calling
  
  selected <- input$lcomp_dens_selector
  data <-  rv$synapses.data$df
  
  # Ghost calls (init)
  if ( is.null(selected) || length(selected) == 0) return()
  
  # Extract selected
  selected <- get_samples_from_selector(selected)
  
  # Create DF
  df <- merge(data, selected)
  
  # Check that we have at least 2 layers with 2 samples each
  if ( length(unique(df$layer)) < 2 ){
    createAlert(session, "lcomp_dens_alert", "lcomp_dens_warn", title = "More layers needed",
                content = "At least samples from two different layers are needed to do the comparison",
                style = "error", dismiss = F, append = F)
    updateButton(session, "lcomp_dens_run", disabled = T)
    return(NULL)
  }
  else{
    upairs <- unique(df[,c("sample","samplenum","layer")])
                     
    # Check that we have at least two samples per layer
    samplesPerLayer <- aggregate( sample ~ layer, upairs, length)
    # Number of layer with more than 1 sample
    if ( sum(samplesPerLayer[,2]>1) <  2 ){
      createAlert(session, "lcomp_dens_alert", "lcomp_dens_warn", title = "More samples needed",
                  content = "At least samples from two different layers are needed to do the comparison",
                  style = "error", dismiss = F, append = F)
      updateButton(session, "lcomp_dens_run", disabled = T)
      return(NULL)
    }
    else{
      # Warn if we are going to exclude some layer
      notSelected <- samplesPerLayer[samplesPerLayer[,2] <= 1, 1]
      if(length(notSelected) > 0)
          createAlert(session, "lcomp_dens_alert", "lcomp_dens_warn", title = "Some layers will be ignored",
                  content = "Some layers do not have enough samples to be compared. They will be ignored.",
                  style = "warning", dismiss = F, append = F)
      else
        closeAlert(session,"lcomp_dens_warn")
      
      updateButton(session, "lcomp_dens_run", disabled = F)
      return(NULL)
    }
  }
  #shinyLogger$exiting()
}

lcomp_dens_run_listener <- function(rv, input, output, session){
  
  # shinyLogger$entering() # Add function calling
  
  selected <- input$lcomp_dens_selector
  data <-  rv$synapses.data$df
  
  # Ghost calls (init)
  if ( is.null(selected) || length(selected) == 0) return()
  
  # Extract selected
  selected <- get_samples_from_selector(selected)
  
  # Create DF
  df <- merge(data, selected)
  
  upairs <- unique(df[,c("sample","samplenum","layer")])
  
  # Check that we have at least two samples per layer and select them
  samplesPerLayer <- aggregate( sample ~ layer, upairs, length)
  selected <- samplesPerLayer[samplesPerLayer[,2] > 1, 1]
  notSelected <- samplesPerLayer[samplesPerLayer[,2] <= 1, 1]
  df <- df[df$layer %in% selected, ]
  
  # Now we can do the comparison with df
  
  # FIRST: compute density per sample
  pairs <- unique(df[,c("sample","samplenum","layer")])

  intensities <- t(apply(pairs,1,function(x,data){
    subset <- data[ data$sample == x[1] & data$samplenum == as.numeric(x[2]), ]
    return(c(layer = unname(x[3]), lambda = sampleIntensity(subset$x, subset$y, subset$z )))
  }, df))
  
  diffs <- sigDiffTestLayer(as.numeric(intensities[,"lambda"])*1E9, intensities[,"layer"] )
  stat <- "synaptic density"
  
  # log
  output$lcomp_dens_log <- renderUI({create_dlcomp_log(diffs,stat)})
  # Plot 
  output$lcomp_dens_plot <- renderPlot({plotDensityLayer(diffs$values,diffs$groups)})
  
}