#' Update table (based on selector)
lcomp_dist_button_enable <- function(rv, input, output, session){
  
  # shinyLogger$entering() # Add function calling
  
  selected <- input$lcomp_dist_selector
  data <-  rv$synapses.data$df
  
  # Ghost calls (init)
  if ( is.null(selected) || length(selected) == 0) return()
  
  # Extract selected
  selected <- get_samples_from_selector(selected)
  
  # Create DF
  df <- merge(data, selected)
  
  # Check that we have at least 2 layers with 2 samples each
  if ( length(unique(df$layer)) < 2 ){
    createAlert(session, "lcomp_dist_alert", "lcomp_dist_warn", title = "More layers needed",
                content = "At least samples from two different layers are needed to do the comparison",
                style = "error", dismiss = F, append = F)
    updateButton(session, "lcomp_dist_run", disabled = T)
    return(NULL)
  }
  else{
      closeAlert(session,"lcomp_dist_warn")
      updateButton(session, "lcomp_dist_run", disabled = F)
      return(NULL)
  }
  #shinyLogger$exiting()
}

lcomp_dist_run_listener <- function(rv, input, output, session){
  
  # shinyLogger$entering() # Add function calling
  
  selected <- input$lcomp_dist_selector
  data <-  rv$synapses.data$df
  
  # Ghost calls (init)
  if ( is.null(selected) || length(selected) == 0) return()
  
  # Extract selected
  selected <- get_samples_from_selector(selected)
  
  # Create DF
  df <- merge(data, selected)

  # Now we can do the comparison with df
  # FIRST: compute density per sample
  pairs <- unique(df[,c("sample","samplenum","layer")])
  pairs <- split( pairs, row(pairs))
  
  dist.lapply <- lapply( pairs, function(x,data){
    subset <- data[ data$sample == x$sample & data$samplenum == as.numeric(x$samplenum), ]
    distances <- spatstat::nndist(createSampleSPP(subset$x, subset$y, subset$z,d = subset$feret))
    return(data.frame(dist = as.numeric(distances), layer = x$layer, stringsAsFactors = F))
  }, df )
  
  nndist <- Reduce(rbind.data.frame, dist.lapply) 
  
  diffs <- sigDiffTestLayer(as.numeric(nndist$dist), nndist$layer )
  stat <- "nearest synapse distance"
  
  # log
  output$lcomp_dist_log <- renderUI({create_dlcomp_log(diffs,stat)})
  # Plot 
  output$lcomp_dist_plot <- renderPlot({plotNearestNeighborLayerDist(diffs$values,diffs$groups)})
  
}