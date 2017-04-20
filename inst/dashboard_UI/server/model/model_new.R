#' Updates page height when tab is called
model_new_update_pageHeight <- function(rv, input, output, session){
  # shinyLogger$entering() # Add function calling
  
  if (!is.null(input$sb_menu) && input$sb_menu == "modelling_new")
    multiStepPageUpdateHeight(session, "mod_new_msp" )
  
  #shinyLogger$exiting()
  
}

#' Button tracker (updater)
model_new_nextbutton <- function(rv, input, output, session){
  
  # shinyLogger$entering() # Add function calling
  
  data <- rv$synapses.data$df # Get data + dependency
  
  # Ghost calls (init)
  if ( is.null(data) || nrow(data) == 0) {
    createAlert(session, "mod_new_alert", "mod_new_no_data", 
                title = "No data",style = "error",dismiss = T, append = F,
                content = "Please, load at least 1 sample to build a model" )
    updateButton(session, "mod_new_next", label = "Next step" , disabled = T )
    return();
  }
  else{
    closeAlert(session, "mod_new_no_data")
  }
  
  # Calld by "observeEvent"
  current <- input$mod_new_msp
  label <- "Next step"
  if ( current == 1 ) updateButton(session, "mod_new_next", label = "Next step" , disabled = F )
  ## SPECIAL: LAst one is save and reset
  else if (current == 6) {
    
    # Save
    name <- input$mod_new_name
    
    # Return to first
    updateTextInput(session, "mod_new_name", value = "" )
    changeStep(session, "mod_new_msp", 1 )
    updateButton(session, "mod_new_next", label = "Next step" , disabled = F )
    
    # Reset outputs
    output$mod_new_feretplot <- renderPlot({NULL})
    output$mod_new_feretlog <- renderUI({NULL})
    output$mod_new_rsaplot  <- renderPlot({NULL})
    output$mod_new_rsalog <- renderUI({NULL})
    output$mod_new_fgklplot  <- renderPlot({NULL})
    
    # Erase data selection
    isolate(update_mod_new_selector(rv,input,output,session))
    
    # Save
    name <- input$mod_new_name
    rv$models[[name]] <- rv$currentModel
    rv$currentModel <- NULL
    
    # Move to view
    updateTabItems(session, "sb_menu",selected = "modelling_view")
    updateSelectInput(session,"mod_view_sel", selected = name)
    closeAlert(session,"mod_new_name_dup")
    
    #shinyLogger$exiting()
    return(NULL)
  }
  
  # Get next label
  else if (current == 5) label <- "Save and go to view"
  
  # Change page
  changeStep(session, "mod_new_msp", current + 1 )
  
  # Update button
  updateButton(session, "mod_new_next", label = label , disabled = T )
  
  #shinyLogger$exiting()
}

################## SELECT DATA STEP ########################

#' Update mod_new selector
update_mod_new_selector <- function(rv, input, output, session){
  
  # shinyLogger$entering() # Add function calling
  
  data <- rv$synapses.data$df # Get data + dependency
  
  # Ghost calls (init)
  if ( is.null(data) || nrow(data) == 0) return()
  
  boxdata <- unique(data[,c("sample","layer","samplenum")])
  updateHCBox(session, "mod_new_selector", structure = create_hcbox_selector(boxdata) )
  
  #shinyLogger$exiting()
}

#' Button enabler Warnning if selected data is <= 2
update_mod_new_select_nextbutton <- function(rv, input, output, session){
  
  sel <- input$mod_new_selector
  
  if ( is.null(sel) || length(sel) == 0 ) {
    if ( isolate(input$mod_new_msp) == 2 ) updateButton(session, "mod_new_next", disabled = T ) 
    return();
  }
  # Warning
    #createAlert(session, "mod_new_alert", "mod_new_few_samples", 
    #            title = "Few samples",style = "warning",dismiss = T, append = F,
    #            content = "Few samples selected. Following results will be unaccurate. Please load and select more samples" )
  
  # Update selected data
  selected <- get_samples_from_selector(sel)
  
  data <- isolate(rv$synapses.data$df) # Get data + dependency
  
  # Create DF
  rv$currentModel$data <- merge(data , selected)
  
  #
  updateButton(session, "mod_new_next", disabled = F )
}

################ FERET STEP ###########################

#' Listents to Feret button (observe event)
update_mod_new_feret <- function(rv, input, output, session){
  
  # Feret fit and test
  rv$currentModel$feret <- lgFeretFit( rv$currentModel$data$feret )
  
  # Output
  alpha <- input$mod_new_alpha
  
  if ( rv$currentModel$feret$htest$p.value <= alpha ) {
    createAlert(session, "mod_new_alert", "mod_new_feretrej", 
                title = "Hypothesis rejection",style = "warning",dismiss = T, append = F,
                content = "Goodness-of-fit test <b>REJECTS</b> (at given significance level) lognormal hypothesis. Following results might be inaccurate." )
  }
  
  # Plot fit / histogram
  rv$currentModel$feret_plot <- lgFeretPlot(rv$currentModel$data$feret, rv$currentModel$feret$ml, rv$currentModel$feret$sdl)
  output$mod_new_feretplot <- renderPlot( rv$currentModel$feret_plot )
  
  # Params log
  output$mod_new_feretlog <- renderUI({
    tags$p("Feret diameter distribution has been fitted to a log-normal distribution with parameters:",
           tags$ul(
             tags$li( tags$b("Log mean"),": ", sprintf("%.4f",rv$currentModel$feret$ml) ),
             tags$li( tags$b("Log standard deviation"),": ", sprintf("%.4f",rv$currentModel$feret$sdl) ),
             tags$li( tags$b("Test p-value:"),": ", sprintf("%.4f",rv$currentModel$feret$htest$p.value)," (",rv$currentModel$feret$htest$method, ")")
           )
    )
  })
  
  # Enable next button
  updateButton(session, "mod_new_next", disabled = F )
}

################# MODEL STEP ###########################

# Update crossval ui
update_mod_new_crossvalui <- function(rv, input, output, session){
  
  # Get rv data
  selectedData <- rv$currentModel$data
  availableData <- rv$synapses.data$df
  
  if (is.null(selectedData) || is.null(availableData) ) return(NULL)
  
  # Not available if more than 1 layer
  if ( length(unique(selectedData$layer)) > 1 ) return(NULL)
  else{
    # Sel layer
    layer <- unique(selectedData$layer)
    
    # Extract sample - sampleno pairs from selected data
    selpairs <- apply(unique(selectedData[, c("sample","samplenum")]),1,paste, collapse = "|")
    # Available Pairs
    availablepairs <- apply(unique(availableData[availableData$layer == layer, c("sample","samplenum")]),1,paste, collapse = "|")
    
    crossSet <- setdiff(availablepairs,selpairs)
    
    # If the number of pairs in merged not in available (that is, non selected) is leq than 1, DO NOT create the selector
    if ( (length(crossSet)) < 1 ) return(NULL)
    else{
      output$mod_new_crossval_ui <- renderUI({
        checkboxInput("mod_new_crossval", "Cross-validated envelope", value = T)
      })
    
      # Add crossSet to current
      rv$currentModel$crossSet <- strsplit(crossSet, split = "|", fixed = T)
    }
  }
}

# Compute RSA envelope button listener
listen_mod_new_rsaenvelope <- function(rv, input, output, session){
  
  # Get inputs
  envSims <- input$mod_new_envsims
  avgSims <- input$mod_new_avgsims
  cross <- input$mod_new_crossval
  selectedData <- rv$currentModel$data
  
  if ( is.null(cross) || !cross ) crossSet <- NULL
  else crossSet <- rv$currentModel$crossSet
  
  # SELECTED DATA LAMBDA
  #Layer Avg Intensity
  pairs <- unique(selectedData[,c("sample","samplenum")])
  pairs <- split(pairs, row(pairs) )
  
  # Intensity
  rv$currentModel$lambda <- mean(sapply(pairs,function(x,data){
    subset <- data[ data$sample == x$sample & data$samplenum == x$samplenum, ]
    return(sampleIntensity(subset$x, subset$y, subset$z, subset$feret ))
  }, selectedData))
  
  #Volume
  rv$currentModel$sims$volume <- mean(sapply(pairs,function(x,data){
    subset <- data[ data$sample == x$sample & data$samplenum == x$samplenum, ]
    return(sampleBoxVolume(subset$x, subset$y, subset$z, subset$feret ))
  }, selectedData))
  
  # STEP 1: Compute simulations (2 ways - with / without crossset)
  # WO cross
  if (is.null(crossSet)) {
  
    # Update RV
    rv$currentModel$sims$feret <- rv$currentModel$feret
    rv$currentModel$sims$lambda <- rv$currentModel$lambda
    
  }
  else{
    
    # Get layer data
    data <- rv$synapses.data$df[rv$synapses.data$df$layer ==  unique(selectedData$layer), ]
    
    # Compute average intensity in the cross set
    lambda <- mean(sapply(crossSet, function(x,data){
      subsel <- data[ data$sample == x[1] & data$samplenum == as.numeric(x[2]), ]
      return(sampleIntensity(subsel$x, subsel$y, subsel$z, subsel$feret ))
    },data))

    
    # Compute feret parameters in crossset
    aux <- data.frame( sample = sapply(crossSet, function(x) x[1] ), samplenum = sapply(crossSet, function(x) x[2] ) )
    crossdf <- merge(data, aux )

    rv$currentModel$sims$feret <- lgFeretFit( crossdf$feret )
    rv$currentModel$sims$lambda <- lambda
    
  }
  
  # Compute sims
  rv$currentModel$sims$data <- nRSASimulations(envSims + avgSims, 
                                          rv$currentModel$sims$lambda, 
                                          rv$currentModel$sims$volume, 
                                          rv$currentModel$sims$feret$ml,
                                          rv$currentModel$sims$feret$sdl, parallel = T)
  
  # STEP 2: Compute and plot envelope
  # Two situations: only 1 sample selected or more than 1 sample
  
  if ( length(pairs) == 1 ) {
    # ONE SAMPLE
    
    # Create sample process
    spp <- createSampleSPP(selectedData$x, selectedData$y, selectedData$z, selectedData$feret)
    rv$currentModel$spp <- list(spp)
    
    # Compute envelope
    env <- computeSppLEnvelope(spp, simulations = rv$currentModel$sims$data,
                        nsimavg = avgSims,
                        nsimenv = envSims,
                        name = sprintf("%s (%d)", selectedData[1,"sample"], selectedData[1,"samplenum"]))
    
    # Plot envelope
    output$mod_new_rsaplot <- renderPlot({
      rv$currentModel$rsa_env_plot <- plotSppLEnvelope(RSALEnvelope = env$envLRSA,
                                                name =  sprintf("%s (%d)", 
                                                                selectedData[1,"sample"], 
                                                                selectedData[1,"samplenum"]) )
      isolate(rv$currentModel$rsa_env_plot)
    })
    
  }
  else{
    
    # Create a SPP for each sample in the selected set
    rv$currentModel$spp <- lapply(pairs, function(x,data){
      subsel <- selectedData[ data$sample == x$sample & data$samplenum == x$samplenum, ]
      return(createSampleSPP(subsel$x, subsel$y, subsel$z, subsel$feret ))
    }, selectedData)
    
    # Compute envelope
    env <- computeSetLEnvelope(rv$currentModel$sims$data,
                               nsimavg = avgSims,
                               nsimenv = envSims)
    
    # Plot 
    output$mod_new_rsaplot <- renderPlot({
      rv$currentModel$rsa_env_plot <- plotSetLEnvelope( isolate(rv$currentModel$spp),
                                                env,
                                                name =  "Set envelope")
      isolate(rv$currentModel$rsa_env_plot)
    })
  }
  
  # Almost ended - fill log + Activate button
  
  # Parameter msg
  output$mod_new_rsalog <- renderUI({
    tags$p( sprintf("Envelope and results based on %d simulations: ", envSims + avgSims ),
            tags$ul(
              tags$li( tags$b("Feret diameter log mean:"),  sprintf("%.4f",rv$currentModel$sims$feret$ml) ),
              tags$li( tags$b("Feret diameter log standard deviation:"),  sprintf("%.4f",rv$currentModel$sims$feret$sdl) ),
              tags$li( tags$b("Process intensity:"), sprintf("%.4f",rv$currentModel$sims$lambda*1E9) ," synapses/µm³" )
            )
    )
  })
  
  # Enable next button
  updateButton(session, "mod_new_next", disabled = F )
}

######## FOURTH STEP: FGKL ###########
listen_mod_new_fgkl <- function(rv, input, output, session){
  
  spp <- rv$currentModel$spp
  sims <- rv$currentModel$sims$data
  # Extract spps
  sims <- lapply(sims, function(x) x[[1]])
  
  # Compute average FGKL
  rv$currentModel$fgkl$spp <- computeAvgFGKL(spp)
  rv$currentModel$fgkl$sims <- computeAvgFGKL(sims)
  
  # PLOT
  output$mod_new_fgklplot <- renderPlot({
    
    gplot <- plotFGKL( isolate(rv$currentModel$fgkl$spp),
             isolate(rv$currentModel$fgkl$sims),
             name = "")
    rv$currentModel$fgkl_plot <- gplot
    gplot
  })
  
  # Enable next button
  updateButton(session, "mod_new_next", disabled = F )
}

########### NAME CONTROL ############
watcher_mod_new_name <- function(rv, input, output, session){
  
  name <- input$mod_new_name
  if ( is.null(name) || length(name) == 0 || name == "" ) {
    
    # Disable only in our page
    if(isolate(input$mod_new_msp) == 6 )
      updateButton(session, "mod_new_next", disabled = T )
    
    return(NULL)
  }
  else{
    # Check dup names
    if ( name %in% names(rv$models) )
    {
      # Create alert
      createAlert(session,"mod_new_alert","mod_new_name_dup", title = "Duplicated name", 
                  content = "Chosen name already exists. Please, select another name",
                  style = "warning", dismiss = T, append = F)
      if(isolate(input$mod_new_msp) == 6 )
        updateButton(session, "mod_new_next", disabled = T )
      return(NULL)
    }
    else{
      closeAlert(session, "mod_new_name_dup")
      if(isolate(input$mod_new_msp) == 6 )
        updateButton(session, "mod_new_next", disabled = F )
    }
  }
}

# Update model view selector
update_model_view_selector <- function(rv, input, output, session){
  modelNames <- names(rv$models)
  if(!is.null(modelNames))
    updateSelectInput(session,"mod_view_sel", choices = modelNames)
}

update_model_view <- function(rv, input, output, sesion){
  selectedModel <- input$mod_view_sel
  if (!is.null(selectedModel) && nchar(selectedModel) > 0 ){
    # Get model
    model <- isolate(rv$models[[selectedModel]])
    
    # Create summary
    nlayers <- sort(unique(model$data$layer))
    nsamples <- nrow(unique(model$data[,c("sample","samplenum")]))
    crossval <- ifelse(is.null(model$crossSet),"No","Yes")
    log.mean <- model$feret$ml
    log.sd <- model$feret$sdl
    intensity <- model$lambda * 1E9
    
    output$mod_view_summary <- renderUI(tagList(
      tags$li( tags$b("Number of samples: "),  sprintf("%.0f",nsamples) ),
      tags$li( tags$b("Layers: "),  paste(nlayers,collapse = ", ") ),
      tags$li( tags$b("Cross validated: "), crossval ),
      tags$li( tags$b("Feret diameter log mean: "),  sprintf("%.4f",log.mean) ),
      tags$li( tags$b("Feret diameter log standard deviation: "),  sprintf("%.4f",log.sd) ),
      tags$li( tags$b("Process intensity: "), sprintf("%.4f",intensity) ," synapses/µm³" )
    ))
    
    # Render plots
    output$mod_view_feret <- renderPlot(model$feret_plot)
    output$mod_view_rsaenv <- renderPlot(model$rsa_env_plot)
    output$mod_view_fgkl <- renderPlot(model$fgkl_plot)
  }
}

## Download model button
manage_model_download <- function(rv, input, output, sesion){
  
  selectedModel <- input$mod_view_sel
  if (is.null(selectedModel)) return()
  
  output$mod_view_download <- downloadHandler(
    filename = function() {paste(selectedModel,"RSAmodel.zip",sep = "_")},
    content = function(fname){

      ## Temporaly switch to the temp dir, in case we do not have write permissions in the current wd
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      
      # Create tempdir
      baseDir <- file.path(tempdir(),paste(selectedModel,"files",sep = "_"))
      if (dir.exists(baseDir)) unlink(baseDir, recursive = T )
      dir.create( baseDir )
      
      # Get model
      model <- isolate(rv$models[[selectedModel]])
      
      # Create summary
      layers <- paste0(sort(unique(model$data$layer)),collapse = ", ")
      nsamples <- nrow(unique(model$data[,c("sample","samplenum")]))
      crossval <- ifelse(is.null(model$crossSet),"No","Yes")
      log.mean <- model$feret$ml
      log.sd <- model$feret$sdl
      intensity <- model$lambda * 1E9
      
      # Save plots
      ggplot2::ggsave(file.path(baseDir,"feret.png"), plot = model$feret_plot, dpi = 600, scale = 1.3 )
      ggplot2::ggsave(file.path(baseDir,"rsa_envelope.png"), plot = model$rsa_env_plot, dpi = 600, scale = 1.3 )
      ggplot2::ggsave(file.path(baseDir,"fgkl.png"), plot = model$fgkl_plot, dpi = 600, scale = 1.3 )
      
      # Get model files
      files <- merge(unique(model$data[,c("sample","samplenum","layer")]), isolate(rv$samples.data$df))
      # Create filelist
      filelist <- lapply(1:nrow(files), function(i){
        return(list(
          type = "file",
          filename = files$filename[i],
          extension = files$ext[i],
          sample = sprintf("%s (%d)", files$sample[i], files$samplenum[i]),
          layer = files$layer[i]
        ))
      })
      
      # Create metadata
      meta <- modelParametersOutput(log.mean, log.sd, intensity,
                                    sample = nsamples,
                                    layer = layers,
                                    inputFiles = filelist,
                                    modeler = "",
                                    application = "synapsesSA",
                                    version = "1.21",
                                    output = list(list(type = "file",extension = "xml",filename = "synapsesSA_RSAmodel.xml"
                                                     ,description = "Synapses SA RSA modeling output file")))
      XML::saveXML(meta, file = file.path(baseDir,"synapsesSA_RSAmodel.xml"))
      
      zip(zipfile = fname, files = paste(selectedModel,"files",sep = "_") )
    },
    contentType = "application/zip")
}