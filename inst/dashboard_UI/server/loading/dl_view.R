#' Update dl_view selector
update_dl_view_selector <- function(rv, input, output, session){
  
  # shinyLogger$entering() # Add function calling
  
  data <- rv$synapses.data$df # Get data + dependency
  sims <- rv$simulations$df 
  
  # Ghost calls (init)
  if ( (is.null(data) || nrow(data) == 0) &&
       (is.null(sims) || nrow(sims) == 0) )  return()
  
  # TODO: ADD SIMULATION
  if (is.null(data) || nrow(data) == 0 ){
    boxdata <- data.frame(sample = numeric(0), layer = numeric(0), samplenum = numeric(0))
    bylayer <- F
  }
  else{
    boxdata <- unique(data[,c("sample","layer","samplenum")])
    bylayer <- T
  }
  
  # HC box
  updateHCBox(session, "dl_view_selector", structure = create_hcbox_selector(boxdata, bySample = F ,byLayer = bylayer, simData = sims), 
              multiple = T, checkbox = T )
}

#' Update table (based on selector)
update_dl_view_table <- function(rv, input, output, session){
  
  # shinyLogger$entering() # Add function calling
  
  selected <- input$dl_view_selector
  data <-  isolate(rv$synapses.data$df)
  sims <- isolate(rv$simulations$df)
  
  # Ghost calls (init)
  if ( is.null(selected) || length(selected) == 0) return()
  
  # Extract selected
  selected <- get_samples_from_selector(selected)
  
  if (any(selected$sample == "simulation")) {
    
    sims.data <-  mapply(function(x,y){
      cbind(sample = "simulation", samplenum = y, layer = NA, type = "symmetric", as.data.frame(x[[1]]$data),  feret = x[[2]] )
    }, rv$simulations$data[ as.numeric(selected$samplenum) ], as.numeric(selected$samplenum), SIMPLIFY = F)
    df.sim <- Reduce(rbind, sims.data)
  }
  else{
    df.sim <- NULL
  }

  # Create DF
  df <- merge(data, selected)
  
  output$dl_view_table <- renderDataTable( rbind(df,df.sim) , options = list( pageLength = 10, lengthChange = FALSE,
                                                               info = FALSE, searching = FALSE, searchable = NULL))
  
  #shinyLogger$exiting()
}


manageCSVDownload <- function(rv, input, output, session){
  
  selectedModel <- input$dl_view_selector
  if (is.null(selectedModel) || length(selectedModel) == 0 ) return()
  
  output$dl_view_download <- downloadHandler(
    filename = function() {"synapses_SA_data.zip"},
    content = function(fname){
      
      ## Temporaly switch to the temp dir, in case we do not have write permissions in the current wd
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      
      # Create tempdir
      baseDir <- file.path(tempdir(),"synapses_data")
      if (dir.exists(baseDir)) unlink(baseDir, recursive = T )
      dir.create( baseDir )
      
      # Extract selected
      selected <- get_samples_from_selector(selectedModel)
      data <-  isolate(rv$synapses.data$df)
      sims <- isolate(rv$simulations$df)
      
      # First: non-simulated files (no need metadata)
      not.simulated <- selected[ selected$sample != "simulation", ]
      if (nrow(not.simulated) > 0) {
        for (i in 1:nrow(not.simulated)) {
          num <- as.numeric(not.simulated$samplenum[i])
          df <- data[ data$sample == not.simulated$sample[i] & data$samplenum == num,]
          rownames(df) <- NULL
          write.csv(x = df, 
                    file = file.path(baseDir, sprintf("Sample_%s_num_%d.csv",not.simulated$sample[i],num)),
                    sep = ",",
                    dec = ".",
                    eol = "\n")
        }  
      }
      
      # Then: Simulated
      simulated <- selected[ selected$sample == "simulation", ]
      if (nrow(simulated) > 0) {
        for (i in 1:nrow(simulated)) {
          num <- as.numeric(simulated$samplenum[i])
          data <- cbind(as.data.frame(rv$simulations$data[[num]][[1]]$data), 
                        type = "symmetric", 
                        sample = "simulation",
                        samplenum = num,
                        feret = rv$simulations$data[[num]][[2]])
          
          sim.params <- isolate(rv$simulations$df[num, ])
          
          # Create summary
          modelName <- sim.params$model
          volume <- sim.params$volume
          log.mean <- sim.params$ml
          log.sd <- sim.params$sdl
          intensity <- sim.params$lambda * 1E9
          
          # IF is model != NA
          if (!is.na(modelName)) {
            
            model <- isolate(rv$models[[modelName]])
            layers <- paste0(sort(unique(model$data$layer)),collapse = ", ")
            nsamples <- nrow(unique(model$data[,c("sample","samplenum")]))
            
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
          }
          else{
            modelName <- NULL
            filelist <- NULL
            layers <- NULL
            nsamples <- NULL
          }
          
          # Write meta
          meta <- simulationParametersOutput(log.mean, log.sd, intensity,
                                             sample = nsamples,
                                             layer = layers,
                                             volume = volume,
                                             model = modelName,
                                             inputFiles = filelist,
                                             modeler = "",
                                             application = "synapsesSA",
                                             version = "1.21",
                                             output = list(list(type = "file",extension = "xml",
                                                                filename = sprintf("simulation_%d_meta.xml",num),
                                                                description = "Simulation metadata file"),
                                                           list(type = "file",extension = "csv",
                                                                filename = sprintf("simulation_%d.csv",num),
                                                                description = "Simulation metadata file"))
          )
          rownames(data) <- NULL
          XML::saveXML(meta, file = file.path(baseDir,sprintf("simulation_%d_meta.xml",num) ))
          write.csv(x = data, 
                    file = file.path(baseDir, sprintf("simulation_%d.csv",num)),
                    sep = ",",
                    dec = ".",
                    eol = "\n")
        } # For
      } # IF
      zip(zipfile = fname, files = "synapses_data" )
    },
    contentType = "application/zip")
}