#' Update dash_view selector
update_dash_view_selector <- function(rv, input, output, session){
  
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
  updateHCBox(session, "dash_view_selector", structure = create_hcbox_selector(boxdata, bySample = F ,byLayer = bylayer, simData = sims), 
              multiple = F, checkbox = F )
  
  #shinyLogger$exiting()
}

#' Enables / disables the draw button
#'
dash_view_buttonControl <- function(rv, input, output, session){
  
  # shinyLogger$entering() # Add function calling
  
  # Check file selector
  file <- input$dash_view_selector
  symcheck <- input$dl_dash_symcheck 
  asymcheck <- input$dl_dash_asymcheck
  
  # Update dl_files button
  updateButton(session, "dash_view_draw", disabled = is.null(file) || !(symcheck || asymcheck) )
  
  #shinyLogger$exiting()
}

#' Draws the compositions
dash_view_drawcomposition <- function(rv, input, output, session){
  
  # Get plot options
  sample <- input$dash_view_selector
  ids <- get_samples_from_selector(sample)
  if (ids$sample != "simulation")
    data <- rv$synapses.data$df[ rv$synapses.data$df$sample == ids$sample & rv$synapses.data$df$samplenum == ids$samplenum, ]
  else{
    sim <- rv$simulations$data[[ as.numeric(ids$samplenum) ]] 
    data <- cbind(as.data.frame(sim[[1]]$data), type = "symmetric", feret = sim[[2]] )
  }
    
  
  symcheck <- input$dl_dash_symcheck 
  asymcheck <- input$dl_dash_asymcheck
  symcolor <- input$dl_dash_symcolor
  asymcolor <- input$dl_dash_asymcolor
  
  # Create Objects ( geometry + material)
  if (symcheck)
    symobj <- createSpheres( data[ data$type == "symmetric", ], symcolor)
  else
    symobj <- NULL
  
  if (asymcheck)
    asymobj <- createSpheres( data[ data$type == "asymmetric", ], asymcolor)
  else
    asymobj <- NULL
  
  # Add lights
  lights <- list( threejswrapper::ambientLight(color = "#333333"), 
                  threejswrapper::directionalLight(color = "#FFFFFF", intensity = 0.3, from = c(1,0,1) ),
                  threejswrapper::directionalLight(color = "#FFFFFF", intensity = 0.3, from = c(0,1,1) )
                )
  
  # Add helpers
  bbox <- threejswrapper::boundingBox()
  
  # Create scene
  scene <- threejswrapper::scene( objects = c(symobj,asymobj), lights = lights, helpers = bbox,
                                 camera.pos = c(max(data$x),max(data$y),max(data$z))*1.10,
                                 camera.far = 1E6,
                                 camera.lookat = c(min(data$x),min(data$y),min(data$z))  )
  
  # Render it
  output$dash_view_scene <- threejswrapper::renderScene(scene)
}

createSpheres <- function(data, color){
  
  if (nrow(data) == 0) return(NULL)
  objs <- mapply(function(d, x, y, z){
        return( threejswrapper::objThreeJS( 
          threejswrapper::sphereGeometry(d/2, position = c(x,y,z), wsegs = 20, hsegs = 20),
          threejswrapper::lambertMaterial(color = color) ) )
      },
      d = data$feret,
      x = data$x,
      y = data$y,
      z = data$z,
      SIMPLIFY = F
  )
  return(objs)
}