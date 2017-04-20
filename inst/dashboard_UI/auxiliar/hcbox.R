#' Creates Hierarchical checbox selectors
#' 
create_hcbox_selector <- function(data, byLayer = T , bySample = T , simData = NULL){
  
  # Merge simulations and data
  if ( (is.null(data) || nrow(data) == 0 ) && (is.null(simData) || nrow(simData) == 0 )  ) {
    return(list(
      hcbox_item("layer_selector",  disabled = T, label = "Layers"),
      hcbox_item("sample_selector",  disabled = T, label = "Samples"),
    ))
  }
  
  if (byLayer) 
    layer_item <- hcbox_item("layer_selector",  
                           clist = hcbox_selector_group(paste(data$sample, data$samplenum, sep = "-"),
                                                        labels = paste0(data$sample," (", data$samplenum,")"),
                                                        groupBy = data$layer, 
                                                        idGroupPrefix = F, 
                                                        sep = "_"),
                           label = "Layers")
    else
      layer_item <- NULL

  if (bySample) 
    sample_item <- hcbox_item("sample_selector",  
                             clist = hcbox_selector_group(paste(data$sample, data$samplenum, sep = "-"),
                                                          labels = paste0(data$sample," (", data$samplenum,")"),
                                                          groupBy = data$sample,
                                                          idGroupPrefix = F,
                                                          sep = "_"),
                             label = "Samples")
    else
      sample_item <- NULL
  
  if (!is.null(simData) && nrow(simData) > 0 ) {
    # Replace null models
    simData$model[is.na(simData$model)] <- "Manual"
    sim_item <- hcbox_item("sim_selector",
                           clist = hcbox_selector_group( paste("simulation", seq(1,nrow(simData)),sep = "-"),
                                                         labels = seq(1,nrow(simData)),
                                                         labelFormat = "Sim. %s", 
                                                         groupBy = simData$model,
                                                         idGroupPrefix = F,
                                                         sep = "_" ),
                           
                           label = "Simulations")
  }
  else
    sim_item <- NULL
  
  return( list(layer_item, sample_item, sim_item) )
}

#' Creates a hierarchical selector with given groups
#' (Restricted to level - 1 grouping)
#'
hcbox_selector_group <- function(ids, labels = ids,  labelFormat = "%s", groupBy = NULL, idGroupPrefix = F, sep = "_"){
  
  # Check for no-grouping needs
  if (is.null(groupBy)) {
    
    # Build a box for each id
    boxes <- mapply( function(x,y,format){
        return(hcbox_item( as.character(x), label = sprintf(format,y)))
      },
      x = ids, y = labels,
      MoreArgs = list(format = labelFormat),
      SIMPLIFY = F
    )
    return( boxes ) 
    
  }
  else{
    
    # If need group prefix - add it
    if (idGroupPrefix) {
      ids <- paste( ids, groupBy, sep = sep)
    }
    
    # Split data
    split.ids <- split(ids,groupBy)
    split.labels <- split(labels,groupBy)
    
    #  Create boxes for each group
    boxes <- mapply(function(ids,labels,format){
      
               # Create boxes within each group
              return(mapply( function(x,y,format){
                return(hcbox_item( as.character(x), label = sprintf(format,y)))
              },
              x = ids, y = labels,
              MoreArgs = list(format = format),
              SIMPLIFY = F)
              )
           },
           
           ids = split.ids, labels = split.labels,
           MoreArgs = list(format = labelFormat),
           SIMPLIFY = F)
    
    # Now create group boxes
    groupBoxes <- lapply( names(boxes) , function(x, boxes){
      return(hcbox_item( paste0("_GROUP_",x), label = as.character(x), clist = unname(boxes[[x]]) ))
    }, boxes)
    
    return(groupBoxes)
  }
}

#' Extract a unique list of samples from a somaselector
get_samples_from_selector <- function(x, sep = "_"){
  
  if (length(x) == 0) return(NULL)
  
  # Process each input value
  preproc <- sapply(x, function(z){
    # Split by sep
    s <- strsplit( z , sep, fixed  = T )[[1]]
    
    # Get group and sample
    if (length(s) == 1 ) return(s) # NO GROUP
    else{
      group <- s[length(s)]
      sample <- paste0(s[-length(s)], collapse = sep)
  
      # only return sample
      return(sample)
    }
  })
  
  # Unique (sample - samplenum pair should be unique and all belonging to the same layer)
  preproc <- unique(preproc)
  
  # Extract samplename and samplenum
  ret <- sapply(strsplit(preproc, "-", fixed = T), function(x){
    num <- x[length(x)]
    name <- paste0(x[-length(x)], collapse = "-")
    return(c(samplenum = num, sample = name))
  })
  
  return(as.data.frame(t(ret), stringsAsFactors = F) )
}