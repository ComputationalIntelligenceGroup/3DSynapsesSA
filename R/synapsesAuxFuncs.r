#' 3DSynapses auxiliar functions
#'
#' Basic functions for 3DSynapsesSA Package
#'
#' @author Luis Rodriguez Lujan
#' 

#' createSampleSPP
#' 
#' Creates a single 3D spatial process
#' 
#' Given the x,y,z coordinates of a 3D spatial process creates a SPP based on it
#' 
#' @param x Either a 3-column dataframe (x,y,z,d) or a numeric vector
#' @param y A numeric vector with y coordinates (same length as \code{x} )
#' @param z A numeric vector with z coordinates (same length as \code{x} )
#' @param d A numeric vector with point diameter (same length as \code{x} )
#' 
#' @importFrom spatstat box3 pp3
#' 
#' @return A 3D Spatial process
#' 
#' @export
createSampleSPP <- function(x, y = NULL, z = NULL, d = NULL){
  
  # Check args
  if ( is.data.frame(x) ) {
    if ( (ncol(x) != 3 && ncol(x) != 4)  || any(!sapply(x,is.numeric)) ) stop("X should be a 3D numeric dataframe (x,y,z) or a 4D dataframe (with diameter)")
  }
  else{
    if ( !is.numeric(x) || !is.numeric(y) || !is.numeric(z) ) stop("Arguments should be numeric")
    else if ( length(y) != length(x) || length(z) != length(x) ) stop("Argument lengths do not match")
    
    # Check diameter
    if ( !is.null(d)) {
      if ( !is.numeric(d) ) stop("Diameter should be numeric")
      else if ( length(d) != length(x) ) stop("Argument lengths do not match")
      # d/2 to get radius
      x <- cbind.data.frame(x,y,z,d/2)
    }
    else{
      # As dataframe
      x <- cbind.data.frame(x,y,z)
    }
  }
  
  # Add 0 diameter
  if (ncol(x) == 3) x <- cbind.data.frame(x,0)
  
  # Create process box
  box <- spatstat::box3( c( min(x[,1] - x[,4]), max(x[,1] + x[,4]) ),
                         c( min(x[,2] - x[,4]), max(x[,2] + x[,4]) ),
                         c( min(x[,3] - x[,4]), max(x[,3] + x[,4]) ))
  
  # Create process
  return(spatstat::pp3(x[,1], x[,2], x[,3], box))
}

#' Sample box volume
#' 
#' Compute approximate boxvolume for a given sample
#' 
#'  Given a dataframe (or vectors) with x,y,z cartesian coordiantes of a 3D spatial process, and
#'  optionally a vector or column with the point diameters, computes the volume of the box that
#'  covers the given sample.
#'  
#' @param x Either a 3-column dataframe (x,y,z,d) or a numeric vector
#' @param y A numeric vector with y coordinates (same length as \code{x} )
#' @param z A numeric vector with z coordinates (same length as \code{x} )
#' @param d A numeric vector with point diameter (same length as \code{x} )
#' 
#' @return Sample box volume
#' 
#' @export
sampleBoxVolume <- function(x, y = NULL, z = NULL, d = NULL){
  
  # Check args
  if ( is.data.frame(x) ) {
    if ( (ncol(x) != 3 && ncol(x) != 4) || any(!sapply(x,is.numeric)) ) stop("X should be a 3D numeric dataframe (x,y,z) or a 4D dataframe (with diameter)")
  }
  else{
    if ( !is.numeric(x) || !is.numeric(y) || !is.numeric(z) ) stop("Arguments should be numeric")
    else if ( length(y) != length(x) || length(z) != length(x) ) stop("Argument lengths do not match")
    
    # Check diameter
    if ( !is.null(d)) {
      if ( !is.numeric(d) ) stop("Diameter should be numeric")
      else if ( length(d) != length(x) ) stop("Argument lengths do not match")
      # d/2 to get radius
      x <- cbind.data.frame(x,y,z,d/2)
    }
    else{
      # As dataframe
      x <- cbind.data.frame(x,y,z)  
    }
    
  }
  
  # Add 0 diameter
  if (ncol(x) == 3) x <- cbind.data.frame(x,0)
  
  # Create process box
  xrang <- abs(max(x[,1] + x[,4]) - min(x[,1] - x[,4]))
  yrang <- abs(max(x[,2] + x[,4]) - min(x[,2] - x[,4]))
  zrang <- abs(max(x[,3] + x[,4]) - min(x[,3] - x[,4]))
  
  return(xrang * yrang * zrang)
}

#' sampleIntensity
#' 
#' Computes sample synaptic density
#' 
#'  Given a dataframe (or vectors) with x,y,z cartesian coordiantes of a 3D spatial process, and
#'  optionally a vector or column with the point diameters, computes the observed sample intensity
#'  (Number of points per volume unit)
#' 
#' @param x Either a 3-column dataframe (x,y,z,d) or a numeric vector
#' @param y A numeric vector with y coordinates (same length as \code{x} )
#' @param z A numeric vector with z coordinates (same length as \code{x} )
#' @param d A numeric vector with point diameter (same length as \code{x} )
#' 
#' @return Numeric value of the intensity ( # of synapses / volume )
#' 
#' @export
sampleIntensity <- function(x, y = NULL, z = NULL, d = NULL){
  
  # Check args
  if ( is.data.frame(x) ) {
    if ( (ncol(x) != 3 && ncol(x) != 4)  || any(!sapply(x,is.numeric)) ) stop("X should be a 3D numeric dataframe (x,y,z) or a 4D dataframe (with diameter)")
  }
  else{
    if ( !is.numeric(x) || !is.numeric(y) || !is.numeric(z) ) stop("Arguments should be numeric")
    else if ( length(y) != length(x) || length(z) != length(x) ) stop("Argument lengths do not match")
    
    # Check diameter
    if ( !is.null(d)) {
      if ( !is.numeric(d) ) stop("Diameter should be numeric")
      else if ( length(d) != length(x) ) stop("Argument lengths do not match")
      # d/2 to get radius
      x <- cbind.data.frame(x,y,z,d/2)
    }
    else{
      # As dataframe
      x <- cbind.data.frame(x,y,z)  
    }
  }
  
  # Add 0 diameter
  if (ncol(x) == 3) x <- cbind.data.frame(x,0)
  
  # Compute sample volume
  volume <- sampleBoxVolume(x);
  
  # Points / volume
  return( nrow(x) / volume )
}
