#' 3DSynapses plot functions
#'
#' Basic plots for 3DSynapsesSA Package
#'
#' @author Laura Antón-Sánchez

#library(rgl)

#' showPoints
#' 
#' Plots a set of synapses from real data
#' 
#' @param zz A dataframe with columns "X","Y","Z","Feret" and optionally "Type"
#' @param name Plot name
#' @param samePlot If False previous plot is overwritten
#' @param clorAS Color for asymmetric synapses
#' @param colorSS Color for symmetric synapses
#'
#' @return 3D RGL Plot
#' 
#' @examples
#' showPoints(synapses)
#' showPoints(synapses_2,samePlot=T,colorAS="blue") 
#' 
#' @export
showPoints <-function(zz,name="synapses",samePlot=FALSE, colorAS="green", colorSS="red") {
  
  # Set background
  bg3d(alpha=c(0.0),color="red")
  
  # Get colors and radius
  if(is.null(zz$Type)){
    colors <- rep(colorAS,times=nrow(zz))
  }
  else{
    colors <- sapply(as.character(zz$Type),function(x){if(x=="asymmetric" | x=="1"){return(colorAS)}else{return(colorSS)}})
  }
  
  rads <- as.numeric(zz$Feret)/2
  
  # Plot!
  plot3d(zz[,c("X","Y","Z")]
         ,xlab="",ylab="",zlab=""
         #,xlab=paste(name,"_x",sep=""),ylab=paste(name,"_y",sep=""),zlab=paste(name,"_z",sep="")
         ,type="s",radius=rads,col=colors,aspect=FALSE, add=samePlot,box=F,axes=F)
  
  # plot axes
  # axes3d(edges = "bbox", labels = F, tick = F,  color="white")
  #box3d(color="white")
  
}

#' showSim
#' 
#' Plots a set of synapses from a simulation
#' 
#' @param x X coordinates of the synapses
#' @param y Y coordinates of the synapses
#' @param z Z coordinates of the synapses
#' @param diam Synapses Feret's diameters
#' @param name Plot name
#' @param samePlot If False previous plot is overwritten
#' @param color Synapses plot color
#'
#' @return 3D RGL Plot
#' 
#' @examples
#' showSim(simulation)
#' showSim(simulation_2,samePlot=T,colorAS="blue") 
#' 
#' @export
showSim <-function(x,y,z,diam,name="sim",samePlot=FALSE,color='red') {
  
  cat(paste0("na\n"),file = stderr())
  n <- length(x)
  bg3d(color="black")
  #plot3d(x[1],y[1],z[1],xlab="",ylab="",zlab="",type="s",radius=(diam[1]/2),col=color,aspect=FALSE, add=samePlot)
  rgl.spheres(x,y,z,radius=(diam/2),col=color,aspect=FALSE, add=samePlot)
  box3d(color="white")
}