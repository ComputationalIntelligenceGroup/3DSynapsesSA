#' 3DSynapses Simulation functions
#'
#' Functions to simulate synapses 3D samples
#'
#' @author Laura Antón-Sánchez

# library(spatstat)

#' calculateFunctionsSPP
#' 
#' Given a SPP computes its F,G,K and L functions
#' 
#' @param spp Single spatial process
#' @param numR An integer. Number of points to compute each function
#' @param rmax A numeric value. Maximum value of r.
#' 
#' @return A named list. spp <- spatial process, f,g,k,l <- each function
#' 
#' @export
calculateFunctionsSPP <- function(spp,numR=128,rmax=1250) {
  
  fspp <- F3est(spp,rmax=rmax,nrval=numR)
  gspp <- G3est(spp,rmax=rmax,nrval=numR)
  kspp <- K3est(spp,rmax=rmax,nrval=numR)
  #pcfspp <- pcf3est(spp,rmax=1250,nrval=numR)
  
  lspp <- kspp
  lspp$trans <- (lspp$trans/(2/3*2*pi))^(1/3)
  
  return(list(spp=spp,f=fspp,g=gspp,k=kspp,l=lspp))
}

#' computeFGKLAverage
#' 
#' Computes average F,G,K,L functions
#'
#' Given a list of F,G,K,L function from different SPPs, computes its average proportional
#' to the sample volume.
#' 
#' @param flist A list. Each member as returned by \link{calculateFunctionsSPP}
#' 
#' @return Named list. Average functions
#' 
#' @export
computeFGKLAverage <- function(flist){
  
  # Compute average
  avgFGKLFuncs <- list()
  # flist with one element -> No average
  if(length(flist)==1){
    avgFGKLFuncs$f <- flist[[1]]$f*1.0
    avgFGKLFuncs$g <- flist[[1]]$g*1.0
    avgFGKLFuncs$k <- flist[[1]]$k*1.0
    avgFGKLFuncs$l <- flist[[1]]$l*1.0
  }
  else{
    # TotalVolume
    totalVol<-Reduce("+",lapply(flist,function(x){volume.box3(x$spp$domain)}))
    
    # Reduce with a little trick - first time both x and y will be list, after that x will be a dataframe and y a fv.
    avgFGKLFuncs$f <- Reduce(function(x,y){
      if(class(x)=="list"){ return(x$f*volume.box3(x$spp$domain)+y$f)} 
      else{return(x+y$f*volume.box3(y$spp$domain))}},flist)/totalVol
    avgFGKLFuncs$g <- Reduce(function(x,y){
      if(class(x)=="list"){ return(x$g*volume.box3(x$spp$domain)+y$g*volume.box3(y$spp$domain))} 
      else{return(x+y$g*volume.box3(y$spp$domain))}},flist)/totalVol
    avgFGKLFuncs$k <- Reduce(function(x,y){
      if(class(x)=="list"){ return(x$k*volume.box3(x$spp$domain)+y$k*volume.box3(y$spp$domain))} 
      else{return(x+y$k*volume.box3(y$spp$domain))}},flist)/totalVol
    avgFGKLFuncs$l <- Reduce(function(x,y){
      if(class(x)=="list"){ return(x$l*volume.box3(x$spp$domain)+y$l*volume.box3(y$spp$domain))} 
      else{return(x+y$l*volume.box3(y$spp$domain))}},flist)/totalVol
  }
  return(avgFGKLFuncs)
}

#' plotFGKLgeneric
#' 
#' Plot in a 2x2 Grid F,G,K,L functions
#' 
#' Given F,G,K,L functions as returned by \link{calculateFunctionsSPP}, creates a 2x2 plot with each function.
#' In each plot the given function, RSA and CSR curves are plotted.
#' 
#' @param realFGKLFuncs functions as returned by \link{calculateFunctionsSPP}
#' @param rsaMeanFunc Mean RSA FGKL functions from simulations. functions as returned by \link{calculateFunctionsSPP}
#' @param lw An integer. Plot line width
#' @param name A sting. Plot title
#' 
#' @return 2x2 Plot
#' 
#' @importFrom cowplot plot_grid
#' 
#' @export
plotFGKL <- function(realFGKLFuncs,rsaMeanFuncs,lw = 4, name = ""){

  ##############################
  # First plot: F function
  ##############################
  
  # CSR 
  plotData <- data.frame(x = realFGKLFuncs$f$r, csr = realFGKLFuncs$f$theo, 
                         obs = realFGKLFuncs$f$rs, rsa = rsaMeanFuncs$f$rs)
  Fplot <- ggplot2::ggplot(plotData, aes(x=x) ) + 
    ggplot2::xlab("Distance (nm)") +
    ggplot2::ylab("F(d)") +
    ggplot2::geom_line(mapping = aes( y = rsa, color = "RSA" ) , size = 1.75 ) +
    ggplot2::geom_line( mapping = aes( y = csr, color = "CSR"), size = 1.75) +
    ggplot2::geom_line( mapping = aes( y = obs, color = "Observed"), size = 1.75) +
    ggplot2::scale_colour_manual(guide_legend(title = ""), 
                                 values = c("CSR" = "red" , "RSA" = "darkgreen", "Observed" = "blue4" )) +
    ggplot2::scale_fill_manual(values = c("RSA" = "darkgreen")) +
    ggplot2::theme(legend.position="bottom", text = element_text(family="arial",size = 14),
                   axis.text = element_text(family="arial",size = 12),
                   legend.text = element_text(family="arial",size = 14), 
                   legend.background =  element_rect(colour = "black"),
                   plot.margin=unit(c(1,1,1,3),"cm"))
    
  ##############################
  # First plot: G function
  ##############################
  plotData <- data.frame(x = realFGKLFuncs$g$r, csr = realFGKLFuncs$g$theo, 
                         obs = realFGKLFuncs$g$rs, rsa = rsaMeanFuncs$g$rs)
  
  Gplot <- ggplot2::ggplot(plotData, aes(x=x) ) + 
    ggplot2::xlab("Distance (nm)") +
    ggplot2::ylab("G(d)") +
    ggplot2::geom_line(mapping = aes( y = rsa, color = "RSA" ) , size = 1.75 ) +
    ggplot2::geom_line( mapping = aes( y = csr, color = "CSR"), size = 1.75) +
    ggplot2::geom_line( mapping = aes( y = obs, color = "Observed"), size = 1.75) +
    ggplot2::scale_colour_manual(guide_legend(title = ""), 
                                 values = c("CSR" = "red" , "RSA" = "darkgreen", "Observed" = "blue4" )) +
    ggplot2::scale_fill_manual(values = c("RSA" = "darkgreen")) +
    ggplot2::theme(legend.position="bottom", text = element_text(family="arial",size = 14),
                   axis.text = element_text(family="arial",size = 12),
                   legend.text = element_text(family="arial",size = 14), 
                   legend.background =  element_rect(colour = "black"),
                   plot.margin=unit(c(1,3,1,1),"cm"))
  
  ##############################
  # First plot: K function
  ##############################
  plotData <- data.frame(x = realFGKLFuncs$k$r, csr = realFGKLFuncs$k$theo, 
                         obs = realFGKLFuncs$k$trans, rsa = rsaMeanFuncs$k$trans)
  
  Kplot <- ggplot2::ggplot(plotData, aes(x=x) ) + 
    ggplot2::xlab("Distance (nm)") +
    ggplot2::ylab("K(d)") +
    ggplot2::geom_line(mapping = aes( y = rsa, color = "RSA" ) , size = 1.75 ) +
    ggplot2::geom_line( mapping = aes( y = csr, color = "CSR"), size = 1.75) +
    ggplot2::geom_line( mapping = aes( y = obs, color = "Observed"), size = 1.75) +
    ggplot2::scale_colour_manual(guide_legend(title = ""), 
                                 values = c("CSR" = "red" , "RSA" = "darkgreen", "Observed" = "blue4" )) +
    ggplot2::scale_fill_manual(values = c("RSA" = "darkgreen")) +
    ggplot2::theme(legend.position="bottom", text = element_text(family="arial",size = 14),
                   axis.text = element_text(family="arial",size = 12),
                   legend.text = element_text(family="arial",size = 14), 
                   legend.background =  element_rect(colour = "black"),
                   plot.margin=unit(c(1,1,1,3),"cm"))
  
  ##############################
  # First plot: L function
  ##############################
  plotData <- data.frame(x = realFGKLFuncs$l$r, csr = realFGKLFuncs$l$r, 
                         obs = realFGKLFuncs$l$trans, rsa = rsaMeanFuncs$l$trans)
  
  Lplot <- ggplot2::ggplot(plotData, aes(x=x) ) + 
    ggplot2::xlab("Distance (nm)") +
    ggplot2::ylab("L(d)") +
    ggplot2::geom_line(mapping = aes( y = rsa, color = "RSA" ) , size = 1.75 ) +
    ggplot2::geom_line( mapping = aes( y = csr, color = "CSR"), size = 1.75) +
    ggplot2::geom_line( mapping = aes( y = obs, color = "Observed"), size = 1.75) +
    ggplot2::scale_colour_manual(guide_legend(title = ""), 
                                 values = c("CSR" = "red" , "RSA" = "darkgreen", "Observed" = "blue4" )) +
    ggplot2::scale_fill_manual(values = c("RSA" = "darkgreen")) +
    ggplot2::theme(legend.position="bottom", text = element_text(family="arial",size = 14),
                   axis.text = element_text(family="arial",size = 12),
                   legend.text = element_text(family="arial",size = 14), 
                   legend.background =  element_rect(colour = "black"),
                   plot.margin=unit(c(1,3,1,1),"cm"))
  
  
  return(cowplot::plot_grid(Fplot,Gplot,Kplot,Lplot, labels = c("F","G","K","L")))
}

#' Compute Average FGKL functions
#' 
#' Given a list of 3D Spatial processes compute the average value of each FGKL function
#' 
#' @param sppList The list of spatial processes
#' @param ncores The number of parallel workers to use
#' 
#' @return The average FGKL functions
#' 
#' @importFrom parallel makeCluster clusterEvalQ parLapply stopCluster
#' 
#' @export
computeAvgFGKL <- function(sppList, ncores = parallel::detectCores() - 1 ){
  #Compute FGKL Func for each simulation + spp
  if (ncores > 1) {
    # Create cluster
    cl <- parallel::makeCluster(ncores, type = "SOCK")
    parallel::clusterEvalQ(cl, {library(spatstat); NULL})
    funcs <- parallel::parLapply(cl,sppList,calculateFunctionsSPP)
    parallel::stopCluster(cl)
  }
  else{
    funcs <- lapply(sppList,calculateFunctionsSPP)
  }
  return(computeFGKLAverage(funcs))
}


