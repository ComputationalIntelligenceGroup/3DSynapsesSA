#' 3D Synapses RSA Envelope 
#'
#' Functions to compute and plot RSA model and its envelope
#'
#' @author Laura Antón-Sánchez

#library(spatstat)

#
# Single sample L functs and Envelope 
#

#' computeSppLEnvelope
#' 
#' Computes sample L-function envelope
#' 
#' Given a sample and its RSA simulations computes the RSA and CSR envelopes of the sample.
#' 
#' @param spp The spatial process 
#' @param simulation A list of simulations \link{singleSampleSimulations}. List of simulations to compute the envelope
#' @param nsimavg An integer. Number of simulations used to compute the RSA function
#' @param nsimenv An integer. Number of simulattion used to compute the envelope.
#' @param name A string. Envelope name
#' @param nrank Integer. Rank of the envelope value amongst the nsim simulated values. A rank of 1 means that the minimum and maximum simulated values will be used.
#' 
#' @return A named list. envLCSR <- CSR envelope. envLRSA <- RSA envelope, sampleSPP <- Original sample SPP
#' 
#' @export
computeSppLEnvelope <- function(spp, simulations,
                                nsimavg=length(simulations)/2,
                                nsimenv=length(simulations) - nsimavg,
                                name="", nrank = 1){
  
  # Extract spp from simulations
  sims <- lapply(simulations,FUN = function(x){return(x[[1]])})
  
  # RSA Envelope (L Func)
  envLRSA <- spatstat::envelope(spp,
                     simulate = sims,
                     correction = "translation",
                     rmax = 1250,
                     transform = expression(((3/(4*pi))*(.)) ^ (1/3)), # Transform from R function to L function
                     Yname = name,
                     global = TRUE,
                     ginterval = c(0,1250),
                     nsim = nsimavg,
                     nsim2 = nsimenv,
                     nrank = nrank)
  
  envLCSR <- spatstat::envelope(spp, 
                      simulate = NULL, # CSR Model closed formulation is known. No simulation needed
                      correction = "translation", 
                      rmax = 1250, 
                      transform = expression(((3/(4*pi))*(.)) ^ (1/3)),
                      Yname = name, 
                      global = TRUE, 
                      ginterval = c(0,1250), 
                      nsim = nsimavg,
                      nsim2 = nsimenv,
                      nrank = nrank)
  
  return(list(envLCSR = envLCSR,envLRSA = envLRSA, spp = spp))
}

#' computeSetLEnvelope
#' 
#' Computes L-function envelope for a set of simulations
#' 
#' Given a list of spatial processes computes the average L RSA function and the maximum distance to the mean
#' 
#' @param simulations List of simulations to compute the envelope \link{singleSampleSimulations}. 
#' @param nsimavg An integer. Number of simulations used to compute the RSA function
#' @param nsimenv An integer. Number of simulattion used to compute the envelope.
#' @param ncores An integer. The number of paralallel cores to use (default: 4)
#' 
#' @importFrom parallel makeCluster clusterEvalQ parLapply stopCluster
#' 
#' @return A named list. \code{Lmean} is the average L RSA function, \code{dmax} is the envelope maximum distance to the mean
#' 
#' @export
computeSetLEnvelope <- function(simulations,
                                nsimavg = length(simulations) / 2,
                                nsimenv = length(simulations) / 2,
                                ncores = parallel::detectCores() - 1 ){
  
  # Parallel
  if (ncores > 1) {
    cl <- parallel::makeCluster(ncores, type = "SOCK")
  }
  
  # Extract spp from simulations
  sims <- lapply(simulations,FUN = function(x){return(x[[1]])})
  
  ## Parallel execution
  if (ncores > 1) {
    
    # load spatstat in every thread
    parallel::clusterEvalQ(cl, {library(spatstat); NULL})
    
    # Compute Average L function 
    LTransAvg <- Reduce("+",
                        parallel::parLapply(cl,sims[1:nsimavg],
                               fun = function(x){return((K3est(x,rmax = 1250,nrval = 128)$trans/(2/3*2*pi))^(1/3))}))/nsimavg
    
    # Compute Max diff over simenv set
    dif <- max(unlist(parallel::parLapply(cl,sims[(nsimavg+1):(nsimavg+nsimenv)],
                             fun = function(x){return(max((K3est(x,rmax=1250,nrval=128)$trans/(2/3*2*pi))^(1/3)-LTransAvg))})
              )  )
    
    parallel::stopCluster(cl)
  }
  #Single threaded
  else{
    # Compute Average L function 
    LTransAvg <- Reduce("+",
                        lapply(sims[1:nsimavg],
                               FUN = function(x){return((K3est(x,rmax=1250,nrval=128)$trans/(2/3*2*pi))^(1/3))}))/nsimavg
    
    # Compute Max diff over simenv set
    dif <- max(unlist(lapply(sims[(nsimavg+1):(nsimavg+nsimenv)],
                             FUN = function(x){return(max((K3est(x,rmax=1250,nrval=128)$trans/(2/3*2*pi))^(1/3)-LTransAvg))})
    )  )
  }
  
  return(list(Lmean = LTransAvg, dmax = dif ))
}

#' plotSppLEnvelope
#' 
#' Plot a L-envelope for a single sample
#' 
#' Given an envelope computed by \link{computeSampleLEnvelope} creates a plot depicting 
#' CSR theoretical model, RSA based on simulations, its envelope and the original SPP
#' 
#' @param RSALEnvelope An envelope, as returned by \code{\link{computeSppLEnvelope}}
#' @param name A string. Plot title
#' @param lw A numeric value. Plot line width
#' 
#' @return Envelope plot
#' 
#' @export
plotSppLEnvelope <- function(RSALEnvelope, name = "Sample", lw = 6){
  
  # Set plot params
  #par(mar = c(5, 6, 4, 2))
  # Create ggplot + ribbons
  plotData <- as.data.frame(RSALEnvelope)
  plotData$lo[plotData$lo < 0] <- 0
  plot <- ggplot2::ggplot( plotData , aes(x = r) ) +
    #cowplot::theme_cowplot() +
    ggplot2::xlab("Distance (nm)") +
    ggplot2::ylab("L(d)") +
    ggplot2::geom_ribbon(mapping = aes(ymin = lo, ymax = hi, fill = "RSA"), alpha = 0.20, show.legend = F) +
    ggplot2::geom_line(mapping = aes( y = mmean, color = "RSA" ) , size = 1.75 ) +
    ggplot2::geom_line( mapping = aes( y = r, color = "CSR"), size = 1.75) +
    ggplot2::geom_line( mapping = aes( y = obs, color = "Observed"), size = 1.75) +
    ggplot2::scale_colour_manual(guide_legend(title = ""), 
                                 values = c("CSR" = "red" , "RSA" = "darkgreen", "Observed" = "blue4" )) +
    ggplot2::scale_fill_manual(values = c("RSA" = "darkgreen")) +
    ggplot2::theme(legend.position="bottom", text = element_text(family="arial",size = 16),
                   axis.text = element_text(family="arial",size = 14),
                   legend.text = element_text(family="arial",size = 16), 
                   legend.background =  element_rect(colour = "black"),
                   plot.margin=unit(c(1,1,1,3),"cm"))
  
  return(plot)
}

#' plotSetLEnvelope
#' 
#' Plot a L-envelope for a set of SPP
#' 
#' Given the average L function of the layer and the maximum difference (computed by \link{computeLayerLEnvelope })
#' plots the CSR theoretical model, RSA simulation and its envelope, each sample from the layer and the average.
#' 
#' @param processes A list of SPPs (Created by \code{\link{createSampleSPP}} )
#' 
#' @param simAvgLTrans Envelope as returned by \link{computesetLEnvelope}
#' @param name A string. Plot title
#' @param lw A numeric value. Plot line width
#' 
#' @return A plot
#' 
#' @export
plotSetLEnvelope <- function(processes , RSAsetLEnvelope, name="Sample", lw = 6 ){
  
  # Set margins
  # par(mar = c(5, 6, 4, 2)) 
  
  # Create K Function (SPPs) for each process
  samplesK <- lapply(processes,function(x){
    k <- K3est(x, rmax = 1250, nrval = 128)
    # Update K
    k$trans <- (k$trans/(2/3*2*pi)) ^ (1/3)
    return(k)
  })
  
  # Compute layer average trans
  LTransAvg <- Reduce("+", samplesK)/length(samplesK)
  
  # Envelope
  lo <- RSAsetLEnvelope$Lmean - RSAsetLEnvelope$dmax
  lo[lo < 0] <- 0
  hi <- RSAsetLEnvelope$Lmean + RSAsetLEnvelope$dmax
  
  
  # Create ggplot + ribbons
  plotData <- data.frame(x = samplesK[[1]]$r, lo = lo, hi = hi, 
                         rsa = RSAsetLEnvelope$Lmean, layer = LTransAvg$trans)
  plot <- ggplot2::ggplot( plotData, aes(x = x) ) +
    #cowplot::theme_cowplot() +
    ggplot2::xlab("Distance (nm)") +
    ggplot2::ylab("L(d)") +
    ggplot2::geom_ribbon(mapping = aes(ymin = lo, ymax = hi, fill = "RSA"), alpha = 0.20, show.legend = F) +
    ggplot2::geom_line(mapping = aes( y = rsa, color = "RSA" ) , size = 1.75 ) +
    ggplot2::geom_line( mapping = aes( y = x, color = "CSR"), size = 1.75) +
    ggplot2::geom_line( mapping = aes( y = layer, color = "Observed"), size = 1.75) +
    ggplot2::scale_colour_manual(guide_legend(title = ""), 
                        values = c("CSR" = "red" , "RSA" = "darkgreen", "Observed" = "blue4" )) +
    ggplot2::scale_fill_manual(values = c("RSA" = "darkgreen")) +
    ggplot2::theme(legend.position="bottom", text = element_text(family="arial",size = 16),
          axis.text = element_text(family="arial",size = 14),
          legend.text = element_text(family="arial",size = 16), 
          legend.background =  element_rect(colour = "black"),
          plot.margin=unit(c(1,1,1,3),"cm"))

  # PLOT individual samples as dotted lines (FIXME can be done faster)
  for(i in 1:length(samplesK)){
    plot <- plot + ggplot2::geom_line( data = as.data.frame(samplesK[[i]]),
                              mapping = aes(x = r, y = trans ), 
                              size = 1, linetype = "dashed", color = "blue")
  }
  return(plot)
}
