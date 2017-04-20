#' 3DSynapses Simulation functions
#'
#' Functions to simulate synapses 3D samples
#'
#' @author Laura Antón-Sánchez

#library(spatstat)
#library(foreach)
#library(doMC)

# Depends: synapsesPlot, synapsesFeret

#' RSA3DSimulation
#' 
#' Creates a RSA simulation
#'
#' Create a RSA simulation with n synapses, sampling the feret's diameter of each sample by calling radSamplerFunc.
#' As convergence is not guaranteed, a stop mechanism based on the number of iterations is provided.
#' 
#' @param n An integer. The number of synapses to generate
#' @param box An object of class "box3". Three dimensional box, as given by box3()
#' @param diamSamplerFunc A function that get one parameter: the number of values to generate. Returns diameters
#' @param maxIter A integer. Maximum number of iterations before failure
#' 
#' @return A matrix with 4 columns (X,Y,Z,R) or null if failure
#' 
#' @export
RSA3DSimulation <- function(n,box,diamSamplerFunc,maxIter=100){
  
  # (x,y,z,r) coordinates of each sphere. Vector declared in advance to avoid memory realloc
  coords <- matrix(NA,ncol=4,nrow=n,byrow = T)
  colnames(coords) <- c("X","Y","Z","R")
  
  # Generate n-validpoints each time to reduce runif calls
  validPoints <-0
  iter<-0
  while(validPoints<n && iter<maxIter)
  {
    # Avoid infinite loop
    iter<-iter+1
    
    toGenerate <- n-validPoints
    # Worst case: at least as good as the previous implementation
    coordCandidates <- runifpoint3(toGenerate,box)
    diamCandidates <- diamSamplerFunc(toGenerate) # Should return a numeric vector 
    
    # Check each candidate against validPoints
    for( i in 1:toGenerate){
      #intersect <- F
      #j <- 1
      # Check interesection against valid points
      
      #while(!intersect && j<=validPoints ){
      # Compute distance and check if they interesct
      #dist <- sqrt(sum((coords(coordCandidates)[i,] - coords[j,1:3])^2))
      #  dist <- dist(rbind(coords(coordCandidates)[i,], coords[j,1:3]))
      #  intersect <- as.logical( dist < ((coords[j,4] + radCandidates[i])/2) )
      #  j<-j+1
      #}
      if(validPoints>1){
        dist <- sqrt(rowSums(sweep(coords[1:validPoints,1:3],2,as.numeric(coords(coordCandidates)[i,]))^2)) - ((coords[1:validPoints,4] + diamCandidates[i])/2)
        intersect <- any(dist < 0)
        #showSim(coords[1:validPoints,1], coords[1:validPoints,2],coords[1:validPoints,3],coords[1:validPoints,4],name="sim",samePlot=FALSE,color='violet')
      }else if (validPoints==1) {
        dist <- sqrt(sum((coords[1,1:3] - as.numeric(coords(coordCandidates)[i,]))^2)) - ((coords[1,4] + diamCandidates[i])/2)  
        intersect <- any(dist < 0)
      }
      else{
        intersect <- F
      }
      # If intersect is still false -> insert in the set of valid points
      if(!intersect){
        validPoints <- validPoints +1
        coords[validPoints,] <- as.numeric(c(coords(coordCandidates)[i,],diamCandidates[i]))
      }
    }
  }
  if(validPoints<n){
    return (NULL)
  }else{
    # Return generated coords
    return(coords)
  }
}

#Si singleSample=TRUE crea un RSA ajustando una lognormal utilizando solo los diametros de SOLO esa muestra en concreto

#' RSA3DLognormFromData
#' 
#' Creates a RSA simulation with log-normal feret diameters
#'
#' Create a RSA simulation replicating the given sample or using the parameters given.
#' 
#' Domain is always cubic. If \code{volume} is not provided, sample volume is used. Same applies to parameters.
#' If \code{replicated} is true, number of points is calculated as the product of volume and lambdaRPP, if not,
#' the simulation will have the same number of points as the sample.
#' 
#' @param feretDiam A numeric vector. Sample feret diameters
#' @param realSample A dataframe. Spatial coordinates (X,Y,Z) of the synapses 
#' @param plot A boolean value. If true, simulation is plotted before return
#' @param singleSample A boolean value. If true, "Feret" colum from real sample is used insted of feretDiam
#' @param replicated A boolean value. If true, number of points is computed as the \code{volume * lambdaRPP}
#' @param lambdaRPP A numeric value. Simulation Intesity (synaptic density) parameter
#' @param volume A numeric value. Simulation (cubic) volume.
#' @param color Synapses plot color. Only applies if \code{plot} is True
#' @param mu A numeric value. Log mean parameter for the Feret's diameter log-normal distribution
#' @param sdl A numeric value. Log std parameter for the Feret's diameter log-normal distribution
#' @param maxIter An integer. Maximum number of trials before failure.
#' 
#' @return A named list: spp <- 3D spatial process, diam <- ferets diameters, ml,sdl <- feret's diams params
#' 
#' @export
RSA3DLognormFromData <- function (feretDiam, realSample, plot=FALSE, singleSample=FALSE, 
                                  replicated=FALSE, lambdaRPP=0, volume=0,color="violet",
                                  mu=-1,sigma=-1,maxIter=10) {
  
  s <- realSample
  #
  # BOUNDING BOX
  #
  
  # If volume is 0, Bounding box is set to fit provided data
  if ( volume == 0) {
    box <- box3(xrange = c(min(s$X), max(s$X)),yrange = c(min(s$Y), max(s$Y)),zrange = c(min(s$Z), max(s$Z)))
  }
  # If not, bounding box is a cube with given volume
  else {
    l <- (volume^(1/3))
    box <- box3(xrange = c(0, l),yrange = c(0, l),zrange = c(0, l))
  }
  
  #
  # LOGNORMAL PARAMETERS
  #
  
  # Lognormal Params can be given and not obtained from data
  if ((mu!=-1) && (sigma!=-1)) { 
    ml <- mu
    sdl <- sigma
  }
  else {
    # Different behaviour for single sample and for layer-oriented process
    if (singleSample==FALSE) {
      # In this case we have feret diameters from every sample of the same layer (but not the sample itself)
      fit <- fitdistr(feretDiam,"log-normal") 
      ml <- fit$estimate[1]
      sdl <- fit$estimate[2]
    }
    # We use the feret diameter from the sample
    else {
      fit <- fitdistr(s$Feret,"log-normal") 
      ml <- fit$estimate[1]
      sdl <- fit$estimate[2]
    }
  }
  
  #
  # NUMBER OF POINTS
  #
  if (replicated) { 
    # Number of points is calculated based on the volume and the layer density
    numpts <-  round(volume(box)*lambdaRPP)
  }
  else { 
    #Same number of points as the original sample
    numpts <- nrow(s) 
  }
  
  #
  # Call simulation procedure
  #
  
  # At most the sample is repeated maxIter times 
  iter <- 0
  simSample <- NULL
  while(is.null(simSample) && iter<maxIter){
    simSample <- RSA3DSimulation(numpts,box,function(n){rlnorm(n,meanlog = ml,sdlog=sdl)})
    iter<-iter+1
  }
  
  # Return parameters and simulation
  if(!is.null(simSample)){
    spp <- pp3(simSample[,1], simSample[,2],simSample[,3],box)
    diam <- simSample[,4]
  }
  else{
    spp <- NULL
    diam <- NULL
  }
  
  # Plot if needed
  if (plot == TRUE) {
    showSim(simSample[,1], simSample[,2],simSample[,3],diam,name="sim",samePlot=FALSE,color=color)
  }
  
  return (list(spp, diam, ml, sdl))
}


#' nRSASimulations
#' 
#' Creates a number of RSA simulations
#' 
#' Creates a number of RSA simulations (replicated) and returns them in a list. \link{RSA3DLognormFromData}
#' 
#' @param numSimulation An integer. Number of simulations to create
#' @param lambdaRPP A numeric value. Simulations intensity (synaptic density)
#' @param volume A numeric value. Simulation box-volume
#' @param mu A numeric value. Log mean for the log-normal Feret's diameters distribution
#' @param sigma A numeric value. Log std for the log-normal Feret's diameters distribution
#' @param parallel A boolean value. If true, simulations are parallelized
#' 
#' @return List of simulations as returned by \link{RSA3DLognormFromData}
#' 
#' @export
nRSASimulations <- function(numSimulations,lambdaRPP=1E-9, volume=150E9, mu=6E-3 , sigma=0.5, parallel=T){
  
  # Set number of procs (if parallel)
  if(!parallel){
    ncores<-1
  }
  else{
    ncores <- detectCores()
    if(is.na(ncores))
      ncores<-1
    else
      ncores<-ncores-1
  }
  
  if(ncores>1){
    cl <- makeCluster(ncores, type = "SOCK")
    registerDoParallel(cl,ncores)
  }
  
  # Run simulations
  sims <- foreach(i=1:numSimulations) %dopar% {
    RSA3DLognormFromData(NULL,
                         NULL, # Since its replicated and not a single sample this parameter is not used
                         plot=F, 
                         singleSample=F, 
                         replicated=T,
                         lambdaRPP=lambdaRPP, 
                         volume=volume,
                         mu=mu,
                         sigma=sigma)
  }
  
  # Stop cluster
  if(ncores>1){
    stopImplicitCluster()
    stopCluster(cl)
  }
  
  # Simulations are returned in a list
  return(sims)
}

#' singleSampleSimulations
#' 
#' Creates RSA simulations replicating a single sample
#' 
#' Creates a number of RSA simulations (replicated) computing the parameters from the given sample
#' and returns them in a list. \link{nRSASimulations}
#' 
#' @param realSample A dataframe with columns X,Y,Z,Feret,Sample.no. and Layer. Real data samples
#' @param sampleNo An integer. Id of the sample to be replicated
#' @param layer A roman number as string. The layer of the selected sample
#' @param numSimulation An integer. Number of simulations to create
#' @param mu A numeric value. Log mean for the log-normal Feret's diameters distribution
#' @param sigma A numeric value. Log std for the log-normal Feret's diameters distribution
#' @param parallel A boolean value. If true, simulations are parallelized
#' 
#' @return List of simulations as returned by \link{RSA3DLognormFromData}
#' 
#' @export
singleSampleSimulations <- function(realSamples,sampleNo, layer ,numSimulations,ml=-1,sdl=-1,parallel=T){
  
  #Select only data from same layer
  selData <- realSamples[realSamples[,"Layer"]==layer,]
  
  #Compute box volume for each sample in the selected data
  #
  # FIXME: should add/substract rad. to each coord?
  #
  #limits <- aggregate(selData[,c("X","Y","Z")],by=list(sample=selData[,"Sample.no."]),FUN=function(x){return(max(as.numeric(x))-min(as.numeric(x)))})
  #volumes <- cbind(limits[,1],rowProds(as.matrix(limits[,2:4])))
  volumes <- calculateSampleVolumes(selData)
  
  # Aggregate points -> compute lambda
  # Exclude selected sample from the average 
  lambdai <- (count(selData[,"Sample.no."]!=sampleNo)/sum(volumes[ volumes[,1]!=sampleNo ,3 ]))
  
  # Compute box volume for selected sample
  selsampleVol <- sum(volumes[ volumes[,1]==sampleNo ,2 ])
  
  # Set number of procs
  
  # Set number of procs (if parallel)
  if(!parallel){
    ncores<-1
  }
  else{
    ncores <- detectCores()
    if(is.na(ncores))
      ncores<-1
    else
      ncores<-ncores-1
    #registerDoMC(detectCores()-1)}
  }
  
  if(ncores>1)
  {
    cl <- makeCluster(ncores, type = "SOCK")
    registerDoParallel(cl,ncores)
  }
  
  #if(parallel){registerDoMC(detectCores()-1)}
  #else{registerDoMC(1)}
  
  # Run simulations
  sims <- foreach(i=1:numSimulations) %dopar% {
    RSA3DLognormFromData(selData[,"Feret"], 
                         NULL, # Since its replicated and not a single sample this parameter is not used
                         plot=F, 
                         singleSample=F, 
                         replicated=T,
                         lambdaRPP=lambdai, 
                         volume=selsampleVol,
                         mu=ml,
                         sigma=sdl)
  }
  
  if(ncores>1){
    stopImplicitCluster()
    stopCluster(cl)
  }
  
  # Simulations are returned in a list
  return(sims)
}

#' layerSimulations
#' 
#' Creates RSA simulations replicating a single sample
#' 
#' Creates a number of RSA simulations (replicated) computing the parameters from the given layer
#' and returns them in a list. \link{nRSASimulations}
#' 
#' @param realSample A dataframe with columns X,Y,Z,Feret,Sample.no. and Layer. Real data samples
#' @param layer A roman number as string. The layer of the selected sample
#' @param numSimulation An integer. Number of simulations to create
#' @param mu A numeric value. Log mean for the log-normal Feret's diameters distribution
#' @param sigma A numeric value. Log std for the log-normal Feret's diameters distribution
#' @param parallel A boolean value. If true, simulations are parallelized
#' 
#' @return List of simulations as returned by \link{RSA3DLognormFromData}
#' 
#' @export
layerSimulations <- function(realSamples, layer, numSimulations,ml=-1,sdl=-1,parallel=T){
  
  #Select only data from same layer
  selData <- realSamples[realSamples[,"Layer"]==layer,]
  
  #Compute box volume for each sample in the selected data
  #limits <- aggregate(selData[,c("X","Y","Z")],by=list(sample=selData[,"Sample.no."]),FUN=function(x){return(max(as.numeric(x))-min(as.numeric(x)))})
  #volumes <- cbind(limits[,1],rowProds(as.matrix(limits[,2:4])))
  volumes <- calculateSampleVolumes(selData)
  
  # Sample avg. lambda
  lambdai <- nrow(selData)/sum(volumes[,2]) #* 1E9
  
  # Average layer volumes
  avgvol <- sum(volumes[,2])/nrow(volumes)
  
  # Set number of procs (if parallel)
  if(!parallel){
    ncores<-1
  }
  else{
    ncores <- detectCores()
    if(is.na(ncores))
      ncores<-1
    else
      ncores<-ncores-1
    #registerDoMC(detectCores()-1)}
  }
  
  if(ncores>1)
  {
    cl <- makeCluster(ncores, type = "SOCK")
    registerDoParallel(cl,ncores)
  }
  
  # Run simulations
  sims <- foreach(i=1:numSimulations) %dopar% {
    RSA3DLognormFromData(selData[,"Feret"], 
                         NULL, # Since its replicated and not a single sample this parameter is not used
                         plot=F, 
                         singleSample=F, 
                         replicated=T,
                         lambdaRPP=lambdai, 
                         volume=avgvol,
                         mu=ml,
                         sigma=sdl)
  }
  
  if(ncores>1){
    stopImplicitCluster()
    stopCluster(cl)
  }
  
  return(sims)
}

