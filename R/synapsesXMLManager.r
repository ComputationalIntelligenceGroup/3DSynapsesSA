#' Output XML files manager
#'
#' Functions to create / read XML files as output/input files for synapsesSA
#'
#' @author Laura Antón-Sánchez Luis Rodríguez-Luján


#' createMetaNode
#' 
#' Auxiliar function to create XML metadata nodes
#' 
#' @param tree metadata tree
#' @param value Value to put in tag
#' @param name Node name
#' 
#' @return XML Node
createMetaNode <- function(tree,value,name){
  if(!is.null(value))
    return(newXMLNode(name,value,parent=tree))
  else
    return(NULL)
}

#' createFileNode
#'
#' Auxiliar function to create a file "element" of the list of inputs
#' 
#' @param type type of IO node (file or manual so far)
#' @param filename file name (only for type file)
#' @param extension Despite of the name, this is actually file mimetype
#' @param description Description of the file contents
#' @param parent Parent XML node
#' @param sample Sample  ID number
#' @param layer Sample layer
#'
#'@return XML IO node
#'
createIONode <- function(type=c("file","manual"),filename=NULL,
                         extension=NULL,description=NULL,parent=NULL,
                         sample=NULL,layer=NULL){
  node <- switch(type,
    file= newXMLNode("file",attrs=c(filename=filename,
                                    mimetype=extension,
                                    layer=layer,
                                    sample=sample,
                                    description=description),parent=parent),
    manual=newXMLNode("manual",parent=parent)
  )
  return(node)
}

#' synapsesSAMetaheader
#' 
#' Creates an XML with common metadata header for serveral outputs
#' 
#' @param inputFiles List of files used as input
#' @param modeler Modeler name
#' @param application Application name
#' @param version Application version
#' @param execution Type of execution
#' @param output Output file list
#' 
#' @return XML tree (metaHeader)
#' 
#' @export
synapsesSAMetaheader<-function(input=NULL,
                     modeler=NULL,
                     application="synapsesSA",
                     version="1.2",
                     execution=NULL,
                     output=NULL){
  
  # Create root metadata tree
  metaTree <- newXMLNode("metaHeader")
  
  # Create application node
  createMetaNode(metaTree,application,"application")
                     
  # Create version node
  createMetaNode(metaTree,version,"version")
  
  # Execution
  createMetaNode(metaTree,execution,"execution")
  
  # Modeler
  createMetaNode(metaTree,modeler,"modeler")
  
  # outputFiles (We only use files, but it can be... idk... something else)
  if(!is.null(output) && (length(output)>0) ){
    outputNode <- newXMLNode("output",parent=metaTree)
    for(i in 1:length(output)){
      createIONode(type=output[[i]]$type,
                   filename=output[[i]]$filename,
                   extension=output[[i]]$extension,
                   description=output[[i]]$description,parent=outputNode)
    }
  }
  
  # Same for the input
  if(!is.null(input) && (length(input)>0) ){
    inputNode <- newXMLNode("input",parent=metaTree)
    for(i in 1:length(input)){
      if(input[[i]]$sample==0) input[[i]]$sample <- "In file" # Input sample = 0 means in file.
      createIONode(type=input[[i]]$type,
                   filename=input[[i]]$filename,
                   extension=input[[i]]$extension,
                   description=input[[i]]$description,
                   sample=input[[i]]$sample,
                   layer=input[[i]]$layer,
                   parent=inputNode)
    }
  }
  
  return(metaTree)
}

#' modelParametersOutput
#' 
#' Creates an XML file that contains parameters and other metadata for the RSA model.
#' 
#' 
#' @param mu Log-mean of the Feret's diameters lognormal fit
#' @param sigma Log-std of the feret's diameters lognormal fit
#' @param lambda Synaptic intensity
#' @param sample Sample number
#' @param layer Sample yaer
#' @param inputFiles Files used as input
#' @param application App. name
#' @param version Version number
#' @param output Output files list
#' 
#' @return XML Model Node
#'
#' @export
modelParametersOutput <- function(mu,sigma,lambda,
                                  sample=NULL,
                                  layer=NULL,
                                  inputFiles=NULL,
                                  modeler=NULL,
                                  application="synapsesSA",
                                  version="1.2",
                                  output=NULL){
  
  # Create root
  tree <- newXMLNode("synapsesSA")
  
  # Create metadata header
  header <-synapsesSAMetaheader(input=inputFiles,
                                modeler=modeler,
                                application=application,
                                version=version,
                                execution="model fit",
                                output=output)
  # Add header to root
  XML::addChildren(tree,header)
  
    
    # Add model
    model<-newXMLNode("model",attrs=c(name="RSA", description="Random sampling absorption"),parent=tree)
  
      # Add selector parameters
      # layer
      if(!is.null(layer))
        newXMLNode("parameter",layer,
             attrs=c(type="roman",name="layer",description="Layers in the model"),
             parent=model)
  
      # sample number
      if(!is.null(sample))
        newXMLNode("parameter",sample,
               attrs=c(type="integer",name="sample",description="Number of samples"),
               parent=model)
  
      # Add model parameters
      # mu
      newXMLNode("parameter",mu,
                   attrs=c(type="double",name="mu",description="Feret Diameter log-normal fit Mu parameter (log-mean)"),
                 parent=model)
  
      # mu
      newXMLNode("parameter",sigma,
               attrs=c(type="double",name="sigma",description="Feret Diameter log-normal fit sigma parameter (log-std)"),
               parent=model)
  
      # Lambda
      newXMLNode("parameter",lambda,
               attrs=c(type="double",name="lambda",description="RSA intensity parameter - synapses per volume unit (um)"),
               parent=model)

  return(newXMLDoc(tree))
}


#' modelParametersOutput
#' 
#' Creates an XML file that contains parameters and other metadata for the RSA simulation.
#' 
#' @param mu Log-mean of the Feret's diameters lognormal fit
#' @param sigma Log-std of the feret's diameters lognormal fit
#' @param lambda Synaptic intensity
#' @param sample Sample number
#' @param nsims Number of simulations
#' @param volume Simulated volume
#' @param layer Sample yaer
#' @param inputFiles Files used as input
#' @param application App. name
#' @param version Version number
#' @param output Output files list
#' 
#' @return XML node
#'
#' @export
simulationParametersOutput <- function(mu,sigma,lambda,
                                  sample=NULL,
                                  layer=NULL,
                                  nsims=NULL,
                                  volume=NULL,
                                  inputFiles=NULL,
                                  modelName = NULL,
                                  modeler=NULL,
                                  application="synapsesSA",
                                  version="1.2",
                                  output=NULL){
  
  # Create root
  tree <- newXMLNode("synapsesSA")
  
  # Create metadata header
  header <-synapsesSAMetaheader(input=inputFiles,
                                modeler=modeler,
                                application=application,
                                version=version,
                                execution="RSA simulation",
                                output=output)
  # Add header to root
  XML::addChildren(tree,header)
  
  
  # Add model
  model<-newXMLNode("simulation",attrs=c(name="RSA", description="random sampling absorption"),parent=tree)
  
  # Add selector parameters
  # layer
  if(!is.null(layer))
    newXMLNode("parameter",layer,
               attrs=c(type="roman",name="layer",description="Layers value"),
               parent=model)
  
  # sample number
  if(!is.null(sample))
    newXMLNode("parameter",sample,
               attrs=c(type="integer",name="sample",description="Number of samples in the model"),
               parent=model)
  
  if(!is.null(nsims))
    newXMLNode("parameter",nsims,
             attrs=c(type="integer",name="nsims",description="Number of simulations"),
             parent=model)
  
  if(!is.null(modelName))
    newXMLNode("model",modelName,
               attrs=c(type="string",name="model",description="Model name"),
               parent=model)
  
  
  newXMLNode("parameter",volume,
             attrs=c(type="double",name="volume",description="Simulation volume in micrometers"),
             parent=model)
  
  # Add model parameters
  # mu
  newXMLNode("parameter",mu,
             attrs=c(type="double",name="mu",description="Feret Diameter log-normal fit Mu parameter (log-mean)"),
             parent=model)
  
  # mu
  newXMLNode("parameter",sigma,
             attrs=c(type="double",name="sigma",description="Feret Diameter log-normal fit sigma parameter (log-std)"),
             parent=model)
  
  # Lambda
  newXMLNode("parameter",lambda,
             attrs=c(type="double",name="lambda",description="RSA intensity parameter - synapses per volume unit (um)"),
             parent=model)
  
  return(newXMLDoc(tree))
}