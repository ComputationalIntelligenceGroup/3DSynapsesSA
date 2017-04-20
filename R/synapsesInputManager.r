#' Input files manager
#'
#' Functions to read CSV / XLS(X) Files. Includes error checking
#'
#' @author Laura Antón-Sánchez Luis Rodríguez-Luján
#' 

#' readSynapsesCSV
#' 
#' Read synapses from a CSV file, run checks and validate its contents
#' 
#' @param filepath CSV local file path
#' @param fieldSeparator CSV file field separator
#' @param decimalPoint CSV file numeric decimal point character
#' 
#' @return named list (data, error (T/F), errorCode, errorDescription)
#' 
#' @export
readSynapsesCSV <- function(filepath,
                            fieldSeparator=",",
                            decimalPoint=".",
                            overwriteName = F,
                            samplename = NULL,
                            samplenum=NULL,
                            layer=NULL,
                            CSVcolNames=c("Feret","Type","X","Y","Z","Sample.no.","Layer","Sample"),
                            unifiedColNames=c("feret","type","x","y","z","samplenum","layer","sample")){
  
  # Read CSV Table
  tmpTable <- read.csv(filepath,
                       header = T, 
                       sep = fieldSeparator,
                       dec = decimalPoint,
                       stringsAsFactors = F)
  
  # Unify column names
  colnames(tmpTable) <- unifyColnames(colnames(tmpTable),CSVcolNames,unifiedColNames)
  
  # If type is not present. assume Symmetric
  if (!("type" %in% colnames(tmpTable))) {
    tmpTable <- cbind.data.frame(tmpTable,type = rep("symmetric",nrow(tmpTable)))
  }
  
  # If type is numeric, substitute for its string representation
  tmpTable[tmpTable[,"type"] == 1,"type"] <- "symmetric"
  tmpTable[tmpTable[,"type"] == 2,"type"] <- "asymmetric"
  # All lower
  tmpTable[,"type"] <- tolower(tmpTable[,"type"])
  
  # If sample parameter is not null, change sample column
  if (!is.null(samplenum) && samplenum != 0) {
    if (!("samplenum" %in% colnames(tmpTable))) {
      tmpTable <- cbind.data.frame(tmpTable, samplenum = rep(samplenum,nrow(tmpTable)))
    }
    else
      tmpTable[,"samplenum"] <- samplenum
  }
  
  # If sample parameter is not null, change sample column
  if (!is.null(samplename) && samplename != "") {
    if (!("sample" %in% colnames(tmpTable))) {
      tmpTable <- cbind.data.frame(tmpTable, sample = rep(samplename,nrow(tmpTable)))
    }
    else if (overwriteName)
      tmpTable[,"sample"] <- samplename
  }
  # Trim data
  tmpTable$sample <- gsub("^\\s+|\\s+$", "", tmpTable$sample)
  
  # Same procedure for layer
  if (!is.null(layer) && layer != "In file") {
    if (!("layer" %in% colnames(tmpTable))) {
      tmpTable <- cbind.data.frame(tmpTable,layer = rep(layer,nrow(tmpTable)))
      tmpTable[,"layer"] <- as.character(tmpTable[,"layer"])
    }
    else
      tmpTable[,"layer"] <- layer
  }
  
  validation <- validateSynapsesDF(tmpTable)
  validation$data <- tmpTable
  
  return(validation)
}

  

#' readSynapsesXSL
#' 
#' Read synapses from a Excel spreadsheet file, run checks and validate its contents
#' 
#' @param filepath local file path
#' 
#' @return named list (data, error (T/F), errorCode, errorDescription)
#' 
#' @export
readSynapsesXLS <- function(filepath,
                            overwriteName = F,
                            samplename = NULL,
                            samplenum=NULL,
                            layer=NULL,
                            XSLcolNames=c("Feret Diameter","Category","Centroid X","Centroid Y","Centroid Z","Sample no.","Layer","Sample"),
                            unifiedColNames=c("feret","type","x","y","z","samplenum","layer","sample")){
  
  wbData <- NULL
  
  # Assume one or several sheets with a common header
  for (s in excel_sheets(filepath)) {
      
      #Read sheet with header
      tmpData <- na.omit(read_excel(filepath,sheet = s))
      
      # Unify column names
      colnames(tmpData) <- unifyColnames(colnames(tmpData),XSLcolNames,unifiedColNames)
      
      # If sample parameter is not null, change sample column
      if (!is.null(samplenum) && samplenum != 0) {
        if (!("samplenum" %in% colnames(tmpData))) {
          tmpData <- cbind.data.frame(tmpData,samplenum = rep(samplenum,nrow(tmpData)))
        }
        else
          tmpData[,"samplenum"] <- samplenum
      }
      
      # If sample parameter is not null, change sample column
      if (!is.null(samplename) && samplename != "") {
        if (!("sample" %in% colnames(tmpData))) {
          tmpData <- cbind.data.frame(tmpData, sample = rep(samplename,nrow(tmpData)))
        }
        else if (overwriteName)
          tmpData[,"sample"] <- samplename
      }
      tmpData$sample <- gsub("^\\s+|\\s+$", "", tmpData$sample)
      
      # Same procedure for layer
      if (!is.null(layer) && layer != "In file") {
        if (!("layer" %in% colnames(tmpData))) {
          tmpData <- cbind.data.frame(tmpData,layer = rep(layer,nrow(tmpData)))
          tmpData[,"layer"] <- as.character(tmpData[,"layer"])
        }
        else
          tmpData[,"layer"] <- layer
      }
      
      # If type is not present. assume Symmetric
      if (!("type" %in% colnames(tmpData))) {
        tmpData <- cbind.data.frame(tmpData,type = rep("symmetric",nrow(tmpData)))
      }
      
      # If type is numeric, substitute for its string representation
      tmpData[tmpData[,"type"] == 1,"type"] <- "symmetric"
      tmpData[tmpData[,"type"] == 2,"type"] <- "asymmetric"
      # All lower
      tmpData[,"type"] <- tolower(tmpData[,"type"])
      
      # Now. Validate input
      validation <- validateSynapsesDF(tmpData)
      
      # If error -> return Errror
      if (validation$error) {
        validation$data = NULL
        return(validation)
      }
      # If not, append to general table
      else{
        wbData <- rbind.data.frame(wbData,tmpData[,c("sample","samplenum","layer","type","x","y","z","feret")])
      }
  }
  return(list(data = wbData, error = F, errorCode = 0, errorDescription = "Valid"))
}

unifyColnames <- function(colnames,original,unified){
  
  for(i in 1:length(original)){
    colnames[colnames==original[i]]<-unified[i]
  }
  return(colnames)
}


#' validateSynapsesDF
#' 
#' Validate contents of a dataframe cotnaining synapses
#' 
#' @param data Synapses Dataframe
#' 
#' @return list (error (T/F), errorCode, errorDescription)
#'
#'@export
validateSynapsesDF <- function(data){
  
  # Validate columns within dataframe
  columnNames <- checkColumnNames(data)
  if(columnNames$error)
    return(columnNames)
  
  # Validate layer column
  layerCheck <- checkLayerColumn(data)
  if(layerCheck$error)
    return(layerCheck)
  
  # Validate Sample column
  sampleCheck <- checkSampleColumn(data)
  if(sampleCheck$error)
    return(sampleCheck)
  
  # Validate Centroids
  centroidCheck <- checkCentroids(data)
  if(centroidCheck$error)
    return(centroidCheck)
  
  # Validate Feret diameters
  feretCheck <- checkFerets(data)
  if(feretCheck$error)
    return(feretCheck)
  
  # Validate Type
  typeCheck <- checkType(data)
  if(typeCheck$error)
    return(typeCheck)
  
  return (list(error=F,errorCode=0,errorDescription="Valid"))
}

#'checkColumnNames
#'
#'Verify that required columns are present
#'
#'@param data Synapses dataframe
#'
#'@return list(error,errorCode,errorDescription)
checkColumnNames <- function(data){
  
  error <- F
  errorDescription <- NULL
  # Check mandatory columns
  for(col in c("sample","samplenum","layer","x","y","z","feret")){
    if(!(col %in% colnames(data))){
      errorDescription <- paste0("- Missing column: '",col,"'<br>",errorDescription)
      error <- T
    }
    # Check for empty columns
    else if(is.na(data[,col]) || is.null(data[,col])){
      errorMsg <- paste0("- Error in column: '",col,"'<br>",errorDescription)
      error <- T
    }
  }
  
  return(list(error = error, errorCode = 1, errorDescription = errorDescription))
}

checkLayerColumn <- function(data){
  
  # Check Layer format
  error<-F
  errorMsg<-NULL
  if(any(is.na(as.roman(as.character(data[,"layer"]))))){
    errorMsg <- "- Layer should be a roman number<br>"
    error <-T
  }
  return(list(error=error,errorCode=2,errorDescription=errorMsg))
  
}

checkSampleColumn <- function(data){
  # Check column format
  # Sample.no.
  error <- F
  errorMsg <- NULL
  if (!is.numeric(unlist(data[,"samplenum"]))) {
    errorMsg <- "- Data in column Sample.no. should be numeric<br>"
    error <- T
  }
  return(list(error = error, errorCode = 2, errorDescription = errorMsg))
}

checkCentroids <- function(data){
  # Check column format
  # X,Y,Z
  error<-F
  errorMsg<-NULL
  if(!is.numeric(unlist(data[,c("x","y","z")]))){
    errorMsg<-"- Data in columns X,Y,Z should be numeric<br>"
    error <-T
  }
  return(list(error=error,errorCode=3,errorDescription=errorMsg))
}

checkFerets <- function(data){
  # Check column format
  # X,Y,Z,Feret
  error<-F
  errorMsg<-NULL
  if(!is.numeric(unlist(data[,"feret"]))){
    errorMsg<-"- Data in column Feret should be numeric<br>"
    error <-T
  }
  return(list(error=error,errorCode=4,errorDescription=errorMsg))
}

checkType <- function(data){
  # Really needed?
  return(list(error=F,errorCode=0,errorDescription=""))
}

#' Checks for overlapping synapses
#' @export
checkOverlappingSynapses <- function(data){
  
  # Create process box
  box <- spatstat::box3( c( min(data$x - data$feret/2), max(data$x + data$feret/2) ),
                         c( min(data$y - data$feret/2), max(data$y + data$feret/2) ),
                         c( min(data$z - data$feret/2), max(data$z + data$feret/2) ))
  
  # Create pp3
  process <- spatstat::pp3(data$x, data$y, data$z,box)
  distances <- spatstat::pairdist(process)
  
  # Create minimum distance matrix (feret/2 + feret/2 )
  mindist <- matrix(0, ncol = nrow(data), nrow = nrow(data))
  for(i in 1:(nrow(data)-1) ){
    for(j in (i+1):nrow(data) ){
      mindist[i,j] <- (data$feret[i] + data$feret[j])/2
      mindist[j,i] <- (data$feret[i] + data$feret[j])/2
    }
  }
  
  overlapping <- distances < mindist
  
  return(overlapping)
}