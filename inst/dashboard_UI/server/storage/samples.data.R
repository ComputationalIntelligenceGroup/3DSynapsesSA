#' samples.data
#' 
#' Auxiliar functions for samples.data table

synapsesSA_addSample <- function(rv, name, number, filename, extension ){
  
  df <- rv$samples.data$df 
  
  df[ nrow(df) + 1, ] <- list( sample = name,
                                   samplenum = number,
                                   filename = filename,
                                   extension = extension)
  
  rv$samples.data$df <- df
}

sampleExists <- function(rv, name, number){
  return( any(rv$samples.data$df$sample == name & rv$samples.data$df$samplenum == number) )
}