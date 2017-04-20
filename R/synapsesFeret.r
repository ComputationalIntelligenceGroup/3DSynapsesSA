#' 3DSynapses Feret's diameter functions
#'
#' Fit and plot functions for Feret's Diameters
#'
#' @author Laura Antón-Sánchez

#library(MASS)

#' lgFeretFit
#' 
#' Feret's diameter fit
#' 
#' Fit the given vector of Feret's diameters to a log-normal distribution
#' 
#' @param diameters A numeric vector of ferets diamters
#' 
#' @return Named list with: feret-> original data, ml -> log mean, sdl -> log-standard-dev. htest -> ks test result
#'
#'@export
lgFeretFit <- function (diameters) {
  
  # Check diameters is numeric
  if(!is.numeric(diameters))
    stop("Specified diameters are not numeric")
  # Check positiveness
  else if(any(diameters < 0))
    stop("Diameters should be non negative")
  
  # Fir lognormal distribution to the given sample. Output: meanlog and stdlog.
  fit <- fitdistr(diameters,"log-normal") 
  ml <- fit$estimate[1]
  sdl <- fit$estimate[2]
  
  # Goodness of fit tests
  ks <- ks.test(diameters,"plnorm", meanlog=ml, sdlog=sdl)
  
  # Return list (data vector, mean log, std log, p-test)
  return(list(feret=diameters,ml=ml,sdl=sdl,htest=ks))
}

##
# Feret Plot
##

#' lgFeretPlot
#' 
#' Plot log-normal Feret fit
#' 
#' Plots the log-normal distribution fitted from a vector of ferets diameters 
#' 
#' @param feretDiams A numeric vector of Feret's diameters
#' @param ml Log-normal log mean
#' @param sdl Log-normal log standard deviation
#' @param histmin minimum value for the x axis
#' @param histmax maximum value for the x axis
#' @param color histogram bars color
#' @param name plot nmae (title)
#' 
#' @return 2D plot (histogram + curve)
#'
#' @export
lgFeretPlot <- function(feretDiams,ml,sdl,histmin=0, histmax=1200,color="skyblue",name="Feret's diameter histogram"){
  
  # Check diameters is numeric
  if(!is.numeric(feretDiams))
    stop("Specified diameters are not numeric")
  # Check positiveness
  else if(any(feretDiams < 0))
    stop("Diameters should be non negative")
  else if(!(is.numeric(ml)) || !(is.numeric(sdl)) )
    stop("Parameters should be numeric")
  else if((ml<0) || (sdl)<0)
    stop("Parameters should be non negative")
  
  
  ggplot(data.frame(x = feretDiams) , aes( x )) + 
    geom_histogram( aes( y = ..density.. , colour = "b" ), fill = "blue", alpha = .2 ) +
    stat_function(data = data.frame(x = range(feretDiams)), mapping = aes(x , colour = "r"),
                                    fun = dlnorm, size = 1, args = list(ml,sdl)) +
    labs( title = "Histogram for Feret's diameters") +
    labs(x = "Diameter", y = "Density") +
    theme(text = element_text(size = 16, family = "Verdana"),  legend.position = "bottom" ) +
    theme(legend.background = element_blank() ) +
    scale_color_manual( name = "", values = c("r" = "red", "b" = "darkblue"),
                        labels = c("r" = sprintf("Log-normal(%.2f,%.2f)",ml,sdl),
                                   "b" = "Observed"))
#par(mar=c(5, 6, 4, 4))
#   b<-seq(histmin,histmax,by=50)
#   feretDiams<-feretDiams[feretDiams<histmax]
#   hist(feretDiams,breaks=b,prob=TRUE,col=color,xlab="Feret's diameter (nm)",main=name,family = "Verdana",cex=2,cex.axis=2,cex.lab=2, cex.main=2)
#   #ylim <- max(c(dlnorm(seq(1,1200,0.5), meanlog=ml, sdlog=sdl),aux$density))
#   curve(dlnorm(x, meanlog=ml, sdlog=sdl), add=TRUE,col="red",lwd=4,ylim=c(0,ylim))
# 
#   # Set Legend (top - right corner)
#   limits <- par()$usr
#   legend(x=(limits[2]-limits[1])*0.65, y=(limits[4]-limits[3])*0.95, 
#          legend="Log-normal\ndistribution", lty = 1, col=c("red"),lwd=4 , cex=1.5)
}
