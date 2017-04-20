#' 3DSynapses Layer comparison
#'
#' Functions to compare each layer and plot the results
#'
#' @author Laura Antón-Sánchez
#'
 
#library(spatstat)
#library(car) #Levene Test
#library(ggplot2)


#' calculateNearestNeighbor
#' 
#' Computes the distance to the nearest point in each SPP process in the list
#' 
#' Given a list of spatial processes computes the distance to the nearest point in the sample
#' and creates a dataframe with the results
#' 
#' @param sppList A list of Spatial processes (with sample number and layer ID)
#' 
#' @return A dataframe. Columns: Layer, Sample and Dist
#' 
#' @export
calculateNearestNeighbor <- function(sppList){
  ret<-sapply(sppList,function(x){ 
    nndists <- nndist.pp3(x$spp)
    x <- cbind(rep(x$Layer, length(nndists)),rep(x$Sample, length(nndists)),nndists)
    #list(spp=x$spp, Layer=x$Layer, Sample=x$Sample, nndists = nndist(x$spp) )
  })
  
  # Combine matrices
  ret <- Reduce(rbind,ret)
  colnames(ret) <- c("Layer","Sample","Dist")
  
  # To dataframe
  ret <- as.data.frame(ret)
  rownames(ret)<-NULL
  
  # Dist column as numerics
  ret$Dist <- as.numeric(as.character(ret$Dist))
  # Layer as Roman numbers
  # ret$Layer <- as.roman(as.character(ret$Layer))
  return(ret)
}

#
# Significative differences tests
#

#' sigDiffTestLayer
#' 
#' Analyzes significative differences between groups
#'
#' Given a set of values and their groups, analyzes normality, homogeneity and computes
#' the degree of divergence between each pair of groups.
#' 
#' @param values A numeric vector. Set of values to be analyzed.
#' @param groups A generic vector. Groups for each value
#' @param sigLevel A numeric value. Significance level to accept the null hypothesis.
#' 
#' @return Named list: \describe{
#' \item{values}{Vector of input values}
#' \item{groups}{Values group}
#' \item{norm}{Result of the normality test}
#' \item{varLevene}{Result of the levene test}
#' \item{normAndHomo}{Boolean value. True if groups are normal and homogene}
#' \item{gdif}{Group differences test}
#' \item{pval}{groups 2 by 2 difference test}
#' }
#' 
#' @export
sigDiffTestLayer <- function(values,groups,sigLevel=0.05){
  
  # Normality test (per group)
  normalResults <- aggregate(values,by=list(group=groups), function(x){
    fit <- fitdistr(x,"normal") 
    ks <- ks.test(x,"pnorm", mean=fit$estimate[1], sd=fit$estimate[2])
    return(ks$p.value)
  })
  
  ## Check return values to confirm normality
  normality <- !any(normalResults[,2] < sigLevel) # Sig level 5%
  
  # Check var. homogeneity
  varHomogLevene <- leveneTest(values, group=as.factor(groups))
  homogVar <- varHomogLevene[1,3]>=sigLevel # Sig level: 5%
  
  # If normal data and var. homogene -> Anova
  if(normality && homogVar){
    ## Diffs bw groups
    aov.model <- aov(values ~ groups)
    gdif <- TukeyHSD(aov.model,conf.level = sigLevel)
    # if sign difference
    if(summary(aov.model)[[1]]$'Pr(>F)'[1]<sigLevel){
      ## Trasnform gdif$groups to a more friendly format for pvalues
      splitGroups <- strsplit(rownames(gdif$groups),"-")
      pvals <- rbind( sapply(splitGroups,function(x){return(x[[1]])}),
                      sapply(splitGroups,function(x){return(x[[2]])}),
                      ((gdif$groups[,2]*gdif$groups[,3])<=0)*gdif$groups[,4])
      return(list(values=values,groups=groups,norm=normalResults,varLevene=varHomogLevene,normAndHomo=T,
                  gdif=gdif, pval=pvals))
    }
    else
    {
      return(list(values=values,groups=groups,norm=normalResults,varLevene=varHomogLevene,normAndHomo=T,
                  gdif=gdif, pval=NULL))
    }
  }
  # If not -> Kruskall wallis
  else{
    gdif <- kruskal.test(values, g=as.factor(groups))
  
    # Significat diff bw groups
    if(gdif$p.value<sigLevel ){
      # Compare every pair
      pvalues <- combn(unique(groups),2,function(x){
        first = subset(values, groups ==  x[1])
        second = subset(values, groups == x[2])
        p<-wilcox.test(first,second,paired=FALSE)
        return(c(x[1],x[2],p$p.value))
      })
      
      # Bonferroni correction
      pvalues[3,] <- p.adjust(pvalues[3,], method="bonferroni")
      return(list(values=values,groups=groups,norm=normalResults,varLevene=varHomogLevene,normAndHomo=F,
                  gdif=gdif, pval=pvalues))
    }
    else
    {
      # If theres no sig. difference paired pvals -> null or we use gdif
      return(list(values=values,groups=groups,norm=normalResults,varLevene=varHomogLevene,normAndHomo=F,
                  gdif=gdif, pval=NULL))
    }
  }


}


#' layerDensitiesDiff
#' 
#' Density comparison
#' 
#' Computes the sample density for each layer and performs a difftest \link{sigDiffTestLayer}
#' 
#' @param realSamples A dataframe with columns X,Y,Z,Feret,Sample.no. and Layer
#' 
#' @return same as \link{sigDiffTestLayer}
#'
#' 
# layerDensitiesDiff <- function (realSamples){
#   
#   # All pairs Layer - Sample
#   samplelayer <- unique(realSamples[,c("Layer","Sample.no.")])
#   
#   # Compute Intensity
#   layerIntens <- apply(samplelayer,1,function(x){
#     c(x[1],sampleIntensity(realSamples,layer = x[1],sampleNo = as.numeric(x[2]),singleSample = T)*1E9)
#   })
#   
#   return(sigDiffTestLayer(as.numeric(layerIntens[2,]),layerIntens[1,]))
#}

 
#' colorStackedBarPlot
#' 
#' Variable height stacked bar plot
#'
#' Creates a stacked bar plot. Each box has different height and its colored according to its value. Color scale
#' can be customized.
#' 
#' @param values A Numeric vector. Set of values to plot.
#' @param height A numeric vector. Height of each box. Must add up to 1
#' @param boxLegend A string vector. String to be added to each box.
#' @param barLegend A sting. Legend to put over the colorbar 
#' @param title A string. Plot title
#' @param min_c A numeric value. Color scale lower bound.
#' @param max_c A numeric value. Color scale upper bound.
#' @param ncolor An integer. Color scale resolution
#' @param colpal A color vector. Color scale values.
#' 
#' @return Stacked barplot
#' 
#' @export
colorStackedBarPlot <- function(values,height,boxlegend=NULL,barLegend=NULL,
                                title=NULL,min_c=400,max_c=800,ncolors=1000,
                                colpal=c("blue", "green", "yellow", "red")){
  
  #Reverse
  values<-rev(values)
  height<-rev(height)
  boxlegend <- rev(boxlegend)
  
  # Color palette
  palette <- colorRampPalette(colpal)(ncolors)
  
  # Create color scale based on the predefined palette
  sc <- scale_fill_gradientn(colours = palette, limits=c(min_c, max_c))
  
  # Create a dateframe with value, height vector , in-box legend
  dataToPlot <- data.frame(cbind(values,height,boxlegend),stringsAsFactors = F)
  colnames(dataToPlot) <- c("Value","Height","Label")
  rownames(dataToPlot) <- NULL
  dataToPlot$Value <- as.numeric(dataToPlot$Value)
  dataToPlot$Height <- as.numeric(dataToPlot$Height)
  
  # Theme (remove axis, bg, titles...)
  theme <- theme(axis.line=element_blank(),
                 axis.text.x=element_blank(),
                 axis.text.y=element_blank(),
                 axis.ticks=element_blank(),
                 axis.title.x=element_blank(),
                 axis.title.y=element_blank(),
                 panel.background=element_blank(),
                 panel.border=element_blank(),
                 panel.grid.major=element_blank(),
                 panel.grid.minor=element_blank(),
                 plot.background=element_blank(),
                 plot.title = element_text(lineheight=1, face="bold",size = 16))
  
  # General plot data
  k<- ggplot(dataToPlot,aes(x=factor(''),y=Height,fill=Value))
  p <- k + sc + theme + # Plot + colorscale + theme
    geom_bar(stat="identity",position="fill",color="white",size=1.5) + # Stacked bars
    labs(fill = barLegend ,title = title) +  # Labels
    geom_text(aes(label=Label,y=Height,ymax=1), # Inside Text (vjust -> Text in box)
              stat="identity",position="fill",vjust=1.5, hjust=0.5,fontface="bold") + 
    guides(fill = guide_colorbar(barwidth = 1, barheight = 15)) # Make colorbar longer
  
  print(p)
}

#' plotNearestNeighborLayerDist
#' 
#' Stacked colored bar plot based on NN distance
#'
#' @param distances A numeric vector. Set of NN distances
#' @param layers A vector. Layer of each distance
#' @param min_c A numeric value. Color scale lower bound.
#' @param max_c A numeric value. Color scale upper bound.
#' @param layerSize A named numeric vector. Proportional size for each layer
#' 
#' @return Stacked barplot
#' 
#' @export
plotNearestNeighborLayerDist <- function(distances,layers,min_c=400,max_c=800,
                                         layerSize=c(I=0.05,II=0.09,III=0.17,IV=0.17,V=0.31,VI=0.21)){
  
  # Compute average distance per layer
  meanDist <- aggregate(distances,by=list(layer=as.character(layers)),mean)
  # Reverse order
  meanDist <- meanDist[order(as.roman(meanDist[,1])),]
  
  # Legend
  legendText<-as.vector(
    apply(meanDist,1,function(x){paste0("Layer ",x[1]," (",sprintf("%.2f",as.numeric(x[2]))," nm)")}))
  
  # Normalize height
  height <- layerSize[meanDist[,1]]/sum(layerSize[meanDist[,1]])
  
  # Plot
  colorStackedBarPlot (meanDist[,2],height,boxlegend=legendText,
                       barLegend="Distance (nm)",
                       title="Average distance to \n nearest synapse",
                       min_c=min_c,max_c=max_c)
}

#' plotDensityLayer
#' 
#' Stacked colored bar plot based on densities
#'
#' @param densities A numeric vector. Set of densities
#' @param layers A vector. Layer of each density
#' @param min_c A numeric value. Color scale lower bound.
#' @param max_c A numeric value. Color scale upper bound.
#' @param layerSize A named numeric vector. Proportional size for each layer
#' 
#' @return Stacked barplot
#' 
#' @export
plotDensityLayer <- function(densities,layers,min_c=0,max_c=1.35,
                             layerSize=c(I=0.05,II=0.09,III=0.17,IV=0.17,V=0.31,VI=0.21)){
  
  # Compute average density per layer
  avgLambda <- aggregate(densities,by=list(layer=as.character(layers)),mean)
  # Reverse order
  avgLambda <- avgLambda[order(as.roman(avgLambda[,1])),]
  
  # Legend
  legendText<-as.vector(
    apply(avgLambda,1,function(x){paste0("Layer ",x[1]," (",sprintf("%.2f",as.numeric(x[2]))," synapses/µm³)")}))
  
  # Normalize height
  height <- layerSize[avgLambda[,1]]/sum(layerSize[avgLambda[,1]])
  
  # Plot
  colorStackedBarPlot (avgLambda[,2],height,boxlegend=legendText,
                       barLegend="Density (synapses/µm³)",
                       title="Average density",
                       min_c=min_c,max_c=max_c)
}
