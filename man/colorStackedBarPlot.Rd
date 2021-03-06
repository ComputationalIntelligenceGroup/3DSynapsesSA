% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/synapsesLayerComp.r
\name{colorStackedBarPlot}
\alias{colorStackedBarPlot}
\title{layerDensitiesDiff}
\usage{
colorStackedBarPlot(values, height, boxlegend = NULL, barLegend = NULL,
  title = NULL, min_c = 400, max_c = 800, ncolors = 1000,
  colpal = c("blue", "green", "yellow", "red"))
}
\arguments{
\item{values}{A Numeric vector. Set of values to plot.}

\item{height}{A numeric vector. Height of each box. Must add up to 1}

\item{barLegend}{A sting. Legend to put over the colorbar}

\item{title}{A string. Plot title}

\item{min_c}{A numeric value. Color scale lower bound.}

\item{max_c}{A numeric value. Color scale upper bound.}

\item{colpal}{A color vector. Color scale values.}

\item{realSamples}{A dataframe with columns X,Y,Z,Feret,Sample.no. and Layer}

\item{boxLegend}{A string vector. String to be added to each box.}

\item{ncolor}{An integer. Color scale resolution}
}
\value{
same as \link{sigDiffTestLayer}


colorStackedBarPlot

Variable height stacked bar plot

Creates a stacked bar plot. Each box has different height and its colored according to its value. Color scale
can be customized.

Stacked barplot
}
\description{
Density comparison
}
\details{
Computes the sample density for each layer and performs a difftest \link{sigDiffTestLayer}
}

