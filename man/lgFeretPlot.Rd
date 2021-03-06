% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/synapsesFeret.r
\name{lgFeretPlot}
\alias{lgFeretPlot}
\title{lgFeretPlot}
\usage{
lgFeretPlot(feretDiams, ml, sdl, histmin = 0, histmax = 1200,
  color = "skyblue", name = "Feret's diameter histogram")
}
\arguments{
\item{feretDiams}{A numeric vector of Feret's diameters}

\item{ml}{Log-normal log mean}

\item{sdl}{Log-normal log standard deviation}

\item{histmin}{minimum value for the x axis}

\item{histmax}{maximum value for the x axis}

\item{color}{histogram bars color}

\item{name}{plot nmae (title)}
}
\value{
2D plot (histogram + curve)
}
\description{
Plot log-normal Feret fit
}
\details{
Plots the log-normal distribution fitted from a vector of ferets diameters
}

