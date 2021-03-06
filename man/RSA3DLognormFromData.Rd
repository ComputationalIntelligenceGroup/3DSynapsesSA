% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/synapsesSimulation.r
\name{RSA3DLognormFromData}
\alias{RSA3DLognormFromData}
\title{RSA3DLognormFromData}
\usage{
RSA3DLognormFromData(feretDiam, realSample, plot = FALSE,
  singleSample = FALSE, replicated = FALSE, lambdaRPP = 0, volume = 0,
  color = "violet", mu = -1, sigma = -1, maxIter = 10)
}
\arguments{
\item{feretDiam}{A numeric vector. Sample feret diameters}

\item{realSample}{A dataframe. Spatial coordinates (X,Y,Z) of the synapses}

\item{plot}{A boolean value. If true, simulation is plotted before return}

\item{singleSample}{A boolean value. If true, "Feret" colum from real sample is used insted of feretDiam}

\item{replicated}{A boolean value. If true, number of points is computed as the \code{volume * lambdaRPP}}

\item{lambdaRPP}{A numeric value. Simulation Intesity (synaptic density) parameter}

\item{volume}{A numeric value. Simulation (cubic) volume.}

\item{color}{Synapses plot color. Only applies if \code{plot} is True}

\item{mu}{A numeric value. Log mean parameter for the Feret's diameter log-normal distribution}

\item{maxIter}{An integer. Maximum number of trials before failure.}

\item{sdl}{A numeric value. Log std parameter for the Feret's diameter log-normal distribution}
}
\value{
A named list: spp <- 3D spatial process, diam <- ferets diameters, ml,sdl <- feret's diams params
}
\description{
Creates a RSA simulation with log-normal feret diameters
}
\details{
Create a RSA simulation replicating the given sample or using the parameters given.

Domain is always cubic. If \code{volume} is not provided, sample volume is used. Same applies to parameters.
If \code{replicated} is true, number of points is calculated as the product of volume and lambdaRPP, if not,
the simulation will have the same number of points as the sample.
}

