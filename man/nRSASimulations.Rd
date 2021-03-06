% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/synapsesSimulation.r
\name{nRSASimulations}
\alias{nRSASimulations}
\title{nRSASimulations}
\usage{
nRSASimulations(numSimulations, lambdaRPP = 1e-09, volume = 1.5e+11,
  mu = 0.006, sigma = 0.5, parallel = T)
}
\arguments{
\item{lambdaRPP}{A numeric value. Simulations intensity (synaptic density)}

\item{volume}{A numeric value. Simulation box-volume}

\item{mu}{A numeric value. Log mean for the log-normal Feret's diameters distribution}

\item{sigma}{A numeric value. Log std for the log-normal Feret's diameters distribution}

\item{parallel}{A boolean value. If true, simulations are parallelized}

\item{numSimulation}{An integer. Number of simulations to create}
}
\value{
List of simulations as returned by \link{RSA3DLognormFromData}
}
\description{
Creates a number of RSA simulations
}
\details{
Creates a number of RSA simulations (replicated) and returns them in a list. \link{RSA3DLognormFromData}
}

