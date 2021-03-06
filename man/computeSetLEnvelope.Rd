% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/synapsesEnvelope.r
\name{computeSetLEnvelope}
\alias{computeSetLEnvelope}
\title{computeSetLEnvelope}
\usage{
computeSetLEnvelope(simulations, nsimavg = length(simulations)/2,
  nsimenv = length(simulations)/2, ncores = parallel::detectCores() - 1)
}
\arguments{
\item{simulations}{List of simulations to compute the envelope \link{singleSampleSimulations}.}

\item{nsimavg}{An integer. Number of simulations used to compute the RSA function}

\item{nsimenv}{An integer. Number of simulattion used to compute the envelope.}

\item{ncores}{An integer. The number of paralallel cores to use (default: 4)}
}
\value{
A named list. \code{Lmean} is the average L RSA function, \code{dmax} is the envelope maximum distance to the mean
}
\description{
Computes L-function envelope for a set of simulations
}
\details{
Given a list of spatial processes computes the average L RSA function and the maximum distance to the mean
}

