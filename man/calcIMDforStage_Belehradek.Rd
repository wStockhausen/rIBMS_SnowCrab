% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calcIntermoltDuration_Belehradek.R
\name{calcIMDforStage_Belehradek}
\alias{calcIMDforStage_Belehradek}
\title{Calculate the intermolt duration using Belehradek equation}
\usage{
calcIMDforStage_Belehradek(sources = "all", stages = "all", T, dt = 1)
}
\arguments{
\item{sources}{- "all", "Ouellet and Ste Marie 2018", or "Yamamoto et al. 2014"}

\item{stages}{- "all" or character vector with required life stages}

\item{T}{- temperature (either a single number or a time series)}

\item{dt}{- time step (same time units as a), if T is a time series}
}
\value{
A tibble with columns 'source', 'stage', 'T', and 'D'.
}
\description{
Function to calculate the intermolt duration using Belehradek equation for given
stages based on given sources.
}
\details{
The intermolt duration using Belehradek equation is \eqn{D = a/(T-b)^c} for development at constant T.
Note that this => \eqn{1/D = [(T-b)^c]/a} => \eqn{S (1/D) dt = S [(T-b)^c]/a dt} => \eqn{1 = S [(T-b)^c]/a dt}, where S is the
time integral from 0 to D. For variable T, then, the intermolt duration is assumed to be
given by the time at which the integral \eqn{S [(T-b)^c]/a dt = 1}. Since a is constant, this condition is equivalent
to \eqn{S [(T-b)^c] dt = a}. Note that Reamur's Law can be obtained by setting the exponent "c" to 1.
}
