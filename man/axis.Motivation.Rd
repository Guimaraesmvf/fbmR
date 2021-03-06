% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/axisMotivation.R
\name{axis.Motivation}
\alias{axis.Motivation}
\title{Motivation axis acquisiton}
\usage{
axis.Motivation(n, pleasure, hope, acceptance, fear, pain, rejection,
  est.pleasure, est.hope, est.acceptance, est.fear, est.pain, est.rejection,
  full.estimation)
}
\arguments{
\item{n}{The number of observation on the dataset or to estimate}

\item{pleasure}{vector of values for pleasure}

\item{hope}{vector of values for hope}

\item{acceptance}{vector of values for acceptance}

\item{fear}{vector of values for fear}

\item{pain}{vector of values for pain}

\item{rejection}{vector of values for rejection}

\item{est.pleasure}{estimates a vector of values for pleasure based on the n size}

\item{est.hope}{estimates  a vector of values for hope based on the n size}

\item{est.acceptance}{estimates  a vector of values for acceptance based on the n size}

\item{est.fear}{estimates a vector of values for fear based on the n size}

\item{est.pain}{estimates a vector of values for pain based on the n size}

\item{est.rejection}{estimates a vector of values for rejection based on the n size}

\item{full.estimation}{performs a estimation for all variables based on the n provided.
If any variable is informed it will be ignored.}
}
\value{
This function returns the Motivation value for each observation on
the dataset informed or estimated
}
\description{
This function provides the value for the Motivation axis
}
\details{
The domain of values is between 0 to 1. In order to obtain a valid
result, when a vector of values is informed, not estimated, it must be on
the range 0,1

When a vector of values is informed and another variable is estimated, the n
must be equal to all variables in order to calculate.

The amount of positive variables must be equal to negative variables.
}
\examples{
m.estimated <- axis.Motivation(n=300, full.estimation=TRUE)
m <- axis.Motivation(n=1000, pleasure= df[1:1000,1], est.pain=TRUE)

}
\references{
Marcus Guimaraes <guimaraesmvf@gmail.com>
}

