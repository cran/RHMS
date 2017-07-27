\name{loss.base}
\alias{loss.base}
\title{
base function for class of \code{reachRouting}
}
\description{
this function provides parametric methods (e.g. \code{"horton"} and \code{"SCS"}) to compute loss and direct runoff depths
}
\usage{
\method{loss}{base}(precipitation,lossParams,
simulation,lossMethod)
}
\arguments{
  \item{precipitation}{a vector of precipitation time series(mm)}
  \item{lossParams}{a list: list of parameters associated to the selcted type of \code{lossMethod}:
  \code{CN} for \code{"SCS"} and \code{f0}, \code{f1}, \code{k} for \code{"horton"}}
  \item{simulation}{a vector of simulation interval (in second) and time steps (an integer)}
  \item{lossMethod}{a string including the type of lossMethod: \code{"SCS"} and \code{"horton"}}
}
\value{
a dataframe: including precipitation, loss, and exess rainfall depth
}
\seealso{\code{\link[RHMS]{loss}}}
\author{
Rezgar Arabzadeh
}
\keyword{ graphs }
