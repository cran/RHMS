\name{loss.base}
\alias{loss.base}
\title{
base function for class of \code{reachRouting}
}
\description{
this function provides parametric methods (e.g. \code{"horton"} and \code{"SCS"}) to compute loss and direct runoff depths
}
\usage{
\method{loss}{base}(precipitation,lossMethod,lossParams)
}
\arguments{
  \item{precipitation}{a vector of precipitation time series(mm)}
  \item{lossMethod}{a string including the type of lossMethod: \code{"SCS"} and \code{"horton"}. default to \code{"SCS"} method}
  \item{lossParams}{a list of parameters associated to the selcted type of \code{lossMethod}:
     \itemize{
         \item the curve number, \code{CN}, and \code{imperviousness} in precentage for \code{"SCS"} method
         \item \code{f0}, \code{f1}, \code{k} for \code{"horton"} method
         \item \code{timeInterval}: the interval of each steps in seconds needed for \code{"horton"} method
     }
  }
}
\value{
a dataframe: including precipitation, loss, and exess rainfall depth
}
\seealso{\code{\link[RHMS]{loss}}}
\author{
Rezgar Arabzadeh
}
\keyword{ matrix }
