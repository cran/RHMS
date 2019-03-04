\name{loss.default}
\alias{loss.default}
\title{
default function for class of \code{loss}
}
\description{
this function provides parametric methods (e.g. \code{"horton"} and \code{"SCS"}) to compute loss and direct runoff depths
}
\usage{
\method{loss}{default}(precipitation,lossMethod,
            lossParams=list(f0=NULL,
                            f1=NULL,
                            k=NULL,
                            timeInterval=NULL,
                            CN=NULL,
                            imperviousness=NULL))
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
