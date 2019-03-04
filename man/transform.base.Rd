\name{transform.base}
\alias{transform.base}
\title{
base function for class of \code{transform}
}
\description{
This function transforms an excess rainfall event to a direct runoff hydorgraph.
}
\usage{
\method{transform}{base}(rainfall,transformMethod,transformParams,Area,UH,simulation)
}
\arguments{
  \item{rainfall}{an object inherited from \code{loss} function}
  \item{transformMethod}{a string: the type of transformation method. available types: \code{"SCS"}, \code{"snyder"}, and \code{"user"}. default to \code{"SCS"}}
  \item{transformParams}{a list of parameters associated to the selcted type of \code{transformMethod}:
     \itemize{
         \item \code{Tlag} for \code{"SCS"} method
         \item \code{Ct}, \code{Cp}, \code{L}, and \code{Lc} for \code{"snyder"} method
      }  
  }
  \item{Area}{the area of drainage basin (Km^2)}
  \item{UH}{a data.frame: must be provided when \code{transformMethod} is set to \code{"user"}. \code{UH} is the ordinates of a user defined UH by the which its first collumn is time (Hr) and the second collumn includes flow rates (cms)}
  \item{simulation}{a list of simulation time and dates as below:
     \itemize{
         \item \code{start}: the date which simulation starts, must be in \code{'YYYY-MM-DD'} format
         \item \code{start}: the date which simulation ends, must be in \code{'YYYY-MM-DD'} format
         \item \code{by}: the interval of each steps in seconds
     }
  }
}
\value{
Hydrogaph of direct runoff
}
\seealso{ \code{\link[RHMS]{transform}} }

\author{
Rezgar Arabzadeh
}

\keyword{ array }