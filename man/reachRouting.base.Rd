\name{reachRouting.base}
\alias{reachRouting.base}
\title{
base function for class of \code{reachRouting}
}
\description{
function for flood routing using Muskingum and muskingum-cunge techniques.
}
\usage{
\method{reachRouting}{base}(inflow,routingMethod,
             routingParams,simulation)
}
\arguments{
  \item{inflow}{a vector of runoff (cms) or an object inherited from any of the following classes :{\code{transform} ; \code{reachRouting} ; \code{reservoirRouting}}.}
  \item{routingMethod}{a string: the type of channel routing method: \code{"muskingum"} or \code{"muskingumcunge"}. default to \code{"muskingum"}}
  \item{routingParams}{a list : parameters associated to the \code{routingMethod}:
      \itemize{
        \item \code{k} and \code{x} for \code{"muskingum"},
        \item \code{bedWith} (m), \code{sideSlope} (m/m), \code{channelSlope} (m/m), \code{manningRoughness}, \code{riverLength} (Km) for \code{"muskingumcunge"}
      }
  }
  \item{simulation}{a list of simulation time and dates as below:
     \itemize{
         \item \code{start}: the date which simulation starts, must be in \code{'YYYY-MM-DD'} format
         \item \code{start}: the date which simulation ends, must be in \code{'YYYY-MM-DD'} format
         \item \code{by}: the interval of each steps in seconds
     }
  }
}
\value{
a data.frame: including inflow time series routing resaults and simulation details 
}
\references{
Chow, V. T., Maidment, D. R., & Mays, L. W. (1988). Applied hydrology.
}
\seealso{ \code{\link[RHMS]{reachRouting}} }
\author{
Rezgar Arabzadeh
}
\keyword{ list }