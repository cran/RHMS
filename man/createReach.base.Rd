\name{createReach.base}
\alias{createReach.base}
\title{
base function for class of \code{createReach}
}
\description{
instantiates an object from class of \code{createReach}
}
\usage{
\method{createReach}{base}(name,routingMethod,inflow,
                           routingParams,
                           delayInflow,downstream)
}
\arguments{
  \item{name}{
(optional) a string: the name of reach to be instantiated
}
  \item{routingMethod}{
a string: the method of channel routing. available types: \code{"muskingum"}, and \code{"muskingumcunge"}. default to \code{"muskingum"}
}
  \item{inflow}{
(optional): a vector of direct inflow rather than flows comming from upstream (cms)
}
  \item{routingParams}{
   a list : parameters associated to the \code{routingMethod}:
      \itemize{
        \item \code{k} and \code{x} for \code{"muskingum"},
        \item \code{bedWith} (m), \code{sideSlope} (m/m), \code{channelSlope} (m/m), \code{manningRoughness}, \code{riverLength} (Km) for \code{"muskingumcunge"}
      }
  }
  \item{delayInflow}{
(optional): an integer presenting the time steps to delay direct inflow time series
}
  \item{downstream}{
(optional) an object from either of classes: \code{createDiversion}, \code{createReservoir}, \code{createSubbasin}, \code{createJunction}, \code{createReach}.
}
}
\value{
a list: an object from class of list instantiated by \code{createReach}
}
\seealso{ \code{\link[RHMS]{addObjectToBasin}} }
\author{
Rezgar Arabzadeh
}

\keyword{ list }
