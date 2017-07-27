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
                           delayInflow,label,downstream)
}
\arguments{
  \item{name}{
a string: the name of reach to be instantiated
}
  \item{routingMethod}{
a string: the method of channel routing. available types: \code{"muskingum"}, and \code{"muskingumcunge"}
}
  \item{inflow}{
a vector (optional): a time series of direct inflow rather than flows comming from upstream (cms)
}
  \item{routingParams}{
a list : parameters associated to the \code{routingMethod}: \code{k} and \code{x} for \code{"muskingum"}, \code{bedWith} (m),
\code{sideSlope} (m/m), \code{channelSlope} (m/m), \code{manningRoughness}, \code{riverLength} (Km) for \code{"muskingumcunge"}
}
  \item{delayInflow}{
an integer (optional): presenting the time steps to delay direct inflow time series
}
  \item{label}{
an integer: a unique number, as label
}
  \item{downstream}{
an integer: the code of downstream object 
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
