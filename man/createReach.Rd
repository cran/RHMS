\name{createReach}
\alias{createReach}
\title{
creates a reach object
}
\description{
creates a reach object, supporting direct inflow
}
\usage{
createReach(name,routingMethod,inflow,routingParams,delayInflow,label,downstream)
}
\arguments{
  \item{name}{
a string: the name of reach to be instantiated
}
  \item{routingMethod}{
a string: the method of channel routing. available types: 'muskingum', and 'muskingumcunge'
}
  \item{inflow}{
a vector (optional): a time series of direct inflow rather than flows comming from upstream
}
  \item{routingParams}{
a list : parameters associated to the 'routingMethod': 'k' and 'x' for 'muskingum', 'bedWith' (m),
'sideSlope' (m/m), channelSlope (m/m), manningRoughness, riverLength (Km) for 'muskingumcunge'
}
  \item{delayInflow}{
an integer: presenting the time steps ro delay direct inflow time series
}
  \item{label}{
an integer: a unique number known as code label
}
  \item{downstream}{
an integer: the downstream object code
}
}
\value{
a list: an object from class of list instantiated by createReach
}
\seealso{ \code{\link[RHMS]{addObjectToBasin}} }
\author{
Rezgar Arabzadeh
}

\keyword{ list }
