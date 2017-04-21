\name{createReach.default}
\alias{createReach.default}
\title{
creates a reach object
}
\description{
creates a reach object, supporting direct inflow
}
\usage{
\method{createReach}{default}(name="Unttitled",routingMethod="muskingum",inflow=NA,
                              routingParams=list(k=3,x=0.2,bedWith=NULL,
                                                 sideSlope=2,channelSlope=NULL,
                                                 manningRoughness=0.025,riverLength=NULL),
                              delayInflow=1,label,downstream=NA)
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
a list : parameters associated to the 'routingMethod': 'k' and 'x' for 'muskingum', 
'bedWith' (m), 'sideSlope' (m/m), channelSlope (m/m), manningRoughness, riverLength (Km) for 'muskingumcunge'
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
a list: an object from class of createReach
}
\seealso{ \code{\link[RHMS]{addObjectToBasin}} }
\author{
Rezgar Arabzadeh
}

\keyword{ list }
