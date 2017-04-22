\name{createReach.default}
\alias{createReach.default}
\title{
default function for class of \emph{createReach}
}
\description{
instantiates an object from class of \emph{createReach}
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
a string: the method of channel routing. available types: \emph{'muskingum'}, and \emph{'muskingumcunge'}
}
  \item{inflow}{
a vector (optional): a time series of direct inflow rather than flows comming from upstream (cms)
}
  \item{routingParams}{
a list : parameters associated to the \emph{routingMethod}: \emph{'k'} and \emph{'x'} for \emph{'muskingum'}, \emph{'bedWith'} (m),
\emph{'sideSlope'} (m/m), \emph{'channelSlope'} (m/m), \emph{'manningRoughness'}, \emph{'riverLength'} (Km) for \emph{'muskingumcunge'}
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
a list: an object from class of \emph{createReach}
}
\seealso{ \code{\link[RHMS]{addObjectToBasin}} }
\author{
Rezgar Arabzadeh
}

\keyword{ list }
