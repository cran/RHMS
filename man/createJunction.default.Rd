\name{createJunction.default}
\alias{createJunction.default}
\title{
default function for class of \code{createJunction}
}
\description{
instantiates an object from class of \code{createJunction}
}
\usage{
\method{createJunction}{default}(name = "Unttitled", downstream=NA, 
                                 inflow = NA, delayInflow = 1)
}
\arguments{
  \item{name}{
(optional) a string: the name of junction to be instantiated
}
  \item{downstream}{
(optional) an object from either of classes: \code{createDiversion}, \code{createReservoir}, \code{createSubbasin}, \code{createJunction}, \code{createReach}.
}
  \item{inflow}{
(optional): a vector of direct inflow rather than flows comming from upstream (cms)
}
  \item{delayInflow}{
(optional): an integer presenting the time steps to delay direct inflow time series
}
}
\value{
a list: an object from class of \code{createJunction}
}
\seealso{ \code{\link[RHMS]{addObjectToBasin}} }
\author{
Rezgar Arabzadeh
}
\keyword{ list }
