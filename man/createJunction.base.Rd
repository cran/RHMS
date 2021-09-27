\name{createJunction.base}
\alias{createJunction.base}
\title{
base function for class of \code{createJunction}
}
\description{
instantiates an object from class of \code{createJunction}
}
\usage{
\method{createJunction}{base}(name , downstream, 
                              inflow , delayInflow )
}
\arguments{
  \item{name}{
(optional) a string: the name of junction to be instantiated
}
  \item{downstream}{
(optional) an object from either of classes: \code{createDiversion}, \code{createReservoir}, \code{createSubbasin}, \code{createJunction}, \code{createReach}.
}
  \item{inflow}{
(optional): a vector of direct/lateral (cms)
}
  \item{delayInflow}{
(optional): an integer presenting the time steps to delay direct/lateral inflow time series
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