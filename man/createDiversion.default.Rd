\name{createDiversion.default}
\alias{createDiversion.default}
\title{
default function for class of \code{createDiversion}
}
\description{
instantiates an object from class of \code{createDiversion}
}
\usage{
\method{createDiversion}{default}(name="Unttitled",downstream=NA,divertTo,capacity)
}
\arguments{
  \item{name}{
(optional) a string: the name of diversion to be instantiated
}
  \item{downstream}{
(optional) an object from either of classes: \code{createDiversion}, \code{createReservoir}, \code{createSubbasin}, \code{createJunction}, \code{createReach}.
}
  \item{divertTo}{
an object from either of classes: \code{createDiversion}, \code{createReservoir}, \code{createSubbasin}, \code{createJunction}, \code{createReach}.
}
  \item{capacity}{
diversion capacity (cms)
}
}
\value{
a list: an object from class of \code{createDiversion}
}
\seealso{ \code{\link[RHMS]{addObjectToBasin}} }
\author{
Rezgar Arabzadeh
}
\keyword{ list }
