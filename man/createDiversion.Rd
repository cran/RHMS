\name{createDiversion}
\alias{createDiversion}
\title{
creates a diversion object
}
\description{
instantiates an object from class of \code{createDiversion}
}
\usage{
createDiversion(name,downstream,divertTo,capacity)
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
a list: an object from class of list instantiated by \code{createDiversion}
}
\seealso{ \code{\link[RHMS]{addObjectToBasin}} }
\author{
Rezgar Arabzadeh
}
\keyword{ list }
