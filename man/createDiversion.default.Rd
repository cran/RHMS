\name{createDiversion.default}
\alias{createDiversion.default}
\title{
default function for class of \code{createDiversion}
}
\description{
instantiates an object from class of \code{createDiversion}
}
\usage{
\method{createDiversion}{default}(name="Unttitled",label,downstream=NA,divertTo,capacity)
}
\arguments{
  \item{name}{
a string: the name of diversion to be instantiated
}
  \item{downstream}{
an integer: the code of downstream object 
}
  \item{label}{
an integer: a unique number, as label
}
  \item{divertTo}{
an integer: the code of an object at the diversion outlet
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
