\name{createDiversion}
\alias{createDiversion}
\title{
creates a diversion object
}
\description{
instantiates an object from class of \emph{createDiversion}
}
\usage{
createDiversion(name,label,downstream,divertTo,capacity)
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
a list: an object from class of list instantiated by \emph{createDiversion}
}
\seealso{ \code{\link[RHMS]{addObjectToBasin}} }
\author{
Rezgar Arabzadeh
}
\keyword{ list }
