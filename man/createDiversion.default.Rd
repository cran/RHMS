\name{createDiversion.default}
\alias{createDiversion.default}
\title{
creates a junction object
}
\description{
creates a diversion object
}
\usage{
\method{createDiversion}{default}(name="Unttitled",label,downstream=NA,divertTo,capacity)
}
\arguments{
  \item{name}{
a string: the name of diversion to be instantiated
}
  \item{downstream}{
an integer: the downstream object code
}
  \item{label}{
an integer: a unique number known as code label
}
  \item{divertTo}{
an integer: the object lable code the flow to be diverted to
}
  \item{capacity}{
diversion capacity flow rate (cms)
}
}
\value{
a list: an object from class of list instantiated by createDiversion
}
\seealso{ \code{\link[RHMS]{addObjectToBasin}} }
\author{
Rezgar Arabzadeh
}
\keyword{ list }
