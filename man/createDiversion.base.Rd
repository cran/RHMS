\name{createDiversion.base}
\alias{createDiversion.base}
\title{
creates a diversion object
}
\description{
creates a diversion object
}
\usage{
\method{createDiversion}{base}(name,label,downstream,divertTo,capacity)
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
