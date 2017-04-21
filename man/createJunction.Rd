\name{createJunction}
\alias{createJunction}
\title{
creates a junction object
}
\description{
creates a junction object, supporting direct inflow
}
\usage{
createJunction(name, downstream,label, 
               inflow, delayInflow)
}
\arguments{
  \item{name}{
a string: the name of junction to be instantiated
}
  \item{downstream}{
an integer: the downstream object code
}
  \item{label}{
an integer: a unique number known as code label
}
  \item{inflow}{
a vector (optional): a time series of direct inflow rather than flows comming from upstream (cms)
}
  \item{delayInflow}{
an integer: presenting the time steps ro delay direct inflow time series
}
}
\value{
a list: an object from class of list instantiated by createJunction
}
\seealso{ \code{\link[RHMS]{addObjectToBasin}} }
\author{
Rezgar Arabzadeh
}
\keyword{ list }
