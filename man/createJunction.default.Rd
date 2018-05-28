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
                                 label, inflow = NA, delayInflow = 1)
}
\arguments{
  \item{name}{
a string: the name of junction to be instantiated
}
  \item{downstream}{
an integer: the code of downstream object 
}
  \item{label}{
an integer: a unique number, as label
}
  \item{inflow}{
a vector (optional): a time series of direct inflow rather than flows comming from upstream (cms)
}
  \item{delayInflow}{
an integer (optional): presenting the time steps to delay direct inflow time series
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
