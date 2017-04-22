\name{createJunction.base}
\alias{createJunction.base}
\title{
base function for class of \emph{createJunction}
}
\description{
instantiates an object from class of \emph{createJunction}
}
\usage{
\method{createJunction}{base}(name , downstream, 
                              label, inflow , delayInflow )
}
\arguments{
  \item{name}{
a string: the name of junction to be instantiated
}
  \item{downstream}{
an integer: the downstream object code
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
a list: an object from class of \emph{createJunction}
}
\seealso{ \code{\link[RHMS]{addObjectToBasin}} }
\author{
Rezgar Arabzadeh
}
\keyword{ list }
