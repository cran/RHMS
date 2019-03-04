\name{createBasin.base}
\alias{createBasin.base}
\title{
base function for class of \code{createBasin}
}
\description{
instantiates an object from class of \code{createBasin}
}
\usage{
\method{createBasin}{base}(name, simulation)
}
\arguments{
  \item{name}{a string: a name for the basin}
  \item{simulation}{a list of simulation time and dates as below:
     \itemize{
         \item \code{start}: the date which simulation starts, must be in \code{'YYYY-MM-DD'} format
         \item \code{start}: the date which simulation ends, must be in \code{'YYYY-MM-DD'} format
         \item \code{by}: the interval of each steps in seconds
     }
  }
}
\value{
a list: an object from class of \code{creatBasin}
}
\seealso{ \code{\link[RHMS]{addObjectToBasin}} }
\author{
Rezgar Arabzadeh
}
\keyword{ list }
