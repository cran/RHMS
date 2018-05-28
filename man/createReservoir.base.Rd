\name{createReservoir.base}
\alias{createReservoir.base}
\title{
base function for class of \code{createReservoir}
}
\description{
instantiates an object from class of \code{createReservoir}
}
\usage{
\method{createReservoir}{base}(name , inflow , ratingCurve,
                               dischargeCurve, initialStorage, capacity,
                               delayInflow , label, downstream )
}
\arguments{
  \item{name}{
a string: the name of reservoir to be instantiated
}
  \item{inflow}{
a vector (optional): a time series of direct inflow rather than flows comming from upstream (cms)
}
  \item{ratingCurve}{
a data frame: a data frame at which its first collumn includes height (masl)
and second collums presents equivalant volume to the height at first collumn (MCM)
}
  \item{dischargeCurve}{
a data frame: a data frame at which its first collumn includes height (masl)
and second collums presents equivalant discharge rate to the height at first collumn (cms)
}
  \item{initialStorage}{
the initial storage of reservoir at the first time step of simulation (MCM)
}
  \item{capacity}{
the maximum volume of reservoir capacity (MCM)
}
  \item{delayInflow}{
an integer (optional): presenting the time steps to delay direct inflow time series
}
  \item{label}{
an integer: a unique number, as label
}
  \item{downstream}{
an integer: the code of downstream object 
}
}
\value{
a list: an object from class of \code{createReservoir}
}
\seealso{ \code{\link[RHMS]{addObjectToBasin}} }
\author{
Rezgar Arabzadeh
}
\keyword{ list }
