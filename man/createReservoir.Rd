\name{createReservoir}
\alias{createReservoir}
\title{
creates a reservoir object
}
\description{
creates a reservoir object, supporting direct inflow
}
\usage{
createReservoir(name , inflow , ratingCurve,
                dischargeCurve, initialStorage, capacity,
                delayInflow , label, downstream )
}
\arguments{
  \item{name}{
a string: the name of reservoir to be instantiated
}
  \item{inflow}{
a vector (optional): a time series of direct inflow rather than flows comming from upstream
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
an integer: presenting the time steps ro delay direct inflow time series
}
  \item{label}{
an integer: a unique number known as code label
}
  \item{downstream}{
an integer: the downstream object code
}
}
\value{
a list: an object from class of createReservoir
}
\seealso{ \code{\link[RHMS]{addObjectToBasin}} }
\author{
Rezgar Arabzadeh
}
\keyword{ list }
