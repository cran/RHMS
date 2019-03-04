\name{createReservoir.default}
\alias{createReservoir.default}
\title{
default function for class of \code{createReservoir}
}
\description{
instantiates an object from class of \code{createReservoir}
}
\usage{
\method{createReservoir}{default}(name = "Unttitled", inflow = NA,
                      geometry=list(storageElevationCurve=NULL,
                                    dischargeElevationCurve=NULL,
                                    capacity=NULL),
                                  initialStorage = NA,
                                  delayInflow = 1, downstream = NA)
}
\arguments{
  \item{name}{(optional): a string: the name of reservoir to be instantiated}
  \item{inflow}{(optional): a vector of direct inflow rather than flows comming from upstream (cms)}
  \item{geometry}{a list of geometric specifications of the reservoir:
     \itemize{
         \item \code{storageElevationCurve}: a data frame: a data frame at which its first collumn includes height (masl) and second collums presents equivalant volume to the height at first collumn (MCM)
         \item \code{dischargeElevationCurve}: a data frame: a data frame at which its first collumn includes height (masl) and second collums presents equivalant discharge rate to the height at first collumn (cms)
         \item \code{storage}: the maximum volume of reservoir capacity (MCM)
     }
  }
  \item{initialStorage}{(optional): the initial storage of reservoir at the first time step of simulation (MCM)}
  \item{delayInflow}{(optional): an integer presenting the time steps to delay direct inflow time series}
  \item{downstream}{(optional): an object from either of classes: \code{createDiversion}, \code{createReservoir}, \code{createSubbasin}, \code{createJunction}, \code{createReach}.}
}
\value{a list: an object from class of \code{createReservoir}}
\seealso{ \code{\link[RHMS]{addObjectToBasin}} }
\author{
Rezgar Arabzadeh
}
\keyword{ list }
