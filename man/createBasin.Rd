\name{createBasin}
\alias{createBasin}
\title{
creates a basin
}
\description{
this function builds a basin for a desired simulation time period and interval
}
\usage{
createBasin(name, simPeriod, interval)
}
\arguments{
  \item{name}{
a string, presenting the name of basin
}
  \item{simPeriod}{
the simulation period in seconds
}
  \item{interval}{
number of simulation time steps
}
}
\value{
a list: an object from class of creatBasin
}
\seealso{ \code{\link[RHMS]{addObjectToBasin}} }
\author{
Rezgar Arabzadeh
}
\keyword{ list }