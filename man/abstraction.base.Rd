\name{abstraction.base}
\alias{abstraction.base}
\title{
base function for class of \code{abstraction}
}
\description{
instantiates an object from class of \code{abstraction}
}
\usage{
\method{abstraction}{base}(rainfall,abstractionParams)
}
\arguments{
  \item{rainfall}{a vector : a time series of precipitation hyetograph (mm)}
  \item{abstractionParams}{a list: including parameters of simple surface and simple canopy methods.
     \itemize{
         \item \code{canopyAbstraction} depth of canopy abstraction in (mm). default to zero
         \item \code{surfaceAbstraction} depth of surface abstraction in (mm). default to zero
     }
  }
}
\value{
a list: a list features for the constructed sub-basin
}
\seealso{ \code{\link[RHMS]{createSubbasin}} }
\author{
Rezgar Arabzadeh
}
\keyword{ list }