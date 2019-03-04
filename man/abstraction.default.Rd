\name{abstraction.default}
\alias{abstraction.default}
\title{
default function for class of \code{abstraction}
}
\description{
instantiates an object from class of \code{abstraction}
}
\usage{
\method{abstraction}{default}(rainfall,
        abstractionParams=list(canopyAbstraction=NULL,
                               surfaceAbstraction=NULL))
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
a list: an object from class of \code{abstraction}
}
\seealso{ \code{\link[RHMS]{createSubbasin}} }
\author{
Rezgar Arabzadeh
}
\keyword{ list }