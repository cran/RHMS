\name{abstraction}
\alias{abstraction}
\title{
computes surface and canopy abstractions
}
\description{
computes surface and canopy abstractions for a given rainfall event.
}
\usage{
abstraction(rainfall,abstractionParams)
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
\value{a list: an object from class of \code{abstraction}}
\seealso{ \code{\link[RHMS]{createSubbasin}} }
\examples{
rainfall<-5*exp(((seq(2.5,7.5,length.out=36))-5)^2/-0.8)
abstractionParams<-list(canopyAbstraction=2,surfaceAbstraction=3.5)
abstraction(rainfall,abstractionParams)
}
\author{
Rezgar Arabzadeh
}
\keyword{ list }