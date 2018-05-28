\name{transform.default}
\alias{transform.default}
\title{
default function for class of \code{transform}
}
\description{
This function transforms an excess rainfall event to a direct runoff hydorgraph.
}
\usage{
\method{transform}{default}(rainfall,
          transformParams=list(Tlag=NULL,
                               Cp  =NULL,
                               Ct  =NULL,
                               L   =NULL,
                               Lc  =NULL),
          Area,simulation=c(interval=3600*1,period=NA),UH,
          transformMethod)
}
\arguments{
  \item{rainfall}{an object inherited from \code{loss} function}
  \item{transformParams}{a list: list of parameters associated to the selcted type of \code{transformMethod}:\code{Tlag} for \code{"SCS"} and \code{Ct}, \code{Cp}, \code{L}, and \code{Lc} other for \code{"snyder"}}
  \item{Area}{the area of drainage basin (Km^2)}
  \item{simulation}{a vector of simulation interval (in second) and time steps (an integer)}
  \item{UH}{a data.frame: must be provided when \code{transformMethod} is set to \code{"user"}. \code{UH} is the ordinates of a user defined UH by the which its first collumn is time (Hr) and the second collumn includes flow rates (cms)}
  \item{transformMethod}{a string: the type of transformation method. available types: \code{"SCS"}, \code{"snyder"}, and \code{"user"}}
}
\value{
Hydrogaph of direct runoff
}
\seealso{ \code{\link[RHMS]{transform}} }
\author{
Rezgar Arabzadeh
}
\keyword{ array }