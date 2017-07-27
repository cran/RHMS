\name{createSubbasin}
\alias{createSubbasin}
\title{
creates a sub-basin object
}
\description{
instantiates an object from class of \code{createSubbasin}
}
\usage{
createSubbasin(name,precipitation,
inflow,Area,delayInflow,label,downstream,
transformMethod,lossMethod,UH,
transformParams,lossParams)
}
\arguments{
  \item{name}{
a string: the name of sub-basin to be instantiated
}
  \item{precipitation}{
a vector : a time series of precipitation hytograph (mm)
}
  \item{inflow}{
a vector (optional): a time series of direct inflow rather than flows comming from upstream (cms)
}
  \item{Area}{
the area of basin (Km^2)
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
  \item{transformMethod}{
a string: the type of transformation method. Available types: \code{"SCS"}, \code{"snyder"}, and \code{"user"} for user defined unit hydrograph
}
  \item{lossMethod}{
a string: the type of loss method. Available types: \code{"SCS"} and \code{"horton"}
}
  \item{UH}{
a data.frame: including the ordinates of user UH. the HU first collumn indicates time (Hr) and second collumn include flow rates (cms) 
}
  \item{transformParams}{
a list: list of parameters associated to the selcted type of \code{transformMethod}:
\code{Tlag} for \code{"SCS"}
and \code{Ct}, \code{Cp}, \code{L}, and \code{Lc} other for \code{"snyder"}
}
  \item{lossParams}{
a list: list of parameters associated to the selcted type of \code{lossMethod}:
\code{CN} for \code{"SCS"}
and \code{f0}, \code{f1}, \code{k} other for \code{"horton"}
}
}
\value{a list: an object from class of \code{createSubbasin}}
\seealso{ \code{\link[RHMS]{addObjectToBasin}} }
\author{
Rezgar Arabzadeh
}
\keyword{ list }