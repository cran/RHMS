\name{createSubbasin.base}
\alias{createSubbasin.base}
\title{
creates a sub-basin object
}
\description{
creates a sub-basin object simulated by methods proposed by NRCS, supporting direct inflow
}
\usage{
\method{createSubbasin}{base}(name,precipitation,
inflow,Area,delayInflow,label,downstream,
transformMethod,lossMethod,
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
the area of drainage basin (Km^2)
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
  \item{transformMethod}{
a string: the type of transformation method. Available types: "SCS" and "snyder"
}
  \item{lossMethod}{
a string: the type of loss method. Available types: "SCS" and "horton"
}
  \item{transformParams}{
a list: list of parameters associated to the selcted type of 'transformMethod': 'Tlag' for 'SCS'
and 'Ct', 'Cp', 'L', and 'Lc' other for 'snyder'
}
  \item{lossParams}{
a list: list of parameters associated to the selcted type of 'transformMethod': 'CN' for 'SCS'
and 'f0', 'f1', 'k' other for 'horton'
}
}
\value{
a list: an object from class of list instantiated by createSubbasin
}
\seealso{ \code{\link[RHMS]{addObjectToBasin}} }
\author{
Rezgar Arabzadeh
}
\keyword{ list }
