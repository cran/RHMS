\name{createSubbasin.default}
\alias{createSubbasin.default}
\title{
default function for class of \emph{createSubbasin}
}
\description{
instantiates an object from class of \emph{createSubbasin}
}
\usage{
\method{createSubbasin}{default}(name="Unttitled",
precipitation,inflow=NA,Area,delayInflow=1,
label,downstream=NA,
transformMethod="SCS",lossMethod="SCS",UH=NA,
transformParams=list(Tlag=NULL,Cp=NULL,
                     Ct=NULL,L=NULL,Lc=NULL),
lossParams=list(CN=NULL,f0=NULL,
                f1=NULL,k=NULL))
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
an integer (optional): presenting the time steps to delay direct inflow time series
}
  \item{label}{
an integer: a unique number, as label
}
  \item{downstream}{
an integer: the code of downstream object 
}
  \item{transformMethod}{
a string: the type of transformation method. Available types: \emph{'SCS'}, \emph{snyder'}, and \emph{'user'} for user defined unit hydrograph
}
  \item{lossMethod}{
a string: the type of loss method. Available types: \emph{'SCS'} and \emph{'horton'}
}
  \item{UH}{
a data.frame: including the ordinates of user UH. the HU first collumn indicates time (Hr) and second collumn include flow rates (cms) 
}
  \item{transformParams}{
a list: list of parameters associated to the selcted type of \emph{'transformMethod'}:
\emph{'Tlag'} for \emph{'SCS'}
and \emph{'Ct'}, \emph{'Cp'}, \emph{'L'}, and \emph{'Lc'} other for \emph{'snyder'}
}
  \item{lossParams}{
a list: list of parameters associated to the selcted type of \emph{'lossMethod'}:
\emph{'CN'} for \emph{'SCS'}
and \emph{'f0'}, \emph{'f1'}, \emph{'k'} other for \emph{'horton'}
}
}
\value{
a list: an object from class of \emph{createSubbasin}
}
\seealso{ \code{\link[RHMS]{addObjectToBasin}} }
\author{
Rezgar Arabzadeh
}
\keyword{ list }
