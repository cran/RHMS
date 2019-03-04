\name{createSubbasin.default}
\alias{createSubbasin.default}
\title{
default function for class of \code{createSubbasin}
}
\description{
instantiates an object from class of \code{createSubbasin}
}
\usage{
\method{createSubbasin}{default}(name="Unttitled",
       precipitation,inflow=NA,Area,delayInflow=1,
       downstream=NA,
       transformMethod="SCS",
       lossMethod="none",
       BFSMethod='none',
       UH=NA,
       abstractionParams=list(canopyAbstraction=NULL,surfaceAbstraction=NULL),
       transformParams=list(Tlag=NULL,Cp=NULL,Ct=NULL,L=NULL,Lc=NULL),
       lossParams=list(CN=NULL,f0=NULL,f1=NULL,k=NULL),
       BFSParams=list(alpha=NULL,BFI=NULL,k=NULL))
}
\arguments{
  \item{name}{(optional): a string: the name of sub-basin to be instantiated}
  \item{precipitation}{a vector : a time series of precipitation hytograph (mm)}
  \item{inflow}{(optional): a vector of direct inflow rather than flows comming from upstream (cms)}
  \item{Area}{the area of basin (Km^2)}
  \item{delayInflow}{(optional): an integer presenting the time steps to delay direct inflow time series}
  \item{downstream}{(optional): an object from either of classes: \code{createDiversion}, \code{createReservoir}, \code{createSubbasin}, \code{createJunction}, \code{createReach}.}
  \item{transformMethod}{a string: the type of transformation method. Available types: \code{"SCS"}, \code{"snyder"}, and \code{"user"} for user defined unit hydrograph. default to \code{"SCS"}}
  \item{lossMethod}{a string: the type of loss method. Available types: \code{"SCS"} and \code{"horton"}}
  \item{BFSMethod}{a string: The method of base flow separation. Available methods: \code{'nathan'}, \code{'chapman'}, \code{'eckhardt'}, \code{'recession'}}
  \item{UH}{a data.frame: including the ordinates of user UH. the HU first collumn indicates time (Hr) and second collumn include flow rates (cms)}
  \item{abstractionParams}{a list: including parameters of simple surface and simple canopy methods.
     \itemize{
         \item \code{canopyAbstaction} depth of canopy abstraction in (mm)
         \item \code{surfaceAbstaction} depth of surface abstraction in (mm)
     }
  }
  \item{BFSParams}{a list including parameters associated with the method coerced in \code{'BFSMethod'}.
     \itemize{
         \item \code{alpha} is in \code{[0, 1]} interval required for \code{'nathan'}, \code{'chapman'}, and \code{'eckhardt'} methods
         \item \code{BFI} is in \code{[0, 1]} interval required for \code{'eckhardt'} method
         \item \code{k} is in \code{[0, 1]} interval and \code{timeInterval} is in day required for \code{'recession'} method
     }
  }
  \item{transformParams}{a list of parameters associated to the selcted type of \code{transformMethod}:
     \itemize{
         \item \code{Tlag} for \code{"SCS"} method in (Hours)
         \item \code{Ct}, \code{Cp}, \code{L}, and \code{Lc} for \code{"snyder"} method
      }  
  }
  \item{lossParams}{a list of parameters associated to the selcted type of \code{lossMethod}:
     \itemize{
         \item \code{CN} for \code{"SCS"} method
         \item \code{f0}, \code{f1}, \code{k} other for \code{"horton"} method
     }
  }
}
\value{
a list: an object from class of \code{createSubbasin}
}
\seealso{ \code{\link[RHMS]{addObjectToBasin}} }
\author{
Rezgar Arabzadeh
}
\keyword{ list }