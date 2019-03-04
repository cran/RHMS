\name{transform}
\alias{transform}
\title{
Transforms a rainfall event to runoff
}
\description{
This function transforms an excess rainfall event to a direct runoff hydorgraph.
}
\usage{
transform(rainfall,transformMethod,transformParams,Area,UH,simulation)
}
\arguments{
  \item{rainfall}{an object inherited from \code{loss} function}
  \item{transformMethod}{a string: the type of transformation method. available types: \code{"SCS"}, \code{"snyder"}, and \code{"user"}. default to \code{"SCS"}}
  \item{transformParams}{a list of parameters associated to the selcted type of \code{transformMethod}:
     \itemize{
         \item \code{Tlag} for \code{"SCS"} method
         \item \code{Ct}, \code{Cp}, \code{L}, and \code{Lc} for \code{"snyder"} method
      }  
  }
  \item{Area}{the area of drainage basin (Km^2)}
  \item{UH}{a data.frame: must be provided when \code{transformMethod} is set to \code{"user"}. \code{UH} is the ordinates of a user defined UH by the which its first collumn is time (Hr) and the second collumn includes flow rates (cms)}
  \item{simulation}{a list of simulation time and dates as below:
     \itemize{
         \item \code{start}: the date which simulation starts, must be in \code{'YYYY-MM-DD'} format
         \item \code{start}: the date which simulation ends, must be in \code{'YYYY-MM-DD'} format
         \item \code{by}: the interval of each steps in seconds
     }
  }
}
\value{
Hydrogaph of direct runoff
}
\seealso{ \code{\link[RHMS]{sim}} }
\examples{
Area=200
lossMethod<-"SCS"
lossParams<-list(CN=65)
transformMethod<-c("snyder","SCS","user")
simulation<-list(start='2000-01-01',end='2000-01-7',by=7200)
precipitation<-sin(seq(0.1,pi-0.1,length.out=10))*20
transformParams=list(Tlag=4,Cp=0.15,Ct=2,L=100,Lc=15)
UH<-data.frame(t=1:20,q=sin(seq(0,pi,length.out=20))*1)

SCS_loss<-loss(precipitation,lossMethod,lossParams)

snyder_transformation<-transform(rainfall=SCS_loss,
                                 transformMethod=transformMethod[1],
                                 transformParams,Area,UH=NA,simulation)
SCS_transformation   <-transform(rainfall=SCS_loss,
                                 transformMethod=transformMethod[2],
                                 transformParams,Area,UH=NA,simulation)
user_transformation  <-transform(rainfall=SCS_loss,
                                 transformMethod=transformMethod[3],
                                 transformParams,Area,UH,simulation)
}
\author{
Rezgar Arabzadeh
}
\keyword{ array }