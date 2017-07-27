\name{transform}
\alias{transform}
\title{
Transforms a rainfall event to runoff
}
\description{
This function transforms an excess rainfall event to a direct runoff hydorgraph.
}
\usage{
transform(rainfall,
          transformParams,
          Area,simulation,UH,
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
\seealso{ \code{\link[RHMS]{sim}} }
\examples{

Area=200
lossMethod<-"SCS"
lossParams<-list(CN=65)
transformMethod<-c("snyder","SCS","user")
simulation<-c(interval=3600,period=100)
precipitation<-sin(seq(0.1,pi-0.1,length.out=10))*20
transformParams=list(Tlag=4,Cp=0.15,Ct=2,L=100,Lc=15)
UH<-data.frame(t=1:20,q=sin(seq(0,pi,length.out=20))*1)

SCS_loss<-loss(precipitation,lossParams,simulation,lossMethod)

snyder_transformation<-transform(rainfall=SCS_loss,
                                 transformParams,
                                 Area,
                                 simulation,
                                 transformMethod=transformMethod[1])
SCS_transformation<-transform(rainfall=SCS_loss,
                                 transformParams,
                                 Area,
                                 simulation,
                                 transformMethod=transformMethod[2])
user_transformation<-transform(rainfall=SCS_loss,
                                 transformParams,
                                 Area,
                                 simulation,
                                 UH,
                                 transformMethod=transformMethod[3])

}
\author{
Rezgar Arabzadeh
}
\keyword{ array }