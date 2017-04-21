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
          transformParams=list(Tlag=NULL,
                               Cp  =NULL,
                               Ct  =NULL,
                               L   =NULL,
                               Lc  =NULL),
          Area,simulation=c(interval=3600*1,period=NA),
          transformMethod)
}
\arguments{
  \item{rainfall}{an object inherited from class of loss}
  \item{transformParams}{a list: list of parameters associated to the selcted type of 'transformMethod':
 'Tlag' for 'SCS' and other for 'snyder'}
  \item{Area}{the area of drainage basin (Km^2)}
  \item{simulation}{a vector of simulation interval (in second) and time step (an integer)}
  \item{transformMethod}{a string: the type of transformation method.
Available types: "SCS" and "snyder"}
}
\value{
Hydrogaph of direct runoff
}
\seealso{ \code{\link[RHMS]{sim}} }
\examples{

Area=200
lossMethod<-"SCS"
lossParams<-list(CN=65)
transformMethod<-c("snyder","SCS")
simulation<-c(interval=3600,period=100)
precipitation<-sin(seq(0.1,pi-0.1,length.out=10))*20
transformParams=list(Tlag=4,Cp=0.15,Ct=2,L=100,Lc=15)

SCS_loss<-loss(precipitation,lossParams,simulation,lossMethod)
snyder_transformation<-transform(rainfall=SCS_loss,
                                 transformParams,
                                 Area,
                                 simulation,
                                 transformMethod[1])
SCS_transformation<-transform(rainfall=SCS_loss,
                                 transformParams,
                                 Area,
                                 simulation,
                                 transformMethod[2])
}
\author{
Rezgar Arabzadeh
}
\keyword{ array }