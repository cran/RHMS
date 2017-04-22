\name{loss}
\alias{loss}
\title{
Excess rainfall computation
}
\description{
this function provides parametric methods (e.g. \emph{'horton'} and \emph{'SCS'}) to compute loss and direct runoff depth
}
\usage{
loss(precipitation,lossParams=list(f0,f1,k,CN),
simulation=c(interval=3600,period=NA),lossMethod)
}
\arguments{
  \item{precipitation}{a vector of precipitation time series(mm)}
  \item{lossParams}{a list: list of parameters associated to the selcted type of \emph{'lossMethod'}:
  \emph{'CN'} for \emph{'SCS'} and \emph{'f0'}, \emph{'f1'}, \emph{'k'} for \emph{'horton'}}
  \item{simulation}{a vector of simulation interval (in second) and time steps (an integer)}
  \item{lossMethod}{a string including the type of lossMethod: \emph{'SCS'} and \emph{'horton'}}
}
\value{
a dataframe: including precipitation, loss, and exess rainfall depth
}
\seealso{\code{\link[RHMS]{createSubbasin}}}
\examples{
precipitation<-sin(seq(0.1,pi-0.1,length.out=10))*30
lossParams<-list(f0=20,f1=5,k=2,CN=65)
simulation<-c(interval=3600,period=NA)
lossMethod<-c("horton","SCS")
Horton_loss<-loss(precipitation,lossParams,simulation,lossMethod[1])
SCS_loss<-loss(precipitation,lossParams,simulation,lossMethod[2])
}
\author{
Rezgar Arabzadeh
}
\keyword{ graphs }
