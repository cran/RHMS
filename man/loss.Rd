\name{loss}
\alias{loss}
\title{
Base for computation of rainfall loss
}
\description{
this function provides parametric methods (e.g. 'horton' and 'SCS') to compute loss and direct runoff depth
}
\usage{
loss(precipitation,lossParams=list(f0,f1,k,CN),
simulation=c(interval=3600,period=NA),lossMethod)
}
\arguments{
  \item{precipitation}{a vector of precipitation time series}
  \item{lossParams}{a list: list of parameters associated to the selcted type of 'transformMethod':
  'CN' for 'SCS' and 'f0', 'f1', 'k' for 'horton'}
  \item{simulation}{a vector of simulation interval (in second) and time step (an integer)}
  \item{lossMethod}{a string denoting the type of lossMethod: 'SCS' and 'horton'}
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
