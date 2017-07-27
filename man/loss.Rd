\name{loss}
\alias{loss}
\title{
Excess rainfall computation
}
\description{
this function provides parametric methods (e.g. \code{"horton"} and \code{"SCS"}) to compute loss and direct runoff depths
}
\usage{
loss(precipitation,lossParams,
simulation,lossMethod)
}
\arguments{
  \item{precipitation}{a vector of precipitation time series(mm)}
  \item{lossParams}{a list: list of parameters associated to the selcted type of \code{lossMethod}:
  \code{CN} for \code{"SCS"} and \code{f0}, \code{f1}, \code{k} for \code{"horton"}}
  \item{simulation}{a vector of simulation interval (in second) and time steps (an integer)}
  \item{lossMethod}{a string including the type of lossMethod: \code{"SCS"} and \code{"horton"}}
}
\value{
a dataframe: including precipitation, loss, and exess rainfall depth
}
\seealso{\code{\link[RHMS]{transform}}}
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
