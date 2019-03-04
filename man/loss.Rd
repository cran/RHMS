\name{loss}
\alias{loss}
\title{
Excess rainfall computation
}
\description{
this function provides parametric methods (e.g. \code{"horton"} and \code{"SCS"}) to compute loss and direct runoff depths
}
\usage{
loss(precipitation,lossMethod,lossParams)
}
\arguments{
  \item{precipitation}{a vector of precipitation time series(mm)}
  \item{lossMethod}{a string including the type of lossMethod: \code{"SCS"} and \code{"horton"}. default to \code{"SCS"} method}
  \item{lossParams}{a list of parameters associated to the selcted type of \code{lossMethod}:
     \itemize{
         \item the curve number, \code{CN}, and \code{imperviousness} in precentage for \code{"SCS"} method
         \item \code{f0}, \code{f1}, \code{k} for \code{"horton"} method
         \item \code{timeInterval}: the interval of each steps in seconds needed for \code{"horton"} method
     }
  }
}
\value{
a dataframe: including precipitation, loss, and exess rainfall depth
}
\seealso{\code{\link[RHMS]{transform}}}
\examples{
precipitation<-sin(seq(0.1,pi-0.1,length.out=20))*30
lossParams<-list(f0=20,f1=5,k=2,timeInterval=3600,CN=65)
lossMethod<-c("horton","SCS")
(Horton_loss<-loss(precipitation,lossMethod[1],lossParams))
(SCS_loss<-loss(precipitation,lossMethod[2],lossParams))
}
\author{
Rezgar Arabzadeh
}
\keyword{ matrix }
