\name{reachRouting}
\alias{reachRouting}
\title{
channel routing computation
}
\description{
base function for flood routing using 
parameteric Muskingum and muskingum-cunge techniques.
}
\usage{
reachRouting(inflow,routingMethod="muskingum",
             routingParams=list(k=3,
                                x=0.2,
                                bedWith=NULL,
                                sideSlope=2,
                                channelSlope=NULL,
                                manningRoughness=0.025,
                                riverLength=NULL),
             simulation=c(interval=3600*1,period=NA))
}
\arguments{
  \item{inflow}{
a vector : direct inflow time series (cms)
}
  \item{routingMethod}{
a string: the type of channel routing method: \code{"muskingum"} or \code{"muskingumcunge"}
}
  \item{routingParams}{
a list : parameters associated to the \code{routingMethod}: \code{k} and \code{x} for \code{"muskingum"}, 
\code{bedWith} (m), \code{sideSlope} (m/m), \code{channelSlope} (m/m), 
 \code{manningRoughness}, \code{riverLength} (Km) for \code{"muskingumcunge"}
}
  \item{simulation}{
a vectors: number of simulation time steps and the simulation period in seconds.
}
}
\value{
a data.frame: including inflow time series routing resaults and simulation details 
}
\references{
Chow, V. T., Maidment, D. R., & Mays, L. W. (1988). Applied hydrology.
}
\examples{
inflow<-c(100,500,1500,2500,5000,11000,22000,28000,28500,26000,
          22000,17500,14000,10000,7000,4500,2500,1500,1000,500,100)
routingMethod<-c("muskingum","muskingumcunge")
routingParams<-list(k=3,x=0.2,bedWith=50,sideSlope=2,channelSlope=0.0001,
                    manningRoughness=0.01,riverLength=100)
simulation<-c(interval=3600*1,period=100)

reachRouting(inflow,routingMethod[1],routingParams,simulation)
reachRouting(inflow,routingMethod[2],routingParams,simulation)
}
\author{
Rezgar Arabzadeh
}
\keyword{ list }