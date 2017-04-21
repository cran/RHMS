\name{reachRouting}
\alias{reachRouting}
\title{
channel routing
}
\description{
base function for routing channel floods using 
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
a vector : a time series of direct inflow to be routed (cms)
}
  \item{routingMethod}{
a string: the type of channel routing method: "muskingum" or "muskingumcunge"
}

  \item{routingParams}{
a list : parameters associated to the 'routingMethod': 'k' and 'x' for 'muskingum', 
'bedWith' (m), 'sideSlope' (m/m), 'channelSlope' (m/m), 
 'manningRoughness', 'riverLength' (Km) for 'muskingumcunge'
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