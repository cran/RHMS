\name{reservoirRouting}
\alias{reservoirRouting}
\title{
reservoir routing
}
\description{
base function for routing reservoir floods using classical Muskingum technique
}
\usage{
reservoirRouting(inflow, ratingCurve, dischargeCurve,
                 initialStorage, capacity, 
                 simulation = c(interval = 3600 * 1, period = NA))
}
\arguments{
  \item{inflow}{
a vector : a time series of direct inflow to be routed (cms)
}
  \item{ratingCurve}{
a data frame: a data frame at which its first collumn includes height (masl) and second collums presents equivalant volume to the height at first collumn (MCM)
}
  \item{dischargeCurve}{
a data frame: a data frame at which its first collumn includes height (masl) and second collums presents equivalant discharge rate to the height at first collumn (cms)
}
  \item{initialStorage}{
the initial storage of reservoir at the first time step of simulation (MCM)
}
  \item{capacity}{
the maximum volume of reservoir capacity (MCM)
}
  \item{simulation}{
a vectors: number of simulation time steps and the simulation period noted in seconds and a multiple of 3600s
}
}
\value{
a list: including inflow time series routing resaults and simulation details 
}
\references{
Chow, V. T., Maidment, D. R., & Mays, L. W. (1988). Applied hydrology.
}
\examples{
inflow1<-sin(seq(0,pi,length.out=50))*1000
ratingCurve1<-data.frame(s=0:49*2,h=100:149)
dischargeCurve1<-data.frame(q=0:9*250,h=140:149)
reservoir_sim<-reservoirRouting(inflow=inflow1, ratingCurve=ratingCurve1,
                                dischargeCurve=dischargeCurve1, 
                                initialStorage=80, capacity=80, 
                                simulation = c(interval = 3600 * 1, period = 60))
plot(reservoir_sim[,2],typ="o",
     ylab="Discharge rate (cms)",
     xlab="Time step")
lines(reservoir_sim[,4])
}
\author{
Rezgar Arabzadeh
}
\keyword{ list }
