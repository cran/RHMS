\name{tune}
\alias{tune}
\title{
tunning an RHMS model
}
\description{
a function for tunning an RHMS model based on a set of observed time series, using \emph{'particle swarm optimization'}
}
\usage{
tune(object, observationTS, labelTS, delay=0,tuneLabels,maxiter,
     transformBandWith=list(ct=c(1  ,2.5),
                            cp=c(0.1,0.3),
                            cn=c(25 ,85 ),
                            k =c(0.1,2  )),
     routingBandWith=list(manning = c(0.0001, 0.1),
                          x       = c(0.2   , 0.6), 
                          k       =c(1      , 5  )),
     update=FALSE)
}
\arguments{
  \item{object}{
an object from class of \emph{createBasin}
}
  \item{observationTS}{
a vector: an observed time series (cms)
}
  \item{labelTS}{
an integer: label of the object associated to the \emph{'observationTS'}
}
  \item{delay}{
an integer: presenting the number of time steps to delay direct inflow time series
}
  \item{tuneLabels}{
a vector of integers: the label(s) of element(s) to be tunned
}
  \item{maxiter}{
an integer: maximum number of iterations
}
  \item{transformBandWith}{
an list: a list of vector(s), including upper and lower limit of parameters of tansformation methods. Each parameter search domain is set as a two-value vector, whose first element includes lower limit and second elemnt is upper limit.
\emph{Ct=[1, 2.5]} and \emph{Cp=[0.1, 0.3]} are parameters for \emph{Snyder Unit Hydrograph (SUH)}, \emph{cn=[25, 85]} curve number for \emph{SCS} loss method,  and \emph{'k'} for \emph{horton} loss method.
}
  \item{routingBandWith}{
an list: a list of vector(s), including upper and lower limit of parameters of routing methods. Each parameter search domain is set as a two-value vector, whose first element includes lower limit and second elemnt is upper limit.
\emph{manning=[0.0001, 0.1]} is a parameter used \emph{muskingum cunge} method, and \emph{x = [0.2, 0.6]} and \emph{k=[1, 5]} belong to \emph{muskingum} channel routing method.
}
  \item{update}{
logical: If FALSE, the optimized parameter(s) are returned,If TRUE, the calibrated object frpm class of \emph{createBasin} is returned}
}
\value{
a vector of tunned parameters or an object from class of \emph{createBasin}
}
\references{
Kennedy, J. (1997). "The particle swarm: social adaptation of knowledge". Proceedings of IEEE International Conference on Evolutionary Computation. pp. 303-308
}
\examples{

S1<-createSubbasin(name = "S1", 
                   precipitation=sin(seq(0,pi,length.out=20))*40,
                   Area=100,label=1, downstream=3,
                   transformMethod="SCS",lossMethod="SCS",
                   transformParams=list(Tlag=4),lossParams=list(CN=60))
S2<-createSubbasin(name = "S2", 
                   precipitation=sin(seq(0,pi,length.out=20))*30,
                   Area=300,label=2, downstream=4,
                   transformMethod="snyder",lossMethod="horton",
                   transformParams=list(Cp=0.17,Ct=2,L=30,Lc=15),
                   lossParams=list(f0=10,f1=4,k=1))

R1<-createReach(name="R1",routingMethod="muskingum",
                routingParams=list(k=3,x=0.2),
                label=3,downstream=5)
R2<-createReach(name="R2",routingMethod="muskingumcunge",
                routingParams=list(bedWith=50,
                                   sideSlope=2,
                                   channelSlope=0.0005,
                                   manningRoughness=0.025,
                                   riverLength=100),
                label=4,downstream=5)
J1<-createJunction (name="J1",downstream=NA,label=5)

basin1<-createBasin(name = "Ghezil_Ozan", simPeriod=100, interval=3600)
basin1<-addObjectToBasin(S1, basin1, "subbasin")
basin1<-addObjectToBasin(S2, basin1, "subbasin")
basin1<-addObjectToBasin(R1, basin1, "reach")
basin1<-addObjectToBasin(R2, basin1, "reach")
basin1<-addObjectToBasin(J1, basin1, "junction")

simulated<-sim(basin1)

observationTS1<-simulated$operation$junctions[[1]]$outflow
set.seed(1)
observationTS1<-observationTS1+rnorm(length(observationTS1),0,200)
y<-observationTS1; x<-1:length(observationTS1)
observationTS1<-predict(loess(y~x),x)
observationTS1[which(observationTS1<0)]<-0

observationTS<-observationTS1
maxiter <- 20
transformBandWith=list(ct=c(1  ,2.5),
                       cp=c(0.1,0.3),
                       cn=c(25 ,85) ,
                       k =c(0.1,2))
routingBandWith=list(maning = c(0.0001,0.1), 
                     x      = c(0.2   ,0.6),
                     k      = c(1     ,5))
labelTS<-5 ; tuneLabels<-1:4
\dontrun{
tune(basin1, observationTS,maxiter,
     labelTS,tuneLabels,
     transformBandWith,routingBandWith)
}
}
\author{
Rezgar Arabzadeh
}

\keyword{ optimize }
