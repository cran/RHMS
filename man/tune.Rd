\name{tune}
\alias{tune}
\title{
tunning an RHMS model
}
\description{
a function for tunning an RHMS model based on a set of observed time series, using \emph{particle swarm optimization}
}
\usage{

tune(object,targetObject,decisionObjects,
     observationTS,delay=0,
     transformBandWith=list(ct=c(1  , 2.5),
                            cp=c(0.1, 0.3),
                            cn=c(25 , 85 ),
                            k =c(0.1, 2 )),
     routingBandWith=list(manning = c(0.0001, 0.1),
                          x       = c(0.2   , 0.6), 
                          k       = c(1     , 5 )),
     maxiter=NA,update=FALSE,plot=FALSE)
}
\arguments{
  \item{object}{an object from class of \code{createBasin}}
  \item{targetObject}{an object from either of classes: \code{createDiversion}, \code{createReservoir}, \code{createSubbasin}, \code{createJunction}, \code{createReach} associated to the \code{observationTS}}
  \item{decisionObjects}{A list of objects, also, already existing in the \code{object} which their parameters needed to be optimized. They objects must be from either of classes: \code{createSubbasin}, \code{createReach}}
  \item{observationTS}{a vector: an observed flow time series  (cms)}
  \item{delay}{(optional) an integer presenting the number of time steps to delay \code{observationTS} time series}
  \item{transformBandWith}{an list: a list of vector(s), including upper and lower limit of parameters of tansformation methods. Each parameter search domain is set as a two-value vector, whose first element indicates lower limit and second elemnt is upper limit.
      \itemize{
        \item \code{Ct}=[1, 2.5] and \code{Cp}=[0.1, 0.3] are parameters for \code{"Snyder"} Unit Hydrograph (SUH)
        \item \code{cn}=[25, 85] curve number for \code{"SCS"} loss method
        \item \code{k} for \code{"horton"} loss method
      }
  }
  \item{routingBandWith}{an list: a list of vector(s), including upper and lower limit of parameters of routing methods. Each parameter search domain is set as a two-value vector, whose first element indicates lower limit and second elemnt is upper limit.
      \itemize{
        \item \code{manning}=[0.0001, 0.1] is a parameter used \code{"muskingumcunge"} method
        \item \code{x} = [0.2, 0.6] and \code{k}=[1, 5] belong to \code{"muskingum"} channel routing method
      }
  }
  \item{maxiter}{(optional) an integer: maximum number of iterations. default to the square of dimension of decision variables}
  \item{plot}{(optional) logical: plots the optimization results}
  \item{update}{(optional) logical: If FALSE, the optimized parameter(s) are returned,If TRUE, the calibrated object from class of \code{createBasin} is returned}}
\value{
a vector of tunned parameters or an object from class of \code{createBasin}
}
\references{
Kennedy, J. (1997). "The particle swarm: social adaptation of knowledge". Proceedings of IEEE International Conference on Evolutionary Computation. pp. 303-308
}
\examples{

J1<-createJunction (name="J1")
R1<-createReach(name="R1",routingMethod="muskingum",
                routingParams=list(k=3,x=0.2),
                downstream=J1)
R2<-createReach(name="R2",routingMethod="muskingumcunge",
                routingParams=list(bedWith=50,
                                   sideSlope=2,
                                   channelSlope=0.0005,
                                   manningRoughness=0.025,
                                   riverLength=100),
                downstream=J1)
S1<-createSubbasin(name = "S1", 
                   precipitation=sin(seq(0,pi,length.out=20))*40,
                   Area=100,downstream=R1,
                   transformMethod="SCS",lossMethod="SCS",
                   transformParams=list(Tlag=4),lossParams=list(CN=60))
S2<-createSubbasin(name = "S2", 
                   precipitation=sin(seq(0,pi,length.out=20))*30,
                   Area=300,downstream=R2,
                   transformMethod="snyder",lossMethod="horton",
                   transformParams=list(Cp=0.17,Ct=2,L=30,Lc=15),
                   lossParams=list(f0=10,f1=4,k=1))

basin1<-createBasin(name = "Ghezil_Ozan",
                    simulation=list(start='2000-01-01',
                                    end  ='2000-01-05',
                                    by   =3600))
basin1<-addObjectToBasin(S1, basin1)
basin1<-addObjectToBasin(S2, basin1)
basin1<-addObjectToBasin(R1, basin1)
basin1<-addObjectToBasin(R2, basin1)
basin1<-addObjectToBasin(J1, basin1)

\dontrun{plot(basin1)}

simulated<-sim(basin1)
plot(simulated)
observationTS1<-simulated$operation$junctions[[1]]$outflo[,1]
set.seed(1)
observationTS1<-observationTS1+rnorm(length(observationTS1),0,25)
y<-observationTS1; x<-1:length(observationTS1)
observationTS1<-predict(loess(y~x),x)
observationTS1[which(observationTS1<0)]<-0
observationTS<-observationTS1
plot(simulated$operation$junctions[[1]]$outflow[,1],typ='o',ylab='flow rate (cms)',xlab='time step')
lines(observationTS,col=2)

transformBandWith=list(ct=c(1  ,2.5),
                       cp=c(0.1,0.3),
                       cn=c(25 ,85) ,
                       k =c(0.1,2))
routingBandWith=list(maning = c(0.0001,0.1), 
                     x      = c(0.2   ,0.6),
                     k      = c(1     ,5))
targetObject<-J1
decisionObjects<-list(R1,R2,S1,S2)
\dontrun{
tune(object=basin1, 
     targetObject=targetObject,
     decisionObjects=decisionObjects,
     observationTS=observationTS,
     routingBandWith=routingBandWith,
     transformBandWith=transformBandWith,
     plot=TRUE)
}
}
\author{
Rezgar Arabzadeh
}

\keyword{ optimize }