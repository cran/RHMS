\name{addObjectToBasin}
\alias{addObjectToBasin}
\title{
adds an object to basin
}
\description{
adds an object inherited from either of \code{RHMS} package constructors to an object instantiated by class of \code{createBasin}.
}
\usage{
addObjectToBasin(object, basin)
}
\arguments{
  \item{object}{
an object inherited from one of the following classes: \code{\link[RHMS]{createReservoir}}, \code{\link[RHMS]{createReach}}, \code{\link[RHMS]{createSubbasin}}, \code{\link[RHMS]{createJunction}}
}  
  \item{basin}{
an object inherited from class of \code{createBasin}
}
}
\value{
an object from class of \code{createBasin}
}
\seealso{ \code{\link[RHMS]{sim}} }
\examples{
storageElevationCurve<-data.frame(s=0:100*10,h=100:200)
dischargeElevationCurve<-data.frame(q=seq(0,5000,length.out=10),
                                    h=seq(180,200,length.out=10))
geometry<-list(storageElevationCurve=storageElevationCurve,
               dischargeElevationCurve=dischargeElevationCurve,
               capacity=800)
Res1<-createReservoir(name = "Reservoir1",
                      geometry=geometry,initialStorage=550)
R1<-createReach(name="Reach1",routingParams=list(k=5,x=0.3))
R2<-createReach(name="Reach2",routingParams=list(k=5,x=0.3))
R3<-createReach(name="Reach3",routingParams=list(k=5,x=0.3))
R4<-createReach(name="Reach4",routingMethod="muskingumcunge",
                              routingParams=list(bedWith=100,
                                                 sideSlope=2,
                                                 channelSlope=0.01,
                                                 manningRoughness=0.05,
                                                 riverLength=120))
D1<-createDiversion(name="Diversion1",capacity=80)
                
Junc1<-createJunction(name = "Junc1")
S1<-createSubbasin(name="Sub1",Area=500,
                   precipitation=round(sin(seq(0,pi,length.out=24))*20),
                   transformMethod="SCS",lossMethod="SCS",BFSMethod='recession',
                   transformParams=list(Tlag=4),lossParams=list(CN=70),BFSParams=list(k=1.1))
S2<-createSubbasin(name="Sub2",Area=500,
                   precipitation=round(sin(seq(0,pi,length.out=24))*20),
                   transformMethod="SCS",lossMethod="SCS",BFSMethod='recession',
                   transformParams=list(Tlag=4),lossParams=list(CN=70),BFSParams=list(k=1.1))
S3<-createSubbasin(name="Sub3",Area=650,
                   precipitation=round(sin(seq(0,pi,length.out=24))*20),
                   transformMethod="snyder",lossMethod="horton",
                   transformParams=list(Cp=0.17,Ct=1.5,L=140,Lc=30),
                   lossParams=list(f0=5,f1=1,k=1))

S1<-set.as(R2,S1,'downstream')
R2<-set.as(Junc1,R2,'downstream')
Junc1<-set.as(R1,Junc1,'downstream')
R1<-set.as(Res1,R1,'downstream')
S3<-set.as(R3,S3,'downstream')
R3<-set.as(Junc1,R3,'downstream')
S2<-set.as(R4,S2,'downstream')
R4<-set.as(D1,R4,'downstream')
D1<-set.as(Junc1,D1,'downstream')
D1<-set.as(S1,D1,'divertTo')

basin1<-createBasin(name = "Unknown", simulation=list(start='2000-01-01',end='2000-01-10',by=7200))
basin1<-addObjectToBasin(Junc1, basin1)
basin1<-addObjectToBasin(R1, basin1)
basin1<-addObjectToBasin(R2, basin1)
basin1<-addObjectToBasin(R3, basin1)
basin1<-addObjectToBasin(R4, basin1)
basin1<-addObjectToBasin(S1, basin1)
basin1<-addObjectToBasin(S2, basin1)
basin1<-addObjectToBasin(S3, basin1)
basin1<-addObjectToBasin(Res1, basin1)
basin1<-addObjectToBasin(D1, basin1)

\dontrun{plot(basin1)}

object<-sim(basin1)

plot(object)

summary(object)
}
\author{
Rezgar Arabzadeh
}

\keyword{ iteration }