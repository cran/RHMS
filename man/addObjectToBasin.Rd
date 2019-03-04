\name{addObjectToBasin}
\alias{addObjectToBasin}
\title{
adds an object to basin
}
\description{
adds an object inherited from a hydrologic feature class to a \code{basin} instantiated from class of \code{createBasin}.
}
\usage{
addObjectToBasin(object, basin)
}
\arguments{
  \item{object}{
an object to be added to the \code{basin} inherited from one of the following classes: \code{'createReservoir'},\code{'createReach'},\code{'createSubbasin'},\code{'createJunction'}
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
                      geometry=geometry,initialStorage=800)
R1<-createReach(name="Reach1",downstream=Res1,
                routingParams=list(k=3,x=0.2))
Junc1<-createJunction(name = "Junc1", downstream=R1)
R2<-createReach(name="Reach2",downstream=Junc1,
                routingParams=list(k=3,x=0.2))
R3<-createReach(name="Reach3",downstream=Junc1,
                routingParams=list(k=3,x=0.2))
S1<-createSubbasin(name="Sub1",Area=500,downstream=R2,
                   precipitation=round(sin(seq(0,pi,length.out=24))*20),
                   transformMethod="SCS",lossMethod="SCS",BFSMethod='recession',
                   transformParams=list(Tlag=4),lossParams=list(CN=70),BFSParams=list(k=1.1))
D1<-createDiversion(name="Diversion1",downstream=Junc1,
                    divertTo=S1,capacity=80)
R4<-createReach(name="Reach4",downstream=D1,
                routingMethod="muskingumcunge",
                routingParams=list(bedWith=100,
                                   sideSlope=2,
                                   channelSlope=0.01,
                                   manningRoughness=0.05,
                                   riverLength=120))
S2<-createSubbasin(name="Sub2",Area=500,downstream=R4,
                   precipitation=round(sin(seq(0,pi,length.out=24))*20),
                   transformMethod="SCS",lossMethod="SCS",BFSMethod='recession',
                   transformParams=list(Tlag=4),lossParams=list(CN=70),BFSParams=list(k=1.1))
S3<-createSubbasin(name="Sub3",Area=650,downstream=R3,
                   precipitation=round(sin(seq(0,pi,length.out=24))*20),
                   transformMethod="snyder",lossMethod="horton",
                   transformParams=list(Cp=0.17,Ct=1.5,L=140,Lc=30),
                   lossParams=list(f0=5,f1=1,k=1))
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