\name{sim}
\alias{sim}
\title{
RHMS simulation function
}
\description{
simulates an object inherited form class of \code{createBasin}
}
\usage{
sim(object)
}
\arguments{
  \item{object}{
an object from class of \code{createBasin}
}
}
\value{
a list: the same as objects inherited from class of \code{createBasin}
}
\references{
NRCS, U. (1986). Urban hydrology for small watersheds-Technical Release 55 (TR55). Water Resources Learning Center. Washington DC.

Chow, V. T., Maidment, D. R., & Mays, L. W. (1988). Applied hydrology.
}
\examples{

data(Zaab)
geometry<-list(storageElevationCurve=Zaab[[1]]$Kanisib$storageElevationCurve,
               dischargeElevationCurve=Zaab[[1]]$Kanisib$dischargeElevationCurve,
               capacity=Zaab[[1]]$Kanisib$capacity)
KanisibDam<-createReservoir(name="Kanisib", geometry=geometry,
                            initialStorage=geometry$capacity)
R1<-createReach(name="Reach 1",downstream=KanisibDam)
J1<-createJunction(name="Junction 1",downstream=R1)
R2<-createReach(name="Reach 2",downstream=J1)
R3<-createReach(name="Reach 3",downstream=J1)
J2<-createJunction(name="Junction 1",downstream=R2)
R4<-createReach(name="Reach 4",downstream=J2)
R5<-createReach(name="Reach 5",downstream=J2)
geometry<-list(storageElevationCurve=Zaab[[1]]$Gordebin$storageElevationCurve,
               dischargeElevationCurve=Zaab[[1]]$Gordebin$dischargeElevationCurve,
               capacity=Zaab[[1]]$Gordebin$capacity)
GordebinDam<-createReservoir(name="Gordebin", geometry=geometry,
                             initialStorage=geometry$capacity,downstream=R4)
R6<-createReach(name="Reach 6",downstream=GordebinDam)
Zangabad<-createSubbasin(name="Zangabad",
                         precipitation=Zaab[[2]]$zangabad,
                         Area=338.2,
                         downstream=R6,
                         lossMethod="SCS",
                         transformParams=list(Tlag=4),
                         lossParams=list(CN=70))
geometry<-list(storageElevationCurve=Zaab[[1]]$Silveh$storageElevationCurve,
               dischargeElevationCurve=Zaab[[1]]$Silveh$dischargeElevationCurve,
               capacity=Zaab[[1]]$Silveh$capacity)
SilvehDam<-createReservoir(name="Silveh", geometry=geometry,
                           initialStorage=geometry$capacity,downstream=R5)
R7<-createReach(name="Reach 7",downstream=SilvehDam)
Darbekaykhaneh<-createSubbasin(name="Darbekaykhaneh",
                         precipitation=Zaab[[2]]$darbekaykhaneh,
                         Area=338.8,
                         downstream=R7,
                         lossMethod="SCS",
                         transformParams=list(Tlag=3),
                         lossParams=list(CN=65))
D1<-createDiversion(name="Diversion 1",downstream=R3,
                    divertTo=SilvehDam,capacity=100)
R8<-createReach(name="Reach 8",downstream=D1)
Pardanan<-createSubbasin(name="Pardanan",
                         precipitation=Zaab[[2]]$pardanan,
                         Area=200.1,
                         downstream=R8,
                         lossMethod="SCS",
                         transformParams=list(Tlag=2),
                         lossParams=list(CN=75))
ZaabRB<-createBasin(name="Zaab",
                    simulation=list(start='2000-01-01', 
                                    end  ='2000-01-15',
                                    by   =3600))
ZaabRB<-addObjectToBasin(R1,ZaabRB)
ZaabRB<-addObjectToBasin(R2,ZaabRB)
ZaabRB<-addObjectToBasin(R3,ZaabRB)
ZaabRB<-addObjectToBasin(R4,ZaabRB)
ZaabRB<-addObjectToBasin(R5,ZaabRB)
ZaabRB<-addObjectToBasin(R6,ZaabRB)
ZaabRB<-addObjectToBasin(R7,ZaabRB)
ZaabRB<-addObjectToBasin(R8,ZaabRB)
ZaabRB<-addObjectToBasin(J1,ZaabRB)
ZaabRB<-addObjectToBasin(J2,ZaabRB)
ZaabRB<-addObjectToBasin(D1,ZaabRB)
ZaabRB<-addObjectToBasin(SilvehDam,ZaabRB)
ZaabRB<-addObjectToBasin(GordebinDam,ZaabRB)
ZaabRB<-addObjectToBasin(KanisibDam,ZaabRB)
ZaabRB<-addObjectToBasin(Pardanan,ZaabRB)
ZaabRB<-addObjectToBasin(Zangabad,ZaabRB)
ZaabRB<-addObjectToBasin(Darbekaykhaneh,ZaabRB)

\dontrun{plot(ZaabRB)}

plot(sim(ZaabRB))
}
\author{
Rezgar Arabzadeh
}
\keyword{ list }
