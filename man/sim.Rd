\name{sim}
\alias{sim}
\title{
RHMS simulation function
}
\description{
HMS package simulation function for sim class
}
\usage{
sim(object)
}
\arguments{
  \item{object}{
an object from class of createBasin
}
}
\value{
a list: the same as objects inherited from class of createBasin
}
\references{
NRCS, U. (1986). Urban hydrology for small watersheds-Technical Release 55 (TR55). Water Resources Learning Center. Washington DC.

Chow, V. T., Maidment, D. R., & Mays, L. W. (1988). Applied hydrology.
}
\examples{

data(Zaab)

Zaab_sim<-sim(Zaab)

\dontrun{plot(Zaab_sim)}

summary(Zaab_sim)

}
\author{
Rezgar Arabzadeh
}
\keyword{ list }
