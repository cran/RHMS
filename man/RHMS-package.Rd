\name{RHMS-package}
\alias{RHMS-package}
\alias{RHMS}
\docType{package}
\title{
Hydrologic Modelling System for R Users
}
\description{
The RHMS package provides functionalities for R users to simulate hydrologic events. The packages includes functions to build, simulate, visualize, and calibrating hydrologic features.
}
\details{
\tabular{ll}{
Package: \tab RHMS\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2017-03-28\cr
License: \tab GPL-3\cr
}
the package include three major types of functions as follows:
   1- functions to build and manipulate hydrologic features.

      a) createBasin.
      b) createJunction.
      c) createReach.
      d) createReservoir.
      e) createSubbasin.
      f) addObjectToBasin.

   2- functions to analyze and simulate the hydrologic event.
      a) reachRouting.
      b) reservoirRouting.
      d) sim.

   3- functions to tunning, summerize, and visualize resaults of simulations.
      a) plot.
      b) summerize.
      c) tune.
}
\seealso{ \code{\link[RHMS]{sim}} }
\author{
Rezgar Arabzadeh ; Shahab Araghinejad

Maintainer: Rezgar Arabzadeh <rezgararabzadeh@ut.ac.ir>
}
\references{
Chow, V. T., Maidment, D. R., & Mays, L. W. (1988). Applied hydrology.
}
\keyword{ package }