\name{RHMS-package}
\alias{RHMS-package}
\alias{RHMS}
\docType{package}
\title{
Hydrologic Modelling System for R Users
}
\description{
The RHMS package provides tools to R users for simulation of hydrologic events. The packages includes functions and methods for building, simulation, visualization, and calibration of hydrologic systems.
}
\details{
\tabular{ll}{
Package: \tab RHMS\cr
Type: \tab Package\cr
Version: \tab 1.3\cr
Date: \tab 2017-07-27\cr
License: \tab GPL-3\cr
}
the package include three major types of functions as follows:


1- functions for construction and manipulatation of hydrologic features.

   a) \code{\link[RHMS]{createBasin}}.         constructor for basin

   b) \code{\link[RHMS]{createJunction}}.      constructor for junction

   c) \code{\link[RHMS]{createReach}}.         constructor for reach, rivers, and channels

   d) \code{\link[RHMS]{createReservoir}}.     constructor for reservoirs

   e) \code{\link[RHMS]{createSubbasin}}.      constructor for sub-bains

   f) \code{\link[RHMS]{createDiversion}}.     constructor for diversions

   g) \code{\link[RHMS]{addObjectToBasin}}.    adds objects form mentioned above constructors to a basin inherited from class of \code{createBasin}




2- functions for analysis and simulation of hydrologic events.

   a) \code{\link[RHMS]{reachRouting}}.         routes a flood in a channel or river

   b) \code{\link[RHMS]{reservoirRouting}}.     routes a flood in a reservoir

   c) \code{\link[RHMS]{transform}}.            trnasforms a rainfall event to runoff

   d) \code{\link[RHMS]{loss}}.                 computes the excess rainfall and loss

   e) \code{\link[RHMS]{sim}}.                  simulates an objects inherited from class of \code{createBasin}




3- functions for tunning, summerizing, and visualization.

   a) \code{\link[RHMS]{plot.sim}}.                 plots the results of simulations for the objects built-in the \code{basin}

   b) \code{\link[RHMS]{summary.sim}}.            summerzies the simulation results in the tabular form for every objects existing in the \code{basin}

   c) \code{\link[RHMS]{tune}}.                 calibrates an objects inherited from class of \code{createBasin}

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