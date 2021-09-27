\name{RHMS-package}
\alias{RHMS-package}
\alias{RHMS}
\docType{package}
\title{
Hydrologic Modelling System for R Users
}
\description{
The RHMS package provides tools to R users for simulation of hydrologic events. The packages includes functions and methods for building, simulation, visualization, and calibration of a hydrologic model.
}
\details{
\tabular{ll}{
Package: \tab RHMS\cr
Type: \tab Package\cr
Version: \tab 1.7\cr
Date: \tab 2021-09-27\cr
License: \tab GPL-3\cr
}
the package include three major types of functions as follows:

1- functions for construction and manipulatation of hydrologic features.
\itemize{
\item \code{\link[RHMS]{createBasin}}.         constructor for basin
\item \code{\link[RHMS]{createJunction}}.      constructor for junction
\item \code{\link[RHMS]{createReach}}.         constructor for reach, rivers, and channels
\item \code{\link[RHMS]{createReservoir}}.     constructor for reservoirs
\item \code{\link[RHMS]{createSubbasin}}.      constructor for sub-bains
\item \code{\link[RHMS]{createDiversion}}.     constructor for diversions
\item \code{\link[RHMS]{set.as}}.              objects connector
\item \code{\link[RHMS]{addObjectToBasin}}.    adds objects form above constructors to a basin inherited from class of \code{createBasin}
}

2- functions for analysis and simulation of hydrologic events.
\itemize{
\item \code{\link[RHMS]{reachRouting}}.         routes a flood in a channel or river
\item \code{\link[RHMS]{reservoirRouting}}.     routes a flood in a reservoir
\item \code{\link[RHMS]{transform}}.            trnasforms a rainfall event to runoff
\item \code{\link[RHMS]{loss}}.                 computes excess rainfall and loss depths
\item \code{\link[RHMS]{baseFlowSeparation}}.   separates baseflow from a given discharge series
\item \code{\link[RHMS]{abstraction}}.          computes simple surface and canopy methods
\item \code{\link[RHMS]{sim}}.                  simulates an objects inherited from class of \code{createBasin}
}

3- functions for tunning, summerizing, and visualization.
\itemize{
\item \code{\link[RHMS]{plot.sim}}.             plots the objects inherited from class of \code{\link[RHMS]{sim}}
\item \code{\link[RHMS]{plot.createBasin}}.     plots the objects inherited from class of \code{\link[RHMS]{createBasin}}
\item \code{\link[RHMS]{summary.sim}}.          summerzies the simulation results in the tabular form for every objects existing in the \code{basin}
\item \code{\link[RHMS]{tune}}.                 calibrates an objects inherited from class of \code{createBasin}
}
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