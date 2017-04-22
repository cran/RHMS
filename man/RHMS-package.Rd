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
Version: \tab 1.1\cr
Date: \tab 2017-03-28\cr
License: \tab GPL-3\cr
}
the package include three major types of functions as follows:

   1- functions for construction and manipulatation of hydrologic features.

      a) \emph{createBasin}.          constructor for basin

      b) \emph{createJunction}.       constructor for junction

      c) \emph{createReach}.          constructor for reach, rivers, and channels

      d) \emph{createReservoir}.      constructor for reservoirs

      e) \emph{createSubbasin}.       constructor for sub-bains

      f) \emph{createDiversion}.      constructor for diversions

      g) \emph{addObjectToBasin}.     adding objects form mentioned above constructors to a basin

   2- functions for analysis and simulation of hydrologic events.

      a) \emph{reachRouting}.         routes a flood in a channel or river

      b) \emph{reservoirRouting}.     routes a flood in a reservoir

      c) \emph{transform}.            trnasforms a rainfall event to runoff

      d) \emph{loss}.                 computes the excess rainfall and loss

      e) \emph{sim}.                  simulates an objects inherited from class of \emph{createBasin}

   3- functions for tunning, summerizing, and visualization.

      a) \emph{plot}.

      b) \emph{summerize}.

      c) \emph{tune}.

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