\name{baseFlowSeparation.base}
\alias{baseFlowSeparation.base}
\title{
base function for class of \code{baseFlowSeparation}
}
\description{
base function of methods separating baseflow for a given flow discharge.
}
\usage{
\method{baseFlowSeparation}{base}(discharge,BFSMethod,BFSParams,plot)
}
\arguments{
  \item{discharge}{a vector of flow time series (cms) or an object inherited from class of \code{'transform'}}
  \item{BFSMethod}{a string: The method of base flow separation. Available methods: \code{'nathan'}, \code{'chapman'}, \code{'eckhardt'}, \code{'recession'}}
  \item{BFSParams}{a list including parameters associated with the method coerced in \code{'BFSMethod'}.
     \itemize{
         \item \code{alpha} is in \code{[0, 1]} interval required for \code{'nathan'}, \code{'chapman'}, and \code{'eckhardt'} methods
         \item \code{BFI} is in \code{[0, 1]} interval required for \code{'eckhardt'} method
         \item \code{k} is in \code{[0, 1]} interval and \code{timeInterval} is in day required for \code{'recession'} method
     }
  }
  \item{plot}{(optional) logical statement to plot the result or not. default to \code{FALSE}}
}
\value{
a matrix: A matrix of results including computed separated flow for \code{Q} series
}
\seealso{ \code{\link[RHMS]{baseFlowSeparation}}}
\author{
Rezgar Arabzadeh
}
\keyword{array}