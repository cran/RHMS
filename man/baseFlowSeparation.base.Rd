\name{baseFlowSeparation.base}
\alias{baseFlowSeparation.base}
\title{
base function for class of \code{baseFlowSeparation}
}
\description{
base function of methods separating baseflow for a given flow discharge.
}
\usage{
\method{baseFlowSeparation}{base}(Q,BFSMethod,BFSParams,plot)
}
\arguments{
  \item{Q}{a vector: The flow time series (cms)}
  \item{BFSMethod}{a string: The method of base flow separation. Available methods: \code{'nathan'}, \code{'chapman'}, \code{'eckhardt'}, \code{'recession'}}
  \item{BFSParams}{a list including parameters associated with the method coerced in \code{'BFSMethod'}. \code{alpha} is in \code{[0, 1]} interval required for \code{'nathan'}, \code{'chapman'}, and \code{'eckhardt'} methods; \code{BFI} in \code{[0, 1]} interval required for \code{'eckhardt'}; \code{k} in \code{[0, 1]} interval and \code{timeInterval} in day required for \code{'recession'} method}
  \item{plot}{logical: whether to plot the result or not}
}
\value{
a matrix: A matrix of results including computed separated flow for \code{Q} series
}
\seealso{ \code{\link[RHMS]{baseFlowSeparation}}}
\author{
Rezgar Arabzadeh
}
\keyword{array}