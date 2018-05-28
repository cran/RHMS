\name{baseFlowSeparation.default}
\alias{baseFlowSeparation.default}
\title{
default function for class of \code{baseFlowSeparation}
}
\description{
default function of methods separating baseflow for a given flow discharge
}
\usage{
\method{baseFlowSeparation}{default}(Q,BFSMethod='none'                 ,
                                       BFSParams=list(alpha=NULL        ,
                                                      BFI=NULL          ,
                                                      k=NULL            ,
                                                      timeInterval=NULL),
                                     plot=TRUE)
}
\arguments{
  \item{Q}{a vector: The flow time series (cms)}
  \item{BFSMethod}{a string: The method of base flow separation. Available methods: \code{'nathan'}, \code{'chapman'}, \code{'eckhardt'}, \code{'recession'}}
  \item{BFSParams}{a list including parameters associated with the method coerced in \code{'BFSMethod'}. \code{alpha} is in \code{[0, 1]} interval required for \code{'nathan'}, \code{'chapman'}, and \code{'eckhardt'} methods; \code{BFI} in \code{[0, 1]} interval required for \code{'eckhardt'}; \code{k} in \code{[0, 1]} interval and \code{timeInterval} in day required for \code{'recession'} method}
  \item{plot}{logical: whether to plot the result or not}
}
\value{
a list: an \code{object} from class of \code{baseFlowSeparation} consisting matrix of results available at object$operation.
}
\seealso{ \code{\link[RHMS]{createSubbasin}}}
\author{
Rezgar Arabzadeh
}
\keyword{ list }