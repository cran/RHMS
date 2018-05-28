\name{baseFlowSeparation}
\alias{baseFlowSeparation}
\title{
Parametric methods for separating baseflow
}
\description{
This function calculates baseflow for agiven discharge series, \code{Q}, using a number of method proposed in \code{BFSMethod}.
}
\usage{
baseFlowSeparation(Q,BFSMethod,BFSParams,plot)
}
\arguments{
  \item{Q}{a vector: The flow time series (cms)}
  \item{BFSMethod}{a string: The method of base flow separation. Available methods: \code{'nathan'}, \code{'chapman'}, \code{'eckhardt'}, \code{'recession'}}
  \item{BFSParams}{a list including parameters associated with the method coerced in \code{'BFSMethod'}. \code{alpha} is in \code{[0, 1]} interval required for \code{'nathan'}, \code{'chapman'}, and \code{'eckhardt'} methods; base flow index,\code{BFI}, is in \code{[0, 1]} interval required for \code{'eckhardt'}; \code{k} in \code{[0, Inf]} interval and \code{timeInterval} in daily scale needed for \code{'recession'} method}
  \item{plot}{logical: whether to plot the result or not}
}
\value{
a list: an \code{object} from class of \code{baseFlowSeparation} consisting matrix of results available at object$operation.
}
\references{
Chapman, Tom. "A comparison of algorithms for stream flow recession and baseflow separation." Hydrological Processes 13.5 (1999): 701-714.}
\seealso{ \code{\link[RHMS]{baseFlowSeparation}}}
\examples{
Q<-Q<-(dnorm(seq(-3,4,length.out=200),-.3,1)+dnorm(seq(-1,7,length.out=200),4.5,1)*2)*1200
BFSMethod<-c('nathan','chapman','eckhardt','recession')
BFSParams<-list(alpha=0.6,BFI=0.3,k=1.1,timeInterval=15*60)
baseFlowSeparation(Q,BFSMethod[1],BFSParams)
baseFlowSeparation(Q,BFSMethod[2],BFSParams)
baseFlowSeparation(Q,BFSMethod[3],BFSParams)
baseFlowSeparation(Q,BFSMethod[4],BFSParams)
}
\author{
Rezgar Arabzadeh
}
\keyword{array}