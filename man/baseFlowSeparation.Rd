\name{baseFlowSeparation}
\alias{baseFlowSeparation}
\title{
Parametric methods for separating baseflow
}
\description{
This function calculates baseflow for agiven discharge series, \code{discharge}, using a number of method proposed in \code{BFSMethod}.
}
\usage{
baseFlowSeparation(discharge,BFSMethod,BFSParams,plot)
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
a list: an \code{object} from class of \code{baseFlowSeparation} consisting matrix of results available at object$operation.
}
\references{
Chapman, Tom. "A comparison of algorithms for stream flow recession and baseflow separation." Hydrological Processes 13.5 (1999): 701-714.}
\seealso{ \code{\link[RHMS]{baseFlowSeparation}}}
\examples{
discharge<-(dnorm(seq(-3,4,length.out=200),-.3,1)+dnorm(seq(-1,7,length.out=200),4.5,1)*2)*1200
BFSMethod<-c('nathan','chapman','eckhardt','recession')
BFSParams<-list(alpha=0.6,BFI=0.3,k=1.1,timeInterval=15*60)
simulation<-list(start='2000-01-01',end='2000-01-02',by=400)
baseFlowSeparation(discharge,BFSMethod[1],BFSParams,plot=TRUE)
baseFlowSeparation(discharge,BFSMethod[2],BFSParams,plot=TRUE)
baseFlowSeparation(discharge,BFSMethod[3],BFSParams,plot=TRUE)
baseFlowSeparation(discharge,BFSMethod[4],BFSParams,plot=TRUE)
}
\author{
Rezgar Arabzadeh
}
\keyword{array}