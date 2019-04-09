\name{set.as}
\alias{set.as}
\title{
RHMS objects connector 
}
\description{
this function connects a \code{base} object as a either of: \code{'downstream'} or \code{'divertTo'} to a \code{target} object, which are both instantiated by RHMS constructors.
}
\usage{
set.as(base,target,type='downstream')
}
\arguments{
\item{base}{An object; from either of classes of \code{\link[RHMS]{createReservoir}}, \code{\link[RHMS]{createJunction}}, \code{\link[RHMS]{createDiversion}}, \code{\link[RHMS]{createSubbasin}}, or \code{\link[RHMS]{createReach}}}
\item{target}{An object; from either of classes of \code{\link[RHMS]{createReservoir}}, \code{\link[RHMS]{createJunction}}, \code{\link[RHMS]{createDiversion}}, \code{\link[RHMS]{createSubbasin}}, or \code{\link[RHMS]{createReach}}}
\item{type}{the type of \code{base} object to be set as to the \code{target} object: \code{'downstream'},  or \code{'divertTo'}}
}
\value{
an object from class of \code{target} object.
}
\author{
Rezgar Arabzadeh
}
\seealso{\code{\link[RHMS]{addObjectToBasin}}}
\keyword{list}