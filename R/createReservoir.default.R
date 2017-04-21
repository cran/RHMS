createReservoir.default <-
function(name="Unttitled",inflow=NA,ratingCurve,dischargeCurve,initialStorage,capacity,delayInflow=1,label,downstream=NA)
{
   if(missing(ratingCurve)){stop("reservoir rating curve is not specified!")}
   if(missing(dischargeCurve)){stop("spillway discharge curve is not specified!")}
   if(missing(initialStorage)){stop("reservoir initial storage is not specified!")}
   if(missing(capacity)){stop("reservoir maximum capacity is not specified!")}
   if(missing(label)){stop("label code is not specified!")}

   result<-list()
   operation<-createReservoir.base(name,inflow,
                                      ratingCurve,
                                      dischargeCurve,
                                      initialStorage,
                                      capacity,delayInflow,
                                      label,downstream)
   result$operation<-operation
   result$call<-match.call()
   class(result)<-"createReservoir"
   return(result)

}
