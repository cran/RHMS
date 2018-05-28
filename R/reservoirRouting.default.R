reservoirRouting.default <-
function(inflow,ratingCurve,dischargeCurve,initialStorage,capacity,simulation=c(interval=3600*1,period=NA))
{
   if(missing(ratingCurve)){stop("reservoir rating curve is not specified!")}
   if(missing(dischargeCurve)){stop("spillway discharge curve is not specified!")}
   if(missing(initialStorage)){stop("reservoir initial storage is not specified!")}
   if(missing(capacity)){stop("reservoir maximum capacity is not specified!")}
   if(any(c(ncol(ratingCurve),ncol(ratingCurve)) != 2)) {stop("wrong dimensions of reservoir ratig curves!")}
   if(class(inflow)=='transform')
   {
      inflow<-inflow$operation[,ncol(inflow$operation)]
   }
   if(class(inflow)=='reservoirRouting')
   {
      inflow<-inflow$operation[,4]
   }
   if(class(inflow)=='reachRouting')
   {
      inflow<-inflow$operation[,ncol(inflow$operation)]
   }
   result<-list()
   operation<-reservoirRouting.base(inflow,ratingCurve,dischargeCurve,initialStorage,capacity,simulation)
   result$operation<-operation
   result$call<-match.call()
   class(result)<-"reservoirRouting"
   return(result)
}
