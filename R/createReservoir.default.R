createReservoir.default <-
function(name="Unttitled",inflow=NA,geometry=list(storageElevationCurve=NULL,dischargeElevationCurve=NULL,capacity=NULL),initialStorage=NA,delayInflow=1,downstream=NA)
{
   if(!any(class(downstream)==c('createJunction','createDiversion','createReservoir','createSubbasin','createReach')))
   {
      if(!is.na(downstream)) stop('Bad object specified as downstream!')
   }
   if(any(class(downstream)==c('createJunction','createDiversion','createReservoir','createSubbasin','createReach'))) downstream<-downstream$operation$label
   if(is.null(geometry$storageElevationCurve)){stop("reservoir rating curve is not specified!")}
   if(is.null(geometry$dischargeElevationCurve)){stop("spillway discharge curve is not specified!")}
   if(is.na(initialStorage)){initialStorage<-geometry$capacity}
   if(is.null(geometry$capacity)){stop("reservoir maximum capacity is not specified!")}
   if ((ncol(geometry$storageElevationCurve)+ncol(geometry$dischargeElevationCurve))>4) {stop("gemetric curves must be provided as a 2-collumn matrix!")}
   result<-list()
   operation<-createReservoir.base(name,inflow,
                                      geometry,
                                      initialStorage,
                                      delayInflow,
                                      downstream)
   result$operation<-operation
   result$call<-match.call()
   class(result)<-"createReservoir"
   return(result)

}