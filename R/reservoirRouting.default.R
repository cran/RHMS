reservoirRouting.default <-
function(inflow,geometry=list(storageElevationCurve=NULL,dischargeElevationCurve=NULL,capacity=NULL),initialStorage=NA,simulation=list(start=NULL,end=NULL,by=NULL))
{
   if(missing(inflow)){ stop ('inflow time series is required!')}
   if(any(is.na(inflow))) inflow[which(is.na(inflow))]<-0
   if(class(inflow)=='transform')
   {
      inflow<-inflow$operation[,ncol(inflow$operation)]
   }
   if(class(inflow)=='reservoirRouting')
   {
      inflow<-inflow$operation[,3]
   }
   if(class(inflow)=='reachRouting')
   {
      inflow<-inflow$operation[,ncol(inflow$operation)]
   }
   if(is.null(simulation$start)){stop("simulation start date is not specified!")}
   if(is.null(simulation$end)){stop("simulation end date is not specified!")}
   if(is.null(simulation$by)){stop("simulation interval is not specified!")}
   start<-as.numeric(strsplit(simulation$start,'-')[[1]])
   end<-as.numeric(strsplit(simulation$end,'-')[[1]])
   start<-ISOdate(start[1],start[2],start[3])
   end<-ISOdate(end[1],end[2],end[3])
   simulation$simulationSteps<-seq(start,end,simulation$by)
   if(length(inflow)>length(simulation$simulationSteps))
   {
      inflow<-inflow[1:length(simulation$simulationSteps)]
   }
   if(length(inflow)<length(simulation$simulationSteps))
   {
      temp<-rep(0,length(simulation$simulationSteps))
      temp[1:length(inflow)]<-inflow
      inflow<-temp
   }
   storageElevationCurve<-geometry$storageElevationCurve
   dischargeElevationCurve<-geometry$dischargeElevationCurve
   capacity<-geometry$capacity
   if(is.null(storageElevationCurve)){stop("reservoir rating curve is not specified!")}
   if(is.null(dischargeElevationCurve)){stop("spillway discharge curve is not specified!")}
   if(is.na(initialStorage)){initialStorage<-capacity}
   if(is.null(capacity)){stop("reservoir maximum capacity is not specified!")}
   if(any(c(ncol(storageElevationCurve),ncol(storageElevationCurve)) != 2)) {stop("wrong dimensions of reservoir ratig curves!")}
   result<-list()
   operation<-reservoirRouting.base(inflow,geometry,initialStorage,simulation)
   result$operation<-operation
   result$call<-match.call()
   class(result)<-"reservoirRouting"
   return(result)
}
