reachRouting.default <-function(inflow,routingMethod="muskingum",routingParams=list(k=3,x=0.2,bedWith=NULL,sideSlope=2,channelSlope=NULL,manningRoughness=0.025,riverLength=NULL),simulation=list(start=NULL,end=NULL,by=NULL))
{
   if(missing(routingParams)){stop("routingParams is not specified!")}
   if(missing(inflow)){stop("inflow time series is not specified!")}
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
   if(routingMethod=="muskingum")
   {
      if(is.null(routingParams$k) && is.null(routingParams$x)){stop("One of the following is not specified: x, k")}
   }
   if(routingMethod=="muskingumcunge")
   {
      if(any(c(is.null(routingParams$bedWith),is.null(routingParams$channelSlope),is.null(routingParams$riverLength),is.null(routingParams$manningRoughness)))) {stop("One of the following is not specified: bedWith, sideSlope, channelSlope, manningRoughness, riverLength")}
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
   result<-list()
   operation<-reachRouting.base(inflow,routingMethod,routingParams,simulation)
   result$operation<-operation
   result$call<-match.call()
   class(result)<-"reachRouting"
   return(result)
}