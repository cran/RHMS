reachRouting.default <-
function(inflow,routingMethod="muskingum",routingParams=list(k=3,x=0.2,bedWith=NULL,sideSlope=2,channelSlope=NULL,manningRoughness=0.025,riverLength=NULL),simulation=c(interval=3600*1,period=NA))
{
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
   if(missing(routingParams)){stop("routingParams is not specified!")}
   if(routingMethod=="muskingum")
   {
      if(is.null(routingParams$k) && is.null(routingParams$x)){stop("One of the following is not specified: x, k")}
   }
   if(routingMethod=="muskingumcunge")
   {
      if(is.null(routingParams$bedWith) && is.null(routingParams$channelSlope) && is.null(routingParams$riverLength) && is.null(routingParams$manningRoughness))     {stop("One of the following is not specified: bedWith, sideSlope, channelSlope, manningRoughness, riverLength")}
   }
   result<-list()
   operation<-reachRouting.base(inflow,routingMethod,routingParams,simulation)
   result$operation<-operation
   result$call<-match.call()
   class(result)<-"reachRouting"
   return(result)
}
