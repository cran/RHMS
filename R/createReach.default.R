createReach.default <-
function(name="Unttitled",routingMethod="muskingum",inflow=NA,routingParams=list(k=3,x=0.2,bedWith=NULL,sideSlope=2,channelSlope=NULL,manningRoughness=0.025,riverLength=NULL),delayInflow=1,label,downstream=NA)
{
   if(missing(routingParams)){stop("routingParams is not specified!")}
   if(missing(label)){stop("label code is not specified!")}
   if(routingMethod=="muskingum")
   {
      if(is.null(routingParams$k) && is.null(routingParams$x)){stop("One of the following is not specified: x, k")}
   }
   if(routingMethod=="muskingumcunge")
   {
      if(is.null(routingParams$bedWith) && is.null(routingParams$channelSlope) && is.null(routingParams$riverLength) && is.null(routingParams$manningRoughness))     {stop("One of the following is not specified: bedWith, sideSlope, channelSlope, manningRoughness, riverLength")}
   }

   result<-list()
   operation<-createReach.base(name,routingMethod,inflow,routingParams,delayInflow,label,downstream)
   result$operation<-operation
   result$call<-match.call()
   class(result)<-"createReach"
   return(result)

}
