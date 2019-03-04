createReach.default <-
function(name="Unttitled",routingMethod="muskingum",inflow=NA,routingParams=list(k=3,x=0.2,bedWith=NULL,sideSlope=2,channelSlope=NULL,manningRoughness=0.025,riverLength=NULL),delayInflow=1,downstream=NA)
{
   if(!any(class(downstream)==c('createJunction','createDiversion','createReservoir','createSubbasin','createReach')))
   {
      if(!is.na(downstream)) stop('Bad object specified as downstream!')
   }
   if(any(class(downstream)==c('createJunction','createDiversion','createReservoir','createSubbasin','createReach'))) downstream<-downstream$operation$label
   if(routingMethod=="muskingum")
   {
      if(is.null(routingParams$k) && is.null(routingParams$x)){stop("One of the following is not specified: x, k")}
   }
   if(routingMethod=="muskingumcunge")
   {
      if(is.null(routingParams$bedWith) && is.null(routingParams$channelSlope) && is.null(routingParams$riverLength) && is.null(routingParams$manningRoughness))     {stop("One of the following is not specified: bedWith, sideSlope, channelSlope, manningRoughness, riverLength")}
   }

   result<-list()
   operation<-createReach.base(name,routingMethod,inflow,routingParams,delayInflow,downstream)
   result$operation<-operation
   result$call<-match.call()
   class(result)<-"createReach"
   return(result)
}