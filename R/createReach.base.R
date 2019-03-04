createReach.base <-
function(name,routingMethod,inflow,routingParams,delayInflow,downstream)
{
   reach<-list(name=name,
               routingMethod=routingMethod,
               routingParams=routingParams,
               inflow=inflow,
               delayInflow=delayInflow,
               label=runif(1),
               downstream=downstream)
   return(reach)
}
