createReach.base <-
function(name,routingMethod,inflow,routingParams,delayInflow,label,downstream)
{
   reach<-list(name=name,
               routingMethod=routingMethod,
               routingParams=routingParams,
               inflow=inflow,
               delayInflow=delayInflow,
               label=label,
               downstream=downstream)
   return(reach)
}
