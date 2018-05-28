createJunction.base <-
function(name,downstream,label,inflow,delayInflow)
{
   junction<-list(name=name,downstream=downstream,label=label,inflow=inflow,delayInflow=delayInflow)
   return(junction)
}
