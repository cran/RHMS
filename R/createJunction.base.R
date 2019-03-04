createJunction.base <-
function(name,downstream,inflow,delayInflow)
{
   junction<-list(name=name,downstream=downstream,label=runif(1),inflow=inflow,delayInflow=delayInflow)
   return(junction)
}
