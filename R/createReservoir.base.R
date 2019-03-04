createReservoir.base <-
function(name,inflow,geometry,initialStorage,delayInflow,downstream)
{
   reservoir<-list(name=name,
                   inflow=as.matrix(inflow),
                   geometry=geometry,
                   initialStorage=initialStorage,
                   delayInflow=delayInflow,
                   label=runif(1),
                   downstream=downstream)
   return(reservoir)
}
