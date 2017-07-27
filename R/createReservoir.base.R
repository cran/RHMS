createReservoir.base <-
function(name,inflow,ratingCurve,dischargeCurve,initialStorage,capacity,delayInflow,label,downstream)
{
   reservoir<-list(name=name,
                   inflow=as.matrix(inflow),
                   ratingCurve=ratingCurve,
                   dischargeCurve=dischargeCurve,
                   initialStorage=initialStorage,
                   capacity=capacity,
                   delayInflow=delayInflow,
                   label=label,
                   downstream=downstream)
   return(reservoir)
}
