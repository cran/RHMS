createDiversion.base <-
function(name,downstream,divertTo,capacity)
{
   diversion<-list(name=name,label=runif(1),downstream=downstream,divertTo=divertTo,capacity=capacity)
   return(diversion) 
}
