createDiversion.base <-
function(name,label,downstream,divertTo,capacity)
{
   diversion<-list(name=name,label=label,downstream=downstream,divertTo=divertTo,capacity=capacity)
   return(diversion) 
}
