createSubbasin.base <-
function(name,precipitation,inflow,Area,delayInflow,label,downstream,transformMethod,lossMethod,transformParams,lossParams)
{
   subBasin<-list(name=name,precipitation=precipitation,Area=Area,delayInflow=delayInflow,label=label,downstream=downstream,inflow=inflow,transformMethod=transformMethod,lossMethod=lossMethod,transformParams=transformParams,lossParams=lossParams)
   return(subBasin) 
}
