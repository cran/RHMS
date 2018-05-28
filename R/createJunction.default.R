createJunction.default <-
function(name="Unttitled",downstream=NA,label,inflow=NA,delayInflow=1)
{
   if(missing(label)){stop("label code is not specified!")}
   result<-list()
   operation<-createJunction.base(name,downstream,label,inflow,delayInflow)
   result$operation<-operation
   result$call<-match.call()
   class(result)<-"createJunction"
   return(result)
}
