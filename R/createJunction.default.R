createJunction.default <-
function(name="Unttitled",downstream=NA,inflow=NA,delayInflow=1)
{
   if(!any(class(downstream)==c('createJunction','createDiversion','createReservoir','createSubbasin','createReach')))
   {
      if(!is.na(downstream)) stop('Bad object specified as downstream!')
   }
   if(any(class(downstream)==c('createJunction','createDiversion','createReservoir','createSubbasin','createReach'))) downstream<-downstream$operation$label
   result<-list()
   operation<-createJunction.base(name,downstream,inflow,delayInflow)
   result$operation<-operation
   result$call<-match.call()
   class(result)<-"createJunction"
   return(result)
}