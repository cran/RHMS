createDiversion.default <-
function(name="Unttitled",downstream=NA,divertTo=NA,capacity)
{
   if(missing(capacity)){stop("diversion capacity rate is not specified!")}
   if(!any(class(downstream)==c('createJunction','createDiversion','createReservoir','createSubbasin','createReach')))
   {
      if(!is.na(downstream)) stop('Bad object specified as downstream!')
   }
   if(any(class(downstream)==c('createJunction','createDiversion','createReservoir','createSubbasin','createReach'))) downstream<-downstream$operation$label
   if(!(any(class(divertTo)==c('createJunction','createDiversion','createReservoir','createSubbasin','createReach'))))
   {
      if(!all(is.na(divertTo))) stop('Bad object specified as the diversion outlet!')
   }else{
      divertTo<-divertTo$operation$label
   }
   resault<-list()
   operation<-createDiversion.base(name,downstream,divertTo,capacity)
   resault$operation<-operation
   resault$call<-match.call()
   class(resault)<-'createDiversion'
   return(resault)
}
