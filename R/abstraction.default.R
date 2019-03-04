abstraction.default<-function(rainfall,abstractionParams=list(canopyAbstraction=NULL,surfaceAbstraction=NULL))
{
   if(missing(rainfall)) {stop('rainfall is missing with no default!')}
   if(is.null(abstractionParams$canopyAbstraction)){abstractionParams$canopyAbstraction<-0}
   if(is.null(abstractionParams$surfaceAbstraction)){abstractionParams$surfaceAbstraction<-0}
   result<-list()
   operation<-abstraction.base(rainfall,abstractionParams)
   result$operation<-operation
   result$call<-match.call()
   class(result)<-"abstraction"
   return(result)
}