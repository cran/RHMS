sim.default <-
function(object)
{
   if (missing(object))
   {
      stop("Missing HMS object!")
   }
   result<-list()
   operation<-sim.base(object)
   result$operation<-operation
   result$call<-match.call()
   class(result)<-"sim"
   return(result)
}
