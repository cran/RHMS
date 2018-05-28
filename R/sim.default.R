sim.default <-
function(object)
{
   if (missing(object))
   {
      stop("Missing HMS object!")
   }
   if(class(object) != "createBasin")
   {
      stop("the object must be from class of createBasin!")
   }
   result<-list()
   operation<-sim.base(object)
   result$operation<-operation
   result$call<-match.call()
   class(result)<-"sim"
   return(result)
}
