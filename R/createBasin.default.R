createBasin.default <-
function(name="Untittled",simPeriod,interval)
{
   if(missing(simPeriod)){stop("simulation period is not specified!")}
   if(missing(interval)){stop("simulation interval is not specified!")}
   result<-list()
   operation<-createBasin.base(name,simPeriod,interval)
   result$operation<-operation
   result$call<-match.call()
   class(result)<-"createBasin"
   return(result)
}
