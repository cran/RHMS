createBasin.default <-
function(name="Untittled",simulation=list(start=NULL,end=NULL,by=NULL))
{
   if(is.null(simulation$start)){stop("simulation start date is not specified!")}
   if(is.null(simulation$end)){stop("simulation end date is not specified!")}
   if(is.null(simulation$by)){stop("simulation interval is not specified!")}
   start<-as.numeric(strsplit(simulation$start,'-')[[1]])
   end<-as.numeric(strsplit(simulation$end,'-')[[1]])
   start<-ISOdate(start[1],start[2],start[3])
   end<-ISOdate(end[1],end[2],end[3])
   simulation$simulationSteps<-seq(start,end,simulation$by)
   result<-list()
   operation<-createBasin.base(name,simulation)
   result$operation<-operation
   result$call<-match.call()
   class(result)<-"createBasin"
   return(result)
}
