loss.default <-function(precipitation,lossParams=list(f0=NULL,f1=NULL,k=NULL,CN=NULL),simulation=c(interval=3600,period=NA),lossMethod)
{
   if(missing(precipitation)){stop("precipitation code is missing!")}
   if(missing(lossParams)){stop("lossParams code is missing!")}
   if(missing(precipitation)){stop("simulation code is missing!")}
   if(missing(precipitation)){stop("lossMethod code is missing!")}
   if(lossMethod=="SCS")
   {
      if(is.null(lossParams$CN))
      {
         stop("One of the following is missing: CN")
      }
   }
   if(lossMethod=="horton")
   {
      if(is.null(lossParams$f0) && is.null(lossParams$f1) && is.null(lossParams$k))
      {
         stop("One of the following is missing: f0, f1, k")
      }
   }
   resault<-list()
   operation<-loss.base(precipitation,lossParams,simulation,lossMethod)
   resault$operation<-operation
   resault$call<-match.call()
   class(resault)<-'loss'
   return(resault)
}