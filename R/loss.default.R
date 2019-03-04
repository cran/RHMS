loss.default <-function(precipitation,lossMethod='SCS',lossParams=list(f0=NULL,f1=NULL,k=NULL,timeInterval=NULL,CN=NULL,imperviousness=NULL))
{
   if(missing(lossParams)){stop("loss parameters is missing!")}
   if(missing(precipitation)){stop("precipitation object is missing!")}
   if(class(precipitation)=='abstraction')
   {
      precipitation<-precipitation$operation$excessRainfall
   }
   if(lossMethod=="SCS")
   {
      if(is.null(lossParams$CN))
      {
         stop("One of the following is missing: CN")
      }
      if(is.null(lossParams$imperviousness))
      {
         lossParams$imperviousness<-0
      }
   }
   if(lossMethod=="horton")
   {
      if(any(c(is.null(lossParams$f0),is.null(lossParams$f1),is.null(lossParams$k),is.null(lossParams$timeInterval))))
      {
         stop("One of the followings are missing: f0, f1, k, timeInterval")
      }
   }
   resault<-list()
   operation<-loss.base(precipitation,lossMethod,lossParams)
   resault$operation<-operation
   resault$call<-match.call()
   class(resault)<-'loss'
   return(resault)
}