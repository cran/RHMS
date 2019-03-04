baseFlowSeparation.default<-function(discharge,BFSMethod='none',BFSParams=list(alpha=NULL,BFI=NULL,k=NULL,timeInterval=NULL),plot=FALSE)
{
   if(missing(discharge))
   {
      stop('discharge time series is missing!')
   }
   if(class(discharge)=='transform')
   {
      discharge<-discharge$operation[,ncol(discharge$operation)]
   }
   if(missing(BFSMethod))
   {
      stop('base flow separation method is missing!')
   }
   if(any(BFSMethod==c('nathan','chapman','eckhardt','recession','none'))==FALSE)
   {
      stop('bad method type specified!')
   }
   if(any(BFSMethod==c('nathan','chapman','eckhardt')) && is.null(BFSParams$alpha))
   {
      stop('missing parameter, alpha, required for the nathan and chapman method!')
   }
   if(BFSMethod=='eckhardt' && is.null(BFSParams$BFI))
   {
      stop('parameter BFI is missing, required for the eckhardt method!')
   }
   if(BFSMethod=='recession' && (is.null(BFSParams$k) | is.null(BFSParams$timeInterval)))
   {
      stop('missing parameter(s), k and or timeInterval, required for the recession method!')
   }

   result<-list()
   operation<-baseFlowSeparation.base(discharge,BFSMethod,BFSParams,plot)
   result$operation<-operation
   result$call<-match.call()
   class(result)<-"baseFlowSeparation"
   return(result)
}