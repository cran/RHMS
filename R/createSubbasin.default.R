createSubbasin.default <-
function(name="Unttitled",
         precipitation,
         inflow=NA,
         Area,
         delayInflow=1,
         label,
         downstream=NA,
         transformMethod="SCS",
         lossMethod="none",
         BFSMethod="none",
         UH=NA,
         transformParams=list(Tlag=NULL,Cp=NULL,Ct=NULL,L=NULL,Lc=NULL),
         lossParams=list(CN=NULL,f0=NULL,f1=NULL,k=NULL),
         BFSParams=list(alpha=NULL,BFI=NULL,k=NULL))
{
   if(missing(label)){stop("label code is not specified!")}
   if(missing(precipitation)){stop("Precipitation is not specified!")}
   if(missing(Area)){stop("Area is not specified!")}
   if(missing(transformParams)){stop("transform parameters is not specified!")}

   if(transformMethod=="user")
   {
      if(all(is.na(UH))){stop("missing user defined unit hydrograph!")} 
      if(ncol(UH)!=2){stop("UH must be a 2-collumn data.frame!")}
   }

   if(transformMethod=="SCS")
   {
      if(is.null(transformParams$Tlag))
      {
         stop("One of the following is not specified: Tlag")
      }
   }
   if(transformMethod=="snyder")
   {
      if(is.null(transformParams$Cp) && is.null(transformParams$Ct) && is.null(transformParams$L) && is.null(transformParams$Lc))
      {
         stop("One of the following is not specified: Cp, Ct, L, Lc")
      }
   }
   if(!(lossMethod=="none"))
   {
      if(lossMethod=="SCS")
      {
         if(is.null(lossParams$CN))
         {
            stop("One of the following is not specified: CN")
         }
      }
      if(lossMethod=="horton")
      {
         if(is.null(lossParams$f0) && is.null(lossParams$f1) && is.null(lossParams$k))
         {
            stop("One of the following is not specified: f0, f1, k")
         }
      }
   }
   if(!(BFSMethod=="none"))
   {
      if(any(BFSMethod==c('nathan','chapman','eckhardt','recession'))==FALSE)
      {
         stop('bad method type specified!')
      }
      if(any(BFSMethod==c('nathan','chapman','eckhardt')) && is.null(BFSParams$alpha))
      {
         stop('missing parameter, alpha, required for the nathan and chapman method!')
      }
      if(BFSMethod=='eckhardt' && is.null(BFSParams$BFI))
      {
         stop('missing parameter, BFI, required for the eckhardt method!')
      }
      if(BFSMethod=='recession' && is.null(BFSParams$k))
      {
         stop('missing parameter, k, required for the recession method!')
      }
   }

   resault<-list()
   operation<-createSubbasin.base(name,precipitation,inflow,
                                  Area,delayInflow,label,downstream,
                                  transformMethod,lossMethod,BFSMethod,
                                  UH,transformParams,lossParams,BFSParams)
   resault$operation<-operation
   resault$call<-match.call()
   class(resault)<-'createSubbasin'
   return(resault)
}