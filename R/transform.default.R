transform.default <-
function(rainfall,transformParams=list(Tlag=NULL,Cp=NULL,Ct=NULL,L=NULL,Lc=NULL),
         Area,simulation=c(interval=3600*1,period=NA),UH,transformMethod)
{
   if(missing(rainfall)){stop("rainfall is missing!")}
   if(missing(Area))    {stop("area is missing!")}
   if(transformMethod=="user")
   {
      if(all(is.na(UH))){stop("missing user defined unit hydrograph!")} 
      if(ncol(UH) != 2){stop("UH must be a 2-collumn data-frame!")}
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
   resault<-list()
   operation<-transform.base(rainfall,transformParams,Area,simulation,UH,transformMethod)
   resault$operation<-operation
   resault$call<-match.call()
   class(resault)<-'transform'
   return(resault)
}