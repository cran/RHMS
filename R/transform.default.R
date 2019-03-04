transform.default <-
function(rainfall,transformMethod='SCS',
         transformParams=list(Tlag=NULL,Cp=NULL,Ct=NULL,L=NULL,Lc=NULL),
         Area,UH=NA,
         simulation=list(start=NULL,end=NULL,by=NULL))
{
   if(missing(Area)) {stop("area is missing!")}
   if(missing(rainfall)){stop("rainfall is missing!")}
   if(class(rainfall)=='loss') {rainfall<-rainfall$operation[,3]}
   if(class(rainfall)=='abstraction') {rainfall<-rainfall$operation[,4]}
   if(is.null(simulation$start)){stop("simulation start date is not specified!")}
   if(is.null(simulation$end)){stop("simulation end date is not specified!")}
   if(is.null(simulation$by)){stop("simulation interval is not specified!")}
   start<-as.numeric(strsplit(simulation$start,'-')[[1]])
   end<-as.numeric(strsplit(simulation$end,'-')[[1]])
   start<-ISOdate(start[1],start[2],start[3])
   end<-ISOdate(end[1],end[2],end[3])
   simulation$simulationSteps<-seq(start,end,simulation$by)

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
      if(any(c(is.null(transformParams$Cp),is.null(transformParams$Ct),is.null(transformParams$L),is.null(transformParams$Lc))))
      {
         stop("One of the following is not specified: Cp, Ct, L, Lc")
      }
   }
   resault<-list()
   operation<-transform.base(rainfall,transformMethod,transformParams,Area,UH,simulation)
   resault$operation<-operation
   resault$call<-match.call()
   class(resault)<-'transform'
   return(resault)
}