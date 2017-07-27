createBasin.base <-
function(name,simPeriod,interval)
{
   reservoirs        <-list()
   reachs            <-list()
   junctions         <-list()
   subbasins         <-list()
   diversions        <-list()
   dateAndTimeCreated<-Sys.time()
   basin<-list(name              =name              ,
               reservoirs        =reservoirs        ,
               reachs            =reachs            ,
               junctions         =junctions         ,
               dateAndTimeCreated=dateAndTimeCreated,
               simPeriod         =simPeriod         ,
               interval          =interval)
   return(basin)
}
