createBasin.base <-
function(name,simulation)
{
   reservoirs        <-list()
   reachs            <-list()
   junctions         <-list()
   subbasins         <-list()
   diversions        <-list()
   timeOfCreation<-Sys.time()
   basin<-list(name              =name          ,
               reservoirs        =reservoirs    ,
               reachs            =reachs        ,
               junctions         =junctions     ,
               subbasins         =subbasins     ,
               diversions        =diversions    ,
               simulation        =simulation    ,
               timeOfCreation    =timeOfCreation)
   return(basin)
}
