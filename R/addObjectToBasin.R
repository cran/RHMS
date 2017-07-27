addObjectToBasin <-function(object,basin)
{
    if(missing(object)){stop("object is not specified!")}
    if(missing(basin)){stop("basin is not specified!")}
    if(class(object)=="createReservoir")
    {
      object<-object$operation
      if(is.na(object$inflow))
      {
        outflow<-directInflow<-rep(0,basin$operation$simPeriod)
        object$inflow <-as.data.frame(directInflow)
        object$outflow<-outflow
        basin$operation$reservoirs[[length(basin$operation$reservoirs)+1]]<-object
        return(basin)
      }else{
        directInflow<-rep(0,basin$operation$simPeriod)
        ts<-c(rep(0,object$delayInflow),object$inflow)
        if(length(ts)<=length(directInflow))
        {
          directInflow[1:length(ts)]<-ts
        }else{
          directInflow<-ts[1:length(directInflow)]
        }
        object$inflow<-as.data.frame(directInflow)
        object$outflow<-rep(0,length(directInflow))
        basin$operation$reservoirs[[length(basin$operation$reservoirs)+1]]<-object
        return(basin)
      }
    }  
    
    if(class(object)=="createReach")
    {
      object<-object$operation
      if(is.na(object$inflow))
      {
        outflow<-directInflow<-rep(0,basin$operation$simPeriod)
        object$inflow <-as.data.frame(directInflow)
        object$outflow<-outflow
        basin$operation$reachs[[length(basin$operation$reachs)+1]]<-object
        return(basin)
      }else{
        directInflow<-rep(0,basin$operation$simPeriod)
        ts<-c(rep(0,object$delayInflow),object$inflow)
        if(length(ts)<=length(directInflow))
        {
          directInflow[1:length(ts)]<-ts
        }else{
          directInflow<-ts[1:length(directInflow)]
        }
        object$inflow<-as.data.frame(directInflow)
        object$outflow<-rep(0,length(directInflow))
        basin$operation$reachs[[length(basin$operation$reachs)+1]]<-object
        return(basin)
      }
    }
    
    if(class(object)=="createSubbasin")
    {
      object<-object$operation
      if(is.na(object$inflow))
      {
        outflow<-directInflow<-rep(0,basin$operation$simPeriod)
        object$inflow <-as.data.frame(directInflow)
        object$outflow<-outflow
        basin$operation$subbasins[[length(basin$operation$subbasins)+1]]<-object
        return(basin)
      }else{
        directInflow<-rep(0,basin$operation$simPeriod)
        ts<-c(rep(0,object$delayInflow),object$inflow)
        if(length(ts)<=length(directInflow))
        {
          directInflow[1:length(ts)]<-ts
        }else{
          directInflow<-ts[1:length(directInflow)]
        }
        object$inflow<-as.data.frame(directInflow)
        object$outflow<-rep(0,length(directInflow))
        basin$operation$subbasins[[length(basin$operation$subbasins)+1]]<-object
        return(basin)
      }
    }
    
    if(class(object)=="createJunction")
    {
      object<-object$operation
      if(is.na(object$inflow))
      {
        outflow<-directInflow<-rep(0,basin$operation$simPeriod)
        object$inflow <-as.data.frame(directInflow)
        object$outflow<-outflow
        basin$operation$junctions[[length(basin$operation$junctions)+1]]<-object
        return(basin)
      }else{
        directInflow<-rep(0,basin$operation$simPeriod)
        ts<-c(rep(0,object$delayInflow),object$inflow)
        if(length(ts)<=length(directInflow))
        {
          directInflow[1:length(ts)]<-ts
        }else{
          directInflow<-ts[1:length(directInflow)]
        }
        object$inflow<-as.data.frame(directInflow)
        object$outflow<-rep(0,length(directInflow))
        basin$operation$junctions[[length(basin$operation$junctions)+1]]<-object
        return(basin)
      }
    }
    
    if(class(object)=="createDiversion")
    {
        object<-object$operation
        outflow<-inflow<-rep(0,basin$operation$simPeriod)
        object$inflow <-as.data.frame(inflow)
        object$outflow<-outflow
        basin$operation$diversions[[length(basin$operation$diversions)+1]]<-object
        return(basin)
      }else{
       stop("wrong object class is specified!")
      }
}