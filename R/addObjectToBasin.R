addObjectToBasin <-
function(object,basin,type)
  {
    if(missing(object)){stop("object is not specified!")}
    if(missing(basin)){stop("basin is not specified!")}
    if(missing(type)){stop("type of object is not specified!")}
    object<-object$operation
    if(type=="reservoir")
    {
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
    
    if(type=="reach")
    {
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
    
    if(type=="subbasin")
    {
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
    
    if(type=="junction")
    {
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
    
    if(type=="diversion")
    {
        outflow<-inflow<-rep(0,basin$operation$simPeriod)
        object$inflow <-as.data.frame(inflow)
        object$outflow<-outflow
        basin$operation$diversions[[length(basin$operation$diversions)+1]]<-object
        return(basin)
      }else{
       stop("wrong object type is specified!")
      }
  }
