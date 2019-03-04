addObjectToBasin <-function(object,basin)
{
    if(missing(object)){stop("object is not specified!")}
    if(missing(basin)){stop("basin is not specified!")}
    if(class(object)=="createReservoir")
    {
      object<-object$operation
      if(all(is.na(object$inflow)))
      {
        outflow<- data.frame(outflow=rep(0,length(basin$operation$simulation$simulationSteps)))
        lateralInflow<- data.frame(lateralInflow=rep(0,length(basin$operation$simulation$simulationSteps)))
        rownames(outflow)<-basin$operation$simulation$simulationSteps
        rownames(lateralInflow)<-basin$operation$simulation$simulationSteps
        object$inflow <-lateralInflow
        object$outflow<-outflow
        basin$operation$reservoirs[[length(basin$operation$reservoirs)+1]]<-object
        return(basin)
      }else{
        lateralInflow<- data.frame(lateralInflow=rep(0,length(basin$operation$simulation$simulationSteps)))
        ts<-c(rep(0,object$delayInflow),object$inflow)
        if(length(ts)<=nrow(lateralInflow))
        {
          lateralInflow[1:length(ts),1]<-ts
        }else{
          lateralInflow[,1]<-ts[1:nrow(lateralInflow)]
        }
        outflow<- data.frame(outflow=rep(0,length(basin$operation$simulation$simulationSteps)))
        rownames(lateralInflow)<-basin$operation$simulation$simulationSteps
        rownames(outflow)<-basin$operation$simulation$simulationSteps
        object$inflow<-lateralInflow
        object$outflow<-outflow
        basin$operation$reservoirs[[length(basin$operation$reservoirs)+1]]<-object
        return(basin)
      }
    }  
    
    if(class(object)=="createReach")
    {
      object<-object$operation
      if(all(is.na(object$inflow)))
      {
        outflow<- data.frame(outflow=rep(0,length(basin$operation$simulation$simulationSteps)))
        lateralInflow<- data.frame(lateralInflow=rep(0,length(basin$operation$simulation$simulationSteps)))
        rownames(outflow)<-basin$operation$simulation$simulationSteps
        rownames(lateralInflow)<-basin$operation$simulation$simulationSteps
        object$inflow <-lateralInflow
        object$outflow<-outflow
        basin$operation$reachs[[length(basin$operation$reachs)+1]]<-object
        return(basin)
      }else{
        lateralInflow<- data.frame(lateralInflow=rep(0,length(basin$operation$simulation$simulationSteps)))
        ts<-c(rep(0,object$delayInflow),object$inflow)
        if(length(ts)<=nrow(lateralInflow))
        {
          lateralInflow[1:length(ts),1]<-ts
        }else{
          lateralInflow[,1]<-ts[1:nrow(lateralInflow)]
        }
        outflow<- data.frame(outflow=rep(0,length(basin$operation$simulation$simulationSteps)))
        rownames(lateralInflow)<-basin$operation$simulation$simulationSteps
        rownames(outflow)<-basin$operation$simulation$simulationSteps
        object$inflow<-lateralInflow
        object$outflow<-outflow
        basin$operation$reachs[[length(basin$operation$reachs)+1]]<-object
        return(basin)
      }
    }
    
    if(class(object)=="createSubbasin")
    {
      object<-object$operation
      if(all(is.na(object$inflow)))
      {
        outflow<- data.frame(outflow=rep(0,length(basin$operation$simulation$simulationSteps)))
        lateralInflow<- data.frame(lateralInflow=rep(0,length(basin$operation$simulation$simulationSteps)))
        rownames(outflow)<-basin$operation$simulation$simulationSteps
        rownames(lateralInflow)<-basin$operation$simulation$simulationSteps
        object$inflow <-lateralInflow
        object$outflow<-outflow
        basin$operation$subbasins[[length(basin$operation$subbasins)+1]]<-object
        return(basin)
      }else{
        lateralInflow<- data.frame(lateralInflow=rep(0,length(basin$operation$simulation$simulationSteps)))
        ts<-c(rep(0,object$delayInflow),object$inflow)
        if(length(ts)<=nrow(lateralInflow))
        {
          lateralInflow[1:length(ts),1]<-ts
        }else{
          lateralInflow[,1]<-ts[1:nrow(lateralInflow)]
        }
        outflow<- data.frame(outflow=rep(0,length(basin$operation$simulation$simulationSteps)))
        rownames(lateralInflow)<-basin$operation$simulation$simulationSteps
        rownames(outflow)<-basin$operation$simulation$simulationSteps
        object$inflow<-lateralInflow
        object$outflow<-outflow
        basin$operation$subbasins[[length(basin$operation$subbasins)+1]]<-object
        return(basin)
      }
    }
    
    if(class(object)=="createJunction")
    {
      object<-object$operation
      if(all(is.na(object$inflow)))
      {
        outflow<- data.frame(outflow=rep(0,length(basin$operation$simulation$simulationSteps)))
        lateralInflow<- data.frame(lateralInflow=rep(0,length(basin$operation$simulation$simulationSteps)))
        rownames(outflow)<-basin$operation$simulation$simulationSteps
        rownames(lateralInflow)<-basin$operation$simulation$simulationSteps
        object$inflow <-lateralInflow
        object$outflow<-outflow
        basin$operation$junctions[[length(basin$operation$junctions)+1]]<-object
        return(basin)
      }else{
        lateralInflow<- data.frame(lateralInflow=rep(0,length(basin$operation$simulation$simulationSteps)))
        ts<-c(rep(0,object$delayInflow),object$inflow)
        if(length(ts)<=nrow(lateralInflow))
        {
          lateralInflow[1:length(ts),1]<-ts
        }else{
          lateralInflow[,1]<-ts[1:nrow(lateralInflow)]
        }
        outflow<- data.frame(outflow=rep(0,length(basin$operation$simulation$simulationSteps)))
        rownames(lateralInflow)<-basin$operation$simulation$simulationSteps
        rownames(outflow)<-basin$operation$simulation$simulationSteps
        object$inflow<-lateralInflow
        object$outflow<-outflow
        basin$operation$junctions[[length(basin$operation$junctions)+1]]<-object
        return(basin)
      }
    }
    
    if(class(object)=="createDiversion")
    {
        object<-object$operation
        outflow<- data.frame(outflow=rep(0,length(basin$operation$simulation$simulationSteps)))
        lateralInflow<- data.frame(lateralInflow=rep(0,length(basin$operation$simulation$simulationSteps)))
        rownames(outflow)<-basin$operation$simulation$simulationSteps
        rownames(lateralInflow)<-basin$operation$simulation$simulationSteps
        object$inflow <-lateralInflow
        object$outflow<-outflow
        basin$operation$diversions[[length(basin$operation$diversions)+1]]<-object
        return(basin)
    }else{
       stop("wrong object class is specified!")
    }
}