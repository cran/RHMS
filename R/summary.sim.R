summary.sim <-
function(object,...)
{
  if(missing(object))
  {
     stop("missing object!")
  }
  object<-object$operation
  nRes<-length(object$reservoirs)
  nRec<-length(object$reachs)
  nJun<-length(object$junctions)
  nSub<-length(object$subbasins)
  nDiv<-length(object$diversions)
  
  if(nRes>0)
  {
    for(i in 1:nRes)
    {
        cat(paste("--------------------","Reservoir:",object$reservoirs[[i]]$name,"--------------------","\n"))
        inflow<-object$reservoirs[[i]]$inflow
        outflow<-object$reservoirs[[i]]$outflow
        resault<-matrix(NA,2,ncol(inflow)+1)
        colnames(resault)<-c(paste("inflow","(",colnames(inflow),")",sep=""),"outflow")
        rownames(resault)<-c("Volumes (MCM)","Peak (cms)")
        for(i in 1:ncol(inflow))
        {
          resault[1,i]<-sum(inflow[,i]*object$simulation$by,na.rm=TRUE)/10^6
          resault[2,i]<-max(inflow[,i],na.rm=TRUE)
        }
        resault[1,i+1]<-sum(outflow*object$simulation$by,na.rm=TRUE)/10^6
        resault[2,i+1]<-max(outflow,na.rm=TRUE)
        print(resault)
    }
  }
  
  if(nRec>0)
  {
    for(i in 1:nRec)
    {
        cat(paste("--------------------","Reach:",object$reachs[[i]]$name,"--------------------","\n"))
        inflow<-object$reachs[[i]]$inflow
        outflow<-object$reachs[[i]]$outflow
        resault<-matrix(NA,2,ncol(inflow)+1)
        colnames(resault)<-c(paste("inflow","(",colnames(inflow),")",sep=""),"outflow")
        rownames(resault)<-c("Volumes (MCM)","Peak (cms)")
        for(i in 1:ncol(inflow))
        {
          resault[1,i]<-sum(inflow[,i]*object$simulation$by,na.rm=TRUE)/10^6
          resault[2,i]<-max(inflow[,i],na.rm=TRUE)
        }
        resault[1,i+1]<-sum(outflow*object$simulation$by,na.rm=TRUE)/10^6
        resault[2,i+1]<-max(outflow,na.rm=TRUE)
        print(resault)
    }
  }
  
  if(nJun>0)
  {
    for(i in 1:nJun)
    {
        cat(paste("--------------------","Junction:",object$junctions[[i]]$name,"--------------------","\n"))
        inflow<-object$junctions[[i]]$inflow
        outflow<-object$junctions[[i]]$outflow
        resault<-matrix(NA,2,ncol(inflow)+1)
        colnames(resault)<-c(paste("inflow","(",colnames(inflow),")",sep=""),"outflow")
        rownames(resault)<-c("Volumes (MCM)","Peak (cms)")
        for(i in 1:ncol(inflow))
        {
          resault[1,i]<-sum(inflow[,i]*object$simulation$by,na.rm=TRUE)/10^6
          resault[2,i]<-max(inflow[,i],na.rm=TRUE)
        }
        resault[1,i+1]<-sum(outflow*object$simulation$by,na.rm=TRUE)/10^6
        resault[2,i+1]<-max(outflow,na.rm=TRUE)
        print(resault)
    }
  }
    
  if(nSub>0)
  {
      for(i in 1:nSub)
      {
          cat(paste("--------------------","Sub-basin:",object$subbasins[[i]]$name,"--------------------","\n"))
          inflow<-object$subbasins[[i]]$inflow
          outflow<-object$subbasins[[i]]$outflow
          resault<-matrix(NA,2,ncol(inflow)+1)
          colnames(resault)<-c(paste("inflow","(",colnames(inflow),")",sep=""),"outflow")
          rownames(resault)<-c("Volumes (MCM)","Peak (cms)")
          for(i in 1:ncol(inflow))
          {
            resault[1,i]<-sum(inflow[,i]*object$simulation$by,na.rm=TRUE)/10^6
            resault[2,i]<-max(inflow[,i],na.rm=TRUE)
          }
          resault[1,i+1]<-sum(outflow*object$simulation$by,na.rm=TRUE)/10^6
          resault[2,i+1]<-max(outflow,na.rm=TRUE)
          print(resault)
      }
    }

  if(nDiv>0)
  {
      for(i in 1:nDiv)
      {
          cat(paste("--------------------","Diversion:",object$diversions[[i]]$name,"--------------------","\n"))
          inflow<-object$diversions[[i]]$inflow
          outflow<-object$diversions[[i]]$outflow
          resault<-matrix(NA,2,ncol(inflow)+1)
          colnames(resault)<-c(paste("inflow","(",colnames(inflow),")",sep=""),"outflow")
          rownames(resault)<-c("Volumes (MCM)","Peak (cms)")
          for(i in 1:ncol(inflow))
          {
            resault[1,i]<-sum(inflow[,i]*object$simulation$by,na.rm=TRUE)/10^6
            resault[2,i]<-max(inflow[,i],na.rm=TRUE)
          }
          resault[1,i+1]<-sum(outflow*object$simulation$by,na.rm=TRUE)/10^6
          resault[2,i+1]<-max(outflow,na.rm=TRUE)
          print(resault)
      }
    }
}
