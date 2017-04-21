sim.base <-
function(object)
{
  object<-object$operation
  dateAndTimeofExecution<-Sys.time()
  object$dateAndTimeofExecution<-dateAndTimeofExecution
  nRes<-length(object$reservoirs)
  nRec<-length(object$reachs)
  nJun<-length(object$junctions)
  nSub<-length(object$subbasins)
  nDiv<-length(object$diversions)
  labelMat<-matrix(NA,2,nRes+nRec+nJun+nSub+nDiv)
  if(ncol(labelMat)<1){stop("At least one element is needed for simulation !")}
  name<-c()
  if(nRes>0){for(i in 1:nRes){labelMat[1,i]                    <-object$reservoirs[[i]]$label;labelMat[2,i]                    <-object$reservoirs[[i]]$downstream; name<-c(name,object$reservoirs[[i]]$name)}}
  if(nRec>0){for(j in 1:nRec){labelMat[1,j+nRes]               <-object$reachs    [[j]]$label;labelMat[2,j+nRes]               <-object$reachs    [[j]]$downstream; name<-c(name,object$reachs    [[j]]$name)}}
  if(nJun>0){for(k in 1:nJun){labelMat[1,k+nRec+nRes]          <-object$junctions [[k]]$label;labelMat[2,k+nRec+nRes]          <-object$junctions [[k]]$downstream; name<-c(name,object$junctions [[k]]$name)}}
  if(nSub>0){for(l in 1:nSub){labelMat[1,l+nRec+nRes+nJun]     <-object$subbasins [[l]]$label;labelMat[2,l+nRec+nRes+nJun]     <-object$subbasins [[l]]$downstream; name<-c(name,object$subbasins [[l]]$name)}}
  if(nDiv>0){for(m in 1:nDiv){labelMat[1,m+nRec+nRes+nJun+nSub]<-object$diversions[[m]]$label;labelMat[2,m+nRec+nRes+nJun+nSub]<-object$diversions[[m]]$downstream; name<-c(name,object$diversions[[m]]$name)}}

  colnames(labelMat)<-name
  rownames(labelMat)<-c("code","downstream")
  if(any(duplicated(labelMat[1,]))==TRUE){stop("each object should have a unique code number, please revise the labels!")}
  if(sum(is.na(labelMat[2,]))>1 & sum(is.na(labelMat[2,]))<1){stop("wrong number of outlet!")}
  idUpstream<-which(is.na(match(labelMat[1,],labelMat[2,]))==TRUE)

  find_and_set<-function(downstreamCode,outflow,label,object)
  {
    matched<-NA
    if(nRes>0)
    {
      for(p in 1:nRes)
      {
        if(downstreamCode==object$reservoirs[[p]]$label)
        {
          object$reservoirs[[p]]$inflow<-cbind(object$reservoirs[[p]]$inflow,outflow)
          colnames(object$reservoirs[[p]]$inflow)<-c(colnames(object$reservoirs[[p]]$inflow)[1:(ncol(object$reservoirs[[p]]$inflow)-1)],label)
          matched<-"OK"
          return(object) 
        }
      }
    }
    if(nRec>0)
    {
      for(q in 1:nRec)
      {
        if(downstreamCode==object$reachs[[q]]$label)
        {
          object$reachs[[q]]$inflow<-cbind(object$reachs[[q]]$inflow,outflow)
          colnames(object$reachs[[q]]$inflow)<-c(colnames(object$reachs[[q]]$inflow)[1:(ncol(object$reachs[[q]]$inflow)-1)],label)
          matched<-"OK"
          return(object) 
        }
      }
    }
    if(nJun>0)
    {
      for(r in 1:nJun)
      {
        if(downstreamCode==object$junctions [[r]]$label)
        {
          object$junctions [[r]]$inflow<-cbind(object$junctions [[r]]$inflow,outflow)
          colnames(object$junctions[[r]]$inflow)<-c(colnames(object$junctions[[r]]$inflow)[1:(ncol(object$junctions[[r]]$inflow)-1)],label)
          matched<-"OK"
          return(object) 
        }
      }
    }
    if(nSub>0)
    {
      for(s in 1:nSub)
      {
        if(downstreamCode==object$subbasins [[s]]$label)
        {
          object$subbasins [[s]]$inflow<-cbind(object$subbasins [[s]]$inflow,outflow)
          colnames(object$subbasins[[s]]$inflow)<-c(colnames(object$subbasins[[s]]$inflow)[1:(ncol(object$subbasins[[s]]$inflow)-1)],label)
          matched<-"OK"
          return(object) 
        }
      }
    }
    if(nDiv>0)
    {
      for(t in 1:nDiv)
      {
        if(downstreamCode==object$diversions [[t]]$label)
        {
          object$diversions [[t]]$inflow<-cbind(object$diversions [[t]]$inflow,outflow)
          colnames(object$diversions[[t]]$inflow)<-c(colnames(object$diversions[[t]]$inflow)[1:(ncol(object$diversions[[t]]$inflow)-1)],label)
          matched<-"OK"
          return(object) 
        }
      }
    }
    if(is.na(matched)){stop("please check objects downstream connectivity!")}
  }

  while(length(idUpstream)>0)
  {
    for(n in 1:length(idUpstream))
    {
      currentCode<-labelMat[1,idUpstream[n]]
      label<-paste("from",colnames(labelMat)[idUpstream[n]])

      if(nRes>0)
      {
        for(i in 1:nRes)
        {
          if(currentCode==object$reservoirs[[i]]$label)
          {
            inflow  <-apply(object$reservoirs[[i]]$inflow,1,sum)
            ratingCurve   <-object$reservoirs[[i]]$ratingCurve
            dischargeCurve<-object$reservoirs[[i]]$dischargeCurve
            initialStorage<-object$reservoirs[[i]]$initialStorage
            capacity      <-object$reservoirs[[i]]$capacity
            resault<-reservoirRouting(inflow,
                                      ratingCurve,
                                      dischargeCurve,
                                      initialStorage,
                                      capacity,
                                      simulation=c(interval=object$interval,period=object$simPeriod))
            object$reservoirs[[i]]$simResault<-resault
            outflow<-resault[,4]
            object$reservoirs[[i]]$outflow<-outflow
            downstreamCode<-labelMat[2,idUpstream[n]]
            if(is.na(downstreamCode))
            {
              return(object)
            }else{
              object<-find_and_set(downstreamCode,outflow,label,object)
            }
          }
        }
      }
      
      if(nRec>0)
      {
          for(j in 1:nRec)
          {
            if(currentCode==object$reachs[[j]]$label)
            {
              inflow<-apply(object$reachs[[j]]$inflow,1,sum,na.rm=TRUE)
              routingMethod<-object$reachs[[j]]$routingMethod
              routingParams<-object$reachs[[j]]$routingParams
              resault<-reachRouting(inflow,
                                    routingMethod,
                                    routingParams,
                                    simulation=c(interval=object$interval,period=object$simPeriod))
              if(routingMethod=="muskingum")      {outflow<-resault[,3]}
              if(routingMethod=="muskingumcunge") {outflow<-resault[,ncol(resault)]}
              
              object$reachs[[j]]$outflow<-outflow
              downstreamCode<-labelMat[2,idUpstream[n]]
              if(is.na(downstreamCode))
              {
                 return(object)
              }else{
                 object<-find_and_set(downstreamCode,outflow,label,object)
              }
            }
          }
        }
      
      if(nJun>0)
      {
          for(k in 1:nJun)
          {
            if(currentCode==object$junctions[[k]]$label)
            {
              outflow<-apply(object$junctions[[k]]$inflow,1,sum,na.rm=TRUE)
              object$junctions[[k]]$outflow<-outflow
              downstreamCode<-labelMat[2,idUpstream[n]]
              if(is.na(downstreamCode))
              {
                return(object)
              }else{
                object<-find_and_set(downstreamCode,outflow,label,object)
              }
            }
          }
        }
      
      if(nSub>0)
      {
          for(l in 1:nSub)
          {
            if(currentCode==object$subbasins[[l]]$label)
            {
              inflow<-apply(object$subbasins[[l]]$inflow,1,sum,na.rm=TRUE)
              precipitation<-object$subbasins[[l]]$precipitation
              Area<-object$subbasins[[l]]$Area
              transformMethod<-object$subbasins[[l]]$transformMethod
              lossMethod     <-object$subbasins[[l]]$lossMethod
              transformParams<-object$subbasins[[l]]$transformParams
              lossParams<-object$subbasins[[l]]$lossParams
              rainfall<-loss(precipitation,lossParams,simulation=c(interval=object$interval,period=object$simPeriod),lossMethod)
              resault<-transform (rainfall,transformParams,Area,simulation=c(interval=object$interval,period=object$simPeriod),transformMethod)
              object$subbasins[[l]]$simResault<-resault
              outflow<-resault[,4]+inflow
              object$subbasins[[l]]$outflow<-outflow
              downstreamCode<-labelMat[2,idUpstream[n]]
              if(is.na(downstreamCode))
              {
                return(object)
              }else{
                object<-find_and_set(downstreamCode,outflow,label,object)
              }
            }
          }
        }

      if(nDiv>0)
      {
          for(m in 1:nDiv)
          {
            if(currentCode==object$diversions[[m]]$label)
            {
              divertTo<-object$diversions[[m]]$divertTo
              capacity<-object$diversions[[m]]$capacity
              inflow<-apply(object$diversions[[m]]$inflow,1,sum,na.rm=TRUE)
              diverted<-ifelse(inflow>capacity,capacity,inflow)
              outflow<-inflow-diverted
              object<-find_and_set(divertTo,diverted,label,object)
              resault<-data.frame(inflow=object$diversions[[m]]$inflow,diverted,outflow)
              object$diversions[[m]]$simResault<-resault
              object$diversions[[m]]$outflow<-outflow
              downstreamCode<-labelMat[2,idUpstream[n]]
              if(is.na(downstreamCode))
              {
                return(object)
              }else{
                object<-find_and_set(downstreamCode,outflow,label,object)
              }
            }
          }
        }

    }
    labelMat<-as.matrix(labelMat[,-idUpstream])
    idUpstream<-which(is.na(match(labelMat[1,],labelMat[2,]))==TRUE)
  }
}