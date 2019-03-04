tune<-
function(object,targetObject,decisionObjects,observationTS,delay=0,
transformBandWith=list(ct=c(1,2.5),cp=c(0.1,0.3),cn=c(25,85),k=c(0.1,2)),
routingBandWith=list(manning = c(0.0001,0.1),x= c(0.2,0.6),k=c(1,5)),maxiter=NA,update=FALSE,plot=FALSE)
{
   nRes<-length(object$operation$reservoirs)
   nRec<-length(object$operation$reachs)
   nJun<-length(object$operation$junctions)
   nSub<-length(object$operation$subbasins)
   nDiv<-length(object$operation$diversions)
   targetObject<-targetObject$operation$label
   label<-c()
   for (i in 1:length(decisionObjects)) label<-c(label,decisionObjects[[i]]$operation$label)
   decisionObjects<-label

   obs<-rep(0,length(object$operation$simulation$simulationSteps))
   ts<-c(rep(0,delay),observationTS)
   if(length(ts)<=length(obs)){obs[1:length(ts)]<-ts}
   if(length(ts)> length(obs)){obs<-ts[1:length(obs)]}
   observationTS<-obs
   
   refMat<-matrix(NA,5,1)
   rownames(refMat)<-c("Lower","Upper","Type","Label","NO")

   for(no in 1:length(decisionObjects))
   {
      if(nRes>0){for(i in 1:nRes){if(object$operation$reservoirs[[i]]$label==decisionObjects[no]){stop("wrong label code specified!")}}}
      if(nJun>0){for(i in 1:nJun){if(object$operation$junctions [[i]]$label==decisionObjects[no]){stop("wrong label code specified!")}}}   
      if(nRec>0)
      {
         for(i in 1:nRec)
         {
            if(object$operation$reachs[[i]]$label==decisionObjects[no])
            {
                   if(object$operation$reachs[[i]]$routingMethod=="muskingum")
                   {
                      k<-c(routingBandWith$k,1,decisionObjects[no],i)
                      x<-c(routingBandWith$x,2,decisionObjects[no],i)
                      refMat<-cbind(refMat,k,x)
                   }
                   if(object$operation$reachs[[i]]$routingMethod=="muskingumcunge")
                   {
                      n<-c(routingBandWith$maning[1],routingBandWith$maning[2],3,decisionObjects[no],i)
                      refMat<-cbind(refMat,n)
                   }
            }
        }
      }  
 
      if(nSub>0)
      { 
         for(i in 1:nSub) 
         {
            if(object$operation$subbasins[[i]]$label==decisionObjects[no])
            {
                if(object$operation$subbasins[[i]]$transformMethod=="snyder")
                {
                   Ct<-c(transformBandWith$ct,4,decisionObjects[no],i)
                   Cp<-c(transformBandWith$cp,5,decisionObjects[no],i)
                   refMat<-cbind(refMat,Cp,Ct)
                }
                if(object$operation$subbasins[[i]]$lossMethod=="SCS")
                {
                   CN<-c(transformBandWith$cn,6,decisionObjects[no],i)
                   refMat<-cbind(refMat,CN)
                }
                if(object$operation$subbasins[[i]]$lossMethod=="horton")
                {
                   k<-c(transformBandWith$k,7,decisionObjects[no],i)
                   refMat<-cbind(refMat,k)
                }
             }
          }
      }
   }

   refMat<-refMat[,-1]
   L<-refMat[1,]
   U<-refMat[2,]

   find_and_set<-function(x)
   {
      for(i in 1:length(x))
      {
         if(refMat[3,i]==1)
         {
            object$operation$reachs[[refMat[5,i]]]$routingParams$k<-x[i]
         }
         if(refMat[3,i]==2)
         {
            object$operation$reachs[[refMat[5,i]]]$routingParams$x<-x[i]
         }
         if(refMat[3,i]==3)
         {
            object$operation$reachs[[refMat[5,i]]]$routingParams$manningRoughness<-x[i]
         }
         if(refMat[3,i]==4)
         {
            object$operation$subbasins[[refMat[5,i]]]$transformParams$Cp<-x[i]
         }
         if(refMat[3,i]==5)
         {
            object$operation$subbasins[[refMat[5,i]]]$transformParams$Ct<-x[i]
         }
         if(refMat[3,i]==6)
         {
            object$operation$subbasins[[refMat[5,i]]]$lossParams$CN<-x[i]
         }
         if(refMat[3,i]==7)
         {
            object$operation$subbasins[[refMat[5,i]]]$lossParams$k<-x[i]
         }
      }
      return(object)
   }

   fit<-function(x)
   {
      object<-find_and_set(x)
      resault<-sim(object)$operation

      if(nSub>0){for(i in 1:nSub){if(resault$subbasins [[i]]$label==targetObject){computedTS<-resault$subbasins [[i]]$outflow[,1]}}}
      if(nRes>0){for(i in 1:nRes){if(resault$reservoirs[[i]]$label==targetObject){computedTS<-resault$reservoirs[[i]]$outflow[,1]}}}
      if(nJun>0){for(i in 1:nJun){if(resault$junctions [[i]]$label==targetObject){computedTS<-resault$junctions [[i]]$outflow[,1]}}}
      if(nRec>0){for(i in 1:nRec){if(resault$reachs    [[i]]$label==targetObject){computedTS<-resault$reachs    [[i]]$outflow[,1]}}}
      if(nDiv>0){for(i in 1:nDiv){if(resault$diversions[[i]]$label==targetObject){computedTS<-resault$diversions[[i]]$outflow[,1]}}}
      
      MSE<-(sum((observationTS-computedTS)^2/(length(observationTS)-1)))^0.5
      return(MSE)
   }
   if(is.na(maxiter)) maxiter<-ncol(refMat)^2
   opt<-psoptim(runif(ncol(refMat),L,U),
                fit,
                lower=L,upper=U,
                control=list(abstol=1e-8,trace=1,REPORT=1,maxit=maxiter))
   if(plot)
   {
      object<-find_and_set(opt$par)
      resault<-sim(object)$operation
      if(nSub>0){for(i in 1:nSub){if(resault$subbasins [[i]]$label==targetObject){computedTS<-resault$subbasins [[i]]$outflow[,1]}}}
      if(nRes>0){for(i in 1:nRes){if(resault$reservoirs[[i]]$label==targetObject){computedTS<-resault$reservoirs[[i]]$outflow[,1]}}}
      if(nJun>0){for(i in 1:nJun){if(resault$junctions [[i]]$label==targetObject){computedTS<-resault$junctions [[i]]$outflow[,1]}}}
      if(nRec>0){for(i in 1:nRec){if(resault$reachs    [[i]]$label==targetObject){computedTS<-resault$reachs    [[i]]$outflow[,1]}}}
      if(nDiv>0){for(i in 1:nDiv){if(resault$diversions[[i]]$label==targetObject){computedTS<-resault$diversions[[i]]$outflow[,1]}}}
      MSE<-(sum((observationTS-computedTS)^2/(length(observationTS)-1)))^0.5
      par(mfrow = c(1, 2),pty = "s")
      plot(computedTS,observationTS,xlab='computed',ylab='observed' , main=paste('MSE:',round(MSE,3),'    ','Pearson Coef:',round(cor(computedTS,observationTS),3)),pch=19)
      plot(observationTS,xlab='Time Step',ylab='Discharge (cms)',typ='l',ylim=c(min(computedTS,observationTS),max(computedTS,observationTS)))
      lines(computedTS,col=2)
      legend("topright",legend=c('observed','computed'),col=1:2,lty=c(1,1))
   }
   if(update)
   {
      return(find_and_set(opt$par))
   }
   if(update == FALSE)
   {
      params<-round(rbind(refMat[4,],opt$par),2)
      rownames(params)<-c("label code","value")
      return(params)
   }
}