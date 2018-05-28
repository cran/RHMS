plot.sim <-
function(x,...)
{
   if(missing(x))
   {
      stop("missing object!")
   }
   if(!(class(x)=='sim'))
   {
      stop("bad class type!")
   }
   readkeygraph <- function(prompt)
   {
      getGraphicsEvent(prompt = prompt, 
                       onMouseDown = NULL,
                       onMouseMove = NULL,
                       onMouseUp = NULL,
                       onKeybd = onKeybd,
                       consolePrompt = "[press any key to continue.....]")
      Sys.sleep(0.01)
      return(keyPressed)
   }
   onKeybd <- function(key) keyPressed <<- key

   x   <-x$operation
   nRes<-length(x$reservoirs)
   nRec<-length(x$reachs)
   nJun<-length(x$junctions)
   nSub<-length(x$subbasins)
   nDiv<-length(x$diversions)
   labelMat<-matrix(NA,2,nRes+nRec+nJun+nSub+nDiv)
   if(ncol(labelMat)<1){stop("At least one element is needed for simulation !")}
   name<-c()
   i<-0;j<-0;k<-0;l<-0;m<-0
   if(nRes>0){for(i in 1:nRes){labelMat[1,i]                    <-x$reservoirs[[i]]$label;labelMat[2,i]                    <-x$reservoirs[[i]]$downstream; name<-c(name,x$reservoirs[[i]]$name)}}
   if(nRec>0){for(j in 1:nRec){labelMat[1,j+nRes]               <-x$reachs    [[j]]$label;labelMat[2,j+nRes]               <-x$reachs    [[j]]$downstream; name<-c(name,x$reachs    [[j]]$name)}}
   if(nJun>0){for(k in 1:nJun){labelMat[1,k+nRec+nRes]          <-x$junctions [[k]]$label;labelMat[2,k+nRec+nRes]          <-x$junctions [[k]]$downstream; name<-c(name,x$junctions [[k]]$name)}}
   if(nSub>0){for(l in 1:nSub){labelMat[1,l+nRec+nRes+nJun]     <-x$subbasins [[l]]$label;labelMat[2,l+nRec+nRes+nJun]     <-x$subbasins [[l]]$downstream; name<-c(name,x$subbasins [[l]]$name)}}
   if(nDiv>0){for(m in 1:nDiv){labelMat[1,m+nRec+nRes+nJun+nSub]<-x$diversions[[m]]$label;labelMat[2,m+nRec+nRes+nJun+nSub]<-x$diversions[[m]]$downstream; name<-c(name,x$diversions[[m]]$name)}}
   colnames(labelMat)<-name
   rownames(labelMat)<-c("code","downstream")
   if(any(duplicated(labelMat[1,]))==TRUE){stop("each object should have a unique code number, please revise the labels!")}
   if(sum(is.na(labelMat[2,]))>1 & sum(is.na(labelMat[2,]))<1){stop("wrong number of outlet!")}
   idUpstream<-which(is.na(match(labelMat[1,],labelMat[2,]))==TRUE)


   if(nRes>0)
   {
      for(i in 1:nRes)
      {
            graphics.off()
            name<-paste("Rservoir: ",x$reservoirs[[i]]$name)
            inflow<-x$reservoirs[[i]]$inflow
            outflow<-x$reservoirs[[i]]$outflow
            if(ncol(inflow)>1)
            {
               plot(inflow[,1],typ='l',xlab="Time",ylab="Volume (cms)",ylim=c(min(inflow,outflow,na.rm=TRUE),max(inflow,outflow,na.rm=TRUE)),main=name)
               for(r in 2:ncol(inflow))
               {
                  lines(inflow[,r],col=r,lty=r)
               }
               lines(outflow,col=ncol(inflow)+1,lty=ncol(inflow)+1)
               name<-c(colnames(inflow),"otflow")
               legend("topright",legend=name,col=1:length(name),lty=1:(ncol(inflow)+1))
            }
            if(ncol(inflow)==1)
            {
               name<-paste("Reservir: ",x$reservoirs[[i]]$name)
               plot(inflow[,1],typ='l',xlab="Time",ylab="Volume (cms)",ylim=c(min(inflow,outflow,na.rm=TRUE),max(inflow,outflow,na.rm=TRUE)),main=name)
               lines(outflow,col=ncol(inflow)+1,lty=ncol(inflow)+1)
               name<-c(colnames(inflow),"otflow")
               legend("topright",legend=name,col=1:length(name),lty=1:(ncol(inflow)+1))
            }
            keyPressed = readkeygraph("[press any key to continue.....]")
      }
   }

   if(nRec>0)
   {
      for(i in 1:nRec)
      {
            graphics.off()
            name<-paste("Reach: ",x$reachs[[i]]$name)
            inflow<-x$reachs[[i]]$inflow
            outflow<-x$reachs[[i]]$outflow
            if(ncol(inflow)>1)
            {
               plot(inflow[,1],typ='l',xlab="Time",ylab="Volume (cms)",ylim=c(min(inflow,outflow,na.rm=TRUE),max(inflow,outflow,na.rm=TRUE)),main=name)
               for(r in 2:ncol(inflow))
               {
                  lines(inflow[,r],col=r,lty=r)
               }
               lines(outflow,col=ncol(inflow)+1,lty=ncol(inflow)+1)
               name<-c(colnames(inflow),"otflow")
               legend("topright",legend=name,col=1:length(name),lty=1:(ncol(inflow)+1))
            }
            if(ncol(inflow)==1)
            {
               plot(inflow[,1],typ='l',xlab="Time",ylab="Volume (cms)",ylim=c(min(inflow,outflow,na.rm=TRUE),max(inflow,outflow,na.rm=TRUE)),main=name)
               lines(outflow,col=ncol(inflow)+1,lty=ncol(inflow)+1)
               name<-c(colnames(inflow),"otflow")
               legend("topright",legend=name,col=1:length(name),lty=1:(ncol(inflow)+1))
            }
            keyPressed = readkeygraph("[press any key to continue.....]")
      }
   }

   if(nJun>0)
   {
      for(i in 1:nJun)
      {
            graphics.off()
            name<-paste("Junction: ",x$junctions[[i]]$name)
            inflow<-x$junctions[[i]]$inflow
            outflow<-x$junctions[[i]]$outflow
            if(ncol(inflow)>1)
            {
               plot(inflow[,1],typ='l',xlab="Time",ylab="Volume (cms)",ylim=c(min(inflow,outflow,na.rm=TRUE),max(inflow,outflow,na.rm=TRUE)),main=name)
               for(r in 2:ncol(inflow))
               {
                  lines(inflow[,r],col=r,lty=r)
               }
               lines(outflow,col=ncol(inflow)+1,lty=ncol(inflow)+1)
               name<-c(colnames(inflow),"otflow")
               legend("topright",legend=name,col=1:length(name),lty=1:(ncol(inflow)+1))
            }
            if(ncol(inflow)==1)
            {
               plot(inflow[,1],typ='l',xlab="Time",ylab="Volume (cms)",ylim=c(min(inflow,outflow,na.rm=TRUE),max(inflow,outflow,na.rm=TRUE)),main=name)
               lines(outflow,col=ncol(inflow)+1,lty=ncol(inflow)+1)
               name<-c(colnames(inflow),"otflow")
               legend("topright",legend=name,col=1:length(name),lty=1:(ncol(inflow)+1))
            }
            keyPressed = readkeygraph("[press any key to continue.....]")
      }
   }

   if(nDiv>0)
   {
      for(i in 1:nDiv)
      {
            graphics.off()
            par(mfrow = c(1, 2))
            name<-paste("Diversion: ",x$diversions[[i]]$name)
            inflow<-x$diversions[[i]]$inflow
            outflow<-x$diversions[[i]]$outflow
            diverted<-apply(inflow,1,sum)-outflow
            if(ncol(inflow)>1)
            {
               plot(inflow[,1],typ='l',xlab="Time",ylab="Volume (cms)",ylim=c(min(inflow,outflow,na.rm=TRUE),max(inflow,outflow,na.rm=TRUE)),main=name)
               for(r in 2:ncol(inflow))
               {
                  lines(inflow[,r],col=r,lty=r)
               }
               lines(outflow,col=ncol(inflow)+1,lty=ncol(inflow)+1)
               name<-c(colnames(inflow),"otflow")
               legend("topright",legend=name,col=1:length(name),lty=1:(ncol(inflow)+1))
            }
            if(ncol(inflow)==1)
            {
               plot(inflow[,1],typ='l',xlab="Time",ylab="Volume (cms)",ylim=c(min(inflow,outflow,na.rm=TRUE),max(inflow,outflow,na.rm=TRUE)),main=name)
               lines(outflow,col=ncol(inflow)+1,lty=ncol(inflow)+1)
               name<-c(colnames(inflow),"otflow")
               legend("topright",legend=name,col=1:length(name),lty=1:(ncol(inflow)+1))
            }
            plot(data.frame(Probability=(1:length(diverted))/(length(diverted)+1),Transfered=sort(diverted,decreasing =TRUE)),ylab="Divertd (cms)",typ='l')
            keyPressed = readkeygraph("[press any key to continue.....]")
      }
   }

   if(nSub>0)
   {
      for(i in 1:nSub)
      {
            p0<-hist(0)
            par(mfrow = c(1, 2))
            name<-paste("Sub-basin: ",x$subbasins[[i]]$name)
            inflow<-x$subbasins[[i]]$inflow
            outflow<-x$subbasins[[i]]$outflow
            if(ncol(inflow)>1)
            {
               plot(inflow[,1],typ='l',xlab="Time",ylab="Volume (cms)",ylim=c(min(inflow,outflow,na.rm=TRUE),max(inflow,outflow,na.rm=TRUE)),main=name)
               for(r in 2:ncol(inflow))
               {
                  lines(inflow[,r],col=r,lty=r)
               }
               lines(outflow,col=ncol(inflow)+1,lty=ncol(inflow)+1)
               name<-c(colnames(inflow),"otflow")
               BF<-x$subbasins[[1]]$BFSResault$operation[,2]
               if(x$subbasins[[1]]$BFSMethod!='none')
               {
                  name<-c(name,'baseflow')
                  lines(BF,col=ncol(inflow)+2,lty=ncol(inflow)+2)
                  legend("topright",legend=name,col=1:length(name),lty=1:(ncol(inflow)+2))
               }else{
                  legend("topright",legend=name,col=1:length(name),lty=1:(ncol(inflow)+1))
               }
            }
            if(ncol(inflow)==1)
            {
               name<-paste("Sub-basin: ",x$subbasins[[i]]$name)
               plot(inflow[,1],typ='l',xlab="Time",ylab="Volume (cms)",ylim=c(min(inflow,outflow,na.rm=TRUE),max(inflow,outflow,na.rm=TRUE)),main=name)
               lines(outflow,col=ncol(inflow)+1,lty=ncol(inflow)+1)
               name<-c(colnames(inflow),"otflow")
               BF<-x$subbasins[[1]]$BFSResault$operation[,2]
               if(x$subbasins[[i]]$BFSMethod!='none')
               {
                  name<-c(name,'baseflow')
                  lines(BF,col=ncol(inflow)+2,lty=ncol(inflow)+2)
                  legend("topright",legend=name,col=1:length(name),lty=1:(ncol(inflow)+2))
               }else{
                  legend("topright",legend=name,col=1:length(name),lty=1:(ncol(inflow)+1))
               }
            }
            n<-length(x$subbasins[[i]]$precipitation)
            Precipitation<-x$subbasins[[i]]$transformResault$operation[1:n,1]
            exPrecipitation<-x$subbasins[[i]]$transformResault$operation[1:n,3]
            p1<-list(breaks=1:n,
                     counts=Precipitation,
                     density=Precipitation/sum(Precipitation),
                     mids=seq(1.5,n,1),
                     xname="Precipitation",
                     equidist=TRUE)
            p2<-list(breaks=1:n,
                     counts=exPrecipitation,
                     density=exPrecipitation/sum(exPrecipitation),
                     mids=seq(1.5,n,1),
                     xname="exPrecipitation",
                     equidist=TRUE)
            class(p1)<-class(p2)<-class(p0)
            name<-paste("Sub-basin: ",x$subbasins[[i]]$name)
            plot(p1,xlab="Time",ylab="Precipitation",main="",col='gray',ylim=c(0,max(Precipitation)*1.25))
            plot(p2,col=2,add=T)
            legend("topright",legend=c("Total Rainfall","Excess Rainfall"),col=c('gray','red'),lty=1,lwd=c(10,10))
            keyPressed = readkeygraph("[press any key to continue.....]")
         }
   }
}