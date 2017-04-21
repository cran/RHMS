plot.sim <-
function(x,...)
{
   if(missing(x))
   {
      stop("missing object!")
   }
   object<-x
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
            graphics.off()
            name<-paste("Rservoir: ",object$reservoirs[[i]]$name)
            inflow<-object$reservoirs[[i]]$inflow
            outflow<-object$reservoirs[[i]]$outflow
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
               name<-paste("Reservir: ",object$reservoirs[[i]]$name)
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
            name<-paste("Reach: ",object$reachs[[i]]$name)
            inflow<-object$reachs[[i]]$inflow
            outflow<-object$reachs[[i]]$outflow
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
            name<-paste("Junction: ",object$junctions[[i]]$name)
            inflow<-object$junctions[[i]]$inflow
            outflow<-object$junctions[[i]]$outflow
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
            name<-paste("Diversion: ",object$diversions[[i]]$name)
            inflow<-object$diversions[[i]]$inflow
            outflow<-object$diversions[[i]]$outflow
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
            name<-paste("Sub-basin: ",object$subbasins[[i]]$name)
            inflow<-object$subbasins[[i]]$inflow
            outflow<-object$subbasins[[i]]$outflow
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
               name<-paste("Sub-basin: ",object$subbasins[[i]]$name)
               plot(inflow[,1],typ='l',xlab="Time",ylab="Volume (cms)",ylim=c(min(inflow,outflow,na.rm=TRUE),max(inflow,outflow,na.rm=TRUE)),main=name)
               lines(outflow,col=ncol(inflow)+1,lty=ncol(inflow)+1)
               name<-c(colnames(inflow),"otflow")
               legend("topright",legend=name,col=1:length(name),lty=1:(ncol(inflow)+1))
            }
            n<-length(object$subbasins[[i]]$precipitation)
            Precipitation<-object$subbasins[[i]]$simResault[1:n,1]
            exPrecipitation<-object$subbasins[[i]]$simResault[1:n,3]
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
            name<-paste("Sub-basin: ",object$subbasins[[i]]$name)
            plot(p1,xlab="Time",ylab="Precipitation",main="",col='gray',ylim=c(0,max(Precipitation)*1.25))
            plot(p2,col=2,add=T)
            legend("topright",legend=c("Total Rainfall","Excess Rainfall"),col=c('gray','red'),lty=1,lwd=c(10,10))
            keyPressed = readkeygraph("[press any key to continue.....]")
         }
   }
}
