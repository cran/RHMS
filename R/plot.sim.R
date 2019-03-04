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

   x   <-x$operation
   nRes<-length(x$reservoirs)
   nRec<-length(x$reachs)
   nJun<-length(x$junctions)
   nSub<-length(x$subbasins)
   nDiv<-length(x$diversions)

   labeler<-function(label)
   {
      n<-length(label)
      if(n<15){
         label<-label
         at=1:15
      }
      if(n>15){
         at<-round(seq(1,n,length.out=15))
         label<-label[at]
      }
      return(list(label=label,at=at))
   }

   if(nRes>0)
   {
      for(i in 1:nRes)
      {
            oask <- devAskNewPage(TRUE)
            on.exit(devAskNewPage(oask))
            name<-paste("Rservoir: ",x$reservoirs[[i]]$name)
            inflow<-x$reservoirs[[i]]$inflow
            outflow<-x$reservoirs[[i]]$outflow
            mar<-c(10,4.5,2.5,1.5)
            par(mar=mar)
            if(ncol(inflow)>1)
            {
               plot(inflow[,1],typ='l',xlab='',ylab="Volume (cms)",ylim=c(min(inflow,outflow,na.rm=TRUE),max(inflow,outflow,na.rm=TRUE)),main=name,axes=FALSE)
               axis(1,at=labeler(rownames(inflow))$at,labels=labeler(rownames(inflow))$label,las=2)
               axis(2)
               for(r in 2:ncol(inflow))
               {
                  lines(inflow[,r],col=r,lty=r)
               }
               lines(outflow,col=ncol(inflow)+1,lty=ncol(inflow)+1)
               name<-c(colnames(inflow),"outflow")
               legend("topright",legend=name,col=1:length(name),lty=1:(ncol(inflow)+1))
            }
            if(ncol(inflow)==1)
            {
               name<-paste("Reservir: ",x$reservoirs[[i]]$name)
               plot(inflow[,1],typ='l',xlab="Time",ylab="Volume (cms)",ylim=c(min(inflow,outflow,na.rm=TRUE),max(inflow,outflow,na.rm=TRUE)),main=name,axes=FALSE)
               axis(1,at=labeler(rownames(inflow))$at,labels=labeler(rownames(inflow))$label,las=2)
               axis(2)
               lines(outflow,col=ncol(inflow)+1,lty=ncol(inflow)+1)
               name<-c(colnames(inflow),"outflow")
               legend("topright",legend=name,col=1:length(name),lty=1:(ncol(inflow)+1))
            }
      }
   }

   if(nRec>0)
   {
      for(i in 1:nRec)
      {
            oask <- devAskNewPage(TRUE)
            on.exit(devAskNewPage(oask))
            name<-paste("Reach: ",x$reachs[[i]]$name)
            inflow<-x$reachs[[i]]$inflow
            outflow<-x$reachs[[i]]$outflow
            mar<-c(10,4.5,2.5,1.5)
            par(mar=mar)
            if(ncol(inflow)>1)
            {
               plot(inflow[,1],typ='l',xlab="",ylab="Volume (cms)",ylim=c(min(inflow,outflow,na.rm=TRUE),max(inflow,outflow,na.rm=TRUE)),main=name,axes=FALSE)
               axis(1,at=labeler(rownames(inflow))$at,labels=labeler(rownames(inflow))$label,las=2)
               axis(2)
               for(r in 2:ncol(inflow))
               {
                  lines(inflow[,r],col=r,lty=r)
               }
               lines(outflow,col=ncol(inflow)+1,lty=ncol(inflow)+1)
               name<-c(colnames(inflow),"outflow")
               legend("topright",legend=name,col=1:length(name),lty=1:(ncol(inflow)+1))
            }
            if(ncol(inflow)==1)
            {
               plot(inflow[,1],typ='l',xlab="",ylab="Volume (cms)",ylim=c(min(inflow,outflow,na.rm=TRUE),max(inflow,outflow,na.rm=TRUE)),main=name,axes=FALSE)
               lines(outflow,col=ncol(inflow)+1,lty=ncol(inflow)+1)
               axis(1,at=labeler(rownames(inflow))$at,labels=labeler(rownames(inflow))$label,las=2)
               axis(2)
               name<-c(colnames(inflow),"outflow")
               legend("topright",legend=name,col=1:length(name),lty=1:(ncol(inflow)+1))
            }
      }
   }

   if(nJun>0)
   {
      for(i in 1:nJun)
      {
            oask <- devAskNewPage(TRUE)
            on.exit(devAskNewPage(oask))
            name<-paste("Junction: ",x$junctions[[i]]$name)
            inflow<-x$junctions[[i]]$inflow
            outflow<-x$junctions[[i]]$outflow
            mar<-c(10,4.5,2.5,1.5)
            par(mar=mar)
            if(ncol(inflow)>1)
            {
               plot(inflow[,1],typ='l',xlab="",ylab="Volume (cms)",ylim=c(min(inflow,outflow,na.rm=TRUE),max(inflow,outflow,na.rm=TRUE)),main=name,axes=FALSE)
               axis(1,at=labeler(rownames(inflow))$at,labels=labeler(rownames(inflow))$label,las=2)
               axis(2)
               for(r in 2:ncol(inflow))
               {
                  lines(inflow[,r],col=r,lty=r)
               }
               lines(outflow,col=ncol(inflow)+1,lty=ncol(inflow)+1)
               name<-c(colnames(inflow),"outflow")
               legend("topright",legend=name,col=1:length(name),lty=1:(ncol(inflow)+1))
            }
            if(ncol(inflow)==1)
            {
               plot(inflow[,1],typ='l',xlab="",ylab="Volume (cms)",ylim=c(min(inflow,outflow,na.rm=TRUE),max(inflow,outflow,na.rm=TRUE)),main=name,axes=FALSE)
               axis(1,at=labeler(rownames(inflow))$at,labels=labeler(rownames(inflow))$label,las=2)
               axis(2)
               lines(outflow,col=ncol(inflow)+1,lty=ncol(inflow)+1)
               name<-c(colnames(inflow),"outflow")
               legend("topright",legend=name,col=1:length(name),lty=1:(ncol(inflow)+1))
            }
      }
   }

   if(nDiv>0)
   {
      for(i in 1:nDiv)
      {
         oask <- devAskNewPage(TRUE)
         on.exit(devAskNewPage(oask))
         mar<-c(10,4.5,2.5,1.5)
         par(mar=mar)
         name<-paste("Diversion: ",x$diversions[[i]]$name)
         inflow<-x$diversions[[i]]$inflow
         outflow<-x$diversions[[i]]$outflow
         diverted<-apply(inflow,1,sum)-outflow
         if(ncol(inflow)>1)
         {
            plot(inflow[,1],typ='l',xlab="",ylab="Volume (cms)",ylim=c(min(inflow,outflow,na.rm=TRUE),max(inflow,outflow,na.rm=TRUE)),main=name,axes=FALSE)
            axis(1,at=labeler(rownames(inflow))$at,labels=labeler(rownames(inflow))$label,las=2)
            axis(2)
            for(r in 2:ncol(inflow))
            {
               lines(inflow[,r],col=r,lty=r)
            }
            lines(outflow,col=ncol(inflow)+1,lty=ncol(inflow)+1)
            name<-c(colnames(inflow),"outflow")
            legend("topright",legend=name,col=1:length(name),lty=1:(ncol(inflow)+1))
         }
         if(ncol(inflow)==1)
         {
            plot(inflow[,1],typ='l',xlab="",ylab="Volume (cms)",ylim=c(min(inflow,outflow,na.rm=TRUE),max(inflow,outflow,na.rm=TRUE)),main=name,axes=FALSE)
            axis(1,at=labeler(rownames(inflow))$at,labels=labeler(rownames(inflow))$label,las=2)
            axis(2)
            lines(outflow,col=ncol(inflow)+1,lty=ncol(inflow)+1)
            name<-c(colnames(inflow),"outflow")
            legend("topright",legend=name,col=1:length(name),lty=1:(ncol(inflow)+1))
         }
         mar<-rep(5,4)
         par(mar=mar)
         plot(ecdf(diverted[,1]),verticals=TRUE,pch=NA,xlab='Diverted (cms)',ylab='Probability',main=paste("Diversion: ",x$diversions[[i]]$name))
      }
   }

   if(nSub>0)
   {
      for(i in 1:nSub)
      {
            p0<-hist(0)
            dev.off()
            oask <- devAskNewPage(TRUE)
            on.exit(devAskNewPage(oask))
            name<-paste("Sub-basin: ",x$subbasins[[i]]$name)
            inflow<-x$subbasins[[i]]$inflow
            outflow<-x$subbasins[[i]]$outflow
            mar<-c(10,4.5,2.5,1.5)
            par(mar=mar)
            if(ncol(inflow)>1)
            {
               plot(inflow[,1],typ='l',xlab="",ylab="Volume (cms)",ylim=c(min(inflow,outflow,na.rm=TRUE),max(inflow,outflow,na.rm=TRUE)),main=name,axes=FALSE)
               axis(1,at=labeler(rownames(inflow))$at,labels=labeler(rownames(inflow))$label,las=2)
               axis(2)
               for(r in 2:ncol(inflow))
               {
                  lines(inflow[,r],col=r,lty=r)
               }
               lines(outflow,col=ncol(inflow)+1,lty=ncol(inflow)+1)
               name<-c(colnames(inflow),"outflow")
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
               plot(inflow[,1],typ='l',xlab="",ylab="Volume (cms)",ylim=c(min(inflow,outflow,na.rm=TRUE),max(inflow,outflow,na.rm=TRUE)),main=name,axes=FALSE)
               axis(1,at=labeler(rownames(inflow))$at,labels=labeler(rownames(inflow))$label,las=2)
               axis(2)
               lines(outflow,col=ncol(inflow)+1,lty=ncol(inflow)+1)
               name<-c(colnames(inflow),"outflow")
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
            Precipitation<-x$subbasins[[i]]$precipitation
            n<-length(Precipitation)
            exPrecipitation<-x$subbasins[[i]]$transformResault$operation$rainfall[1:n]
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
            mar<-c(10,4.5,2.5,1.5)
            par(mar=mar)
            plot(p1,xlab="",ylab="Precipitation",col='gray',ylim=c(0,max(Precipitation)*1.25),axes=FALSE,main=name)
            plot(p2,col=2,add=T)
            legend("topright",legend=c("Total Rainfall","Excess Rainfall"),col=c('gray','red'),lty=1,lwd=c(10,10))
            axis(1,at=labeler(rownames(inflow)[1:n])$at,labels=labeler(rownames(inflow)[1:n])$label,las=2)
            axis(2)
         }
   }
}