plot.createBasin<-
function(x,...)
{
   if(missing(x))
   {
      stop("missing object!")
   }
   if(!any(class(x)==c('sim','createBasin')))
   {
      stop("bad class type!")
   }

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
   if(nDiv>0){for(m in 1:nDiv){labelMat[1,m+nRec+nRes+nJun+nSub]<-x$diversions[[m]]$label;labelMat[2,m+nRec+nRes+nJun+nSub]<-x$diversions[[m]]$downstream; name<-c(name,x$diversions[[m]]$name,x$diversions[[m]]$name)}}
   if(nDiv>0){for(m in 1:nDiv){labelMat<-cbind(labelMat,c(x$diversions[[m]]$label,x$diversions[[m]]$divertTo))}}
   colnames(labelMat)<-name
   rownames(labelMat)<-c("code","downstream")
   if(sum(is.na(labelMat[2,]))>1 & sum(is.na(labelMat[2,]))<1){stop("wrong number of outlet!")}
   idUpstream<-which(is.na(match(labelMat[1,],labelMat[2,]))==TRUE)

   type<-c('Reservoir','Reach','Junction','Sub-basin','Diversion')
   availableTypes<-c(ifelse(i>0,1,NA),ifelse(j>0,1,NA),ifelse(k>0,1,NA),ifelse(l>0,1,NA),ifelse(m>0,1,NA))
   type<-type[which(!is.na(availableTypes))]
   types<-rep(type,c(i,j,k,l,2*m)[which(!is.na(availableTypes))])
   color.palette<-c(5,1,2,3,4)[which(!is.na(availableTypes))]
   shape.palette <-c(17,1,3,15,10)[which(!is.na(availableTypes))]
   size.palette<-c(10,0.01,10,10,10)[which(!is.na(availableTypes))]
   names(size.palette)<-type
   names(shape.palette)<-type
   names(color.palette)<-type
   net<-matrix(0,nRes+nRec+nJun+nSub+nDiv*2,nRes+nRec+nJun+nSub+nDiv*2)
   for(n in 1:ncol(net))
   {
      con<-which(labelMat[2,n]==labelMat[1,])
      if(length(con)>0) {net[n,con]<-1}
   }
   colnames(net)<-colnames(labelMat)
   rownames(net)<-colnames(labelMat)
   Net<-net[1:(nRes+nRec+nJun+nSub),]
   if(nDiv>0)
   {
      for(i in 1:nDiv)
      {
         Net<-rbind(Net,net[nRes+nRec+nJun+nSub+(i-1)*2+1,,drop=FALSE]+net[nRes+nRec+nJun+nSub+(i)*2,,drop=FALSE])
      }
      Net<-Net[,-which(duplicated(labelMat[1,]))]
   }
   net<-network(Net)
   set.vertex.attribute(net,"type",types)
   ggnet2(net,color='type',,size='type',shape='type',
          color.palette=color.palette,shape.palette=shape.palette,size.palette=size.palette,
          label=TRUE,arrow.size = 9, arrow.gap = 0.025)+guides(size = FALSE)
}