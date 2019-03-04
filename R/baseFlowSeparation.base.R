baseFlowSeparation.base<-function(discharge,BFSMethod,BFSParams,plot)
{
   res<-NULL
   nathan<-function(discharge,alpha,plot)
   {
      B<-R<-discharge
      B[]<-R[]<-NA
      R[1]<-0
      B[1]<-discharge[1]
      for(t in 2:length(discharge))
      {
         R[t]<-alpha*R[t-1]+(1+alpha)*(discharge[t]+discharge[t-1])/2
         if(R[t]<0){R[t]<-0}
         if(R[t]>discharge[t]){R[t]<-discharge[t]}
         B[t]<-discharge[t]-R[t]
      }
      res<-data.frame(discharge=discharge,baseflow=B,R=R)
      rownames(res)<-paste('Step: ', 1:length(discharge))
      if(plot)
      {
         plot(discharge,ylim=c(min(res),max(res)),typ='o')
         lines(B,col=2)
      }
      return(res)
   }

   chapman<-function(discharge,alpha,plot)
   {
      B<-R<-discharge
      B[]<-R[]<-NA
      R[1]<-0
      B[1]<-discharge[1]
      for(t in 2:length(discharge))
      {
         R[t]<-((3*alpha-1)/(3-alpha))*discharge[t-1]+2/(3-alpha)*(discharge[t]-discharge[t-1])
         if(R[t]<0){R[t]<-0}
         if(R[t]>discharge[t]){R[t]<-discharge[t]}
         B[t]<-discharge[t]-R[t]
      }
      res<-data.frame(discharge=discharge,baseflow=B,R=R)
      rownames(res)<-paste('Step: ', 1:length(discharge))
      if(plot)
      {
         plot(discharge,ylim=c(min(res),max(res)),typ='o')
         lines(B,col=2)
      }
      return(res)
   }

   eckhardt<-function(discharge,alpha,BFI,plot)
   {
      B<-R<-discharge
      B[]<-R[]<-NA
      R[1]<-0
      B[1]<-discharge[1]
      for(t in 2:length(discharge))
      {
         B[t]<-((1-BFI)*alpha*B[t-1]+(1-alpha)*BFI*discharge[t])/(1-alpha*BFI)
         if(B[t]>discharge[t]){B[t]<-discharge[t]}
         R[t]<-discharge[t]-B[t]
      }
      res<-data.frame(discharge=discharge,baseflow=B,R=R)
      rownames(res)<-paste('Step: ', 1:length(discharge))
      if(plot)
      {
         plot(discharge,ylim=c(min(res),max(res)),typ='o')
         lines(B,col=2)
      }
      return(res)
   }

   recession<-function(discharge,k,timeInterval,plot)
   {
      y<-discharge[1:which(discharge==max(discharge))]
      x<-1:length(y)
      m<-lm(y~x)$coefficients
      start<-floor(abs(m[1]/m[2]))+1
      res<-matrix(NA,length(discharge),4)
      res[,1]<-(1:length(discharge))*timeInterval/(3600*24)
      res[,2]<-discharge
      res[1:start,3]<-discharge[1:start]
      res[(start+1):nrow(res),3]<-discharge[start]*(k^res[(start+1):nrow(res),1])
      end<-which(res[,3]>res[,2])[1]
      if(is.na(end)){end<-length(discharge)}
      res[end:nrow(res),3]<-res[end:nrow(res),2]
      res[,4]<-res[,2]-res[,3]
      res<-res[,-1]
      colnames(res)<-c('discharge','baseflow','R')
      rownames(res)<-paste('Step: ', 1:length(discharge))
      if(plot)
      {
         plot(res[,1],ylim=c(min(res),max(res)),typ='o',ylab='discharge')
         lines(res[,2],col=2)
      }
      return(res)
   }

   alpha<-BFSParams$alpha
   BFI<-BFSParams$BFI
   timeInterval<-BFSParams$timeInterval
   k<-BFSParams$k

   if(BFSMethod=='nathan')
   {
      NATHAN<-nathan(discharge,alpha,plot)
      return(NATHAN)
   }
   if(BFSMethod=='chapman')
   {
      CHAPMAN<-chapman(discharge,alpha,plot)
      return(CHAPMAN)
   }
   if(BFSMethod=='eckhardt')
   {
      ECKHARDT<-eckhardt(discharge,alpha,BFI,plot)
      return(ECKHARDT)
   }
   if(BFSMethod=='recession')
   {
      RECESSION<-recession(discharge,k,timeInterval,plot)
      return(RECESSION)
   }
   if(BFSMethod=='none')
   {
      res<-as.data.frame(matrix(0,length(discharge),3))
      colnames(res)<-c('discharge','baseflow','R')
      rownames(res)<-paste('Step: ', 1:length(discharge))
      res[,1]<-discharge
      res[,3]<-discharge
      if(plot)
      {
         plot(discharge,ylim=c(min(res),max(res)),typ='o')
         lines(res[,2],col=2)
      }
      return(res)
   }
}