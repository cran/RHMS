reservoirRouting <-
function(inflow,ratingCurve,dischargeCurve,initialStorage,capacity,simulation=c(interval=3600*1,period=NA))
{
   inflow[which(inflow<0)]<-0
   Q.out<-NULL
   if(initialStorage<capacity)
   {
      id<-which(((cumsum(inflow)+initialStorage)>capacity)==TRUE)[1]
      if(length(id)==0)
      {
         Q.out<-rep(0,length(inflow))
         return(Q.out)
      }else{
         inflow[id]<-sum(inflow[1:id])-(capacity-initialStorage)
         inflow[1:(id-1)]<-0
      }
   }
   if(is.na(simulation[2])){simulation[2]<-simulation[1]*length(inflow)*6/3600}
   SH<-function(H){return(approxExtrap(x=ratingCurve[,2],y=ratingCurve[,1],xout=H)$y-capacity)}
   OH<-function(H){return(approxExtrap(x=dischargeCurve[,2],y=dischargeCurve[,1],xout=H)$y)}
   G <-function(H){SH(H)*10^6/simulation[1]+OH(H)/2}
   ma<- function(x,n=2){filter(x,rep(1/n,n), sides=2)}
   g<-c(0,sapply(seq(min(dischargeCurve[,2]),max(dischargeCurve[,2]),length.out=100),G))
   o<-c(0,sapply(seq(min(dischargeCurve[,2]),max(dischargeCurve[,2]),length.out=100),OH))
   OG<-function(G){approxExtrap(x=g,y=o,xout=G)$y}
   mat<-matrix(0,simulation[2],6)
   colnames(mat)<-c("T","I","Im","O","Im-O","G")
   mat[,1]<-(1:simulation[2])*simulation[1]/3600
   mat[1:length(inflow),2]<-inflow
   mat[1:(length(inflow)),3]<-c(NA,ma(inflow,2)[1:(length(inflow)-1)])
   mat[1,4]<-0
   mat[2,5]<-mat[2,3]-mat[1,4]
   mat[1,6]<-0
   mat[2,6]<-mat[1,6]+mat[2,5]
   for(t in 2:(nrow(mat)-1))
   {
      mat[t,4]<-OG(mat[t,6])
      mat[t+1,5]<-mat[t+1,3]-mat[t,4]
      mat[t+1,6]<-mat[t,6]+mat[t+1,5]
   }
   mat[t+1,4]<-OG(mat[t+1,6])
   mat[which(mat[,4]<0),4]<-0
   return(mat)
}
