reservoirRouting.base <-
function(inflow,geometry,initialStorage,simulation)
{
   storageElevationCurve<-geometry$storageElevationCurve
   dischargeElevationCurve<-geometry$dischargeElevationCurve
   capacity<-geometry$capacity
   inflow[which(inflow<0)]<-0
   Q.out<-NULL
   if(is.na(initialStorage)) initialStorage<-capacity
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
   SH<-function(H){return(approxExtrap(x=storageElevationCurve[,2],y=storageElevationCurve[,1],xout=H)$y-capacity)}
   OH<-function(H){return(approxExtrap(x=dischargeElevationCurve[,2],y=dischargeElevationCurve[,1],xout=H)$y)}
   G <-function(H){SH(H)*10^6/simulation$by+OH(H)/2}
   ma<- function(x,n=2){filter(x,rep(1/n,n), sides=2)}
   g<-c(0,sapply(seq(min(dischargeElevationCurve[,2]),max(dischargeElevationCurve[,2]),length.out=100),G))
   o<-c(0,sapply(seq(min(dischargeElevationCurve[,2]),max(dischargeElevationCurve[,2]),length.out=100),OH))
   OG<-function(G){approxExtrap(x=g,y=o,xout=G)$y}
   mat<-as.data.frame(matrix(0,length(simulation$simulationSteps),5))
   colnames(mat)<-c("I","Im","O","Im-O","G")
   rownames(mat)<-simulation$simulationSteps
   mat[1:length(inflow),1]<-inflow
   mat[1:(length(inflow)),2]<-c(NA,ma(inflow,2)[1:(length(inflow)-1)])
   mat[1,3]<-0
   mat[2,4]<-mat[2,2]-mat[1,3]
   mat[1,5]<-0
   mat[2,5]<-mat[1,5]+mat[2,4]
   for(t in 2:(nrow(mat)-1))
   {
      mat[t,3]<-OG(mat[t,5])
      mat[t+1,4]<-mat[t+1,2]-mat[t,3]
      mat[t+1,5]<-mat[t,5]+mat[t+1,4]
   }
   mat[t+1,3]<-OG(mat[t+1,5])
   mat[which(mat[,3]<0),3]<-0
   return(mat)
}