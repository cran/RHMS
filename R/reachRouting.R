reachRouting <-
function(inflow,routingMethod="muskingum",routingParams=list(k=3,x=0.2,bedWith=NULL,sideSlope=2,channelSlope=NULL,manningRoughness=0.025,riverLength=NULL),simulation=c(interval=3600*1,period=NA))
{
   if(is.na(simulation[2])){simulation[2]<-length(inflow)}
   if(routingMethod=="muskingum")
   {
      k<-routingParams$k
      x<-routingParams$x
      C1<-(simulation[1]/3600+2*k*x)/(simulation[1]/3600+2*k-2*k*x)
      C2<-(simulation[1]/3600-2*k*x)/(simulation[1]/3600+2*k-2*k*x)
      C3<-1-(C1+C2)
      if(is.na(simulation[2])){simulation[2]<-(simulation[1]/3600)*length(inflow)*1.5}
      mat<-matrix(0,simulation[2],3)
      mat[,1]<-(1:simulation[2])*simulation[1]/3600
      colnames(mat)<-c("T","I","O")
      mat[1:length(inflow),2]<-inflow
      mat[1,3]<-0
      for(t in 2:(nrow(mat)))
      {
         mat[t,3]<-C1*mat[t-1,2]+C2*mat[t,2]+C3*mat[t-1,3]
      }
      mat[which(mat[,3]<0),3]<-0
      return(mat)
   }
   if(routingMethod=="muskingumcunge")
   {
      b<-routingParams$bedWith
      S<-routingParams$channelSlope
      m<-routingParams$sideSlope
      n<-routingParams$manningRoughness
      L<-routingParams$riverLength
      interval<-simulation[1]
      simPeriod<-simulation[2]
      dt<-interval
      Qave<-mean(inflow)
      y0<-uniroot(function(y){(1.49*S^0.5/n)* ((y*(b+m*y))^(5/3))/((b+2*y*(1+m^2))^(2/3))-Qave},lower=0,upper=100)$root
      V<-Qave/(y0*(b+2*y0))
      ck<-5*V/3
      B<-b+2*m*y0
      dx<-(0.5*(ck*dt+Qave/(B*S*ck)))*0.9
      cn<-ck*dt/dx
      X<-0.5*(1-Qave/(B*S*ck*dx))
      C0<-(0.5*cn-X)/(1-X+0.5*cn)
      C1<-(0.5*cn+X)/(1-X+0.5*cn)
      C2<-(1-0.5*cn-X)/(1-X+0.5*cn)
      mat<-matrix(0,simPeriod,(round(L/(dx/1000))+1))
      mat[1:length(inflow),1]<-inflow

      for(i in 2:(round(L/(dx/1000))+1))
      {
         mat[1,i]<-mat[1,i-1]
         for(j in 2:simPeriod)
         {
            Q0<-C0*mat[j,i-1]
            Q1<-C1*mat[j-1,i-1]
            Q2<-C2*mat[j-1,i]
            mat[j,i]<-Q0+Q1+Q2
         }
      }
      for(i in 2:ncol(mat))
      {
         Q<-mat[,i]
         mat[,i]<-0
         if(length(which(Q<0))>0)
         {
            id<-(which(Q<0)[1]-1)
            mat[1:id,i]<-Q[1:id]
         }
         mat[,i]<-Q
      }
      mat[which(mat<0)]<-0
      colnames(mat)<-c("inflow",paste(round(1:(round(L/(dx/1000)))*dx/1000),"Km"))
      rownames(mat)<-paste(dt/3600*(1:nrow(mat)),"Hr")
      return(mat)
   }
}
