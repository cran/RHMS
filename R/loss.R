loss <-
function(precipitation,lossParams=list(f0,f1,k,CN),simulation=c(interval=3600,period=NA),lossMethod)
{
   if(lossMethod=="horton")
   {
      f0<-lossParams$f0
      f1<-lossParams$f1
      k <-lossParams$k
      mat<-matrix(0,length(precipitation),3)
      colnames(mat)<-c("Rainfall","Loss","ExcessRainfall")
      mat[,1]<-precipitation
      mat[,2]<-(function(t){f1+(f1-f0)*exp(-k*t)})(seq(simulation[1]/3600,simulation[1]/3600*length(precipitation),simulation[1]/3600))
      mat[which(mat[,2]<0),2]<-0
      mat[which(mat[,2]>mat[,1]),2]<-mat[which(mat[,2]>mat[,1]),1]
      mat[,3]<-mat[,1]-mat[,2]
      mat[which(mat[,3]<0),3]<-0
      return(mat)
   }
   if(lossMethod=="SCS")  
   {
      CN<-lossParams$CN
      S<-(1000/CN-10)*25.4
      Ia<-0.2*S
      mat<-matrix(NA,length(precipitation),6)
      colnames(mat)<-c("cummulative precipitation","Ia","Fa","cummulative ExcessRainfall","ExcessRainfall","Loss")
      mat[,1]<-cumsum(precipitation)
      mat[,2]<-ifelse(mat[,1]<Ia,mat[,1],Ia)
      mat[,3]<-ifelse(mat[,1]<Ia,NA,S*(mat[,1]-Ia)/(mat[,1]-Ia+S))
      mat[,4]<-mat[,1]-(mat[,2]+mat[,3])
      mat[which(is.na(mat[,4])),4]<-0
      mat[,5]<-c(mat[1,4],diff(mat[,4]))
      mat[,6]<-precipitation-mat[,5]
      return(cbind(Rainfall=precipitation,mat[,6:5]))
   }else{
      stop("wrong loss method type!")
   }
}
