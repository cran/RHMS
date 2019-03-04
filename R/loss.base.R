loss.base <-
function(precipitation,lossMethod,lossParams)
{
   if(lossMethod=="horton")
   {
      f0<-lossParams$f0
      f1<-lossParams$f1
      k <-lossParams$k
      mat<-as.data.frame(matrix(0,length(precipitation),3))
      colnames(mat)<-c("Rainfall","Loss","ExcessRainfall")
      rownames(mat)<-paste('Step:' ,1:length(precipitation))
      mat[,1]<-precipitation
      mat[,2]<-(function(t){f1+(f1-f0)*exp(-k*t)})(seq(lossParams$timeInterval/3600,lossParams$timeInterval/3600*length(precipitation),lossParams$timeInterval/3600))
      mat[which(mat[,2]<0),2]<-0
      mat[which(mat[,2]>mat[,1]),2]<-mat[which(mat[,2]>mat[,1]),1]
      mat[,3]<-mat[,1]-mat[,2]
      mat[which(mat[,3]<0),3]<-0
      return(mat)
   }
   if(lossMethod=="SCS")  
   {
      CN<-lossParams$CN+(lossParams$imperviousness/100)*(98-lossParams$CN)
      S<-(1000/CN-10)*25.4
      Ia<-0.2*S
      mat<-matrix(NA,length(precipitation),7)
      mat[,1]<-cumsum(precipitation)
      mat[,2]<-ifelse(mat[,1]<Ia,mat[,1],Ia)
      mat[,3]<-ifelse(mat[,1]<Ia,NA,S*(mat[,1]-Ia)/(mat[,1]-Ia+S))
      mat[,4]<-mat[,1]-(mat[,2]+mat[,3])
      mat[which(is.na(mat[,4])),4]<-0
      mat[,5]<-c(mat[1,4],diff(mat[,4]))
      mat[,6]<-precipitation-mat[,5]
      mat[,7]<-precipitation
      mat    <-data.frame(mat[,c(7,6,5)])
      colnames(mat)<-c("precipitation","Loss","ExcessRainfall")
      rownames(mat)<-paste('Step:' ,1:length(precipitation))
      return(mat)
   }
   if(lossMethod=="none")  
   {
      mat<-as.data.frame(matrix(NA,length(precipitation),3))
      colnames(mat)<-c("Rainfall","Loss","ExcessRainfall")
      rownames(mat)<-paste('Step:' ,1:length(precipitation))
      mat[,1]<-precipitation
      mat[,2]<-rep(0,length(precipitation))
      mat[,3]<-mat[,1]-mat[,2]
      return(mat)
   }
}