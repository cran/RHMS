transform <-
function(rainfall,transformParams=list(Tlag=NULL,Cp=NULL,Ct=NULL,L=NULL,Lc=NULL),Area,simulation=c(interval=3600*1,period=NA),transformMethod)
{
   exRainfall<-rainfall[,3]
   if(is.na(simulation[2])){simulation[2]<-length(exRainfall)}
   if(transformMethod=="SCS")
   {
      t_by_tp<-c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1,1.1,1.2,1.3,1.4,1.5,1.6,1.7,1.8,1.9,2,2.1,2.2,2.3,2.4,2.5,2.6,2.7,2.8,2.9,3,3.1,3.2,3.3,3.4,3.5,3.6,3.7)
      q_by_qp<-c(0,0.04,0.105,0.2,0.32,0.46,0.64,0.82,0.93,0.985,1,0.995,0.99,0.97,0.945,0.905,0.855,0.8,0.74,0.68,0.62,0.56,0.5,0.44,0.38,0.32,0.27,0.22,0.18,0.15,0.13,0.11,0.09,0.075,0.06,0.04,0.02,0)
      Tlag<-transformParams$Tlag
      tp<-length(exRainfall)*simulation[1]/3600/2+Tlag
      qp<-0.208*Area/tp
      SCS_UH<-data.frame(t=tp*t_by_tp,q=qp*q_by_qp)
      SCS_UH<-as.data.frame(approx(SCS_UH,xout=0:(simulation[1]*(simulation[2]-1)/3600)))
      SCS_UH[which(is.na(SCS_UH[,2])),2]<-0
      if(simulation[2]>nrow(SCS_UH))
      {
         extra<-simulation[2]-nrow(SCS_UH)
         SCS_UH<-rbind(SCS_UH,data.frame(x=48.1+1:extra*1.3,y=rep(0,extra)))
      }
      if(simulation[2]<nrow(SCS_UH))
      {
         less<-nrow(SCS_UH)-simulation[2]
         SCS_UH<-SCS_UH[1:less,]
      }

      Hydrograph<-rep(NA,nrow(SCS_UH))
      exRainfall<-c(exRainfall,rep(0,length(Hydrograph)-length(exRainfall)))
      Rainfall<-c(rainfall[,1],rep(0,length(Hydrograph)-length(rainfall[,1])))
      Losses<-Rainfall-exRainfall
      q<-0
      for(i in 1:nrow(SCS_UH))
      {
         for(j in 1:i)
         {
            q<-q+SCS_UH[j,2]*exRainfall[i-j+1]
         }
         Hydrograph[i]<-q
         q<-0
      }
      exRainfallVolume<-sum(exRainfall)/1000*Area
      HydrographVolume<-sum(Hydrograph*simulation[1])/1000000
      if(exRainfallVolume<HydrographVolume)
      {
         dif<-HydrographVolume-exRainfallVolume
         Hydrograph<-Hydrograph-Hydrograph/sum(Hydrograph)*dif*1000/3.6
      }
      if(exRainfallVolume>HydrographVolume)
      {
         dif<-exRainfallVolume-HydrographVolume
         Hydrograph<-Hydrograph+Hydrograph/sum(Hydrograph)*dif*1000/3.6
      }
      Hydrograph[which(Hydrograph<0)]<-0
      resault<-cbind(Rainfall,Losses,exRainfall,Hydrograph)
      return(resault)
   }

   if(transformMethod=="snyder")
   {
      Ct<-transformParams$Ct
      Cp<-transformParams$Cp
      L <-transformParams$L
      Lc<-transformParams$Lc
      TR<-length(exRainfall)*simulation[1]/3600

      Tl<-Ct*(L*Lc)^0.3
      TD<-Tl/5.5
      Tlr<-Tl+0.25*(TR-TD)
      Qp<-Cp*Area/Tlr
      Tp<-TR/2+Tlr
      Tb<-(3+7*Area/10000)*Tp
      W75<-0.13*(Area/Qp)^1.08
      W50<-0.23*(Area/Qp)^1.08

      p1<-c(0,0)
      p2<-c(Tp-W50/3,Qp/2)
      p3<-c(Tp-W75/3,Qp*3/4)
      p4<-c(Tp,Qp)
      p5<-c(Tp+W75*2/3,Qp*3/4)
      p6<-c(Tp+W50*2/3,Qp/2)
      p7<-c(Tb,0)
      SnyderUH_Ordinates<-t(matrix(c(p1,p2,p3,p4,p5,p6,p7),2,7))
      t<-SnyderUH_Ordinates[,1]
      q<-SnyderUH_Ordinates[,2]
      smoothed <- loess(q~t)
      t <- seq(0,max(t), simulation[1]/3600)
      q<-predict(smoothed,t)
      q[which(q<0)]<-0
      Snyder_UH<-data.frame(t,q)
      if(simulation[2]>nrow(Snyder_UH))
      {
         extra<-simulation[2]-nrow(Snyder_UH)
         Snyder_UH<-rbind(Snyder_UH,data.frame(t=((nrow(Snyder_UH)+1):simulation[2])*simulation[1]/3600,q=rep(0,extra)))
      }
      if(simulation[2]<nrow(Snyder_UH))
      {
         less<-nrow(Snyder_UH)-simulation[2]
         Snyder_UH<-Snyder_UH[1:(nrow(Snyder_UH)-less),]
      }

      Hydrograph<-rep(NA,nrow(Snyder_UH))
      exRainfall<-c(exRainfall,rep(0,length(Hydrograph)-length(exRainfall)))
      Rainfall<-c(rainfall[,1],rep(0,length(Hydrograph)-length(rainfall[,1])))
      Losses<-Rainfall-exRainfall
      q<-0
      for(i in 1:nrow(Snyder_UH))
      {
         for(j in 1:i)
         {
            q<-q+Snyder_UH[j,2]*exRainfall[i-j+1]
         }
         Hydrograph[i]<-q
         q<-0
      }
      (exRainfallVolume<-sum(exRainfall)/1000*Area)
      (HydrographVolume<-sum(Hydrograph*simulation[1])/1000000)
      if(exRainfallVolume<HydrographVolume)
      {
         dif<-HydrographVolume-exRainfallVolume
         Hydrograph<-Hydrograph-Hydrograph/sum(Hydrograph)*dif*1000/3.6
      }
      if(exRainfallVolume>HydrographVolume)
      {
         dif<-exRainfallVolume-HydrographVolume
         Hydrograph<-Hydrograph+Hydrograph/sum(Hydrograph)*dif*1000/3.6
      }

      Hydrograph[which(Hydrograph<0)]<-0
      resault<-cbind(Rainfall,Losses,exRainfall,Hydrograph)
      return(resault)
  }
}
