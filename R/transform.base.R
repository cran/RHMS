transform.base <-
function(rainfall,transformMethod,transformParams,Area,UH,simulation)
{
   sr<-smooth.spline(1:length(rainfall),rainfall)$y
   sr[sr<0]<-0
   ind_min <- which(diff(sign(diff(c(0, sr)))) == 2)
   if ((sr[length(sr)] - sr[length(sr) - 1]) < 0) 
   {
       ind_min <- c(ind_min, length(sr))
   }
   if ((sr[1] - sr[2]) < 0)
   {
       ind_min <- c(1, ind_min)
   }
   ind_min<-c(ind_min,1,length(sr))
   ind_min<-ind_min[!duplicated(ind_min)]
   ind_min <- ind_min[which(duplicated(ind_min) == FALSE)]
   nCycles<-length(ind_min)-1
   cyclesMat<-matrix(NA,3,nCycles)
   rownames(cyclesMat)<-c("start","end","length")
   colnames(cyclesMat)<-paste("Cycle",1:nCycles)
   cyclesMat[1,]<-ind_min[1:nCycles]
   cyclesMat[2,]<-ind_min[-1]-1
   cyclesMat[2,nCycles]<-cyclesMat[2,nCycles]+1
   cyclesMat[3,]<-cyclesMat[2,]-cyclesMat[1,]+1

   exRainfall<-matrix(0,length(simulation$simulationSteps),nCycles)
   rownames(exRainfall)<-1:nrow(exRainfall)
   colnames(exRainfall)<-paste("Cycle",1:nCycles)
   for(i in 1:nCycles)
   {
      id1<-cyclesMat[1,i]:cyclesMat[2,i] %in% as.numeric(rownames(exRainfall))
      names(id1)<-cyclesMat[1,i]:cyclesMat[2,i]
      id2<-match(names(which(id1)),rownames(exRainfall))
      id2<-id2[!is.na(id2)]
      exRainfall[id2,i]<-rainfall[as.numeric(names(which(id1)))]
   }
 
   if(transformMethod=="SCS")
   {
      t_by_tp<-c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1,1.1,1.2,1.3,1.4,1.5,1.6,1.7,1.8,1.9,2,2.1,2.2,2.3,2.4,2.5,2.6,2.7,2.8,2.9,3,3.1,3.2,3.3,3.4,3.5,3.6,3.7)
      q_by_qp<-c(0,0.04,0.105,0.2,0.32,0.46,0.64,0.82,0.93,0.985,1,0.995,0.99,0.97,0.945,0.905,0.855,0.8,0.74,0.68,0.62,0.56,0.5,0.44,0.38,0.32,0.27,0.22,0.18,0.15,0.13,0.11,0.09,0.075,0.06,0.04,0.02,0)
      Tlag<-transformParams$Tlag
      tp<-length(rainfall)*simulation$by/3600/2+Tlag
      qp<-0.208*Area/tp
      SCS_UH<-data.frame(t=tp*t_by_tp,q=qp*q_by_qp)
      SCS_UH<-as.data.frame(approx(SCS_UH,xout=0:(simulation$by*(length(simulation$simulationSteps)-1)/3600)))
      SCS_UH[which(is.na(SCS_UH[,2])),2]<-0
      if(length(simulation$simulationSteps)>nrow(SCS_UH))
      {
         extra<-length(simulation$simulationSteps)-nrow(SCS_UH)
         SCS_UH<-rbind(SCS_UH,data.frame(x=48.1+1:extra*1.3,y=rep(0,extra)))
      }
      if(length(simulation$simulationSteps)<nrow(SCS_UH))
      {
         SCS_UH<-SCS_UH[1:length(simulation$simulationSteps),]
      }

      Hydrograph<-matrix(NA,nrow(SCS_UH),nCycles)
      q<-0
      for(i in 1:nCycles)
      {
         for(j in 1:nrow(SCS_UH))
         {
            for(k in 1:j)
            {
               q<-q+SCS_UH[k,2]*exRainfall[j-k+1,i]
            }
            Hydrograph[j,i]<-q
            q<-0
         }
      }
   }

   if(transformMethod=="user")
   {
      userUH_Ordinates<-UH
      t<-userUH_Ordinates[,1]
      q<-userUH_Ordinates[,2]
      smoothed <- loess(q~t)
      t <- seq(0,max(t), simulation$by/3600)
      q<-predict(smoothed,t)
      q[which(q<0)]<-0
      q[which(is.na(q))]<-0
      user_UH<-data.frame(t,q)
      if(length(simulation$simulationSteps)>nrow(user_UH))
      {
         extra<-length(simulation$simulationSteps)-nrow(user_UH)
         user_UH<-rbind(user_UH,data.frame(t=((nrow(user_UH)+1):length(simulation$simulationSteps))*simulation$by/3600,q=rep(0,extra)))
      }
      if(length(simulation$simulationSteps)<nrow(user_UH))
      {
         less<-nrow(user_UH)-length(simulation$simulationSteps)
         user_UH<-user_UH[1:(nrow(user_UH)-less),]
      }

      Hydrograph<-matrix(NA,nrow(user_UH),nCycles)
      q<-0
      for(i in 1:nCycles)
      {
         for(j in 1:nrow(user_UH))
         {
            for(k in 1:j)
            {
               q<-q+user_UH[k,2]*exRainfall[j-k+1,i]
            }
            Hydrograph[j,i]<-q
            q<-0
         }
      }
   }

   if(transformMethod=="snyder")
   {
      Ct<-transformParams$Ct
      Cp<-transformParams$Cp
      L <-transformParams$L
      Lc<-transformParams$Lc
      TR<-length(rainfall)*simulation$by/3600

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
      t <- seq(0,max(t), simulation$by/3600)
      q<-predict(smoothed,t)
      q[which(q<0)]<-0
      Snyder_UH<-data.frame(t,q)
      if(length(simulation$simulationSteps)>nrow(Snyder_UH))
      {
         extra<-length(simulation$simulationSteps)-nrow(Snyder_UH)
         Snyder_UH<-rbind(Snyder_UH,data.frame(t=((nrow(Snyder_UH)+1):length(simulation$simulationSteps))*simulation$by/3600,q=rep(0,extra)))
      }
      if(length(simulation$simulationSteps)<nrow(Snyder_UH))
      {
         less<-nrow(Snyder_UH)-length(simulation$simulationSteps)
         Snyder_UH<-Snyder_UH[1:(nrow(Snyder_UH)-less),]
      }

      Hydrograph<-matrix(NA,nrow(Snyder_UH),nCycles)
      q<-0
      for(i in 1:nCycles)
      {
         for(j in 1:nrow(Snyder_UH))
         {
            for(k in 1:j)
            {
               q<-q+Snyder_UH[k,2]*exRainfall[j-k+1,i]
            }
            Hydrograph[j,i]<-q
            q<-0
         }
      }
   }
   Hydrograph<-apply(Hydrograph,1,sum)
   Hydrograph[which(Hydrograph<0)]<-0
   exRainfallVolume<-sum(apply(exRainfall,1,sum))/1000*Area
   HydrographVolume<-sum(Hydrograph*simulation$by)/1000000
   if(exRainfallVolume<HydrographVolume)
   {
      dif<-HydrographVolume-exRainfallVolume
      HydrographV<-Hydrograph*simulation$by/1000000
      Hydrograph<-(HydrographV-dif*(HydrographV/sum(HydrographV)))*1000000/simulation$by
   }
   if(exRainfallVolume>HydrographVolume)
   {
      dif<-exRainfallVolume-HydrographVolume
      HydrographV<-Hydrograph*simulation$by/1000000
      Hydrograph<-(HydrographV+dif*(HydrographV/sum(HydrographV)))*1000000/simulation$by
   }
   Hydrograph[which(Hydrograph<0)]<-0
   resault<-data.frame(rainfall=apply(exRainfall,1,sum),hydrograph=Hydrograph)
   rownames(resault)<-simulation$simulationSteps
   return(resault)
}