abstraction.base<-function(rainfall,abstractionParams)
{
   # ------------- canopy abstraction-----------------
   canopyAbstraction<-abstractionParams$canopyAbstraction
   surfaceAbstraction<-abstractionParams$surfaceAbstraction
   temp<-rainfall
   id<-which(cumsum(temp)>canopyAbstraction)[1]-1
   if((!is.na(id)) && (id != 0))
   {
      temp[id+1]<-rainfall[id+1]-(canopyAbstraction-sum(rainfall[1:id]))
      temp[1:id]<-0
      canopy<-rainfall-temp
   }
   if(id == 0)
   {
      temp[1]<-rainfall[1]-canopyAbstraction
      canopy<-rainfall-temp
   }
   if(is.na(id))
   {
      temp[]<-0
      canopy<-rainfall
   }

   # ------------- surface abstraction-----------------
   temp<-rainfall-canopy
   id<-which(cumsum(temp)>surfaceAbstraction)[1]-1
   if((!is.na(id)) && (id != 0))
   {
      temp[id+1]<-(rainfall[id+1]-canopy[id+1])-(surfaceAbstraction-sum(rainfall[1:id]-canopy[1:id]))
      temp[1:id]<-0
      surface<-rainfall-canopy-temp
   }
   if(id == 0)
   {
      temp[1]<-rainfall[1]-canopy[1]-surfaceAbstraction
      surface<-rainfall-canopy-temp
   }
   if(is.na(id))
   {
      temp[]<-0
      surface<-rainfall-canopy
   }
   mat<-as.data.frame(cbind(rainfall=rainfall,canopy=canopy,surface=surface,excessRainfall=rainfall-canopy-surface))
   rownames(mat)<-paste('Step:', 1:length(rainfall))
   return(mat)
}