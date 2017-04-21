createDiversion.default <-
function(name="Unttitled",label,downstream=NA,divertTo,capacity)
{

   diversion<-list(name=name,label=label,downstream=downstream,divertTo=divertTo,capacity=capacity)

   if(missing(label)){stop("label code is not specified!")}
   if(missing(divertTo)){stop("divertTo code is not specified!")}
   if(missing(capacity)){stop("diversion capacity rate is not specified!")}

   resault<-list()
   operation<-createDiversion.base(name,label,downstream,divertTo,capacity)
   resault$operation<-operation
   resault$call<-match.call()
   class(resault)<-'createDiversion'
   return(resault)
}
