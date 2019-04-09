set.as<-function(base,target,type='downstream')
{
   if(missing(base)){stop('base object is missing, with no default value!')}
   if(missing(target)){stop('target object is missing, with no default value!')}
   if(!any(class(base)==c("createReach","createReservoir","createDiversion","createJunction","createSubbasin")))
   {
      stop("base object is wrongly specified!")
   }
   if(!any(class(target)==c("createReach","createReservoir","createDiversion","createJunction","createSubbasin")))
   {
      stop("base object is wrongly specified!")
   }
   if(!any(type==c('downstream','divertTo')))
   {
      stop('type is wrongly specified!')
   }
   if(type=='downstream')   {target$operation$downstream <-base$operation$label}
   if(type=='divertTo')     {target$operation$divertTo   <-base$operation$label}
   return(target)
}