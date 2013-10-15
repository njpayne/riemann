ROC<-function(x,event,m=100,listdata=F,plot=T)
{
  #  This function computes and plots ROC curve
  #  x = fitted values from logistic regression
  #  event = binary response
  #  m=no of points on curve
  
  start <- min(x)
  end <- max(x)
  
  sens<-onemspec<-1
  
  for (xi in seq(start+0.000001,end-0.000001,by=(end-start)/m))
  {
    tab<-table(event,x>xi)
    if( dim(tab)[2]==1) tab<-cbind(c(0,0),tab)
    sens<-c(sens,tab[2,2]/(tab[2,2]+tab[2,1]))
    onemspec<-c(onemspec,tab[1,2]/(tab[1,2]+tab[1,1]))
  }
  
  sens<-c(sens,0)
  onemspec<-c(onemspec,0)
  if(plot)
  {plot(onemspec,sens,type="n",xlab="1-specificity",ylab="Sensitivity")
   lines(lowess(onemspec,sens,f=0.2))
   #text(onemspec,sens,seq(start+0.000001,end-0.000001,by=(end-start)/m))
  }
  if(listdata)list(sens=sens,onemspec=onemspec)
}
