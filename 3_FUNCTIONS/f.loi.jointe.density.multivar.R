# pour 2 para ou + 
#------------------------


loi.jointe.multivar<-function(vect.coef,vect.names.effects)
{
  
  vect.c<-list()
  vect.labels<-c()
  
  for(i in 1:length(vect.coef))
  {
    vect.c[[i]]<-as.vector(mcmc.table[[vect.coef[i]]])
    vect.labels[[i]]<-paste(vect.coef[[i]],"\n",vect.names.effects[[i]], sep="")
  }
  
  
  def.par <- par(no.readonly = TRUE)
  windows()
  
  
  par(pch='.')
  
  pairs( list.cbind(vect.c),labels=vect.labels[1:length(vect.coefs)], 
         lower.panel=panel.smooth,
         diag.panel=panel.dens, 
         upper.panel=panel.cor,
         cex.labels = 2.0, font.labels=1.2)
  
  par(def.par) 
  
  
  
  
}
