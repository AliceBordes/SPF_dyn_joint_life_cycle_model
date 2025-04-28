
time.series.one<-function(vect,mcmc.output,fin.year,logoupas,poisson,y.min,y.max,decalage1,decalage2,decalage3,decalage4,decalage5,decalage6,decalage7,decalage8)
  
{ 
  #graphic options
  # ---------------------------------  
  
  col <- c("black","lightgrey")
  pch <- c(NA,NA)
  lty <- c(1,NA)
  lwd <- c(2,NA)
  fill <- c(NA,col[2])
  border <- c(NA,NA)
  pt.cex <- c(1,1) 
  
  size.text <- 1
  size.labels <- 1
  cex.abc <- 1
  box.size <- 0.5
  size.text.axis <- 1.4
  size.text.title <- 1.8
  size.text.legend <- 0.9
  x.label = ""
  cex.a <-1
  k<-1.5  
  resol <- 6
  
  
  #set main, axis and figure titles
  
  if(str_contains(vect[1],"N")==TRUE){
    title=paste("Abundance dynamics in",poisson,"populations over time")
    y.label=paste("Number of fish")
    nam<-"abundance_dynamics"
  }
  if(str_contains(vect[1],"L")==TRUE){
    title=paste("Size dynamics in",poisson,"populations over time")
    y.label=paste("Total length (cm)")
    nam<-"size_dynamics"
  }
  if(str_contains(vect[1],"M")==TRUE){
    title=paste("Natural mortality dynamics in",poisson,"populations over time")
    y.label=paste("Natural mortality rate")
    nam<-"nat.mortality_dynamics"
  }
  if(str_contains(vect[1],"F")==TRUE){
    title=paste("Fishing mortality dynamics in",poisson,"populations over time")
    y.label=paste("Fishing mortality rate")
    nam<-"fish.mortality_dynamics"
  }
  if(str_contains(vect[1],"g")==TRUE){
    title=paste("Pseudo-growth dynamics in",poisson,"populations over time")
    y.label=paste("Pseudo-growth rate")
    nam<-"growth_dynamics"
  }
  if(str_contains(vect[1],"Z")==TRUE){
    if(str_contains(vect[1],"Z.SSN")==TRUE){
      title=paste("Stock recruitment dynamics in",poisson,"populations over time")
      y.label=paste("Stock recruitment process ")
      nam<-"Z.SSN_dynamics"
    }else{
      title=paste("Total mortality dynamics in",poisson,"populations over time")
      y.label=paste("Total mortality rate")
      nam<-"Z_dynamics"
    }
  }
  
  if((length(vect)<3) & (str_contains(vect[1],"N.SSN")==TRUE)){
    title=paste("Spawning stock abundance dynamics in",poisson,"populations over time")
    y.label=paste("Number of fish")
    nam<-"N.SSN_dynamics"
  }
  
  if(logoupas=="yes"){
    # median
    y.label=paste(y.label,"(log)")
    nam<-paste(nam,"(log)")
  }
  
  # ---------------------------------    
  
  Mat <- as.matrix(mcmc.output)  
  
  x<-c()
  i<-c()
  var.mcmc<-list()
  var.c<-list()
  var.975<-list()
  var.025<-list()
  
  for (v in 1:length(vect))
  {
    
    x[v]=vect[v]   #variable name
    i[v]=substr(vect[v],2,2)  #collect the age of age class of de variable
    
    #create the matrix related to the variable name
    var.mcmc[[v]] = Mat[,which(substr(colnames(Mat),1,nchar(x[[v]])+1)==paste(x[[v]],"[",sep=""))]
    
    if(poisson=="anchovy")
    {
      var.mcmc[[v]] = var.mcmc[[v]][,which(endsWith(colnames(var.mcmc[[v]]),",1]"))]
    }
    
    if(poisson=="sardine")
    {
      var.mcmc[[v]] = var.mcmc[[v]][,which(endsWith(colnames(var.mcmc[[v]]),",2]"))]
    }
    
    if((poisson=="both") & (vect[[1]]=="Z.SSN"))
    {
      var.mcmc[[1]] = var.mcmc[[v]][,which(endsWith(colnames(var.mcmc[[v]]),",1]"))]
      var.mcmc[[2]] = var.mcmc[[v]][,which(endsWith(colnames(var.mcmc[[v]]),",2]"))]
    }
    
    
    
    if(logoupas=="yes"){
      
      # median of the mcmc draws
      var.c[[v]] = apply(log(var.mcmc[[v]]),2,median)
      
      # quantile
      #0.975
      var.975[[v]] <- apply(log(var.mcmc[[v]]),2,quantile, prob=0.975)
      
      #0.025
      var.025[[v]] <- apply(log(var.mcmc[[v]]),2,quantile, prob=0.025)
      
    }else{
      
      # median of the mcmc draws
      var.c[[v]] = apply((var.mcmc[[v]]),2,median)
      
      # quantile
      #0.975
      var.975[[v]] <- apply((var.mcmc[[v]]),2,quantile, prob=0.975)
      
      #0.025
      var.025[[v]] <- apply((var.mcmc[[v]]),2,quantile, prob=0.025)
      
      
    }
    #print key values
    print(paste("max de",vect[v],poisson,"=",round(max(var.c[[v]]),2)))
    print(paste("min de",vect[v],poisson,"=",round(min(var.c[[v]]),2)))
    print(paste("moyenne de",vect[v],poisson,"=",round(mean(var.c[[v]][1:(fin.year-1999)]),2)))
    
    
    if(poisson=="both"){ 
      print(var.c[[1]])
      print(var.c[[2]])
    }
    
    if((str_contains(vect[1],"L")==TRUE)){
      print(paste(vect[v],poisson,"différence entre la moyenne des 5 premières et les 5 dernières années =",round(mean(var.c[[v]][1:5]),1)-signif(round(mean(var.c[[v]][19:23]),1))))
    }
    
    if((str_contains(vect[1],"N")==TRUE)){
      print(paste(vect[v],poisson,"différence entre la moyenne de la période 2011-23 et 2000-10 =",signif(mean(var.c[[v]][12:23]),2)-signif(mean(var.c[[v]][1:11]),2)))
      print(paste("by a factor",round(signif(mean(var.c[[v]][12:23]),2)/signif(mean(var.c[[v]][1:11]),2),2)))
    }
    
    
  } 
  #-------------------------------------------------
  #GRAPH
  
  #graph visual options 
  y.min = min(unlist(var.025)) #borne min de l'intervalle de confiance bayésien à 95%
  y.max = max(unlist(var.975)) #borne max de l'intervalle de confiance bayésien à 95%
  max.mean=max(unlist(var.c))
  min.mean=min(unlist(var.c))
  # when rectangle for legend --> xmax=fin.year+4
  
  
  if(y.max > (max.mean+1))
  {
    y.max=max.mean+1
  }
  if(y.min < (min.mean-1))
  {
    y.min=min.mean-1
  }
  
  if(str_contains(x[v],"N")||str_contains(x[v],"L"))
  {
    #graph visual options 
    y.max = y.max+2
  }
  
  decal1<-decalage1*((y.max-y.min)/14)
  decal2<-decalage2*((y.max-y.min)/14)
  decal3<-decalage3*((y.max-y.min)/14)
  decal4<-decalage4*((y.max-y.min)/14)
  decal5<-decalage5*((y.max-y.min)/14)
  decal6<-decalage6*((y.max-y.min)/14)
  decal7<-decalage7*((y.max-y.min)/14)
  decal8<-decalage8*((y.max-y.min)/14)
  
  #------------- 
  
  name_figure <- paste(nam,"_",poisson,".png",sep="")
  years <- seq(2000,fin.year)
  years.inv <- fin.year:2000
  
  png(filename = name_figure, height = 380*resol, width = 520*resol, res=72*resol)
  def.par <- par(no.readonly = TRUE)
  
  print(var.c)
  
  if(str_contains(x[v],"Z.SSN") & (length(vect)==1))
  {
    plot(x=years,y=var.c[[1]],type="l", xlab="",ylab="", ylim=c(y.min,y.max),xlim=c(2000,fin.year))
    polygon(x=c(years,years.inv),y=c(var.975[[1]] ,rev(var.025[[1]])),col=alpha("deeppink",0.2),border=NA,ylim=c(y.min,y.max))
    points(x=years,y=var.c[[1]],type="l",col="deeppink",lwd=1, lty=1,ylim=c(y.min,y.max))
    # rect(xleft = (fin.year+1)-0.9,xright = (fin.year+1)+3.3,ybottom = var.c[[1]][fin.year-1999]-((y.max-y.min)/14)+(decal1),ytop = var.c[[1]][fin.year-1999]+((y.max-y.min)/14)+(decal1),col=alpha("deeppink",0.1),border = NA)
    # text(x=(fin.year+1)+1.2,y=var.c[[1]][fin.year-1999]+(decal1),"Spawning stock",col="deeppink")
    
    # legend(legend=c("Spawning stock"),col = c("deeppink"),x="top", lwd=2,xpd = TRUE, bty="n",text.width = NA)
    
  }
  
  if(length(vect)!=1)
  {
    
    if((poisson=="both") & (length(vect)<3))
    {
      plot(x=years,y=var.c[[1]],type="l",xlab="",ylab="",ylim=c(y.min,y.max),xlim=c(2000,fin.year))
      polygon(x=c(years,years.inv),y=c(var.975[[1]] ,rev(var.025[[1]])),col=alpha("deeppink",0.2),border=NA,ylim=c(y.min,y.max))
      points(x=years,y=var.c[[1]],type="l",col="deeppink",lwd=1, lty=1,ylim=c(y.min,y.max))
      # rect(xleft = (fin.year+1)-0.9,xright = (fin.year+1)+3.3,ybottom = var.c[[1]][fin.year-1999]-((y.max-y.min)/14)+(decal1),ytop = var.c[[1]][fin.year-1999]+((y.max-y.min)/14)+(decal1),col=alpha("deeppink",0.1),border = NA)
      # text(x=(fin.year+1)+1.2,y=var.c[[1]][fin.year-1999]+(decal1),"Anchovy spawning stock",col="deeppink")
      
      polygon(x=c(years,years.inv),y=c(var.975[[2]] ,rev(var.025[[2]])),col=alpha("deeppink",0.2),border=NA,ylim=c(y.min,y.max))
      points(x=years,y=var.c[[2]],type="l",col="deeppink",lwd=1, lty=1,ylim=c(y.min,y.max))
      # rect(xleft = (fin.year+1)-0.9,xright = (fin.year+1)+3.3,ybottom = var.c[[2]][fin.year-1999]-((y.max-y.min)/14)+(decal2),ytop = var.c[[2]][fin.year-1999]+((y.max-y.min)/14)+(decal2),col=alpha("deeppink",0.1),border = NA)
      # text(x=(fin.year+1)+1.2,y=var.c[[2]][fin.year-1999]+(decal2),"Sardine spawning stock",col="deeppink")
      
    }else{
      
      plot(x=years,y=var.c[[1]],type="l",xlab="",ylab="", ylim=c(y.min,y.max),xlim=c(2000,fin.year))
      polygon(x=c(years,years.inv),y=c(var.975[[1]] ,rev(var.025[[1]])),col=alpha("black",0.2),border=NA,ylim=c(y.min,y.max))
      points(x=years,y=var.c[[1]],type="l",col="black",lwd=1, lty=1,ylim=c(y.min,y.max))
      # rect(xleft = (fin.year+1)-0.7,xright = (fin.year+1)+1.2,ybottom = var.c[[1]][fin.year-1999]-((y.max-y.min)/14)+(decal1),ytop = var.c[[1]][fin.year-1999]+((y.max-y.min)/14)+(decal1),col=alpha("black",0.1),border = NA)
      # text(x=(fin.year+1)+0.2,y=var.c[[1]][fin.year-1999]+(decal1),"Age 1",col="black")
      
      
      polygon(x=c(years,years.inv),y=c(var.975[[2]] ,rev(var.025[[2]])),col=alpha("red",0.2),border=NA,ylim=c(y.min,y.max))
      points(x=years,y=var.c[[2]],type="l",col="red",lwd=1, lty=1,ylim=c(y.min,y.max))
      # rect(xleft = (fin.year+1)-0.7,xright = (fin.year+1)+1.2,ybottom = var.c[[2]][fin.year-1999]-((y.max-y.min)/14)+(decal2),ytop = var.c[[2]][fin.year-1999]+((y.max-y.min)/14)+(decal2),col=alpha("red",0.1),border = NA)
      # text(x=(fin.year+1)+0.2,y=var.c[[2]][fin.year-1999]+(decal2),"Age 2",col="red")
      
      polygon(x=c(years,years.inv),y=c(var.975[[3]] ,rev(var.025[[3]])),col=alpha("blue",0.2),border=NA,ylim=c(y.min,y.max))
      points(x=years,y=var.c[[3]],type="l",col="blue",lwd=1, lty=1,ylim=c(y.min,y.max))
      # rect(xleft = (fin.year+1)-0.7,xright = (fin.year+1)+1.2,ybottom = var.c[[3]][fin.year-1999]-((y.max-y.min)/14)+(decal3),ytop = var.c[[3]][fin.year-1999]+((y.max-y.min)/14)+(decal3),col=alpha("blue",0.1),border = NA)
      # text(x=(fin.year+1)+0.2,y=var.c[[3]][fin.year-1999]+(decal3),"Age 3",col="blue")
      
      
      if(str_contains(x[v],"N")||str_contains(x[v],"L"))
      {

        polygon(x=c(years,years.inv),y=c(var.975[[4]] ,rev(var.025[[4]])),col=alpha("green",0.2),border=NA,ylim=c(y.min,y.max))
        points(x=years,y=var.c[[4]],type="l",col="#009933",lwd=1, lty=1,ylim=c(y.min,y.max))
        # rect(xleft = (fin.year+1)-0.7,xright = (fin.year+1)+1.2,ybottom = var.c[[4]][fin.year-1999]-((y.max-y.min)/14)+(decal4),ytop = var.c[[4]][fin.year-1999]+((y.max-y.min)/14)+(decal4),col=alpha("green",0.1),border = NA)
        # text(x=(fin.year+1)+0.2,y=var.c[[4]][fin.year-1999]+(decal4),"Age 4",col="#009933")
        
        if(poisson=="anchovy")
        {
          polygon(x=c(years,years.inv),y=c(var.975[[5]] ,rev(var.025[[5]])),col=alpha("deeppink",0.2),border=NA,ylim=c(y.min,y.max))
          points(x=years,y=var.c[[5]],type="l",col="deeppink",lwd=1, lty=1,ylim=c(y.min,y.max))
          # rect(xleft = (fin.year+1)-0.7,xright = (fin.year+1)+3.5,ybottom = var.c[[5]][fin.year-1999]-((y.max-y.min)/14)+(decal5),ytop = var.c[[5]][fin.year-1999]+((y.max-y.min)/14)+(decal5),col=alpha("deeppink",0.1),border = NA)
          # text(x=(fin.year+1)+1.4,y=var.c[[5]][fin.year-1999]+(decal5),"Spawning stock",col="deeppink")
          
          legend(legend=c("Age 1","Age 2","Age 3","Age 4+","Spawning stock"),col = c("black","red","blue","#009933","deeppink"),x="topright", lwd=c(2,2,2,2,2),xpd = TRUE,ncol=4, bty="n",text.width = NA)
          
        }
        
        if(poisson=="sardine")
        {
          polygon(x=c(years,years.inv),y=c(var.975[[5]] ,rev(var.025[[5]])),col=alpha("purple",0.2),border=NA,ylim=c(y.min,y.max))
          points(x=years,y=var.c[[5]],type="l",col="purple",lwd=1, lty=1,ylim=c(y.min,y.max))
          # rect(xleft = (fin.year+1)-0.7,xright = (fin.year+1)+1.2,ybottom = var.c[[5]][fin.year-1999]-((y.max-y.min)/14)+(decal5),ytop = var.c[[5]][fin.year-1999]+((y.max-y.min)/14)+(decal5),col=alpha("purple",0.1),border = NA)
          # text(x=(fin.year+1)+0.2,y=var.c[[5]][fin.year-1999]+(decal5),"Age 5",col="purple")
          
          polygon(x=c(years,years.inv),y=c(var.975[[6]] ,rev(var.025[[6]])),col=alpha("orange",0.2),border=NA,ylim=c(y.min,y.max))
          points(x=years,y=var.c[[6]],type="l",col="orange",lwd=1, lty=1,ylim=c(y.min,y.max))
          # rect(xleft = (fin.year+1)-0.7,xright = (fin.year+1)+1.2,ybottom = var.c[[6]][fin.year-1999]-((y.max-y.min)/14)+(decal6),ytop = var.c[[6]][fin.year-1999]+((y.max-y.min)/14)+(decal6),col=alpha("orange",0.1),border = NA)
          # text(x=(fin.year+1)+0.2,y=var.c[[6]][fin.year-1999]+(decal6),"Age 6",col="orange")
          
          polygon(x=c(years,years.inv),y=c(var.975[[7]] ,rev(var.025[[7]])),col=alpha("deepskyblue",0.2),border=NA,ylim=c(y.min,y.max))
          points(x=years,y=var.c[[7]],type="l",col="deepskyblue",lwd=1, lty=1,ylim=c(y.min,y.max))
          # rect(xleft = (fin.year+1)-0.7,xright = (fin.year+1)+1.2,ybottom = var.c[[7]][fin.year-1999]-((y.max-y.min)/14)+(decal7),ytop = var.c[[7]][fin.year-1999]+((y.max-y.min)/14)+(decal7),col=alpha("deepskyblue",0.1),border = NA)
          # text(x=(fin.year+1)+0.2,y=var.c[[7]][fin.year-1999]+(decal7),"  Age 7+",col="deepskyblue")
          
          polygon(x=c(years,years.inv),y=c(var.975[[8]] ,rev(var.025[[8]])),col=alpha("deeppink",0.2),border=NA,ylim=c(y.min,y.max))
          points(x=years,y=var.c[[8]],type="l",col="deeppink",lwd=1, lty=1,ylim=c(y.min,y.max))
          # rect(xleft = (fin.year+1)-0.7,xright = (fin.year+1)+3.5,ybottom = var.c[[8]][fin.year-1999]-((y.max-y.min)/14)+(decal8),ytop = var.c[[8]][fin.year-1999]+((y.max-y.min)/14)+(decal8),col=alpha("deeppink",0.1),border = NA)
          # text(x=(fin.year+1)+1.4,y=var.c[[8]][fin.year-1999]+(decal8),"Spawning stock",col="deeppink")
          
          
          legend(legend=c("Age 1","Age 2","Age 3","Age 4","Age 5","Age 6","Age 7+","Spawning stock"),ncol=5,col = c("black","red","blue","#009933","purple","orange","deepskyblue","deeppink"),x="topright", lwd=2,xpd = TRUE, bty="n",text.width = NA)
          
        }
      }#fin if 
      
    }#fin else
    
    
    
    
    if(str_contains(x[v],"M")||str_contains(x[v],"g")||str_contains(x[v],"F")||(str_contains(x[v],"Z") & !str_contains(x[v],"SSN")))
    {
      
      if(poisson=="anchovy")
      {
        legend(legend=c("Age 1","Age 2","Age 3"),col = c("black","red","blue"),x="topright", lwd=c(2,2,2),xpd = TRUE,ncol=4, bty="n",text.width = NA)
      }
      
      if(poisson=="sardine")
      {
        polygon(x=c(years,years.inv),y=c(var.975[[4]] ,rev(var.025[[4]])),col=alpha("green",0.2),border=NA,ylim=c(y.min,y.max))
        points(x=years,y=var.c[[4]],type="l",col="#009933",lwd=1, lty=1,ylim=c(y.min,y.max))
        # rect(xleft = (fin.year+1)-0.7,xright = (fin.year+1)+1.2,ybottom = var.c[[4]][fin.year-1999]-((y.max-y.min)/14)+(decal4),ytop = var.c[[4]][fin.year-1999]+((y.max-y.min)/14)+(decal4),col=alpha("green",0.1),border = NA)
        # text(x=(fin.year+1)+0.2,y=var.c[[4]][fin.year-1999]+(decal4),"Age 4",col="#009933")
        
        polygon(x=c(years,years.inv),y=c(var.975[[5]] ,rev(var.025[[5]])),col=alpha("purple",0.2),border=NA,ylim=c(y.min,y.max))
        points(x=years,y=var.c[[5]],type="l",col="purple",lwd=1, lty=1,ylim=c(y.min,y.max))
        # rect(xleft = (fin.year+1)-0.7,xright = (fin.year+1)+1.2,ybottom = var.c[[5]][fin.year-1999]-((y.max-y.min)/14)+(decal5),ytop = var.c[[5]][fin.year-1999]+((y.max-y.min)/14)+(decal5),col=alpha("purple",0.1),border = NA)
        # text(x=(fin.year+1)+0.2,y=var.c[[5]][fin.year-1999]+(decal5),"Age 5",col="purple")
        
        polygon(x=c(years,years.inv),y=c(var.975[[6]] ,rev(var.025[[6]])),col=alpha("orange",0.2),border=NA,ylim=c(y.min,y.max))
        points(x=years,y=var.c[[6]],type="l",col="orange",lwd=1, lty=1,ylim=c(y.min,y.max))
        # rect(xleft = (fin.year+1)-0.7,xright = (fin.year+1)+1.2,ybottom = var.c[[6]][fin.year-1999]-((y.max-y.min)/14)+(decal6),ytop = var.c[[6]][fin.year-1999]+((y.max-y.min)/14)+(decal6),col=alpha("orange",0.1),border = NA)
        # text(x=(fin.year+1)+0.2,y=var.c[[6]][fin.year-1999]+(decal5),"Age 6",col="orange")
        
        legend(legend=c("Age 1","Age 2","Age 3","Age 4","Age 5","Age 6"),ncol=5,col = c("black","red","blue","#009933","purple","orange"),x="topright", lwd=2,xpd = TRUE, bty="n",text.width = NA)
        
      }
    }
    
  }
  
  
  # legend
  
  axis(side = 2, cex.axis=size.labels)
  mtext(y.label, side=2,line=2.2, cex=size.text.axis)
  mtext("Years", side=1,line=2.2, cex=size.text.axis)
  # title(title, cex.main = size.text.title)
  
  if(poisson=="anchovy")
  {
    legend(legend=c("ANCHOVY "),x="topleft")
    
  }
  if(poisson=="sardine")
  {
    legend(legend=c("SARDINE "),x="topleft")
    
  }
  
  
  
  par(def.par)
  dev.off()
  
  
  #-----------------------------------------------------------------------------------------
  return()
  
} 