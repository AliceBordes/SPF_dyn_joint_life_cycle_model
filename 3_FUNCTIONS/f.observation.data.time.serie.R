
obs.time.series<-function(quoi,incertitude, data,fin.year,logoupas,poisson)

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
  size.text.axis <- 1.3
  size.text.title <- 1.5
  size.text.legend <- 0.9
  x.label = ""
  cex.a <-1
  k<-1.5  
  resol <- 6


#set main, axis and figure titles
  
  if(str_contains(quoi[1],"N")==TRUE){
    title=paste("Observed abundances for",poisson,"populations over time")
    y.label=paste("Number of fish")
    nam<-"abundance_obs"
  }
  
  if(str_contains(quoi[1],"L")==TRUE){
    title=paste("Observed sizes for",poisson,"populations over time")
    y.label=paste("Total length (cm)")
    nam<-"size_obs"
  }
  
  if(str_contains(quoi[1],"C")==TRUE){
    title=paste("Observed cathes for",poisson,"populations over time")
    y.label=paste("Number of fish")
    nam<-"catches_obs"
  }
  
  if(str_contains(quoi[1],"Zoo")==TRUE){
    title=paste("Average observed concentrations of zooplankton")
    y.label=paste("Standardized value")
    nam<-"zoo_obs"
  }
  
  if(str_contains(quoi[1],"Temp")==TRUE){
    title=paste("Average observed temperatures")
    y.label=paste("Standardized value")
    nam<-"temp_obs"
  }
  
  if(str_contains(quoi[1],"AMO")==TRUE){
    title=paste("Average observed AMO indexes")
    y.label=paste("Standardized value")
    nam<-"AMO_obs"
  }
  
  if(str_contains(quoi[1],"NAO")==TRUE){
    title=paste("Average observed NAO indexes")
    y.label=paste("Standardized value")
    nam<-"NAO_obs"
  }
  
  if(str_contains(quoi[1],"wabun")==TRUE){
    title=paste("Observed abundances for sprat populations over time")
    y.label=paste("Number of fish")
    nam<-"abundance.sprat_obs"
  }
  
  if(str_contains(quoi[1],"age")==TRUE){
    title=paste("Observed maturities for fish populations over time")
    y.label=paste("Maturity rate")
    nam<-"maturity_obs"
  }
  
x<-c()
incertainty<-c()
incertainty_minus<-c()
incertainty_plus<-c()

for (v in 1:length(quoi))
{
  x[v] <- data[which(!startsWith(colnames(data),"sigma") & endsWith(colnames(data),quoi[v]))]
  incertainty[v] <- data[which(endsWith(colnames(data),incertitude[v]))]
# 
#   incertainty_minus[v] <- x[v]-x[v]*incertainty[v]
#   incertainty_plus[v] <-x[v]+x[v]*incertainty[v]
  
}


  
  if(logoupas=="yes"){
    # median
    y.label=paste(y.label,"(log)")
    nam<-paste(nam,"(log)")
    for (v in 1:length(quoi))
    {  
      x[[v]] = log(x[[v]])
      incertainty[[v]] = log(incertainty[[v]])
      # incertainty_minus[[v]] = log(incertainty_minus[[v]])
      # incertainty_plus[[v]] = log(incertainty_plus[[v]])
    } 
  }



#-------------------------------------------------
#GRAPH

y.min=min(unlist(x),na.rm=T)-1
if(y.min!=0)
{
  y.min=y.min-1
}

y.max=max(unlist(x),na.rm=T)+2

if(str_contains(quoi[1],"age")==TRUE){
  y.min=0
  y.max=1
}

name_figure <- paste(nam,"_",poisson,".png",sep="")
  years <- seq(2000,fin.year)
  years.inv <- fin.year:2000
    

png(filename = name_figure, height = 380*resol, width = 520*resol, res=72*resol)
def.par <- par(no.readonly = TRUE)

 
if((str_contains(quoi[1],"N")&&(nchar(quoi[1])<8))||str_contains(quoi[1],"L")||str_contains(quoi[1],"age")||str_contains(quoi[1],"C"))
{

  plot(x=years,y=x[[1]],type="l",xlab="",ylab="", ylim=c(y.min,y.max),xlim=c(2000,fin.year))
points(x=years,y=x[[1]],type="l",col="black",lwd=1, lty=1,ylim=c(y.min,y.max))
points(x=years,y=x[[2]],type="l",col="red",lwd=1, lty=1,ylim=c(y.min,y.max))
points(x=years,y=x[[3]],type="l",col="blue",lwd=1, lty=1,ylim=c(y.min,y.max))
points(x=years,y=x[[4]],type="l",col="#009933",lwd=1, lty=1,ylim=c(y.min,y.max))

if((str_contains(quoi[1],"N")&&(nchar(quoi[1])<8))||str_contains(quoi[1],"L"))
{
    # polygon(x=c(years.inv,years),y=c(rev(incertainty_minus[[1]]),incertainty_plus[[1]]),col=alpha("black",0.2),border=NA,ylim=c(y.min,y.max))
    # polygon(x=c(years.inv,years),y=c(rev(incertainty_minus[[2]]),incertainty_plus[[2]]),col=alpha("red",0.2),border=NA,ylim=c(y.min,y.max))
    # polygon(x=c(years.inv,years),y=c(rev(incertainty_minus[[3]]),incertainty_plus[[3]]),col=alpha("blue",0.2),border=NA,ylim=c(y.min,y.max))
    # polygon(x=c(years.inv,years),y=c(rev(incertainty_minus[[4]]),incertainty_plus[[4]]),col=alpha("green",0.2),border=NA,ylim=c(y.min,y.max))
    # 
    # polygon(x=c(years.inv[1:2],years[22:23]),y=c(rev(incertainty_minus[[1]][1:2]),incertainty_plus[[1]][22:23]),col=alpha("black",0.2),border=NA,ylim=c(y.min,y.max))
    # polygon(x=c(years.inv[1:2],years[22:23]),y=c(rev(incertainty_minus[[2]][1:2]),incertainty_plus[[2]][22:23]),col=alpha("red",0.2),border=NA,ylim=c(y.min,y.max))
    # polygon(x=c(years.inv[1:2],years[22:23]),y=c(rev(incertainty_minus[[3]][1:2]),incertainty_plus[[3]][22:23]),col=alpha("blue",0.2),border=NA,ylim=c(y.min,y.max))
    # polygon(x=c(years.inv[1:2],years[22:23]),y=c(rev(incertainty_minus[[4]][1:2]),incertainty_plus[[4]][22:23]),col=alpha("green",0.2),border=NA,ylim=c(y.min,y.max))
    # 
    polygon(x=c(years.inv,years),y=c(rev(x[[1]]-(x[[1]]*incertainty[[1]])),x[[1]]+(x[[1]]*incertainty[[1]])),col=alpha("black",0.2),border=NA,ylim=c(y.min,y.max))
    polygon(x=c(years.inv,years),y=c(rev(x[[2]]-(x[[2]]*incertainty[[2]])),x[[2]]+(x[[2]]*incertainty[[2]])),col=alpha("red",0.2),border=NA,ylim=c(y.min,y.max))
    polygon(x=c(years.inv,years),y=c(rev(x[[3]]-(x[[3]]*incertainty[[3]])),x[[3]]+(x[[3]]*incertainty[[3]])),col=alpha("blue",0.2),border=NA,ylim=c(y.min,y.max))
    polygon(x=c(years.inv,years),y=c(rev(x[[4]]-(x[[4]]*incertainty[[4]])),x[[4]]+(x[[4]]*incertainty[[4]])),col=alpha("green",0.2),border=NA,ylim=c(y.min,y.max))

    polygon(x=c(years.inv[1:2],years[22:23]),y=c(rev(x[[1]]-x[[1]]*incertainty[[1]])[1:2],(x[[1]]+x[[1]]*incertainty[[1]])[22:23]),col=alpha("black",0.2),border=NA,ylim=c(y.min,y.max))
    polygon(x=c(years.inv[1:2],years[22:23]),y=c(rev(x[[2]]-x[[2]]*incertainty[[2]])[1:2],(x[[2]]+x[[2]]*incertainty[[2]])[22:23]),col=alpha("red",0.2),border=NA,ylim=c(y.min,y.max))
    polygon(x=c(years.inv[1:2],years[22:23]),y=c(rev(x[[3]]-x[[3]]*incertainty[[3]])[1:2],(x[[3]]+x[[3]]*incertainty[[3]])[22:23]),col=alpha("blue",0.2),border=NA,ylim=c(y.min,y.max))
    polygon(x=c(years.inv[1:2],years[22:23]),y=c(rev(x[[4]]-x[[4]]*incertainty[[4]])[1:2],(x[[4]]+x[[4]]*incertainty[[4]])[22:23]),col=alpha("green",0.2),border=NA,ylim=c(y.min,y.max))

  
  if(poisson=="sardine")
  {
    polygon(x=c(years.inv,years),y=c(rev(x[[5]]-(x[[5]]*incertainty[[5]])),x[[5]]+(x[[5]]*incertainty[[5]])),col=alpha("purple",0.2),border=NA,ylim=c(y.min,y.max))
    polygon(x=c(years.inv,years),y=c(rev(x[[6]]-(x[[6]]*incertainty[[6]])),x[[6]]+(x[[6]]*incertainty[[6]])),col=alpha("orange",0.2),border=NA,ylim=c(y.min,y.max))
    polygon(x=c(years.inv,years),y=c(rev(x[[7]]-(x[[7]]*incertainty[[7]])),x[[7]]+(x[[7]]*incertainty[[7]])),col=alpha("deepskyblue",0.2),border=NA,ylim=c(y.min,y.max))

    polygon(x=c(years.inv[1:2],years[22:23]),y=c(rev(x[[5]]-x[[5]]*incertainty[[5]])[1:2],(x[[5]]+x[[5]]*incertainty[[5]])[22:23]),col=alpha("purple",0.2),border=NA,ylim=c(y.min,y.max))
    polygon(x=c(years.inv[1:2],years[22:23]),y=c(rev(x[[6]]-x[[6]]*incertainty[[6]])[1:2],(x[[6]]+x[[6]]*incertainty[[6]])[22:23]),col=alpha("orange",0.2),border=NA,ylim=c(y.min,y.max))
    polygon(x=c(years.inv[1:2],years[22:23]),y=c(rev(x[[7]]-x[[7]]*incertainty[[7]])[1:2],(x[[7]]+x[[7]]*incertainty[[7]])[22:23]),col=alpha("deepskyblue",0.2),border=NA,ylim=c(y.min,y.max))
      
  }
  
}


if(poisson=="anchovy")
{
  legend(legend=c("ANCHOVY "),x="topleft")
  
  if(str_contains(quoi[1],"age"))
  {
    legend(legend=c("Age 1","Age 2","Age 3","Age 4+"),col = c("black","red","blue","#009933"),x="bottomright", lwd=c(2,2,2,2),xpd = TRUE,ncol=4, bty="n",text.width = NA)
  }else
  {
    legend(legend=c("Age 1","Age 2","Age 3","Age 4+"),col = c("black","red","blue","#009933"),x="topright", lwd=c(2,2,2,2),xpd = TRUE,ncol=4, bty="n",text.width = NA)
  }
  
}


if(poisson=="sardine")
{
  if(str_contains(quoi[1],"C"))
  {
    points(x=years,y=x[[5]],type="l",col="purple",lwd=1, lty=1,ylim=c(y.min,y.max))
    points(x=years,y=x[[6]],type="l",col="orange",lwd=1, lty=1,ylim=c(y.min,y.max))
    legend(legend=c("Age 1","Age 2","Age 3","Age 4","Age 5","Age 6"),ncol=4,col = c("black","red","blue","#009933","purple","orange"),x="topright", lwd=2,xpd = TRUE, bty="n",text.width = NA)
    legend(legend=c("SARDINE "),x="topleft")  

  }else
  {
  points(x=years,y=x[[5]],type="l",col="purple",lwd=1, lty=1,ylim=c(y.min,y.max))
  points(x=years,y=x[[6]],type="l",col="orange",lwd=1, lty=1,ylim=c(y.min,y.max))
  points(x=years,y=x[[7]],type="l",col="deepskyblue",lwd=1, lty=1,ylim=c(y.min,y.max))
  
  legend(legend=c("SARDINE "),x="topleft")
  
  if(str_contains(quoi[1],"age"))
  {
    legend(legend=c("Age 1","Age 2","Age 3","Age 4","Age 5","Age 6","Age 7+"),ncol=4,col = c("black","red","blue","#009933","purple","orange","deepskyblue"),x="bottomright", lwd=2,xpd = TRUE, bty="n",text.width = NA)
    
  }else
  {
    legend(legend=c("Age 1","Age 2","Age 3","Age 4","Age 5","Age 6","Age 7+"),ncol=4,col = c("black","red","blue","#009933","purple","orange","deepskyblue"),x="topright", lwd=2,xpd = TRUE, bty="n",text.width = NA)
  }
  
  }

}

}



if(str_contains(quoi[1],"Zoo"))
{

  plot(x=years,y=x[[1]],type="l",xlab="",ylab="", ylim=c(y.min,y.max),xlim=c(2000,fin.year))
  points(x=years,y=x[[1]],type="l",col="deeppink",lwd=1, lty=1,ylim=c(y.min,y.max))
  points(x=years,y=x[[2]],type="l",col="darkgreen",lwd=1, lty=1,ylim=c(y.min,y.max))
  points(x=years,y=x[[3]],type="l",col="lightblue",lwd=1, lty=1,ylim=c(y.min,y.max))
  
  legend(legend=c("","","",quoi[1],quoi[2],quoi[3]),col = c(NA,NA,NA,"deeppink","darkgreen","lightblue"),x="topright", lwd=c(2,2,2),xpd = TRUE,ncol=3, bty="n",text.width = NA)
  legend(legend=c("ZOOPLANKTON "),x="topleft")
}

if(str_contains(quoi[1],"Temp"))
{
  
  plot(x=years,y=x[[1]],type="l",xlab="",ylab="", ylim=c(y.min,y.max),xlim=c(2000,fin.year))
  points(x=years,y=x[[1]],type="l",col="deeppink",lwd=1, lty=1,ylim=c(y.min,y.max))
  points(x=years,y=x[[2]],type="l",col="darkgreen",lwd=1, lty=1,ylim=c(y.min,y.max))
  points(x=years,y=x[[3]],type="l",col="lightblue",lwd=1, lty=1,ylim=c(y.min,y.max))
  
  legend(legend=c("","","",quoi[1],quoi[2],quoi[3]),col = c(NA,NA,NA,"deeppink","darkgreen","lightblue"),x="topright", lwd=c(2,2,2),xpd = TRUE,ncol=3, bty="n",text.width = NA)
  legend(legend=c("TEMPERATURE "),x="topleft")
}


if(str_contains(quoi[1],"AMO"))
{
  
  plot(x=years,y=x[[1]],type="l",xlab="",ylab="", ylim=c(y.min,y.max),xlim=c(2000,fin.year))
  points(x=years,y=x[[1]],type="l",col="blue2",lwd=1, lty=1,ylim=c(y.min,y.max))
  points(x=years,y=x[[2]],type="l",col="turquoise3",lwd=1, lty=1,ylim=c(y.min,y.max))
  
  legend(legend=c(quoi[1],quoi[2]),col = c("blue2","turquoise3"),x="topright", lwd=c(2,2),xpd = TRUE,ncol=2, bty="n",text.width = NA)
  legend(legend=c("LARGE SCALE INDICATORS "),x="topleft")
}

  

if(str_contains(quoi[1],"wabun"))
{
  
  plot(x=years,y=x[[1]],type="l",xlab="",ylab="", ylim=c(y.min,y.max),xlim=c(2000,fin.year))
  points(x=years,y=x[[1]],type="l",col="black",lwd=1, lty=1,ylim=c(y.min,y.max))
  
  legend(legend=c("Total abundance of sprats (all life-stages)"),col = c("black"),x="topright", lwd=c(2),xpd = TRUE,ncol=1, bty="n",text.width = NA)
  legend(legend=c("SPRAT "),x="topleft")
}

# legend
axis(side = 2, cex.axis=size.labels)
mtext(y.label, side=2,line=2.2, cex=size.text.axis)
mtext("Years", side=1,line=2.2, cex=size.text.axis)
#title(title, cex.main = size.text.title)



par(def.par)
dev.off()
   

#-----------------------------------------------------------------------------------------
return()

} 