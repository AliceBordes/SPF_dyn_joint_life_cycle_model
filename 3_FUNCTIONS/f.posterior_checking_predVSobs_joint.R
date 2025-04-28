
time.series<-function(vect,mcmc.output,fin.year,logoupas,data,poisson)
  
{

  
  for (v in 1:length(vect))
  {
    
  x=vect[v]
  
  i=substr(x,2,2)
  
# Graphic options
# ---------------------------------  

if(str_contains(x,"n")==TRUE){
title=paste("Age ",i," abundance "," (",poisson,")",sep="")    
}else{
  if(str_contains(x,"M")==TRUE){
   title=paste("Age ",i," natural mortality "," (",poisson,")",sep="")   
  }else{
    if(str_contains(x,"F")==TRUE){
      title=paste("Age ",i," fishing mortality "," (",poisson,")",sep="")
    }else{
      if(str_contains(x,"Z")==TRUE && str_contains(x,"SSN")==FALSE){
        title=paste("Age ",i," total mortality "," (",poisson,")",sep="")
      }else{
        if(str_contains(x,"C")==TRUE){
          title=paste("Age ",i," captures "," (",poisson,")",sep="")
        }else{
          if(str_contains(x,"l")==TRUE){
            title=paste("Age ",i," size "," (",poisson,")",sep="")
          }else{
            if(str_contains(x,"Z.SSN")==TRUE){
              title=paste("Mature ages total mortality "," (",poisson,")",sep="")
            }else{
              if(str_contains(x,"g")==TRUE){
                title=paste("Age ",i," growth "," (",poisson,")",sep="")
              }
            }
          }
        }
      }
    }
  }
 
}
  
if(str_contains(x,"n")==TRUE){
  y.label=paste("Number of fish")    
  }else{
    if(str_contains(x,"M")==TRUE){
      y.label=paste("Mortality rate")   
    }else{
      if(str_contains(x,"F")==TRUE){
        y.label=paste("Mortality rate")
      }else{
        if(str_contains(x,"Z")==TRUE){
          y.label=paste("Mortality rate")
        }else{
          if(str_contains(x,"C")==TRUE){
            y.label=paste("Number of fish")
          }else{
            if(str_contains(x,"l")==TRUE){
              y.label=paste("Total length (cm)")
            }else{
              if(str_contains(x,"g")==TRUE){
                y.label=paste("Growth rate")
              }
            }
          }
        }
      }
    }
    
  }

if(logoupas=="yes"){
  y.label=paste(y.label,"(log)")
}
  
col <- c("black","lightgrey","red")
pch <- c(NA,NA,20)
lty <- c(1,NA,NA)
lwd <- c(2,NA,NA)
fill <- c(NA,col[2],NA)
border <- c(NA,NA,NA)
pt.cex <- c(1,1,1.5) 

size.text <- 1
size.labels <- 1
cex.abc <- 1
box.size <- 0.5
size.text.axis <- 1
size.text.title <- 1.2
size.text.legend <- 0.9
x.label = ""
cex.a <-1
k<-1.5


## N1.log
#----------------
#  log.N1[t] ~ dnorm(log(N1[t]),log.N1.tau[t])

Mat <- as.matrix(mcmc.output)
var.mcmc = Mat[,which(substr(colnames(Mat),1,nchar(x)+1)==paste(x,"[",sep=""))]

if(poisson=="anchovy")
{
  dataSP<-data_anchovy
  var.mcmc = var.mcmc[,which(endsWith(colnames(var.mcmc),",1]"))]
}

if(poisson=="sardine")
{
  dataSP<-data_sardine
  var.mcmc = var.mcmc[,which(endsWith(colnames(var.mcmc),",2]"))]
}

if(logoupas=="yes"){
  # median
  var.c = apply(log(var.mcmc),2,median)
  
  # quantile
  #0.975
  var.0975 <- apply(log(var.mcmc),2,quantile, prob=0.975)
  
  #0.025
  var.025 <- apply(log(var.mcmc),2,quantile, prob=0.025)
  
}else{
  
  # median
  var.c = apply((var.mcmc),2,median)
  
  # quantile
  #0.975
  var.0975 <- apply((var.mcmc),2,quantile, prob=0.975)
  
  #0.025
  var.025 <- apply((var.mcmc),2,quantile, prob=0.025)

}



# Plot
X <- var.c

resol <- 6

if(logoupas=="yes"){
  name_figure <- paste("Figure_",x,"_log_",poisson,".png")
}else{
  name_figure <- paste("Figure_",x,"_",poisson,".png")
}

png(filename = name_figure, height = 320*resol, width = 680*resol, res=72*resol)

def.par <- par(no.readonly = TRUE)


years <- seq(2000,fin.year)
years.inv <- fin.year:2000

if(str_contains(x,"M")||str_contains(x,"F")||str_contains(x,"Z")){
  y.max <- max(5)
  y.min <- min(0) 
}

if(str_contains(x,"n")&&logoupas=="yes"){
  if(poisson=="anchovy"){
      y.max <- max(25)
      y.min <- min(13.5)     
  }
  if(poisson=="sardine"){
    y.max <- max(24)
    y.min <- min(15)     
  }

}

if(str_contains(x,"l")&&logoupas!="yes"){
  if(poisson=="anchovy"){
    y.max <- max(22.5)
    y.min <- min(8)     
  }
  if(poisson=="sardine"){
    y.max <- max(27)
    y.min <- min(10)     
  }
  
}

else{
  y.max <- max(X+1)
  y.min <- min(X-1) 
}



plot(x=years,y=X,type="l", ylab= y.label,xlab="Years",ylim=c(y.min,y.max))

polygon(x=c(years,years.inv),y=c(var.0975 ,rev(var.025)),col="lightgrey",border=NA,ylim=c(y.min,y.max))
points(x=years,y=X,type="l",col="black",lwd=3, lty=1,ylim=c(y.min,y.max))

if(data=="obs"){
  
  s1<-substr(x,1,1)
  s1<-str_sub(s1)

if(str_contains(x,"g")==FALSE){
    s1<-toupper(s1)
}

  s2<-substr(x,2,2)
  s2<-str_sub(s2)  
  

p<-paste(s1,s2,sep="")

if(str_contains(x,"n4")&&poisson=="anchovy"){
  p<-paste(s1,s2,"plus",sep="") 
  
}

if(str_contains(x,"n7")&&poisson=="sardine"){
  p<-paste(s1,s2,"plus",sep="") 
  
}

  print(p)
if(str_contains(x,"n")){  
  p3<-paste("CV_N",sep="") 
  errmoins = dataSP[[p]][1:(fin.year-1999)]-(dataSP[[p3]][1:(fin.year-1999)]*dataSP[[p]][1:(fin.year-1999)]) 
  errplus=dataSP[[p]][1:(fin.year-1999)]+(dataSP[[p3]][1:(fin.year-1999)]*dataSP[[p]][1:(fin.year-1999)]) 
  
  if(logoupas=="yes"){
    # add your data
    points(x=years,y=(log(dataSP[[p]])[1:(fin.year-1999)]),type="p",ylim=c(y.min,y.max),col="red", pch=20)
    # Vertical arrow
    #try(arrows(x0=years, y0=log(errmoins), x1=years, y1=log(errplus), angle=90,length=0.06, code=3, col="red", lwd=1))
    
  }else{
    # add your data
    points(x=years,y=((dataSP[[p]])[1:(fin.year-1999)]),type="p",ylim=c(y.min,y.max),col="red", pch=20)
    #try(arrows(x0=years, y0=(errmoins), x1=years, y1=(errplus),angle=90,length=0.06, code=3, col="red", lwd=1))
    
  } 
  
  }

if(str_contains(x,"l")){  p2<-paste("CV_",p,sep="") ; err = dataSP[[p2]][1:(fin.year-1999)]*dataSP[[p]][1:(fin.year-1999)]  

if(logoupas=="yes"){
  # add your data
  points(x=years,y=(log(dataSP[[p]])[1:(fin.year-1999)]),type="p",ylim=c(y.min,y.max),col="red", pch=20)
  # Vertical arrow
  #try(arrows(x0=years, y0=(log(dataSP[[p]][1:(fin.year-1999)]-err)), x1=years, y1=(log(dataSP[[p]][1:(fin.year-1999)]+err)), angle=90,length=0.06, code=3, col="red", lwd=1))
  
}else{
  # add your data
  points(x=years,y=((dataSP[[p]])[1:(fin.year-1999)]),type="p",ylim=c(y.min,y.max),col="red", pch=20)
  #try(arrows(x0=years, y0=((dataSP[[p]][1:(fin.year-1999)]-err)), x1=years, y1=((dataSP[[p]][1:(fin.year-1999)]+err)), angle=90,length=0.06, code=3, col="red", lwd=1))
  
  
} 

}
if(str_contains(x,"C")){  err = 0.05*dataSP[[p]][1:(fin.year-1999)]  
if(logoupas=="yes"){
  # add your data
  points(x=years,y=(log(dataSP[[p]])[1:(fin.year-1999)]),type="p",ylim=c(y.min,y.max),col="red", pch=20)
  # Vertical arrow
  #try(arrows(x0=years, y0=(log(dataSP[[p]][1:(fin.year-1999)]-err)), x1=years, y1=(log(dataSP[[p]][1:(fin.year-1999)]+err)), angle=90,length=0.06, code=3, col="red", lwd=1))
  
}else{
  # add your data
  points(x=years,y=((dataSP[[p]])[1:(fin.year-1999)]),type="p",ylim=c(y.min,y.max),col="red", pch=20)
  #try(arrows(x0=years, y0=((dataSP[[p]][1:(fin.year-1999)]-err)), x1=years, y1=((dataSP[[p]][1:(fin.year-1999)]+err)), angle=90,length=0.06, code=3, col="red", lwd=1))
  
  
} 
}
  


if(str_contains(x,"M")||str_contains(x,"g"))
{
 if(logoupas=="yes"){
  # add your data
    points(x=years,y=(log(dataSP[[p]])[1:(fin.year-1999)]),type="p",ylim=c(y.min,y.max),col="red", pch=20)

     
}else{
  # add your data
    points(x=years,y=((dataSP[[p]])[1:(fin.year-1999)]),type="p",ylim=c(y.min,y.max),col="red", pch=20)

  
 } 
}


  
}

if(str_contains(x,"l")){
  place<-"bottomleft"
}else{
  place<-"bottomright"
}


# legend
legend(place,legend=c("Median pred","95 % BCI pred","Median obs"),lty=lty,pch=pch, fill=fill,bty="n", border=border, col = col, lwd=lwd)
axis(side = 2, cex.axis=size.labels)
title(title, cex.main = size.text.title)

par(def.par)

dev.off()

  }
  
#-----------------------------------------------------------------------------------------
return()

}



