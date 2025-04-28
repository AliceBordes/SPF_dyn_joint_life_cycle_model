# ------------------------------------------------------------------------------------
#       saving outputs : natural mortality (M and sigma.M) + growth (g and sigma.g)
# ------------------------------------------------------------------------------------

extract.M_g_est<-function(data.mcmc,choix,poisson)
{

if(poisson=="anchovy")
  {
  data_SP<-data_anchovy
  
        if(choix=="M"){
            
        #mortality nat est
        x = "M1"
        mcmc <- window(mcmc)
        mcmc.table <- as.data.frame(as.matrix(mcmc))
        x = mcmc.table[,which(substr(colnames(mcmc.table),1,nchar(x)+1)==paste(x,"[",sep=""))]
        x = x[,which(endsWith(colnames(x),",1]"))]
        q.mean1 <- apply((log(x)),2,median)
        q.mean1.sd <- apply((log(x)),2,sd)
        
        x = "M2"
        mcmc <- window(mcmc)
        mcmc.table <- as.data.frame(as.matrix(mcmc))
        x = mcmc.table[,which(substr(colnames(mcmc.table),1,nchar(x)+1)==paste(x,"[",sep=""))]
        x = x[,which(endsWith(colnames(x),",1]"))]
        q.mean2 <- apply((log(x)),2,median)
        q.mean2.sd <- apply((log(x)),2,sd)
        
        x = "M3"
        mcmc <- window(mcmc)
        mcmc.table <- as.data.frame(as.matrix(mcmc))
        x = mcmc.table[,which(substr(colnames(mcmc.table),1,nchar(x)+1)==paste(x,"[",sep=""))]
        x = x[,which(endsWith(colnames(x),",1]"))]
        q.mean3 <- apply((log(x)),2,median)
        q.mean3.sd <- apply((log(x)),2,sd)
        
        
        data_mgz=data.frame("M1"=q.mean1,"M2"=q.mean2,"M3"=q.mean3,"sigma.M1"=q.mean1.sd,"sigma.M2"=q.mean2.sd,"sigma.M3"=q.mean3.sd)
        row.names(data_mgz)=data_SP$Year[1:22]
        
        # write.table(data_mgz,paste0(base,"/",part.file.path,"est_",choix,"_",poisson,".txt"),row.names=F,quote = F,sep="\t")
        # g<-file.path(base,part.file.path,"est_",choix,"_",poisson,".txt")
        
        }
          
        if(choix=="g"){
        
          #growth nat est
          x = "g1"
          mcmc <- window(mcmc)
          mcmc.table <- as.data.frame(as.matrix(mcmc))
          x = mcmc.table[,which(substr(colnames(mcmc.table),1,nchar(x)+1)==paste(x,"[",sep=""))]
          x = x[,which(endsWith(colnames(x),",1]"))]
          q.mean1 <- apply((log(x)),2,median)
          q.mean1.sd <- apply((log(x)),2,sd)
          
          x = "g2"
          mcmc <- window(mcmc)
          mcmc.table <- as.data.frame(as.matrix(mcmc))
          x = mcmc.table[,which(substr(colnames(mcmc.table),1,nchar(x)+1)==paste(x,"[",sep=""))]
          x = x[,which(endsWith(colnames(x),",1]"))]
          q.mean2 <- apply((log(x)),2,median)
          q.mean2.sd <- apply((log(x)),2,sd)
          
          x = "g3"
          mcmc <- window(mcmc)
          mcmc.table <- as.data.frame(as.matrix(mcmc))
          x = mcmc.table[,which(substr(colnames(mcmc.table),1,nchar(x)+1)==paste(x,"[",sep=""))]
          x = x[,which(endsWith(colnames(x),",1]"))]
          q.mean3 <- apply((log(x)),2,median)
          q.mean3.sd <- apply((log(x)),2,sd)
          
          data_mgz=data.frame("g1"=q.mean1,"g2"=q.mean2,"g3"=q.mean3,"sigma.g1"=q.mean1.sd,"sigma.g2"=q.mean2.sd,"sigma.g3"=q.mean3.sd)
          row.names(data_mgz)=data_SP$Year[1:22]
          # write.table(data_growthfile.path(base,part.file.path,"est_growth_",poisson,".txt"),row.names=F,quote = F,sep="\t")
          # g<-file.path(base,part.file.path,"est_growth_",poisson,".txt")
        
        }
  
  if(choix=="Z.SSN"){
    
    #mortality nat est
    x = "Z.SSN"
    mcmc <- window(mcmc)
    mcmc.table <- as.data.frame(as.matrix(mcmc))
    x = mcmc.table[,which(substr(colnames(mcmc.table),1,nchar(x)+1)==paste(x,"[",sep=""))]
    x = x[,which(endsWith(colnames(x),",1]"))]
    q.mean1 <- apply((log(x)),2,median)
    q.mean1.sd <- apply((log(x)),2,sd)
    
    data_mgz=data.frame("Z.SSN"=q.mean1,"sigma.Z.SSN"=q.mean1.sd)
    row.names(data_mgz)=data_SP$Year[1:22]
    
    # write.table(data_mortality_Z.SSN,file.path(base,part.file.path,"est_demographic_variation.Z.SS_",poisson,".txt"),row.names=F,quote = F,sep="\t")
    # g<-file.path(base,part.file.path,"est_demographic_variation.Z.SS_",poisson,".txt")
    
  }
  
}
  

if(poisson=="sardine")
  {

data_SP<-data_sardine
  
    if(choix=="M"){
      
      #mortality nat est
      x = "M1"
      mcmc <- window(mcmc)
      mcmc.table <- as.data.frame(as.matrix(mcmc))
      x = mcmc.table[,which(substr(colnames(mcmc.table),1,nchar(x)+1)==paste(x,"[",sep=""))]
      x = x[,which(endsWith(colnames(x),",2]"))]
      q.mean1 <- apply((log(x)),2,median)
      q.mean1.sd <- apply((log(x)),2,sd)
      
      x = "M2"
      mcmc <- window(mcmc)
      mcmc.table <- as.data.frame(as.matrix(mcmc))
      x = mcmc.table[,which(substr(colnames(mcmc.table),1,nchar(x)+1)==paste(x,"[",sep=""))]
      x = x[,which(endsWith(colnames(x),",2]"))]
      q.mean2 <- apply((log(x)),2,median)
      q.mean2.sd <- apply((log(x)),2,sd)
      
      x = "M3"
      mcmc <- window(mcmc)
      mcmc.table <- as.data.frame(as.matrix(mcmc))
      x = mcmc.table[,which(substr(colnames(mcmc.table),1,nchar(x)+1)==paste(x,"[",sep=""))]
      x = x[,which(endsWith(colnames(x),",2]"))]
      q.mean3 <- apply((log(x)),2,median)
      q.mean3.sd <- apply((log(x)),2,sd)
      
      x = "M4"
      mcmc <- window(mcmc)
      mcmc.table <- as.data.frame(as.matrix(mcmc))
      x = mcmc.table[,which(substr(colnames(mcmc.table),1,nchar(x)+1)==paste(x,"[",sep=""))]
      x = x[,which(endsWith(colnames(x),",2]"))]
      q.mean4 <- apply((log(x)),2,median)
      q.mean4.sd <- apply((log(x)),2,sd)
      
      x = "M5"
      mcmc <- window(mcmc)
      mcmc.table <- as.data.frame(as.matrix(mcmc))
      x = mcmc.table[,which(substr(colnames(mcmc.table),1,nchar(x)+1)==paste(x,"[",sep=""))]
      x = x[,which(endsWith(colnames(x),",2]"))]
      q.mean5 <- apply((log(x)),2,median)
      q.mean5.sd <- apply((log(x)),2,sd)

      x = "M6"
      mcmc <- window(mcmc)
      mcmc.table <- as.data.frame(as.matrix(mcmc))
      x = mcmc.table[,which(substr(colnames(mcmc.table),1,nchar(x)+1)==paste(x,"[",sep=""))]
      x = x[,which(endsWith(colnames(x),",2]"))]
      q.mean6 <- apply((log(x)),2,median)
      q.mean6.sd <- apply((log(x)),2,sd)      
      
      data_mgz=data.frame("M1"=q.mean1,"M2"=q.mean2,"M3"=q.mean3,"M4"=q.mean4,"M5"=q.mean5,"M6"=q.mean6,"sigma.M1"=q.mean1.sd,"sigma.M2"=q.mean2.sd,"sigma.M3"=q.mean3.sd,"sigma.M4"=q.mean4.sd,"sigma.M5"=q.mean5.sd,"sigma.M6"=q.mean6.sd)
      row.names(data_mgz)=data_SP$Year[1:22]
      
      # write.table(data_mortality_nat,file.path(base,part.file.path,"est_mortality_",poisson,".txt"),row.names=F,quote = F,sep="\t")
      # g<-file.path(base,part.file.path,"est_mortality_",poisson,".txt")
      
    }
    
    if(choix=="g"){
      
      #growth nat est
      x = "g1"
      mcmc <- window(mcmc)
      mcmc.table <- as.data.frame(as.matrix(mcmc))
      x = mcmc.table[,which(substr(colnames(mcmc.table),1,nchar(x)+1)==paste(x,"[",sep=""))]
      x = x[,which(endsWith(colnames(x),",2]"))]
      q.mean1 <- apply((log(x)),2,median)
      q.mean1.sd <- apply((log(x)),2,sd)
      
      x = "g2"
      mcmc <- window(mcmc)
      mcmc.table <- as.data.frame(as.matrix(mcmc))
      x = mcmc.table[,which(substr(colnames(mcmc.table),1,nchar(x)+1)==paste(x,"[",sep=""))]
      x = x[,which(endsWith(colnames(x),",2]"))]
      q.mean2 <- apply((log(x)),2,median)
      q.mean2.sd <- apply((log(x)),2,sd)
      
      x = "g3"
      mcmc <- window(mcmc)
      mcmc.table <- as.data.frame(as.matrix(mcmc))
      x = mcmc.table[,which(substr(colnames(mcmc.table),1,nchar(x)+1)==paste(x,"[",sep=""))]
      x = x[,which(endsWith(colnames(x),",2]"))]
      q.mean3 <- apply((log(x)),2,median)
      q.mean3.sd <- apply((log(x)),2,sd)
      
      x = "g4"
      mcmc <- window(mcmc)
      mcmc.table <- as.data.frame(as.matrix(mcmc))
      x = mcmc.table[,which(substr(colnames(mcmc.table),1,nchar(x)+1)==paste(x,"[",sep=""))]
      x = x[,which(endsWith(colnames(x),",2]"))]
      q.mean4 <- apply((log(x)),2,median)
      q.mean4.sd <- apply((log(x)),2,sd)
      
      x = "g5"
      mcmc <- window(mcmc)
      mcmc.table <- as.data.frame(as.matrix(mcmc))
      x = mcmc.table[,which(substr(colnames(mcmc.table),1,nchar(x)+1)==paste(x,"[",sep=""))]
      x = x[,which(endsWith(colnames(x),",2]"))]
      q.mean5 <- apply((log(x)),2,median)
      q.mean5.sd <- apply((log(x)),2,sd)

      x = "g6"
      mcmc <- window(mcmc)
      mcmc.table <- as.data.frame(as.matrix(mcmc))
      x = mcmc.table[,which(substr(colnames(mcmc.table),1,nchar(x)+1)==paste(x,"[",sep=""))]
      x = x[,which(endsWith(colnames(x),",2]"))]
      q.mean6 <- apply((log(x)),2,median)
      q.mean6.sd <- apply((log(x)),2,sd)      
      
      data_mgz=data.frame("g1"=q.mean1,"g2"=q.mean2,"g3"=q.mean3,"g4"=q.mean4,"g5"=q.mean5,"g6"=q.mean6,"sigma.g1"=q.mean1.sd,"sigma.g2"=q.mean2.sd,"sigma.g3"=q.mean3.sd,"sigma.g4"=q.mean4.sd,"sigma.g5"=q.mean5.sd,"sigma.g6"=q.mean6.sd)
      row.names(data_mgz)=data_SP$Year[1:22]
      # write.table(data_growth,file.path(base,part.file.path,"est_growth_",poisson,".txt"),row.names=F,quote = F,sep="\t")
      # g<-file.path(base,part.file.path,"est_growth_",poisson,".txt")
      
    }
  
  if(choix=="Z.SSN"){
    
    #mortality nat est
    x = "Z.SSN"
    mcmc <- window(mcmc)
    mcmc.table <- as.data.frame(as.matrix(mcmc))
    x = mcmc.table[,which(substr(colnames(mcmc.table),1,nchar(x)+1)==paste(x,"[",sep=""))]
    x = x[,which(endsWith(colnames(x),",2]"))]
    q.mean1 <- apply((log(x)),2,median)
    q.mean1.sd <- apply((log(x)),2,sd)
    
    data_mgz=data.frame("Z.SSN"=q.mean1,"sigma.Z.SSN"=q.mean1.sd)
    row.names(data_mgz)=data_SP$Year[1:22]
    
    # write.table(data_mortality_Z.SSN,file.path(base,part.file.path,"est_demographic_variation.Z.SS_",poisson,".txt"),row.names=F,quote = F,sep="\t")
    # g<-file.path(base,part.file.path,"est_demographic_variation.Z.SS_",poisson,".txt")
    
  }
  
}  
  write.table(data_mgz,paste0(base,"/",part.file.path,"/est_",choix,"_",poisson,".txt"),row.names=F,quote = F,sep="\t")
  g<-paste0(base,"/",part.file.path,"/est_",choix,"_",poisson,".txt")
  
  return(g)
  
}