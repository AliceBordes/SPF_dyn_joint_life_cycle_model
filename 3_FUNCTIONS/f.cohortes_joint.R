cohortes<-function(poisson,data_SP)
{
  
  
  if(poisson=="anchovy")
  {
    # --------------------------------------------------------
    #        Abondance estimée et Abondance observée
    # --------------------------------------------------------
    
    vect.x<-c("I1","I2","I3","I4")
    tab_age = data.frame("Year" = data_SP$Year)
    
    for(i in 1:length(vect.x))
    {
      
      #Age x
      x = vect.x[i]
      su<-str_sub(x)
      su<-substr(su,2,nchar(su))
      
      mcmc <- window(mcmc)
      mcmc.table <- as.data.frame(as.matrix(mcmc))
      x = mcmc.table[,which(substr(colnames(mcmc.table),1,nchar(x)+1)==paste(x,"[",sep=""))]
      x = x[,endsWith(colnames(x),",1]")]
      n_plot <- dim(x)[2]
      q.mean <- apply(x,2,mean)
      q.inf <- NULL
      q.sup <- NULL
      for (k in 1:dim(x)[2]) {
        q.inf[k] <- quantile(x[,k],probs=0.05)
        q.sup[k] <- quantile(x[,k],probs=0.95)
      }
      
      
      if(su=="4"){
        N=paste("N",su,"plus",sep="") 
      }else{N=paste("N",su,sep="") }
      
      
      tab_age_name=paste("tab_age_N",su,sep="")
      
      
      
      tab_age2 = data.frame( # création tableau de données pour le ggplot
        "N.obs" = data_SP[[N]],
        "N.est" = q.mean,
        "N.est.inf" = q.inf,
        "N.est.sup" = q.sup,
        "err.n" = data_SP$CV_N*data_SP[[N]]
      )
      
      colnames(tab_age2)<-c(paste(N,".obs",sep=""),paste(N,".est",sep=""),paste(N,".est.inf",sep=""),paste(N,".est.sup",sep=""),paste(N,".err.n",sep=""))
      
      tab_age<-cbind(tab_age,tab_age2)
    }
    
    
    
    # --------------------------------------------------------
    #     Abondance estimée et abondance observée par cohorte
    # --------------------------------------------------------
    
    #data_est = mcmc.table[,startsWith(varnames(mcmc.table),"N")]
    data_est = as.matrix(mcmc[,startsWith(varnames(mcmc),"N")][,-c(1:23)])
    N.est = colMeans(data_est)
    tab.N.est = data.frame(
      "Year" = data_SP$Year,
      "N1.est" = tab_age$N1.est,
      "N2.est" = tab_age$N2.est,
      "N3.est" = tab_age$N3.est,
      "N4plus.est" = tab_age$N4plus.est
    )
    
    tab.N.inf = data.frame(
      "N1.est.inf" = tab_age$N1.est.inf,
      "N2.est.inf" = tab_age$N2.est.inf,
      "N3.est.inf" = tab_age$N3.est.inf,
      "N4plus.est.inf" = tab_age$N4plus.est.inf
    )
    
    tab.N.sup = data.frame(
      "N1.est.sup" = tab_age$N1.est.sup,
      "N2.est.sup" = tab_age$N2.est.sup,
      "N3.est.sup" = tab_age$N3.est.sup,
      "N4plus.est.sup" = tab_age$N4plus.est.sup
    )
    
    # Mise en forme des données par cohortes
    data.obs.co = matrix(NA,ncol = 20, nrow = 4) # donnée d'observation
    for(i in 1:20){
      data.obs.co[,i] = c(data_SP$N1[i],data_SP$N2[i+1],data_SP$N3[i+2],data_SP$N4plus[i+3])
    }
    Tab.obs.co = data.frame(data.obs.co)
    
    data.est.co = matrix(NA, ncol = 20, nrow = 4) # Donnée estimées
    for(i in 1:20){
      data.est.co[,i] = c(tab.N.est$N1.est[i],tab.N.est$N2.est[i+1],tab.N.est$N3.est[i+2],tab.N.est$N4plus.est[i+3])
    }
    Tab.est.co = data.frame(data.est.co)
    
    data.sup.co = matrix(NA, ncol = 20, nrow = 4) # Limite supérieur des données estimée (95%)
    for(i in 1:20){
      data.sup.co[,i] = c(tab.N.sup$N1.est.sup[i],tab.N.sup$N2.est.sup[i+1],tab.N.sup$N3.est.sup[i+2],tab.N.sup$N4plus.est.sup[i+3])
    }
    Tab.sup.co = data.frame(data.sup.co)
    
    data.inf.co = matrix(NA, ncol = 20, nrow = 4) # Limite inférieur des données estimée (95%)
    for(i in 1:20){
      data.inf.co[,i] = c(tab.N.inf$N1.est.inf[i],tab.N.inf$N2.est.inf[i+1],tab.N.inf$N3.est.inf[i+2],tab.N.inf$N4plus.est.inf[i+3])
    }
    Tab.inf.co = data.frame(data.inf.co)
    
    
    # Creation des tableau des cohortes (17 cohortes)
    table.Co.1 = data.frame("Age" = c(1,2,3,4),"Obs" = data.obs.co[,1],"Est" = data.est.co[,1],"N.Inf" = data.inf.co[,1],"N.Sup" = data.sup.co[,1],"err.obs" = c(data_SP$CV_N[1],data_SP$CV_N[2],data_SP$CV_N[3],data_SP$CV_N[4]))
    
    table.Co.2 = data.frame("Age" = c(1,2,3,4),"Obs" = data.obs.co[,2],"Est" = data.est.co[,2],"N.Inf" = data.inf.co[,2],"N.Sup" = data.sup.co[,2],"err.obs" = c(data_SP$CV_N[2],data_SP$CV_N[3],data_SP$CV_N[4],data_SP$CV_N[5]))
    
    table.Co.3 = data.frame("Age" = c(1,2,3,4),"Obs" = data.obs.co[,3],"Est" = data.est.co[,3],"N.Inf" = data.inf.co[,3],"N.Sup" = data.sup.co[,3],"err.obs" = c(data_SP$CV_N[3],data_SP$CV_N[4],data_SP$CV_N[5],data_SP$CV_N[6]))
    
    table.Co.4 = data.frame("Age" = c(1,2,3,4),"Obs" = data.obs.co[,4],"Est" = data.est.co[,4],"N.Inf" = data.inf.co[,4],"N.Sup" = data.sup.co[,4],"err.obs" = c(data_SP$CV_N[4],data_SP$CV_N[5],data_SP$CV_N[6],data_SP$CV_N[7]))
    
    table.Co.5 = data.frame("Age" = c(1,2,3,4),"Obs" = data.obs.co[,5],"Est" = data.est.co[,5],"N.Inf" = data.inf.co[,5],"N.Sup" = data.sup.co[,5],"err.obs" = c(data_SP$CV_N[5],data_SP$CV_N[6],data_SP$CV_N[7],data_SP$CV_N[8]))
    
    table.Co.6 = data.frame("Age" = c(1,2,3,4),"Obs" = data.obs.co[,6],"Est" = data.est.co[,6],"N.Inf" = data.inf.co[,6],"N.Sup" = data.sup.co[,6],"err.obs" = c(data_SP$CV_N[6],data_SP$CV_N[7],data_SP$CV_N[8],data_SP$CV_N[9]))
    
    table.Co.7 = data.frame("Age" = c(1,2,3,4),"Obs" = data.obs.co[,7],"Est" = data.est.co[,7],"N.Inf" = data.inf.co[,7],"N.Sup" = data.sup.co[,7],"err.obs" = c(data_SP$CV_N[7],data_SP$CV_N[8],data_SP$CV_N[9],data_SP$CV_N[10]))
    
    table.Co.8 = data.frame("Age" = c(1,2,3,4),"Obs" = data.obs.co[,8],"Est" = data.est.co[,8],"N.Inf" = data.inf.co[,8],"N.Sup" = data.sup.co[,8],"err.obs" = c(data_SP$CV_N[8],data_SP$CV_N[9],data_SP$CV_N[10],data_SP$CV_N[11]))
    
    table.Co.9 = data.frame("Age" = c(1,2,3,4),"Obs" = data.obs.co[,9],"Est" = data.est.co[,9],"N.Inf" = data.inf.co[,9],"N.Sup" = data.sup.co[,9],"err.obs" = c(data_SP$CV_N[9],data_SP$CV_N[10],data_SP$CV_N[11],data_SP$CV_N[12]))
    
    table.Co.10 = data.frame("Age" = c(1,2,3,4),"Obs" = data.obs.co[,10],"Est" = data.est.co[,10],"N.Inf" = data.inf.co[,10],"N.Sup" = data.sup.co[,10],"err.obs" = c(data_SP$CV_N[10],data_SP$CV_N[11],data_SP$CV_N[12],data_SP$CV_N[13]))
    
    table.Co.11 = data.frame("Age" = c(1,2,3,4),"Obs" = data.obs.co[,11],"Est" = data.est.co[,11],"N.Inf" = data.inf.co[,11],"N.Sup" = data.sup.co[,11],"err.obs" = c(data_SP$CV_N[11],data_SP$CV_N[12],data_SP$CV_N[13],data_SP$CV_N[14]))
    
    table.Co.12 = data.frame("Age" = c(1,2,3,4),"Obs" = data.obs.co[,12],"Est" = data.est.co[,12],"N.Inf" = data.inf.co[,12],"N.Sup" = data.sup.co[,12],"err.obs" = c(data_SP$CV_N[12],data_SP$CV_N[13],data_SP$CV_N[14],data_SP$CV_N[15]))
    
    table.Co.13 = data.frame("Age" = c(1,2,3,4),"Obs" = data.obs.co[,13],"Est" = data.est.co[,13],"N.Inf" = data.inf.co[,13],"N.Sup" = data.sup.co[,13],"err.obs" = c(data_SP$CV_N[13],data_SP$CV_N[14],data_SP$CV_N[15],data_SP$CV_N[16]))
    
    table.Co.14 = data.frame("Age" = c(1,2,3,4),"Obs" = data.obs.co[,14],"Est" = data.est.co[,14],"N.Inf" = data.inf.co[,14],"N.Sup" = data.sup.co[,14],"err.obs" = c(data_SP$CV_N[14],data_SP$CV_N[15],data_SP$CV_N[16],data_SP$CV_N[17]))
    
    table.Co.15 = data.frame("Age" = c(1,2,3,4),"Obs" = data.obs.co[,15],"Est" = data.est.co[,15],"N.Inf" = data.inf.co[,15],"N.Sup" = data.sup.co[,15],"err.obs" = c(data_SP$CV_N[15],data_SP$CV_N[16],data_SP$CV_N[17],data_SP$CV_N[18]))
    
    table.Co.16 = data.frame("Age" = c(1,2,3,4),"Obs" = data.obs.co[,16],"Est" = data.est.co[,16],"N.Inf" = data.inf.co[,16],"N.Sup" = data.sup.co[,16],"err.obs" = c(data_SP$CV_N[16],data_SP$CV_N[17],data_SP$CV_N[18],data_SP$CV_N[19]))
    
    table.Co.17 = data.frame("Age" = c(1,2,3,4),"Obs" = data.obs.co[,17],"Est" = data.est.co[,17],"N.Inf" = data.inf.co[,17],"N.Sup" = data.sup.co[,17],"err.obs" = c(data_SP$CV_N[17],data_SP$CV_N[18],data_SP$CV_N[19],data_SP$CV_N[20]))
    
    table.Co.18 = data.frame("Age" = c(1,2,3,4),"Obs" = data.obs.co[,18],"Est" = data.est.co[,18],"N.Inf" = data.inf.co[,18],"N.Sup" = data.sup.co[,18],"err.obs" = c(data_SP$CV_N[18],data_SP$CV_N[19],data_SP$CV_N[20],data_SP$CV_N[21]))
    
    table.Co.19 = data.frame("Age" = c(1,2,3,4),"Obs" = data.obs.co[,19],"Est" = data.est.co[,19],"N.Inf" = data.inf.co[,19],"N.Sup" = data.sup.co[,19],"err.obs" = c(data_SP$CV_N[19],data_SP$CV_N[20],data_SP$CV_N[21],data_SP$CV_N[22]))
    
    table.Co.20 = data.frame("Age" = c(1,2,3,4),"Obs" = data.obs.co[,20],"Est" = data.est.co[,20],"N.Inf" = data.inf.co[,20],"N.Sup" = data.sup.co[,20],"err.obs" = c(data_SP$CV_N[20],data_SP$CV_N[21],data_SP$CV_N[22],data_SP$CV_N[23]))
    
    # Création des graphiques
    
    co.1 <- ggplot(table.Co.1,aes(x=Age,y=Est))+
      theme_bw()+
      ggtitle("Cohorte 1999")+
      xlab("Age")+
      ylab("Abondance (nb ind)")+
      geom_ribbon(aes(ymin=N.Inf,ymax=N.Sup),fill="lightgray")+
      geom_line(colour="black")+
      geom_point(aes(x=Age,y=Obs),colour = "red")+
      geom_errorbar(aes(ymin=Obs-(err.obs*Obs),ymax=Obs+(err.obs*Obs)),width=0.1)
    
    co.2 <- ggplot(table.Co.2,aes(x=Age,y=Est))+
      theme_bw()+
      ggtitle("Cohorte 2000")+
      xlab("Age")+
      ylab("Abondance (nb ind)")+
      geom_ribbon(aes(ymin=N.Inf,ymax=N.Sup),fill="lightgray")+
      geom_line(colour="black")+
      geom_point(aes(x=Age,y=Obs),colour = "red")+
      geom_errorbar(aes(ymin=Obs-(err.obs*Obs),ymax=Obs+(err.obs*Obs)),width=0.1)
    
    co.3 <- ggplot(table.Co.3,aes(x=Age,y=Est))+
      theme_bw()+
      ggtitle("Cohorte 2001")+
      xlab("Age")+
      ylab("Abondance (nb ind)")+
      geom_ribbon(aes(ymin=N.Inf,ymax=N.Sup),fill="lightgray")+
      geom_line(colour="black")+
      geom_point(aes(x=Age,y=Obs),colour = "red")+
      geom_errorbar(aes(ymin=Obs-(err.obs*Obs),ymax=Obs+(err.obs*Obs)),width=0.1)
    
    co.4 <- ggplot(table.Co.4,aes(x=Age,y=Est))+
      theme_bw()+
      ggtitle("Cohorte 2002")+
      xlab("Age")+
      ylab("Abondance (nb ind)")+
      geom_ribbon(aes(ymin=N.Inf,ymax=N.Sup),fill="lightgray")+
      geom_line(colour="black")+
      geom_point(aes(x=Age,y=Obs),colour = "red")+
      geom_errorbar(aes(ymin=Obs-(err.obs*Obs),ymax=Obs+(err.obs*Obs)),width=0.1)
    
    co.5 <- ggplot(table.Co.5,aes(x=Age,y=Est))+
      theme_bw()+
      ggtitle("Cohorte 2003")+
      xlab("Age")+
      ylab("Abondance (nb ind)")+
      geom_ribbon(aes(ymin=N.Inf,ymax=N.Sup),fill="lightgray")+
      geom_line(colour="black")+
      geom_point(aes(x=Age,y=Obs),colour = "red")+
      geom_errorbar(aes(ymin=Obs-(err.obs*Obs),ymax=Obs+(err.obs*Obs)),width=0.1)
    
    co.6 <- ggplot(table.Co.6,aes(x=Age,y=Est))+
      theme_bw()+
      ggtitle("Cohorte 2004")+
      xlab("Age")+
      ylab("Abondance (nb ind)")+
      geom_ribbon(aes(ymin=N.Inf,ymax=N.Sup),fill="lightgray")+
      geom_line(colour="black")+
      geom_point(aes(x=Age,y=Obs),colour = "red")+
      geom_errorbar(aes(ymin=Obs-(err.obs*Obs),ymax=Obs+(err.obs*Obs)),width=0.1)
    
    co.7 <- ggplot(table.Co.7,aes(x=Age,y=Est))+
      theme_bw()+
      ggtitle("Cohorte 2005")+
      xlab("Age")+
      ylab("Abondance (nb ind)")+
      geom_ribbon(aes(ymin=N.Inf,ymax=N.Sup),fill="lightgray")+
      geom_line(colour="black")+
      geom_point(aes(x=Age,y=Obs),colour = "red")+
      geom_errorbar(aes(ymin=Obs-(err.obs*Obs),ymax=Obs+(err.obs*Obs)),width=0.1)
    
    co.8 <- ggplot(table.Co.8,aes(x=Age,y=Est))+
      theme_bw()+
      ggtitle("Cohorte 2006")+
      xlab("Age")+
      ylab("Abondance (nb ind)")+
      geom_ribbon(aes(ymin=N.Inf,ymax=N.Sup),fill="lightgray")+
      geom_line(colour="black")+
      geom_point(aes(x=Age,y=Obs),colour = "red")+
      geom_errorbar(aes(ymin=Obs-(err.obs*Obs),ymax=Obs+(err.obs*Obs)),width=0.1)
    
    co.9 <- ggplot(table.Co.9,aes(x=Age,y=Est))+
      theme_bw()+
      ggtitle("Cohorte 2007")+
      xlab("Age")+
      ylab("Abondance (nb ind)")+
      geom_ribbon(aes(ymin=N.Inf,ymax=N.Sup),fill="lightgray")+
      geom_line(colour="black")+
      geom_point(aes(x=Age,y=Obs),colour = "red")+
      geom_errorbar(aes(ymin=Obs-(err.obs*Obs),ymax=Obs+(err.obs*Obs)),width=0.1)
    
    co.10 <- ggplot(table.Co.10,aes(x=Age,y=Est))+
      theme_bw()+
      ggtitle("Cohorte 2008")+
      xlab("Age")+
      ylab("Abondance (nb ind)")+
      geom_ribbon(aes(ymin=N.Inf,ymax=N.Sup),fill="lightgray")+
      geom_line(colour="black")+
      geom_point(aes(x=Age,y=Obs),colour = "red")+
      geom_errorbar(aes(ymin=Obs-(err.obs*Obs),ymax=Obs+(err.obs*Obs)),width=0.1)
    
    co.11 <- ggplot(table.Co.11,aes(x=Age,y=Est))+
      theme_bw()+
      ggtitle("Cohorte 2009")+
      xlab("Age")+
      ylab("Abondance (nb ind)")+
      geom_ribbon(aes(ymin=N.Inf,ymax=N.Sup),fill="lightgray")+
      geom_line(colour="black")+
      geom_point(aes(x=Age,y=Obs),colour = "red")+
      geom_errorbar(aes(ymin=Obs-(err.obs*Obs),ymax=Obs+(err.obs*Obs)),width=0.1)
    
    co.12 <- ggplot(table.Co.12,aes(x=Age,y=Est))+
      theme_bw()+
      ggtitle("Cohorte 2010")+
      xlab("Age")+
      ylab("Abondance (nb ind)")+
      geom_ribbon(aes(ymin=N.Inf,ymax=N.Sup),fill="lightgray")+
      geom_line(colour="black")+
      geom_point(aes(x=Age,y=Obs),colour = "red")+
      geom_errorbar(aes(ymin=Obs-(err.obs*Obs),ymax=Obs+(err.obs*Obs)),width=0.1)
    
    co.13 <- ggplot(table.Co.13,aes(x=Age,y=Est))+
      theme_bw()+
      ggtitle("Cohorte 2011")+
      xlab("Age")+
      ylab("Abondance (nb ind)")+
      geom_ribbon(aes(ymin=N.Inf,ymax=N.Sup),fill="lightgray")+
      geom_line(colour="black")+
      geom_point(aes(x=Age,y=Obs),colour = "red")+
      geom_errorbar(aes(ymin=Obs-(err.obs*Obs),ymax=Obs+(err.obs*Obs)),width=0.1)
    
    co.14 <- ggplot(table.Co.14,aes(x=Age,y=Est))+
      theme_bw()+
      ggtitle("Cohorte 2012")+
      xlab("Age")+
      ylab("Abondance (nb ind)")+
      geom_ribbon(aes(ymin=N.Inf,ymax=N.Sup),fill="lightgray")+
      geom_line(colour="black")+
      geom_point(aes(x=Age,y=Obs),colour = "red")+
      geom_errorbar(aes(ymin=Obs-(err.obs*Obs),ymax=Obs+(err.obs*Obs)),width=0.1)
    
    co.15 <- ggplot(table.Co.15,aes(x=Age,y=Est))+
      theme_bw()+
      ggtitle("Cohorte 2013")+
      xlab("Age")+
      ylab("Abondance (nb ind)")+
      geom_ribbon(aes(ymin=N.Inf,ymax=N.Sup),fill="lightgray")+
      geom_line(colour="black")+
      geom_point(aes(x=Age,y=Obs),colour = "red")+
      geom_errorbar(aes(ymin=Obs-(err.obs*Obs),ymax=Obs+(err.obs*Obs)),width=0.1)
    
    co.16 <- ggplot(table.Co.16,aes(x=Age,y=Est))+
      theme_bw()+
      ggtitle("Cohorte 2014")+
      xlab("Age")+
      ylab("Abondance (nb ind)")+
      geom_ribbon(aes(ymin=N.Inf,ymax=N.Sup),fill="lightgray")+
      geom_line(colour="black")+
      geom_point(aes(x=Age,y=Obs),colour = "red")+
      geom_errorbar(aes(ymin=Obs-(err.obs*Obs),ymax=Obs+(err.obs*Obs)),width=0.1)
    
    co.17 <- ggplot(table.Co.17,aes(x=Age,y=Est))+
      theme_bw()+
      ggtitle("Cohorte 2015")+
      xlab("Age")+
      ylab("Abondance (nb ind)")+
      geom_ribbon(aes(ymin=N.Inf,ymax=N.Sup),fill="lightgray")+
      geom_line(colour="black")+
      geom_point(aes(x=Age,y=Obs),colour = "red")+
      geom_errorbar(aes(ymin=Obs-(err.obs*Obs),ymax=Obs+(err.obs*Obs)),width=0.1)
    
    co.18 <- ggplot(table.Co.18,aes(x=Age,y=Est))+
      theme_bw()+
      ggtitle("Cohorte 2016")+
      xlab("Age")+
      ylab("Abondance (nb ind)")+
      geom_ribbon(aes(ymin=N.Inf,ymax=N.Sup),fill="lightgray")+
      geom_line(colour="black")+
      geom_point(aes(x=Age,y=Obs),colour = "red")+
      geom_errorbar(aes(ymin=Obs-(err.obs*Obs),ymax=Obs+(err.obs*Obs)),width=0.1)
    
    co.19 <- ggplot(table.Co.19,aes(x=Age,y=Est))+
      theme_bw()+
      ggtitle("Cohorte 2017")+
      xlab("Age")+
      ylab("Abondance (nb ind)")+
      geom_ribbon(aes(ymin=N.Inf,ymax=N.Sup),fill="lightgray")+
      geom_line(colour="black")+
      geom_point(aes(x=Age,y=Obs),colour = "red")+
      geom_errorbar(aes(ymin=Obs-(err.obs*Obs),ymax=Obs+(err.obs*Obs)),width=0.1)
    
    co.20 <- ggplot(table.Co.20,aes(x=Age,y=Est))+
      theme_bw()+
      ggtitle("Cohorte 2018")+
      xlab("Age")+
      ylab("Abondance (nb ind)")+
      geom_ribbon(aes(ymin=N.Inf,ymax=N.Sup),fill="lightgray")+
      geom_line(colour="black")+
      geom_point(aes(x=Age,y=Obs),colour = "red")+
      geom_errorbar(aes(ymin=Obs-(err.obs*Obs),ymax=Obs+(err.obs*Obs)),width=0.1)
    
    
    # print graphs
    resol <- 6
    
    name_figure <- paste("N_cohortes_",poisson,"_1.png")
    png(filename = name_figure, height = 320*resol, width = 680*resol, res=72*resol)
    
    def.par <- par(no.readonly = TRUE)
    
    grid.arrange(co.1,co.2, co.3,co.4,co.5,co.6,co.7,co.8,co.9,co.10, ncol=4, nrow=3)
    
    par(def.par)
    dev.off()
    
    name_figure <- paste("N_cohortes_",poisson,"_2.png")
    png(filename = name_figure, height = 320*resol, width = 680*resol, res=72*resol)
    
    def.par <- par(no.readonly = TRUE)
    
    grid.arrange(co.11,co.12, co.13,co.14,co.15,co.16,co.17,co.18,co.19,co.20, ncol=4, nrow=3)
    
    par(def.par)
    dev.off()  
    
    
  }  # end if(anchovy)
  
  
  #-----------------------------------------------------------------------------------------
  
  
  if(poisson=="sardine")
  {
    
    data_SP
    # --------------------------------------------------------
    #        Abondance estimée et Abondance observée
    # --------------------------------------------------------
    
    vect.x<-c("I1","I2","I3","I4","I5","I6")
    tab_age = data.frame("Year" = data_SP$Year)
    
    for(i in 1:length(vect.x))
    {
      
      #Age x
      x = vect.x[i]
      su<-str_sub(x)
      su<-substr(su,2,nchar(su))
      
      mcmc <- window(mcmc)
      mcmc.table <- as.data.frame(as.matrix(mcmc))
      x = mcmc.table[,which(substr(colnames(mcmc.table),1,nchar(x)+1)==paste(x,"[",sep=""))]
      x = x[,endsWith(colnames(x),",2]")]
      n_plot <- dim(x)[2]
      q.mean <- apply(x,2,mean)
      q.inf <- NULL
      q.sup <- NULL
      for (k in 1:dim(x)[2]) {
        q.inf[k] <- quantile(x[,k],probs=0.05)
        q.sup[k] <- quantile(x[,k],probs=0.95)
      }
      
    
      if(su=="6"){
       N=paste("N",su,"plus",sep="") 
      }else{N=paste("N",su,sep="") }
      
      
      tab_age_name=paste("tab_age_N",su,sep="")
      
      
      
      tab_age2 = data.frame( # création tableau de données pour le ggplot
        "N.obs" = data_SP[[N]],
        "N.est" = q.mean,
        "N.est.inf" = q.inf,
        "N.est.sup" = q.sup,
        "err.n" = data_SP$CV_N*data_SP[[N]]
      )
      
      colnames(tab_age2)<-c(paste(N,".obs",sep=""),paste(N,".est",sep=""),paste(N,".est.inf",sep=""),paste(N,".est.sup",sep=""),paste(N,".err.n",sep=""))
      
      tab_age<-cbind(tab_age,tab_age2)
    }
    
    
    
    # --------------------------------------------------------
    #     Abondance estimée et abondance observée par cohorte
    # --------------------------------------------------------
    
    #data_est = mcmc.table[,startsWith(varnames(mcmc.table),"N")]
    data_est = as.matrix(mcmc[,startsWith(varnames(mcmc),"N")][,-c(1:23)])
    N.est = colMeans(data_est)
    tab.N.est = data.frame(
      "Year" = data_SP$Year,
      "N1.est" = tab_age$N1.est,
      "N2.est" = tab_age$N2.est,
      "N3.est" = tab_age$N3.est,
      "N4.est" = tab_age$N4.est,
      "N5.est" = tab_age$N5.est,
      "N6plus.est" = tab_age$N6plus.est
    )
    
    tab.N.inf = data.frame(
      "N1.est.inf" = tab_age$N1.est.inf,
      "N2.est.inf" = tab_age$N2.est.inf,
      "N3.est.inf" = tab_age$N3.est.inf,
      "N4.est.inf" = tab_age$N4.est.inf,
      "N5.est.inf" = tab_age$N5.est.inf,
      "N6plus.est.inf" = tab_age$N6plus.est.inf
    )
    
    tab.N.sup = data.frame(
      "N1.est.sup" = tab_age$N1.est.sup,
      "N2.est.sup" = tab_age$N2.est.sup,
      "N3.est.sup" = tab_age$N3.est.sup,
      "N4.est.sup" = tab_age$N4.est.sup,
      "N5.est.sup" = tab_age$N5.est.sup,
      "N6plus.est.sup" = tab_age$N6plus.est.sup
    )
    
    # Mise en forme des données par cohortes
    data.obs.co = matrix(NA,ncol = 18, nrow = 6) # donnée d'observation
    for(i in 1:18){
      data.obs.co[,i] = c(data_SP$N1[i],data_SP$N2[i+1],data_SP$N3[i+2],data_SP$N4[i+3],data_SP$N5[i+4],data_SP$N6plus[i+5])
    }
    Tab.obs.co = data.frame(data.obs.co)
    
    data.est.co = matrix(NA, ncol = 18, nrow = 6) # Donnée estimées
    for(i in 1:18){
      data.est.co[,i] = c(tab.N.est$N1.est[i],tab.N.est$N2.est[i+1],tab.N.est$N3.est[i+2],tab.N.est$N4.est[i+3],tab.N.est$N5.est[i+4],tab.N.est$N6plus.est[i+5])
    }
    Tab.est.co = data.frame(data.est.co)
    
    data.sup.co = matrix(NA, ncol = 18, nrow = 6) # Limite supérieur des données estimée (95%)
    for(i in 1:18){
      data.sup.co[,i] = c(tab.N.sup$N1.est.sup[i],tab.N.sup$N2.est.sup[i+1],tab.N.sup$N3.est.sup[i+2],tab.N.sup$N4.est.sup[i+3],tab.N.sup$N5.est.sup[i+4],tab.N.sup$N6plus.est.sup[i+5])
    }
    Tab.sup.co = data.frame(data.sup.co)
    
    data.inf.co = matrix(NA, ncol = 18, nrow = 6) # Limite inférieur des données estimée (95%)
    for(i in 1:18){
      data.inf.co[,i] = c(tab.N.inf$N1.est.inf[i],tab.N.inf$N2.est.inf[i+1],tab.N.inf$N3.est.inf[i+2],tab.N.inf$N4.est.inf[i+3],tab.N.inf$N5.est.inf[i+4],tab.N.inf$N6plus.est.inf[i+5])
    }
    Tab.inf.co = data.frame(data.inf.co)
    
    
    # Creation des tableau des cohortes (17 cohortes)
    table.Co.1 = data.frame("Age" = c(1,2,3,4,5,6),"Obs" = data.obs.co[,1],"Est" = data.est.co[,1],"N.Inf" = data.inf.co[,1],"N.Sup" = data.sup.co[,1],"err.obs" = c(data_SP$CV_N[1],data_SP$CV_N[2],data_SP$CV_N[3],data_SP$CV_N[4],data_SP$CV_N[5],data_SP$CV_N[6]))
    
    table.Co.2 = data.frame("Age" = c(1,2,3,4,5,6),"Obs" = data.obs.co[,2],"Est" = data.est.co[,2],"N.Inf" = data.inf.co[,2],"N.Sup" = data.sup.co[,2],"err.obs" = c(data_SP$CV_N[2],data_SP$CV_N[3],data_SP$CV_N[4],data_SP$CV_N[5],data_SP$CV_N[6],data_SP$CV_N[7]))
    
    table.Co.3 = data.frame("Age" = c(1,2,3,4,5,6),"Obs" = data.obs.co[,3],"Est" = data.est.co[,3],"N.Inf" = data.inf.co[,3],"N.Sup" = data.sup.co[,3],"err.obs" = c(data_SP$CV_N[3],data_SP$CV_N[4],data_SP$CV_N[5],data_SP$CV_N[6],data_SP$CV_N[7],data_SP$CV_N[8]))
    
    table.Co.4 = data.frame("Age" = c(1,2,3,4,5,6),"Obs" = data.obs.co[,4],"Est" = data.est.co[,4],"N.Inf" = data.inf.co[,4],"N.Sup" = data.sup.co[,4],"err.obs" = c(data_SP$CV_N[4],data_SP$CV_N[5],data_SP$CV_N[6],data_SP$CV_N[7],data_SP$CV_N[8],data_SP$CV_N[9]))
    
    table.Co.5 = data.frame("Age" = c(1,2,3,4,5,6),"Obs" = data.obs.co[,5],"Est" = data.est.co[,5],"N.Inf" = data.inf.co[,5],"N.Sup" = data.sup.co[,5],"err.obs" = c(data_SP$CV_N[5],data_SP$CV_N[6],data_SP$CV_N[7],data_SP$CV_N[8],data_SP$CV_N[9],data_SP$CV_N[10]))
    
    table.Co.6 = data.frame("Age" = c(1,2,3,4,5,6),"Obs" = data.obs.co[,6],"Est" = data.est.co[,6],"N.Inf" = data.inf.co[,6],"N.Sup" = data.sup.co[,6],"err.obs" = c(data_SP$CV_N[6],data_SP$CV_N[7],data_SP$CV_N[8],data_SP$CV_N[9],data_SP$CV_N[10],data_SP$CV_N[11]))
    
    table.Co.7 = data.frame("Age" = c(1,2,3,4,5,6),"Obs" = data.obs.co[,7],"Est" = data.est.co[,7],"N.Inf" = data.inf.co[,7],"N.Sup" = data.sup.co[,7],"err.obs" = c(data_SP$CV_N[7],data_SP$CV_N[8],data_SP$CV_N[9],data_SP$CV_N[10],data_SP$CV_N[11],data_SP$CV_N[12]))
    
    table.Co.8 = data.frame("Age" = c(1,2,3,4,5,6),"Obs" = data.obs.co[,8],"Est" = data.est.co[,8],"N.Inf" = data.inf.co[,8],"N.Sup" = data.sup.co[,8],"err.obs" = c(data_SP$CV_N[8],data_SP$CV_N[9],data_SP$CV_N[10],data_SP$CV_N[11],data_SP$CV_N[12],data_SP$CV_N[13]))
    
    table.Co.9 = data.frame("Age" = c(1,2,3,4,5,6),"Obs" = data.obs.co[,9],"Est" = data.est.co[,9],"N.Inf" = data.inf.co[,9],"N.Sup" = data.sup.co[,9],"err.obs" = c(data_SP$CV_N[9],data_SP$CV_N[10],data_SP$CV_N[11],data_SP$CV_N[12],data_SP$CV_N[13],data_SP$CV_N[14]))
    
    table.Co.10 = data.frame("Age" = c(1,2,3,4,5,6),"Obs" = data.obs.co[,10],"Est" = data.est.co[,10],"N.Inf" = data.inf.co[,10],"N.Sup" = data.sup.co[,10],"err.obs" = c(data_SP$CV_N[10],data_SP$CV_N[11],data_SP$CV_N[12],data_SP$CV_N[13],data_SP$CV_N[14],data_SP$CV_N[15]))
    
    table.Co.11 = data.frame("Age" = c(1,2,3,4,5,6),"Obs" = data.obs.co[,11],"Est" = data.est.co[,11],"N.Inf" = data.inf.co[,11],"N.Sup" = data.sup.co[,11],"err.obs" = c(data_SP$CV_N[11],data_SP$CV_N[12],data_SP$CV_N[13],data_SP$CV_N[14],data_SP$CV_N[15],data_SP$CV_N[16]))
    
    table.Co.12 = data.frame("Age" = c(1,2,3,4,5,6),"Obs" = data.obs.co[,12],"Est" = data.est.co[,12],"N.Inf" = data.inf.co[,12],"N.Sup" = data.sup.co[,12],"err.obs" = c(data_SP$CV_N[12],data_SP$CV_N[13],data_SP$CV_N[14],data_SP$CV_N[15],data_SP$CV_N[16],data_SP$CV_N[17]))
    
    table.Co.13 = data.frame("Age" = c(1,2,3,4,5,6),"Obs" = data.obs.co[,13],"Est" = data.est.co[,13],"N.Inf" = data.inf.co[,13],"N.Sup" = data.sup.co[,13],"err.obs" = c(data_SP$CV_N[13],data_SP$CV_N[14],data_SP$CV_N[15],data_SP$CV_N[16],data_SP$CV_N[17],data_SP$CV_N[28]))
    
    table.Co.14 = data.frame("Age" = c(1,2,3,4,5,6),"Obs" = data.obs.co[,14],"Est" = data.est.co[,14],"N.Inf" = data.inf.co[,14],"N.Sup" = data.sup.co[,14],"err.obs" = c(data_SP$CV_N[14],data_SP$CV_N[15],data_SP$CV_N[16],data_SP$CV_N[17],data_SP$CV_N[18],data_SP$CV_N[19]))
    
    table.Co.15 = data.frame("Age" = c(1,2,3,4,5,6),"Obs" = data.obs.co[,15],"Est" = data.est.co[,15],"N.Inf" = data.inf.co[,15],"N.Sup" = data.sup.co[,15],"err.obs" = c(data_SP$CV_N[15],data_SP$CV_N[16],data_SP$CV_N[17],data_SP$CV_N[18],data_SP$CV_N[19],data_SP$CV_N[20]))
    
    table.Co.16 = data.frame("Age" = c(1,2,3,4,5,6),"Obs" = data.obs.co[,16],"Est" = data.est.co[,16],"N.Inf" = data.inf.co[,16],"N.Sup" = data.sup.co[,16],"err.obs" = c(data_SP$CV_N[16],data_SP$CV_N[17],data_SP$CV_N[18],data_SP$CV_N[19],data_SP$CV_N[20],data_SP$CV_N[21]))
    
    table.Co.17 = data.frame("Age" = c(1,2,3,4,5,6),"Obs" = data.obs.co[,17],"Est" = data.est.co[,17],"N.Inf" = data.inf.co[,17],"N.Sup" = data.sup.co[,17],"err.obs" = c(data_SP$CV_N[17],data_SP$CV_N[18],data_SP$CV_N[19],data_SP$CV_N[20],data_SP$CV_N[21],data_SP$CV_N[22]))
    
    table.Co.18 = data.frame("Age" = c(1,2,3,4,5,6),"Obs" = data.obs.co[,18],"Est" = data.est.co[,18],"N.Inf" = data.inf.co[,18],"N.Sup" = data.sup.co[,18],"err.obs" = c(data_SP$CV_N[18],data_SP$CV_N[19],data_SP$CV_N[20],data_SP$CV_N[21],data_SP$CV_N[22],data_SP$CV_N[23]))
    
    
    
    # Création des graphiques
    
    co.1 <- ggplot(table.Co.1,aes(x=Age,y=Est))+
      theme_bw()+
      ggtitle("Cohorte 1999")+
      xlab("Age")+
      ylab("Abondance (nb ind)")+
      geom_ribbon(aes(ymin=N.Inf,ymax=N.Sup),fill="lightgray")+
      geom_line(colour="black")+
      geom_point(aes(x=Age,y=Obs),colour = "red")+
      geom_errorbar(aes(ymin=Obs-(err.obs*Obs),ymax=Obs+(err.obs*Obs)),width=0.1)
    
    co.2 <- ggplot(table.Co.2,aes(x=Age,y=Est))+
      theme_bw()+
      ggtitle("Cohorte 2000")+
      xlab("Age")+
      ylab("Abondance (nb ind)")+
      geom_ribbon(aes(ymin=N.Inf,ymax=N.Sup),fill="lightgray")+
      geom_line(colour="black")+
      geom_point(aes(x=Age,y=Obs),colour = "red")+
      geom_errorbar(aes(ymin=Obs-(err.obs*Obs),ymax=Obs+(err.obs*Obs)),width=0.1)
    
    co.3 <- ggplot(table.Co.3,aes(x=Age,y=Est))+
      theme_bw()+
      ggtitle("Cohorte 2001")+
      xlab("Age")+
      ylab("Abondance (nb ind)")+
      geom_ribbon(aes(ymin=N.Inf,ymax=N.Sup),fill="lightgray")+
      geom_line(colour="black")+
      geom_point(aes(x=Age,y=Obs),colour = "red")+
      geom_errorbar(aes(ymin=Obs-(err.obs*Obs),ymax=Obs+(err.obs*Obs)),width=0.1)
    
    co.4 <- ggplot(table.Co.4,aes(x=Age,y=Est))+
      theme_bw()+
      ggtitle("Cohorte 2002")+
      xlab("Age")+
      ylab("Abondance (nb ind)")+
      geom_ribbon(aes(ymin=N.Inf,ymax=N.Sup),fill="lightgray")+
      geom_line(colour="black")+
      geom_point(aes(x=Age,y=Obs),colour = "red")+
      geom_errorbar(aes(ymin=Obs-(err.obs*Obs),ymax=Obs+(err.obs*Obs)),width=0.1)
    
    co.5 <- ggplot(table.Co.5,aes(x=Age,y=Est))+
      theme_bw()+
      ggtitle("Cohorte 2003")+
      xlab("Age")+
      ylab("Abondance (nb ind)")+
      geom_ribbon(aes(ymin=N.Inf,ymax=N.Sup),fill="lightgray")+
      geom_line(colour="black")+
      geom_point(aes(x=Age,y=Obs),colour = "red")+
      geom_errorbar(aes(ymin=Obs-(err.obs*Obs),ymax=Obs+(err.obs*Obs)),width=0.1)
    
    co.6 <- ggplot(table.Co.6,aes(x=Age,y=Est))+
      theme_bw()+
      ggtitle("Cohorte 2004")+
      xlab("Age")+
      ylab("Abondance (nb ind)")+
      geom_ribbon(aes(ymin=N.Inf,ymax=N.Sup),fill="lightgray")+
      geom_line(colour="black")+
      geom_point(aes(x=Age,y=Obs),colour = "red")+
      geom_errorbar(aes(ymin=Obs-(err.obs*Obs),ymax=Obs+(err.obs*Obs)),width=0.1)
    
    co.7 <- ggplot(table.Co.7,aes(x=Age,y=Est))+
      theme_bw()+
      ggtitle("Cohorte 2005")+
      xlab("Age")+
      ylab("Abondance (nb ind)")+
      geom_ribbon(aes(ymin=N.Inf,ymax=N.Sup),fill="lightgray")+
      geom_line(colour="black")+
      geom_point(aes(x=Age,y=Obs),colour = "red")+
      geom_errorbar(aes(ymin=Obs-(err.obs*Obs),ymax=Obs+(err.obs*Obs)),width=0.1)
    
    co.8 <- ggplot(table.Co.8,aes(x=Age,y=Est))+
      theme_bw()+
      ggtitle("Cohorte 2006")+
      xlab("Age")+
      ylab("Abondance (nb ind)")+
      geom_ribbon(aes(ymin=N.Inf,ymax=N.Sup),fill="lightgray")+
      geom_line(colour="black")+
      geom_point(aes(x=Age,y=Obs),colour = "red")+
      geom_errorbar(aes(ymin=Obs-(err.obs*Obs),ymax=Obs+(err.obs*Obs)),width=0.1)
    
    co.9 <- ggplot(table.Co.9,aes(x=Age,y=Est))+
      theme_bw()+
      ggtitle("Cohorte 2007")+
      xlab("Age")+
      ylab("Abondance (nb ind)")+
      geom_ribbon(aes(ymin=N.Inf,ymax=N.Sup),fill="lightgray")+
      geom_line(colour="black")+
      geom_point(aes(x=Age,y=Obs),colour = "red")+
      geom_errorbar(aes(ymin=Obs-(err.obs*Obs),ymax=Obs+(err.obs*Obs)),width=0.1)
    
    co.10 <- ggplot(table.Co.10,aes(x=Age,y=Est))+
      theme_bw()+
      ggtitle("Cohorte 2008")+
      xlab("Age")+
      ylab("Abondance (nb ind)")+
      geom_ribbon(aes(ymin=N.Inf,ymax=N.Sup),fill="lightgray")+
      geom_line(colour="black")+
      geom_point(aes(x=Age,y=Obs),colour = "red")+
      geom_errorbar(aes(ymin=Obs-(err.obs*Obs),ymax=Obs+(err.obs*Obs)),width=0.1)
    
    co.11 <- ggplot(table.Co.11,aes(x=Age,y=Est))+
      theme_bw()+
      ggtitle("Cohorte 2009")+
      xlab("Age")+
      ylab("Abondance (nb ind)")+
      geom_ribbon(aes(ymin=N.Inf,ymax=N.Sup),fill="lightgray")+
      geom_line(colour="black")+
      geom_point(aes(x=Age,y=Obs),colour = "red")+
      geom_errorbar(aes(ymin=Obs-(err.obs*Obs),ymax=Obs+(err.obs*Obs)),width=0.1)
    
    co.12 <- ggplot(table.Co.12,aes(x=Age,y=Est))+
      theme_bw()+
      ggtitle("Cohorte 2010")+
      xlab("Age")+
      ylab("Abondance (nb ind)")+
      geom_ribbon(aes(ymin=N.Inf,ymax=N.Sup),fill="lightgray")+
      geom_line(colour="black")+
      geom_point(aes(x=Age,y=Obs),colour = "red")+
      geom_errorbar(aes(ymin=Obs-(err.obs*Obs),ymax=Obs+(err.obs*Obs)),width=0.1)
    
    co.13 <- ggplot(table.Co.13,aes(x=Age,y=Est))+
      theme_bw()+
      ggtitle("Cohorte 2011")+
      xlab("Age")+
      ylab("Abondance (nb ind)")+
      geom_ribbon(aes(ymin=N.Inf,ymax=N.Sup),fill="lightgray")+
      geom_line(colour="black")+
      geom_point(aes(x=Age,y=Obs),colour = "red")+
      geom_errorbar(aes(ymin=Obs-(err.obs*Obs),ymax=Obs+(err.obs*Obs)),width=0.1)
    
    co.14 <- ggplot(table.Co.14,aes(x=Age,y=Est))+
      theme_bw()+
      ggtitle("Cohorte 2012")+
      xlab("Age")+
      ylab("Abondance (nb ind)")+
      geom_ribbon(aes(ymin=N.Inf,ymax=N.Sup),fill="lightgray")+
      geom_line(colour="black")+
      geom_point(aes(x=Age,y=Obs),colour = "red")+
      geom_errorbar(aes(ymin=Obs-(err.obs*Obs),ymax=Obs+(err.obs*Obs)),width=0.1)
    
    co.15 <- ggplot(table.Co.15,aes(x=Age,y=Est))+
      theme_bw()+
      ggtitle("Cohorte 2013")+
      xlab("Age")+
      ylab("Abondance (nb ind)")+
      geom_ribbon(aes(ymin=N.Inf,ymax=N.Sup),fill="lightgray")+
      geom_line(colour="black")+
      geom_point(aes(x=Age,y=Obs),colour = "red")+
      geom_errorbar(aes(ymin=Obs-(err.obs*Obs),ymax=Obs+(err.obs*Obs)),width=0.1)
    
    co.16 <- ggplot(table.Co.16,aes(x=Age,y=Est))+
      theme_bw()+
      ggtitle("Cohorte 2014")+
      xlab("Age")+
      ylab("Abondance (nb ind)")+
      geom_ribbon(aes(ymin=N.Inf,ymax=N.Sup),fill="lightgray")+
      geom_line(colour="black")+
      geom_point(aes(x=Age,y=Obs),colour = "red")+
      geom_errorbar(aes(ymin=Obs-(err.obs*Obs),ymax=Obs+(err.obs*Obs)),width=0.1)
    
    co.17 <- ggplot(table.Co.17,aes(x=Age,y=Est))+
      theme_bw()+
      ggtitle("Cohorte 2015")+
      xlab("Age")+
      ylab("Abondance (nb ind)")+
      geom_ribbon(aes(ymin=N.Inf,ymax=N.Sup),fill="lightgray")+
      geom_line(colour="black")+
      geom_point(aes(x=Age,y=Obs),colour = "red")+
      geom_errorbar(aes(ymin=Obs-(err.obs*Obs),ymax=Obs+(err.obs*Obs)),width=0.1)
    
    co.18 <- ggplot(table.Co.18,aes(x=Age,y=Est))+
      theme_bw()+
      ggtitle("Cohorte 2016")+
      xlab("Age")+
      ylab("Abondance (nb ind)")+
      geom_ribbon(aes(ymin=N.Inf,ymax=N.Sup),fill="lightgray")+
      geom_line(colour="black")+
      geom_point(aes(x=Age,y=Obs),colour = "red")+
      geom_errorbar(aes(ymin=Obs-(err.obs*Obs),ymax=Obs+(err.obs*Obs)),width=0.1)
    
    
    
    # print graphs
    resol <- 6
    
    name_figure <- paste("N_cohortes_",poisson,"_1.png")
    png(filename = name_figure, height = 320*resol, width = 680*resol, res=72*resol)
    
    def.par <- par(no.readonly = TRUE)
    
    grid.arrange(co.1,co.2, co.3,co.4,co.5,co.6,co.7,co.8,co.9, ncol=3, nrow=3)
    
    par(def.par)
    dev.off()
    
    name_figure <- paste("N_cohortes_",poisson,"_2.png")
    png(filename = name_figure, height = 320*resol, width = 680*resol, res=72*resol)
    
    def.par <- par(no.readonly = TRUE)
    
    grid.arrange(co.10,co.11,co.12, co.13,co.14,co.15,co.16,co.17,co.18, ncol=3, nrow=3)
    
    par(def.par)
    dev.off()  
    
    
    
    
    
  }  
  
  
  
  return()
}

