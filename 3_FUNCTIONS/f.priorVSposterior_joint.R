
# --------------------------------------------------------
#  Loi a priori VS a posteriori des mortalités par pêche
# --------------------------------------------------------

posterior<-function(name.mcmc.table,var.name,poisson)
{
  
  if(poisson=="anchovy")
  {
    data_SP<-data_anchovy
    mcmc_Z.SSN.pred = name.mcmc.table[,startsWith(varnames(name.mcmc.table),var.name)]
    mcmc_Z.SSN.pred = mcmc_Z.SSN.pred[,endsWith(varnames(mcmc_Z.SSN.pred),",1]")]
    mcmc_Z.SSN.pred.table <- as.data.frame(as.matrix(window(mcmc_Z.SSN.pred))) 
    
    
    resol<-6
    
    
    
    if(str_contains(var.name,"M")||str_contains(var.name,"F")||str_contains(var.name,"g")||str_contains(var.name,"Z.SSN")){
      
      prior.matrix<-data.frame(
        "var"=c("mu","sigma_0_max"),
        "M1"=c(0.733,5),
        "M2"=c(1.326,5),
        "M3"=c(1.326,5),
        "F1"=c(0.47,10),
        "F2"=c(0.47,10),
        "F3"=c(0.47,10),
        "g1"=c(1.1946,10),
        "g2"=c(1.0547,10),
        "g3"=c(1.0597,10),
        "Z.SSN"=c(0.707,5))
      
      su<-str_sub(var.name)
      
      log_mu_MouFoug = rnorm(1000,log(prior.matrix[[su]][1]),1/sqrt(0.1))
      sigma_MouFoug=runif(1000,0,prior.matrix[[su]][2])
      prior_MouFoug=rlnorm(1000,log_mu_MouFoug,sigma_MouFoug)
      
      png(filename = paste("posterior_",var.name,"_1to8_",poisson,".png"),width=600*resol, height=350*resol,res=72*resol)
      def.par <- par(no.readonly = TRUE)
      
      par(mfrow=c(2,4))
      
      for (i in 1:8){
        
        # taux de transition démographique Z.SSN au premier pas de temps
        par(mar=c(5,5,1,1), bty = "n")
        
        plot(density(mcmc_Z.SSN.pred.table[,i]),ylab = "", xlab="", xaxt="n", yaxt="n", main="",col = "red",type="l",lty = 1, lwd = 2)
        lines(density(prior_MouFoug),ylab = "", xlab="", xaxt="n", yaxt="n", main="",col = "blue",type="l",lty = 2, lwd = 2)
        #abline(v= median(mcmc.table$`Z.SSN[11]`), col = "red")
        axis(side = 1, tick = T, cex.axis = 1, las =1)
        axis(side = 2, tick = T, cex.axis = 1, labels = NA, lwd.ticks = 0)
        mtext(side=1,paste(var.name,"[",i,"]",poisson), bty="n", line=3)
        mtext(side=2,"densite", bty="n", line=3)
        legend(legend = c("Prior","Posterior"),
               col = c("blue","red"),
               lty = c(2,1),lwd = c(2,2), x = "topright",bty="n")
        
      }
      par(def.par)
      dev.off()
      
      
      png(filename = paste("posterior_",var.name,"_9to16_",poisson,".png"),width=600*resol, height=350*resol,res=72*resol)
      def.par <- par(no.readonly = TRUE)
      
      par(mfrow=c(2,4))
      
      for (i in 9:16){
        
        # taux de transition démographique Z.SSN au premier pas de temps
        par(mar=c(5,5,1,1), bty = "n")
        
        plot(density(mcmc_Z.SSN.pred.table[,i]),ylab = "", xlab="", xaxt="n", yaxt="n", main="",col = "red",type="l",lty = 1, lwd = 2)
        lines(density(prior_MouFoug),ylab = "", xlab="", xaxt="n", yaxt="n", main="",col = "blue",type="l",lty = 2, lwd = 2)
        #abline(v= median(mcmc.table$`Z.SSN[11]`), col = "red")
        axis(side = 1, tick = T, cex.axis = 1, las =1)
        axis(side = 2, tick = T, cex.axis = 1, labels = NA, lwd.ticks = 0)
        mtext(side=1,paste(var.name,"[",i,"]",poisson), bty="n", line=3)
        mtext(side=2,"densite", bty="n", line=3)
        legend(legend = c("Prior","Posterior"),
               col = c("blue","red"),
               lty = c(2,1),lwd = c(2,2), x = "topright",bty="n")
        
      }
      par(def.par)
      dev.off()
      
      
      png(filename = paste("posterior_",var.name,"_17to22_",poisson,".png"),width=600*resol, height=350*resol,res=72*resol)
      def.par <- par(no.readonly = TRUE)
      
      par(mfrow=c(2,4))
      
      for (i in 17:22){
        
        # taux de transition démographique Z.SSN au premier pas de temps
        par(mar=c(5,5,1,1), bty = "n")
        
        plot(density(mcmc_Z.SSN.pred.table[,i]),ylab = "", xlab="", xaxt="n", yaxt="n", main="",col = "red",type="l",lty = 1, lwd = 2)
        lines(density(prior_MouFoug),ylab = "", xlab="", xaxt="n", yaxt="n", main="",col = "blue",type="l",lty = 2, lwd = 2)
        #abline(v= median(mcmc.table$`Z.SSN[11]`), col = "red")
        axis(side = 1, tick = T, cex.axis = 1, las =1)
        axis(side = 2, tick = T, cex.axis = 1, labels = NA, lwd.ticks = 0)
        mtext(side=1,paste(var.name,"[",i,"]",poisson), bty="n", line=3)
        mtext(side=2,"densite", bty="n", line=3)
        legend(legend = c("Prior","Posterior"),
               col = c("blue","red"),
               lty = c(2,1),lwd = c(2,2), x = "topright",bty="n")
        
      }
      par(def.par)
      dev.off()
      
    }
    
    
    
    
    
    
    
    if((str_contains(var.name,"N") && (str_contains(var.name,"SSN")==FALSE))){
      prior.matrix<-data.frame(
        "var"=c("min.prior","max.prior"),
        "N1"=c((mean(data_SP$N1[1:5])/2)*0.39,(mean(data_SP$N1[1:5])*2)*0.39),  
        "N2"=c((mean(data_SP$N2[1:5])/2)*0.39,(mean(data_SP$N2[1:5])*2)*0.39),
        "N3"=c((mean(data_SP$N3[1:5])/2)*0.39,(mean(data_SP$N3[1:5])*2)*0.39),
        "N4"=c((mean(data_SP$N4[1:5])/2)*0.39,(mean(data_SP$N4[1:5])*2)*0.39))
      # 0.39 = q.prior      
      
      su<-str_sub(var.name)
      
      prior_init = runif(1000,prior.matrix[[su]][1],prior.matrix[[su]][2])      
      
      png(filename = paste("posterior_",var.name,"_1to8_",poisson,".png"),width=600*resol, height=350*resol,res=72*resol)
      def.par <- par(no.readonly = TRUE)
      
      par(mfrow=c(2,4))
      
      for (i in 1:1){
        
        # taux de transition démographique Z.SSN au premier pas de temps
        par(mar=c(5,5,1,1), bty = "n")
        
        plot(density(mcmc_Z.SSN.pred.table[,i]),ylab = "", xlab="", xaxt="n", yaxt="n", main="",col = "red",type="l",lty = 1, lwd = 2)
        lines(density(prior_init),ylab = "", xlab="", xaxt="n", yaxt="n", main="",col = "blue",type="l",lty = 2, lwd = 2)
        #abline(v= median(mcmc.table$`Z.SSN[11]`), col = "red")
        axis(side = 1, tick = T, cex.axis = 1, las =1)
        axis(side = 2, tick = T, cex.axis = 1, labels = NA, lwd.ticks = 0)
        mtext(side=1,paste(var.name,"[",i,"]",poisson), bty="n", line=3)
        mtext(side=2,"densite", bty="n", line=3)
        legend(legend = c("Prior","Posterior"),
               col = c("blue","red"),
               lty = c(2,1),lwd = c(2,2), x = "topright",bty="n")
        
      }
      for (i in 2:8){
        
        # taux de transition démographique Z.SSN au premier pas de temps
        par(mar=c(5,5,1,1), bty = "n")
        
        plot(density(mcmc_Z.SSN.pred.table[,i]),ylab = "", xlab="", xaxt="n", yaxt="n", main="",col = "red",type="l",lty = 1, lwd = 2)
        #lines(density(prior_N_init),ylab = "", xlab="", xaxt="n", yaxt="n", main="",col = "blue",type="l",lty = 2, lwd = 2)
        #abline(v= median(mcmc.table$`Z.SSN[11]`), col = "red")
        axis(side = 1, tick = T, cex.axis = 1, las =1)
        axis(side = 2, tick = T, cex.axis = 1, labels = NA, lwd.ticks = 0)
        mtext(side=1,paste(var.name,"[",i,"]",poisson), bty="n", line=3)
        mtext(side=2,"densite", bty="n", line=3)
        legend(legend = c("Prior","Posterior"),
               col = c("blue","red"),
               lty = c(2,1),lwd = c(2,2), x = "topright",bty="n")
        
      }
      par(def.par)
      dev.off()
      
      
      png(filename = paste("posterior_",var.name,"_9to16_",poisson,".png"),width=600*resol, height=350*resol,res=72*resol)
      def.par <- par(no.readonly = TRUE)
      
      par(mfrow=c(2,4))
      
      for (i in 9:16){
        
        # taux de transition démographique Z.SSN au premier pas de temps
        par(mar=c(5,5,1,1), bty = "n")
        
        plot(density(mcmc_Z.SSN.pred.table[,i]),ylab = "", xlab="", xaxt="n", yaxt="n", main="",col = "red",type="l",lty = 1, lwd = 2)
        #lines(density(prior_MouFoug),ylab = "", xlab="", xaxt="n", yaxt="n", main="",col = "blue",type="l",lty = 2, lwd = 2)
        #abline(v= median(mcmc.table$`Z.SSN[11]`), col = "red")
        axis(side = 1, tick = T, cex.axis = 1, las =1)
        axis(side = 2, tick = T, cex.axis = 1, labels = NA, lwd.ticks = 0)
        mtext(side=1,paste(var.name,"[",i,"]",poisson), bty="n", line=3)
        mtext(side=2,"densite", bty="n", line=3)
        legend(legend = c("Prior","Posterior"),
               col = c("blue","red"),
               lty = c(2,1),lwd = c(2,2), x = "topright",bty="n")
        
      }
      par(def.par)
      dev.off()
      
      
      png(filename = paste("posterior_",var.name,"_17to22_",poisson,".png"),width=600*resol, height=350*resol,res=72*resol)
      def.par <- par(no.readonly = TRUE)
      
      par(mfrow=c(2,4))
      
      for (i in 17:22){
        
        # taux de transition démographique Z.SSN au premier pas de temps
        par(mar=c(5,5,1,1), bty = "n")
        
        plot(density(mcmc_Z.SSN.pred.table[,i]),ylab = "", xlab="", xaxt="n", yaxt="n", main="",col = "red",type="l",lty = 1, lwd = 2)
        #lines(density(prior_MouFoug),ylab = "", xlab="", xaxt="n", yaxt="n", main="",col = "blue",type="l",lty = 2, lwd = 2)
        #abline(v= median(mcmc.table$`Z.SSN[11]`), col = "red")
        axis(side = 1, tick = T, cex.axis = 1, las =1)
        axis(side = 2, tick = T, cex.axis = 1, labels = NA, lwd.ticks = 0)
        mtext(side=1,paste(var.name,"[",i,"]",poisson), bty="n", line=3)
        mtext(side=2,"densite", bty="n", line=3)
        legend(legend = c("Prior","Posterior"),
               col = c("blue","red"),
               lty = c(2,1),lwd = c(2,2), x = "topright",bty="n")
        
      }
      par(def.par)
      dev.off()
      
    }
    
    
    
    
    
    
    
    
    
    if(str_contains(var.name,"L")){
      prior.matrix<-data.frame(
        "var"="prior",      
        "L1"=mean(data_SP$L1[c(19:20,22:23)]),
        "L2"=mean(data_SP$L2[1:5]),
        "L3"=mean(data_SP$L3[1:5]),
        "L4"=mean(data_SP$L4[1:5]))
      
      su<-str_sub(var.name)
      sigma.L=1
      prior_init = rlnorm(1000,log(prior.matrix[[su]][1]),sigma.L) 
      
      if(str_contains(var.name,"L1")){
        
        png(filename = paste("posterior_",var.name,"_1to8_",poisson,".png"),width=600*resol, height=350*resol,res=72*resol)
        def.par <- par(no.readonly = TRUE)
        
        par(mfrow=c(2,4))
        
        for (i in 1:8){
          
          # taux de transition démographique Z.SSN au premier pas de temps
          par(mar=c(5,5,1,1), bty = "n")
          
          plot(density(mcmc_Z.SSN.pred.table[,i]),ylab = "", xlab="", xaxt="n", yaxt="n", main="",col = "red",type="l",lty = 1, lwd = 2)
          #lines(density(prior_N_init),ylab = "", xlab="", xaxt="n", yaxt="n", main="",col = "blue",type="l",lty = 2, lwd = 2)
          #abline(v= median(mcmc.table$`Z.SSN[11]`), col = "red")
          axis(side = 1, tick = T, cex.axis = 1, las =1)
          axis(side = 2, tick = T, cex.axis = 1, labels = NA, lwd.ticks = 0)
          mtext(side=1,paste(var.name,"[",i,"]",poisson), bty="n", line=3)
          mtext(side=2,"densite", bty="n", line=3)
          legend(legend = c("Prior","Posterior"),
                 col = c("blue","red"),
                 lty = c(2,1),lwd = c(2,2), x = "topright",bty="n")
          
        }
        
        par(def.par)
        dev.off()
        
        
        png(filename = paste("posterior_",var.name,"_9to16_",poisson,".png"),width=600*resol, height=350*resol,res=72*resol)
        def.par <- par(no.readonly = TRUE)
        
        par(mfrow=c(2,4))
        
        for (i in 9:16){
          
          # taux de transition démographique Z.SSN au premier pas de temps
          par(mar=c(5,5,1,1), bty = "n")
          
          plot(density(mcmc_Z.SSN.pred.table[,i]),ylab = "", xlab="", xaxt="n", yaxt="n", main="",col = "red",type="l",lty = 1, lwd = 2)
          #lines(density(prior_MouFoug),ylab = "", xlab="", xaxt="n", yaxt="n", main="",col = "blue",type="l",lty = 2, lwd = 2)
          #abline(v= median(mcmc.table$`Z.SSN[11]`), col = "red")
          axis(side = 1, tick = T, cex.axis = 1, las =1)
          axis(side = 2, tick = T, cex.axis = 1, labels = NA, lwd.ticks = 0)
          mtext(side=1,paste(var.name,"[",i,"]",poisson), bty="n", line=3)
          mtext(side=2,"densite", bty="n", line=3)
          legend(legend = c("Prior","Posterior"),
                 col = c("blue","red"),
                 lty = c(2,1),lwd = c(2,2), x = "topright",bty="n")
          
        }
        par(def.par)
        dev.off()
        
        
        png(filename = paste("posterior_",var.name,"_17to22_",poisson,".png"),width=600*resol, height=350*resol,res=72*resol)
        def.par <- par(no.readonly = TRUE)
        
        par(mfrow=c(2,4))
        
        for (i in 17:21){
          
          # taux de transition démographique Z.SSN au premier pas de temps
          par(mar=c(5,5,1,1), bty = "n")
          
          plot(density(mcmc_Z.SSN.pred.table[,i]),ylab = "", xlab="", xaxt="n", yaxt="n", main="",col = "red",type="l",lty = 1, lwd = 2)
          #lines(density(prior_MouFoug),ylab = "", xlab="", xaxt="n", yaxt="n", main="",col = "blue",type="l",lty = 2, lwd = 2)
          #abline(v= median(mcmc.table$`Z.SSN[11]`), col = "red")
          axis(side = 1, tick = T, cex.axis = 1, las =1)
          axis(side = 2, tick = T, cex.axis = 1, labels = NA, lwd.ticks = 0)
          mtext(side=1,paste(var.name,"[",i,"]",poisson), bty="n", line=3)
          mtext(side=2,"densite", bty="n", line=3)
          legend(legend = c("Prior","Posterior"),
                 col = c("blue","red"),
                 lty = c(2,1),lwd = c(2,2), x = "topright",bty="n")
          
        }
        
        for (i in 22:22){
          
          # taux de transition démographique Z.SSN au premier pas de temps
          par(mar=c(5,5,1,1), bty = "n")
          
          plot(density(mcmc_Z.SSN.pred.table[,i]),ylab = "", xlab="", xaxt="n", yaxt="n", main="",col = "red",type="l",lty = 1, lwd = 2)
          lines(density(prior_init),ylab = "", xlab="", xaxt="n", yaxt="n", main="",col = "blue",type="l",lty = 2, lwd = 2)
          #abline(v= median(mcmc.table$`Z.SSN[11]`), col = "red")
          axis(side = 1, tick = T, cex.axis = 1, las =1)
          axis(side = 2, tick = T, cex.axis = 1, labels = NA, lwd.ticks = 0)
          mtext(side=1,paste(var.name,"[",i,"]",poisson), bty="n", line=3)
          mtext(side=2,"densite", bty="n", line=3)
          legend(legend = c("Prior","Posterior"),
                 col = c("blue","red"),
                 lty = c(2,1),lwd = c(2,2), x = "topright",bty="n")
          
        }
        
        par(def.par)
        dev.off()
        
      }else{
        
        png(filename = paste("posterior_",var.name,"_1to8_",poisson,".png"),width=600*resol, height=350*resol,res=72*resol)
        def.par <- par(no.readonly = TRUE)
        
        par(mfrow=c(2,4))
        
        for (i in 1:1){
          
          # taux de transition démographique Z.SSN au premier pas de temps
          par(mar=c(5,5,1,1), bty = "n")
          
          plot(density(mcmc_Z.SSN.pred.table[,i]),ylab = "", xlab="", xaxt="n", yaxt="n", main="",col = "red",type="l",lty = 1, lwd = 2)
          lines(density(prior_init),ylab = "", xlab="", xaxt="n", yaxt="n", main="",col = "blue",type="l",lty = 2, lwd = 2)
          #abline(v= median(mcmc.table$`Z.SSN[11]`), col = "red")
          axis(side = 1, tick = T, cex.axis = 1, las =1)
          axis(side = 2, tick = T, cex.axis = 1, labels = NA, lwd.ticks = 0)
          mtext(side=1,paste(var.name,"[",i,"]",poisson), bty="n", line=3)
          mtext(side=2,"densite", bty="n", line=3)
          legend(legend = c("Prior","Posterior"),
                 col = c("blue","red"),
                 lty = c(2,1),lwd = c(2,2), x = "topright",bty="n")
          
        }
        
        
        for (i in 2:8){
          
          # taux de transition démographique Z.SSN au premier pas de temps
          par(mar=c(5,5,1,1), bty = "n")
          
          plot(density(mcmc_Z.SSN.pred.table[,i]),ylab = "", xlab="", xaxt="n", yaxt="n", main="",col = "red",type="l",lty = 1, lwd = 2)
          #lines(density(prior_N_init),ylab = "", xlab="", xaxt="n", yaxt="n", main="",col = "blue",type="l",lty = 2, lwd = 2)
          #abline(v= median(mcmc.table$`Z.SSN[11]`), col = "red")
          axis(side = 1, tick = T, cex.axis = 1, las =1)
          axis(side = 2, tick = T, cex.axis = 1, labels = NA, lwd.ticks = 0)
          mtext(side=1,paste(var.name,"[",i,"]",poisson), bty="n", line=3)
          mtext(side=2,"densite", bty="n", line=3)
          legend(legend = c("Prior","Posterior"),
                 col = c("blue","red"),
                 lty = c(2,1),lwd = c(2,2), x = "topright",bty="n")
          
        }
        par(def.par)
        dev.off()
        
        
        png(filename = paste("posterior_",var.name,"_9to16_",poisson,".png"),width=600*resol, height=350*resol,res=72*resol)
        def.par <- par(no.readonly = TRUE)
        
        par(mfrow=c(2,4))
        
        for (i in 9:16){
          
          # taux de transition démographique Z.SSN au premier pas de temps
          par(mar=c(5,5,1,1), bty = "n")
          
          plot(density(mcmc_Z.SSN.pred.table[,i]),ylab = "", xlab="", xaxt="n", yaxt="n", main="",col = "red",type="l",lty = 1, lwd = 2)
          #lines(density(prior_MouFoug),ylab = "", xlab="", xaxt="n", yaxt="n", main="",col = "blue",type="l",lty = 2, lwd = 2)
          #abline(v= median(mcmc.table$`Z.SSN[11]`), col = "red")
          axis(side = 1, tick = T, cex.axis = 1, las =1)
          axis(side = 2, tick = T, cex.axis = 1, labels = NA, lwd.ticks = 0)
          mtext(side=1,paste(var.name,"[",i,"]",poisson), bty="n", line=3)
          mtext(side=2,"densite", bty="n", line=3)
          legend(legend = c("Prior","Posterior"),
                 col = c("blue","red"),
                 lty = c(2,1),lwd = c(2,2), x = "topright",bty="n")
          
        }
        par(def.par)
        dev.off()
        
        
        png(filename = paste("posterior_",var.name,"_17to22_",poisson,".png"),width=600*resol, height=350*resol,res=72*resol)
        def.par <- par(no.readonly = TRUE)
        
        par(mfrow=c(2,4))
        
        for (i in 17:22){
          
          # taux de transition démographique Z.SSN au premier pas de temps
          par(mar=c(5,5,1,1), bty = "n")
          
          plot(density(mcmc_Z.SSN.pred.table[,i]),ylab = "", xlab="", xaxt="n", yaxt="n", main="",col = "red",type="l",lty = 1, lwd = 2)
          #lines(density(prior_MouFoug),ylab = "", xlab="", xaxt="n", yaxt="n", main="",col = "blue",type="l",lty = 2, lwd = 2)
          #abline(v= median(mcmc.table$`Z.SSN[11]`), col = "red")
          axis(side = 1, tick = T, cex.axis = 1, las =1)
          axis(side = 2, tick = T, cex.axis = 1, labels = NA, lwd.ticks = 0)
          mtext(side=1,paste(var.name,"[",i,"]",poisson), bty="n", line=3)
          mtext(side=2,"densite", bty="n", line=3)
          legend(legend = c("Prior","Posterior"),
                 col = c("blue","red"),
                 lty = c(2,1),lwd = c(2,2), x = "topright",bty="n")
          
        }
        par(def.par)
        dev.off()
      }  #fin if (L1) {}else{}  
    } # fin if (L) {}
    
    if(str_contains(var.name,"N.SSN")||(str_contains(var.name,"Z")&&(str_contains(var.name,"Z.SSN")==FALSE))){
      
      png(filename = paste("posterior_",var.name,"_1to8.png"),width=600*resol, height=350*resol,res=72*resol)
      def.par <- par(no.readonly = TRUE)
      
      par(mfrow=c(2,4))
      
      for (i in 1:8){
        
        # taux de transition démographique Z.SSN au premier pas de temps
        par(mar=c(5,5,1,1), bty = "n")
        
        plot(density(mcmc_Z.SSN.pred.table[,i]),ylab = "", xlab="", xaxt="n", yaxt="n", main="",col = "red",type="l",lty = 1, lwd = 2)
        #lines(density(prior_MouFoug),ylab = "", xlab="", xaxt="n", yaxt="n", main="",col = "blue",type="l",lty = 2, lwd = 2)
        #abline(v= median(mcmc.table$`Z.SSN[11]`), col = "red")
        axis(side = 1, tick = T, cex.axis = 1, las =1)
        axis(side = 2, tick = T, cex.axis = 1, labels = NA, lwd.ticks = 0)
        mtext(side=1,paste(var.name,"[",i,"]",poisson), bty="n", line=3)
        mtext(side=2,"densite", bty="n", line=3)
        legend(legend = c("Prior","Posterior"),
               col = c("blue","red"),
               lty = c(2,1),lwd = c(2,2), x = "topright",bty="n")
        
      }
      par(def.par)
      dev.off()
      
      
      png(filename = paste("posterior_",var.name,"_9to16_",poisson,".png"),width=600*resol, height=350*resol,res=72*resol)
      def.par <- par(no.readonly = TRUE)
      
      par(mfrow=c(2,4))
      
      for (i in 9:16){
        
        # taux de transition démographique Z.SSN au premier pas de temps
        par(mar=c(5,5,1,1), bty = "n")
        
        plot(density(mcmc_Z.SSN.pred.table[,i]),ylab = "", xlab="", xaxt="n", yaxt="n", main="",col = "red",type="l",lty = 1, lwd = 2)
        #lines(density(prior_MouFoug),ylab = "", xlab="", xaxt="n", yaxt="n", main="",col = "blue",type="l",lty = 2, lwd = 2)
        #abline(v= median(mcmc.table$`Z.SSN[11]`), col = "red")
        axis(side = 1, tick = T, cex.axis = 1, las =1)
        axis(side = 2, tick = T, cex.axis = 1, labels = NA, lwd.ticks = 0)
        mtext(side=1,paste(var.name,"[",i,"]",poisson), bty="n", line=3)
        mtext(side=2,"densite", bty="n", line=3)
        legend(legend = c("Prior","Posterior"),
               col = c("blue","red"),
               lty = c(2,1),lwd = c(2,2), x = "topright",bty="n")
        
      }
      par(def.par)
      dev.off()
      
      
      png(filename = paste("posterior_",var.name,"_17to22_",poisson,".png"),width=600*resol, height=350*resol,res=72*resol)
      def.par <- par(no.readonly = TRUE)
      
      par(mfrow=c(2,4))
      
      for (i in 17:22){
        
        # taux de transition démographique Z.SSN au premier pas de temps
        par(mar=c(5,5,1,1), bty = "n")
        
        plot(density(mcmc_Z.SSN.pred.table[,i]),ylab = "", xlab="", xaxt="n", yaxt="n", main="",col = "red",type="l",lty = 1, lwd = 2)
        #lines(density(prior_MouFoug),ylab = "", xlab="", xaxt="n", yaxt="n", main="",col = "blue",type="l",lty = 2, lwd = 2)
        #abline(v= median(mcmc.table$`Z.SSN[11]`), col = "red")
        axis(side = 1, tick = T, cex.axis = 1, las =1)
        axis(side = 2, tick = T, cex.axis = 1, labels = NA, lwd.ticks = 0)
        mtext(side=1,paste(var.name,"[",i,"]",poisson), bty="n", line=3)
        mtext(side=2,"densite", bty="n", line=3)
        legend(legend = c("Prior","Posterior"),
               col = c("blue","red"),
               lty = c(2,1),lwd = c(2,2), x = "topright",bty="n")
        
      }
      par(def.par)
      dev.off()
    }
    
  } #end if (anchovy)
  
  #-------------------------------------------------------------------------------------  
  
  if(poisson=="sardine")
  {
    data_SP<-data_sardine
    mcmc_Z.SSN.pred = name.mcmc.table[,startsWith(varnames(name.mcmc.table),var.name)]
    mcmc_Z.SSN.pred = mcmc_Z.SSN.pred[,endsWith(varnames(mcmc_Z.SSN.pred),",2]")]
    mcmc_Z.SSN.pred.table <- as.data.frame(as.matrix(window(mcmc_Z.SSN.pred))) 
    
    
    resol<-6
    
    
    
    if(str_contains(var.name,"M")||str_contains(var.name,"F")||str_contains(var.name,"g")||str_contains(var.name,"Z.SSN")){
      
      prior.matrix<-data.frame(
        "var"=c("mu","sigma_0_max"),
        "M1"=c(0.691,5),
        "M2"=c(0.546,5),
        "M3"=c(0.475,5),
        "M4"=c(0.436,5),
        "M5"=c(0.412,5),
        "M6"=c(0.398,5),
        "F1"=c(0.35,10),
        "F2"=c(0.35,10),
        "F3"=c(0.35,10),
        "F4"=c(0.35,10),
        "F5"=c(0.35,10),
        "F6"=c(0.35,10),
        "g1"=c(1.2097,10),
        "g2"=c(1.0709,10),
        "g3"=c(1.0398,10),
        "g4"=c(1.0349,10),
        "g5"=c(1.0288,10),
        "g6"=c(1.0181,10),
        "Z.SSN"=c(0.444,5))
      
      su<-str_sub(var.name)
      
      log_mu_MouFoug = rnorm(1000,log(prior.matrix[[su]][1]),1/sqrt(0.1))
      sigma_MouFoug=runif(1000,0,prior.matrix[[su]][2])
      prior_MouFoug=rlnorm(1000,log_mu_MouFoug,sigma_MouFoug)
      
      png(filename = paste("posterior_",var.name,"_1to8_",poisson,".png"),width=600*resol, height=350*resol,res=72*resol)
      def.par <- par(no.readonly = TRUE)
      
      par(mfrow=c(2,4))
      
      for (i in 1:8){
        
        # taux de transition démographique Z.SSN au premier pas de temps
        par(mar=c(5,5,1,1), bty = "n")
        
        plot(density(mcmc_Z.SSN.pred.table[,i]),ylab = "", xlab="", xaxt="n", yaxt="n", main="",col = "red",type="l",lty = 1, lwd = 2)
        lines(density(prior_MouFoug),ylab = "", xlab="", xaxt="n", yaxt="n", main="",col = "blue",type="l",lty = 2, lwd = 2)
        #abline(v= median(mcmc.table$`Z.SSN[11]`), col = "red")
        axis(side = 1, tick = T, cex.axis = 1, las =1)
        axis(side = 2, tick = T, cex.axis = 1, labels = NA, lwd.ticks = 0)
        mtext(side=1,paste(var.name,"[",i,"]",poisson), bty="n", line=3)
        mtext(side=2,"densite", bty="n", line=3)
        legend(legend = c("Prior","Posterior"),
               col = c("blue","red"),
               lty = c(2,1),lwd = c(2,2), x = "topright",bty="n")
        
      }
      par(def.par)
      dev.off()
      
      
      png(filename = paste("posterior_",var.name,"_9to16_",poisson,".png"),width=600*resol, height=350*resol,res=72*resol)
      def.par <- par(no.readonly = TRUE)
      
      par(mfrow=c(2,4))
      
      for (i in 9:16){
        
        # taux de transition démographique Z.SSN au premier pas de temps
        par(mar=c(5,5,1,1), bty = "n")
        
        plot(density(mcmc_Z.SSN.pred.table[,i]),ylab = "", xlab="", xaxt="n", yaxt="n", main="",col = "red",type="l",lty = 1, lwd = 2)
        lines(density(prior_MouFoug),ylab = "", xlab="", xaxt="n", yaxt="n", main="",col = "blue",type="l",lty = 2, lwd = 2)
        #abline(v= median(mcmc.table$`Z.SSN[11]`), col = "red")
        axis(side = 1, tick = T, cex.axis = 1, las =1)
        axis(side = 2, tick = T, cex.axis = 1, labels = NA, lwd.ticks = 0)
        mtext(side=1,paste(var.name,"[",i,"]",poisson), bty="n", line=3)
        mtext(side=2,"densite", bty="n", line=3)
        legend(legend = c("Prior","Posterior"),
               col = c("blue","red"),
               lty = c(2,1),lwd = c(2,2), x = "topright",bty="n")
        
      }
      par(def.par)
      dev.off()
      
      
      png(filename = paste("posterior_",var.name,"_17to22_",poisson,".png"),width=600*resol, height=350*resol,res=72*resol)
      def.par <- par(no.readonly = TRUE)
      
      par(mfrow=c(2,4))
      
      for (i in 17:22){
        
        # taux de transition démographique Z.SSN au premier pas de temps
        par(mar=c(5,5,1,1), bty = "n")
        
        plot(density(mcmc_Z.SSN.pred.table[,i]),ylab = "", xlab="", xaxt="n", yaxt="n", main="",col = "red",type="l",lty = 1, lwd = 2)
        lines(density(prior_MouFoug),ylab = "", xlab="", xaxt="n", yaxt="n", main="",col = "blue",type="l",lty = 2, lwd = 2)
        #abline(v= median(mcmc.table$`Z.SSN[11]`), col = "red")
        axis(side = 1, tick = T, cex.axis = 1, las =1)
        axis(side = 2, tick = T, cex.axis = 1, labels = NA, lwd.ticks = 0)
        mtext(side=1,paste(var.name,"[",i,"]",poisson), bty="n", line=3)
        mtext(side=2,"densite", bty="n", line=3)
        legend(legend = c("Prior","Posterior"),
               col = c("blue","red"),
               lty = c(2,1),lwd = c(2,2), x = "topright",bty="n")
        
      }
      par(def.par)
      dev.off()
      
    }
    
    
    
    
    
    
    
    if((str_contains(var.name,"N") && (str_contains(var.name,"SSN")==FALSE))){
      prior.matrix<-data.frame(
        "var"=c("min.prior","max.prior"),
        "N1"=c((mean(data_SP$N1[1:5])/2)*0.43,(mean(data_SP$N1[1:5])*2)*0.43),  
        "N2"=c((mean(data_SP$N2[1:5])/2)*0.43,(mean(data_SP$N2[1:5])*2)*0.43),
        "N3"=c((mean(data_SP$N3[1:5])/2)*0.43,(mean(data_SP$N3[1:5])*2)*0.43),
        "N4"=c((mean(data_SP$N4[1:5])/2)*0.43,(mean(data_SP$N3[1:5])*2)*0.43),
        "N5"=c((mean(data_SP$N5[1:5])/2)*0.43,(mean(data_SP$N3[1:5])*2)*0.43),
        "N6"=c((mean(data_SP$N6[1:5])/2)*0.43,(mean(data_SP$N6[1:5])*2)*0.43),
        "N7"=c((mean(data_SP$N7[1:5])/2)*0.43,(mean(data_SP$N7[1:5])*2)*0.43))
      # 0.39 = q.prior      
      
      su<-str_sub(var.name)
      
      prior_init = runif(1000,prior.matrix[[su]][1],prior.matrix[[su]][2])      
      
      png(filename = paste("posterior_",var.name,"_1to8_",poisson,".png"),width=600*resol, height=350*resol,res=72*resol)
      def.par <- par(no.readonly = TRUE)
      
      par(mfrow=c(2,4))
      
      for (i in 1:1){
        
        # taux de transition démographique Z.SSN au premier pas de temps
        par(mar=c(5,5,1,1), bty = "n")
        
        plot(density(mcmc_Z.SSN.pred.table[,i]),ylab = "", xlab="", xaxt="n", yaxt="n", main="",col = "red",type="l",lty = 1, lwd = 2)
        lines(density(prior_init),ylab = "", xlab="", xaxt="n", yaxt="n", main="",col = "blue",type="l",lty = 2, lwd = 2)
        #abline(v= median(mcmc.table$`Z.SSN[11]`), col = "red")
        axis(side = 1, tick = T, cex.axis = 1, las =1)
        axis(side = 2, tick = T, cex.axis = 1, labels = NA, lwd.ticks = 0)
        mtext(side=1,paste(var.name,"[",i,"]",poisson), bty="n", line=3)
        mtext(side=2,"densite", bty="n", line=3)
        legend(legend = c("Prior","Posterior"),
               col = c("blue","red"),
               lty = c(2,1),lwd = c(2,2), x = "topright",bty="n")
        
      }
      for (i in 2:8){
        
        # taux de transition démographique Z.SSN au premier pas de temps
        par(mar=c(5,5,1,1), bty = "n")
        
        plot(density(mcmc_Z.SSN.pred.table[,i]),ylab = "", xlab="", xaxt="n", yaxt="n", main="",col = "red",type="l",lty = 1, lwd = 2)
        #lines(density(prior_N_init),ylab = "", xlab="", xaxt="n", yaxt="n", main="",col = "blue",type="l",lty = 2, lwd = 2)
        #abline(v= median(mcmc.table$`Z.SSN[11]`), col = "red")
        axis(side = 1, tick = T, cex.axis = 1, las =1)
        axis(side = 2, tick = T, cex.axis = 1, labels = NA, lwd.ticks = 0)
        mtext(side=1,paste(var.name,"[",i,"]",poisson), bty="n", line=3)
        mtext(side=2,"densite", bty="n", line=3)
        legend(legend = c("Prior","Posterior"),
               col = c("blue","red"),
               lty = c(2,1),lwd = c(2,2), x = "topright",bty="n")
        
      }
      par(def.par)
      dev.off()
      
      
      png(filename = paste("posterior_",var.name,"_9to16_",poisson,".png"),width=600*resol, height=350*resol,res=72*resol)
      def.par <- par(no.readonly = TRUE)
      
      par(mfrow=c(2,4))
      
      for (i in 9:16){
        
        # taux de transition démographique Z.SSN au premier pas de temps
        par(mar=c(5,5,1,1), bty = "n")
        
        plot(density(mcmc_Z.SSN.pred.table[,i]),ylab = "", xlab="", xaxt="n", yaxt="n", main="",col = "red",type="l",lty = 1, lwd = 2)
        #lines(density(prior_MouFoug),ylab = "", xlab="", xaxt="n", yaxt="n", main="",col = "blue",type="l",lty = 2, lwd = 2)
        #abline(v= median(mcmc.table$`Z.SSN[11]`), col = "red")
        axis(side = 1, tick = T, cex.axis = 1, las =1)
        axis(side = 2, tick = T, cex.axis = 1, labels = NA, lwd.ticks = 0)
        mtext(side=1,paste(var.name,"[",i,"]",poisson), bty="n", line=3)
        mtext(side=2,"densite", bty="n", line=3)
        legend(legend = c("Prior","Posterior"),
               col = c("blue","red"),
               lty = c(2,1),lwd = c(2,2), x = "topright",bty="n")
        
      }
      par(def.par)
      dev.off()
      
      
      png(filename = paste("posterior_",var.name,"_17to22_",poisson,".png"),width=600*resol, height=350*resol,res=72*resol)
      def.par <- par(no.readonly = TRUE)
      
      par(mfrow=c(2,4))
      
      for (i in 17:22){
        
        # taux de transition démographique Z.SSN au premier pas de temps
        par(mar=c(5,5,1,1), bty = "n")
        
        plot(density(mcmc_Z.SSN.pred.table[,i]),ylab = "", xlab="", xaxt="n", yaxt="n", main="",col = "red",type="l",lty = 1, lwd = 2)
        #lines(density(prior_MouFoug),ylab = "", xlab="", xaxt="n", yaxt="n", main="",col = "blue",type="l",lty = 2, lwd = 2)
        #abline(v= median(mcmc.table$`Z.SSN[11]`), col = "red")
        axis(side = 1, tick = T, cex.axis = 1, las =1)
        axis(side = 2, tick = T, cex.axis = 1, labels = NA, lwd.ticks = 0)
        mtext(side=1,paste(var.name,"[",i,"]",poisson), bty="n", line=3)
        mtext(side=2,"densite", bty="n", line=3)
        legend(legend = c("Prior","Posterior"),
               col = c("blue","red"),
               lty = c(2,1),lwd = c(2,2), x = "topright",bty="n")
        
      }
      par(def.par)
      dev.off()
      
    }
    
    
    
    
    
    
    
    
    
    if(str_contains(var.name,"L")){
      prior.matrix<-data.frame(
        "var"="prior",      
        "L1"=mean(data_SP$L1[c(19:20,22:23)]),
        "L2"=mean(data_SP$L2[1:5]),
        "L3"=mean(data_SP$L3[1:5]),
        "L4"=mean(data_SP$L4[1:5]),
        "L5"=mean(data_SP$L5[1:5]),
        "L6"=mean(data_SP$L6[1:5]),
        "L7"=mean(data_SP$L7[1:5]))
      
      su<-str_sub(var.name)
      sigma.L=1
      prior_init = rlnorm(1000,log(prior.matrix[[su]][1]),sigma.L) 
      
      if(str_contains(var.name,"L1")){
        
        png(filename = paste("posterior_",var.name,"_1to8_",poisson,".png"),width=600*resol, height=350*resol,res=72*resol)
        def.par <- par(no.readonly = TRUE)
        
        par(mfrow=c(2,4))
        
        for (i in 1:8){
          
          # taux de transition démographique Z.SSN au premier pas de temps
          par(mar=c(5,5,1,1), bty = "n")
          
          plot(density(mcmc_Z.SSN.pred.table[,i]),ylab = "", xlab="", xaxt="n", yaxt="n", main="",col = "red",type="l",lty = 1, lwd = 2)
          #lines(density(prior_N_init),ylab = "", xlab="", xaxt="n", yaxt="n", main="",col = "blue",type="l",lty = 2, lwd = 2)
          #abline(v= median(mcmc.table$`Z.SSN[11]`), col = "red")
          axis(side = 1, tick = T, cex.axis = 1, las =1)
          axis(side = 2, tick = T, cex.axis = 1, labels = NA, lwd.ticks = 0)
          mtext(side=1,paste(var.name,"[",i,"]",poisson), bty="n", line=3)
          mtext(side=2,"densite", bty="n", line=3)
          legend(legend = c("Prior","Posterior"),
                 col = c("blue","red"),
                 lty = c(2,1),lwd = c(2,2), x = "topright",bty="n")
          
        }
        
        par(def.par)
        dev.off()
        
        
        png(filename = paste("posterior_",var.name,"_9to16_",poisson,".png"),width=600*resol, height=350*resol,res=72*resol)
        def.par <- par(no.readonly = TRUE)
        
        par(mfrow=c(2,4))
        
        for (i in 9:16){
          
          # taux de transition démographique Z.SSN au premier pas de temps
          par(mar=c(5,5,1,1), bty = "n")
          
          plot(density(mcmc_Z.SSN.pred.table[,i]),ylab = "", xlab="", xaxt="n", yaxt="n", main="",col = "red",type="l",lty = 1, lwd = 2)
          #lines(density(prior_MouFoug),ylab = "", xlab="", xaxt="n", yaxt="n", main="",col = "blue",type="l",lty = 2, lwd = 2)
          #abline(v= median(mcmc.table$`Z.SSN[11]`), col = "red")
          axis(side = 1, tick = T, cex.axis = 1, las =1)
          axis(side = 2, tick = T, cex.axis = 1, labels = NA, lwd.ticks = 0)
          mtext(side=1,paste(var.name,"[",i,"]",poisson), bty="n", line=3)
          mtext(side=2,"densite", bty="n", line=3)
          legend(legend = c("Prior","Posterior"),
                 col = c("blue","red"),
                 lty = c(2,1),lwd = c(2,2), x = "topright",bty="n")
          
        }
        par(def.par)
        dev.off()
        
        
        png(filename = paste("posterior_",var.name,"_17to22_",poisson,".png"),width=600*resol, height=350*resol,res=72*resol)
        def.par <- par(no.readonly = TRUE)
        
        par(mfrow=c(2,4))
        
        for (i in 17:21){
          
          # taux de transition démographique Z.SSN au premier pas de temps
          par(mar=c(5,5,1,1), bty = "n")
          
          plot(density(mcmc_Z.SSN.pred.table[,i]),ylab = "", xlab="", xaxt="n", yaxt="n", main="",col = "red",type="l",lty = 1, lwd = 2)
          #lines(density(prior_MouFoug),ylab = "", xlab="", xaxt="n", yaxt="n", main="",col = "blue",type="l",lty = 2, lwd = 2)
          #abline(v= median(mcmc.table$`Z.SSN[11]`), col = "red")
          axis(side = 1, tick = T, cex.axis = 1, las =1)
          axis(side = 2, tick = T, cex.axis = 1, labels = NA, lwd.ticks = 0)
          mtext(side=1,paste(var.name,"[",i,"]",poisson), bty="n", line=3)
          mtext(side=2,"densite", bty="n", line=3)
          legend(legend = c("Prior","Posterior"),
                 col = c("blue","red"),
                 lty = c(2,1),lwd = c(2,2), x = "topright",bty="n")
          
        }
        
        for (i in 22:22){
          
          # taux de transition démographique Z.SSN au premier pas de temps
          par(mar=c(5,5,1,1), bty = "n")
          
          plot(density(mcmc_Z.SSN.pred.table[,i]),ylab = "", xlab="", xaxt="n", yaxt="n", main="",col = "red",type="l",lty = 1, lwd = 2)
          lines(density(prior_init),ylab = "", xlab="", xaxt="n", yaxt="n", main="",col = "blue",type="l",lty = 2, lwd = 2)
          #abline(v= median(mcmc.table$`Z.SSN[11]`), col = "red")
          axis(side = 1, tick = T, cex.axis = 1, las =1)
          axis(side = 2, tick = T, cex.axis = 1, labels = NA, lwd.ticks = 0)
          mtext(side=1,paste(var.name,"[",i,"]",poisson), bty="n", line=3)
          mtext(side=2,"densite", bty="n", line=3)
          legend(legend = c("Prior","Posterior"),
                 col = c("blue","red"),
                 lty = c(2,1),lwd = c(2,2), x = "topright",bty="n")
          
        }
        
        par(def.par)
        dev.off()
        
      }else{
        
        png(filename = paste("posterior_",var.name,"_1to8_",poisson,".png"),width=600*resol, height=350*resol,res=72*resol)
        def.par <- par(no.readonly = TRUE)
        
        par(mfrow=c(2,4))
        
        for (i in 1:1){
          
          # taux de transition démographique Z.SSN au premier pas de temps
          par(mar=c(5,5,1,1), bty = "n")
          
          plot(density(mcmc_Z.SSN.pred.table[,i]),ylab = "", xlab="", xaxt="n", yaxt="n", main="",col = "red",type="l",lty = 1, lwd = 2)
          lines(density(prior_init),ylab = "", xlab="", xaxt="n", yaxt="n", main="",col = "blue",type="l",lty = 2, lwd = 2)
          #abline(v= median(mcmc.table$`Z.SSN[11]`), col = "red")
          axis(side = 1, tick = T, cex.axis = 1, las =1)
          axis(side = 2, tick = T, cex.axis = 1, labels = NA, lwd.ticks = 0)
          mtext(side=1,paste(var.name,"[",i,"]",poisson), bty="n", line=3)
          mtext(side=2,"densite", bty="n", line=3)
          legend(legend = c("Prior","Posterior"),
                 col = c("blue","red"),
                 lty = c(2,1),lwd = c(2,2), x = "topright",bty="n")
          
        }
        
        
        for (i in 2:8){
          
          # taux de transition démographique Z.SSN au premier pas de temps
          par(mar=c(5,5,1,1), bty = "n")
          
          plot(density(mcmc_Z.SSN.pred.table[,i]),ylab = "", xlab="", xaxt="n", yaxt="n", main="",col = "red",type="l",lty = 1, lwd = 2)
          #lines(density(prior_N_init),ylab = "", xlab="", xaxt="n", yaxt="n", main="",col = "blue",type="l",lty = 2, lwd = 2)
          #abline(v= median(mcmc.table$`Z.SSN[11]`), col = "red")
          axis(side = 1, tick = T, cex.axis = 1, las =1)
          axis(side = 2, tick = T, cex.axis = 1, labels = NA, lwd.ticks = 0)
          mtext(side=1,paste(var.name,"[",i,"]",poisson), bty="n", line=3)
          mtext(side=2,"densite", bty="n", line=3)
          legend(legend = c("Prior","Posterior"),
                 col = c("blue","red"),
                 lty = c(2,1),lwd = c(2,2), x = "topright",bty="n")
          
        }
        par(def.par)
        dev.off()
        
        
        png(filename = paste("posterior_",var.name,"_9to16_",poisson,".png"),width=600*resol, height=350*resol,res=72*resol)
        def.par <- par(no.readonly = TRUE)
        
        par(mfrow=c(2,4))
        
        for (i in 9:16){
          
          # taux de transition démographique Z.SSN au premier pas de temps
          par(mar=c(5,5,1,1), bty = "n")
          
          plot(density(mcmc_Z.SSN.pred.table[,i]),ylab = "", xlab="", xaxt="n", yaxt="n", main="",col = "red",type="l",lty = 1, lwd = 2)
          #lines(density(prior_MouFoug),ylab = "", xlab="", xaxt="n", yaxt="n", main="",col = "blue",type="l",lty = 2, lwd = 2)
          #abline(v= median(mcmc.table$`Z.SSN[11]`), col = "red")
          axis(side = 1, tick = T, cex.axis = 1, las =1)
          axis(side = 2, tick = T, cex.axis = 1, labels = NA, lwd.ticks = 0)
          mtext(side=1,paste(var.name,"[",i,"]",poisson), bty="n", line=3)
          mtext(side=2,"densite", bty="n", line=3)
          legend(legend = c("Prior","Posterior"),
                 col = c("blue","red"),
                 lty = c(2,1),lwd = c(2,2), x = "topright",bty="n")
          
        }
        par(def.par)
        dev.off()
        
        
        png(filename = paste("posterior_",var.name,"_17to22_",poisson,".png"),width=600*resol, height=350*resol,res=72*resol)
        def.par <- par(no.readonly = TRUE)
        
        par(mfrow=c(2,4))
        
        for (i in 17:22){
          
          # taux de transition démographique Z.SSN au premier pas de temps
          par(mar=c(5,5,1,1), bty = "n")
          
          plot(density(mcmc_Z.SSN.pred.table[,i]),ylab = "", xlab="", xaxt="n", yaxt="n", main="",col = "red",type="l",lty = 1, lwd = 2)
          #lines(density(prior_MouFoug),ylab = "", xlab="", xaxt="n", yaxt="n", main="",col = "blue",type="l",lty = 2, lwd = 2)
          #abline(v= median(mcmc.table$`Z.SSN[11]`), col = "red")
          axis(side = 1, tick = T, cex.axis = 1, las =1)
          axis(side = 2, tick = T, cex.axis = 1, labels = NA, lwd.ticks = 0)
          mtext(side=1,paste(var.name,"[",i,"]",poisson), bty="n", line=3)
          mtext(side=2,"densite", bty="n", line=3)
          legend(legend = c("Prior","Posterior"),
                 col = c("blue","red"),
                 lty = c(2,1),lwd = c(2,2), x = "topright",bty="n")
          
        }
        par(def.par)
        dev.off()
      }  #fin if (L1) {}else{}  
    } # fin if (L) {}
    
    if(str_contains(var.name,"N.SSN")||(str_contains(var.name,"Z")&&(str_contains(var.name,"Z.SSN")==FALSE))){   
      
      png(filename = paste("posterior_",var.name,"_1to8_",poisson,".png"),width=600*resol, height=350*resol,res=72*resol)
      def.par <- par(no.readonly = TRUE)
      
      par(mfrow=c(2,4))
      
      for (i in 1:8){
        
        # taux de transition démographique Z.SSN au premier pas de temps
        par(mar=c(5,5,1,1), bty = "n")
        
        plot(density(mcmc_Z.SSN.pred.table[,i]),ylab = "", xlab="", xaxt="n", yaxt="n", main="",col = "red",type="l",lty = 1, lwd = 2)
        #lines(density(prior_MouFoug),ylab = "", xlab="", xaxt="n", yaxt="n", main="",col = "blue",type="l",lty = 2, lwd = 2)
        #abline(v= median(mcmc.table$`Z.SSN[11]`), col = "red")
        axis(side = 1, tick = T, cex.axis = 1, las =1)
        axis(side = 2, tick = T, cex.axis = 1, labels = NA, lwd.ticks = 0)
        mtext(side=1,paste(var.name,"[",i,"]",poisson), bty="n", line=3)
        mtext(side=2,"densite", bty="n", line=3)
        legend(legend = c("Prior","Posterior"),
               col = c("blue","red"),
               lty = c(2,1),lwd = c(2,2), x = "topright",bty="n")
        
      }
      par(def.par)
      dev.off()
      
      
      png(filename = paste("posterior_",var.name,"_9to16_",poisson,".png"),width=600*resol, height=350*resol,res=72*resol)
      def.par <- par(no.readonly = TRUE)
      
      par(mfrow=c(2,4))
      
      for (i in 9:16){
        
        # taux de transition démographique Z.SSN au premier pas de temps
        par(mar=c(5,5,1,1), bty = "n")
        
        plot(density(mcmc_Z.SSN.pred.table[,i]),ylab = "", xlab="", xaxt="n", yaxt="n", main="",col = "red",type="l",lty = 1, lwd = 2)
        #lines(density(prior_MouFoug),ylab = "", xlab="", xaxt="n", yaxt="n", main="",col = "blue",type="l",lty = 2, lwd = 2)
        #abline(v= median(mcmc.table$`Z.SSN[11]`), col = "red")
        axis(side = 1, tick = T, cex.axis = 1, las =1)
        axis(side = 2, tick = T, cex.axis = 1, labels = NA, lwd.ticks = 0)
        mtext(side=1,paste(var.name,"[",i,"]",poisson), bty="n", line=3)
        mtext(side=2,"densite", bty="n", line=3)
        legend(legend = c("Prior","Posterior"),
               col = c("blue","red"),
               lty = c(2,1),lwd = c(2,2), x = "topright",bty="n")
        
      }
      par(def.par)
      dev.off()
      
      
      png(filename = paste("posterior_",var.name,"_17to22_",poisson,".png"),width=600*resol, height=350*resol,res=72*resol)
      def.par <- par(no.readonly = TRUE)
      
      par(mfrow=c(2,4))
      
      for (i in 17:22){
        
        # taux de transition démographique Z.SSN au premier pas de temps
        par(mar=c(5,5,1,1), bty = "n")
        
        plot(density(mcmc_Z.SSN.pred.table[,i]),ylab = "", xlab="", xaxt="n", yaxt="n", main="",col = "red",type="l",lty = 1, lwd = 2)
        #lines(density(prior_MouFoug),ylab = "", xlab="", xaxt="n", yaxt="n", main="",col = "blue",type="l",lty = 2, lwd = 2)
        #abline(v= median(mcmc.table$`Z.SSN[11]`), col = "red")
        axis(side = 1, tick = T, cex.axis = 1, las =1)
        axis(side = 2, tick = T, cex.axis = 1, labels = NA, lwd.ticks = 0)
        mtext(side=1,paste(var.name,"[",i,"]","anch",poisson), bty="n", line=3)
        mtext(side=2,"densite", bty="n", line=3)
        legend(legend = c("Prior","Posterior"),
               col = c("blue","red"),
               lty = c(2,1),lwd = c(2,2), x = "topright",bty="n")
        
      }
      par(def.par)
      dev.off()
    }
    
  }
  
  
  
  return()
  
}
