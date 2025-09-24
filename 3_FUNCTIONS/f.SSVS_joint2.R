SSVS.2 <- function(mcmc.object,var.vect,poisson){
    
  #collecting outputs : mcmc draws
  
  mcmc.table <- as.data.frame(as.matrix(window(mcmc.object)))
  outdf <- ggs(as.mcmc.list(mcmc.object))
  out_summary <- summary(mcmc.object)
  #out_summary$statistics : to obtain the Mean of the mcmc draws --> p_inclusion is either 0 or 1 for each iteration, 
  #so we need the posterior frequecy of inclusion = mean of all the "1" (VS the median of p_inclusion is either 0 or 1)
  out_summary<-as.data.frame(out_summary$statistics)
  
  collect<-c()
  
  if(poisson=="anchovy")
      {
    #anchoy = species 1
    outdf <- outdf[grep(",1]", outdf$Parameter), ]
    out_summary<-out_summary[grep(",1]", rownames(out_summary)), ]
    mcmc.table<-mcmc.table[,grep(",1]", colnames(mcmc.table))]
        
        chiffre<-1
        #collecting covariates time series 
          data_covar.M1<-data_covar.M1.anch
          data_covar.M2<-data_covar.M2.anch
          data_covar.M3<-data_covar.M3.anch
          data_covar.g1<-data_covar.g1.anch
          data_covar.g2<-data_covar.g2.anch
          data_covar.g3<-data_covar.g3.anch
          data_covar.Z.SSN<-data_covar.Z.SSN.anch
          
          #ncov = nb of covariates, for each process (M,g,z.ssn)
          ncov.M<-ncov.M.anch ; ncov.g<-ncov.g.anch ;ncov.zssn<-ncov.zssn.anch 
      }
        if(poisson=="sardine")
      {
          #sardine = species 2
          outdf <- outdf[grep(",2]", outdf$Parameter), ]
          out_summary<-out_summary[grep(",2]", rownames(out_summary)), ]
          mcmc.table<-mcmc.table[,grep(",2]", colnames(mcmc.table))]
        
        chiffre<-2
        #collecting covariates time series 
          data_covar.M1<-data_covar.M1.sard
          data_covar.M2<-data_covar.M2.sard
          data_covar.M3<-data_covar.M3.sard
          data_covar.M4<-data_covar.M4.sard
          data_covar.M5<-data_covar.M5.sard
          data_covar.g1<-data_covar.g1.sard
          data_covar.g2<-data_covar.g2.sard
          data_covar.g3<-data_covar.g3.sard
          data_covar.g4<-data_covar.g4.sard
          data_covar.g5<-data_covar.g5.sard
          data_covar.Z.SSN<-data_covar.Z.SSN.sard
          
          #ncov = nb of covariates, for each process (M,g,z.ssn)
          ncov.M<-ncov.M.sard ; ncov.g<-ncov.g.sard ;ncov.zssn<-ncov.zssn.sard
      }

 
  
plot.v<-list()
 
for(v in 1:length(var.vect)){

#preparing data collected from M, g, Z.SSN 
  
  if(var.vect[v]=="M1"){
    p_incl.pattern<-"^p_inclusion.M1\\["
    bet<-"beta.M1["
    data_covar<-data_covar.M1
    beta.pattern<-"^beta.M1\\["
    title.SSVS<-paste("Age 1 natural mortality")
    title.color<-"black"
    ncov<-ncov.M
  }
  if(var.vect[v]=="M2"){
    p_incl.pattern<-"^p_inclusion.M2\\["
    bet<-"beta.M2["
    data_covar<-data_covar.M2
    beta.pattern<-"^beta.M2\\["
    title.SSVS<-paste("Age 2 natural mortality")
    title.color<-"red"
    ncov<-ncov.M
  }
  if(var.vect[v]=="M3"){
    p_incl.pattern<-"^p_inclusion.M3\\["
    bet<-"beta.M3["
    data_covar<-data_covar.M3
    beta.pattern<-"^beta.M3\\["
    title.SSVS<-paste("Age 3 natural mortality")
    title.color<-"blue"
    ncov<-ncov.M
  }

  if(var.vect[v]=="M4"){
    p_incl.pattern<-"^p_inclusion.M4\\["
    bet<-"beta.M4["
    data_covar<-data_covar.M4
    beta.pattern<-"^beta.M4\\["
    title.SSVS<-paste("Age 4 natural mortality")
    title.color<-"green3"
    ncov<-ncov.M
  }
  
  if(var.vect[v]=="M5"){
    p_incl.pattern<-"^p_inclusion.M5\\["
    bet<-"beta.M5["
    data_covar<-data_covar.M5
    beta.pattern<-"^beta.M5\\["
    title.SSVS<-paste("Age 5 natural mortality")
    title.color<-"purple"
    ncov<-ncov.M
  }
  
  if(var.vect[v]=="M6"){
    p_incl.pattern<-"^p_inclusion.M6\\["
    bet<-"beta.M6["
    data_covar<-data_covar.M6
    beta.pattern<-"^beta.M6\\["
    title.SSVS<-paste("Age 6 natural mortality")
    title.color<-"orange"
    ncov<-ncov.M
  }
  
  if(var.vect[v]=="g1"){
    p_incl.pattern<-"^p_inclusion.g1\\["
    bet<-"beta.g1["
    data_covar<-data_covar.g1
    beta.pattern<-"^beta.g1\\["
    title.SSVS<-paste("Age 1 pseudo-growth")
    title.color<-"black"
    ncov<-ncov.g
  }
  if(var.vect[v]=="g2"){
    p_incl.pattern<-"^p_inclusion.g2\\["
    bet<-"beta.g2["
    data_covar<-data_covar.g2
    beta.pattern<-"^beta.g2\\["
    title.SSVS<-paste("Age 2 pseudo-growth")
    title.color<-"red"
    ncov<-ncov.g
  }
  if(var.vect[v]=="g3"){
    p_incl.pattern<-"^p_inclusion.g3\\["
    bet<-"beta.g3["
    data_covar<-data_covar.g3
    beta.pattern<-"^beta.g3\\["
    title.SSVS<-paste("Age 3 pseudo-growth")
    title.color<-"blue"
    ncov<-ncov.g
  }
  
  if(var.vect[v]=="g4"){
    p_incl.pattern<-"^p_inclusion.g4\\["
    bet<-"beta.g4["
    data_covar<-data_covar.g4
    beta.pattern<-"^beta.g4\\["
    title.SSVS<-paste("Age 4 pseudo-growth")
    title.color<-"green3"
    ncov<-ncov.g
  }
  
  if(var.vect[v]=="g5"){
    p_incl.pattern<-"^p_inclusion.g5\\["
    bet<-"beta.g5["
    data_covar<-data_covar.g5
    beta.pattern<-"^beta.g5\\["
    title.SSVS<-paste("Age 5 pseudo-growth")
    title.color<-"purple"
    ncov<-ncov.g
  }
  
  if(var.vect[v]=="g6"){
    p_incl.pattern<-"^p_inclusion.g6\\["
    bet<-"beta.g6["
    data_covar<-data_covar.g6
    beta.pattern<-"^beta.g6\\["
    title.SSVS<-paste("Age 6 pseudo-growth")
    title.color<-"orange"
    ncov<-ncov.g
  }
  
  if(var.vect[v]=="Z.SSN"){
    p_incl.pattern<-"^p_inclusion.zssn\\["
    bet<-"beta.zssn["
    data_covar<-data_covar.Z.SSN
    beta.pattern<-"^beta.zssn\\["
    title.SSVS<-paste("Recruitement processes")
    title.color<-"deeppink"
    var.vect[v]<-"zssn"
    ncov<-ncov.zssn
  }
  

   
  #collecting the mean posterior frequency of inclusion (p_inclusion) for each covariate (ex: temperature, zoo...)
  inclu_rows <- grepl(rownames(out_summary), pattern = p_incl.pattern)
  prob.inclu <-out_summary[inclu_rows,"Mean"]
  
  
  
  #collecting the posterior of the covariate coefficients when the effect is included (all the mcmc draws where inclusion = 1 )   
  l<-list()
  l.bet<-list()
  coefs<-list()

  
  for (i in 1:ncov){
    l[[i]]<- grepl(mcmc.table[,paste("p_inclusion.",var.vect[v],"[",i,",",chiffre,"]",sep="")], pattern = "1")    #to save only the mcmc draws where the effect is included (p_inclusion=1) and so have the coefficient median value when it is included
        #table(l[[i]])["TRUE"]
    l.bet[[i]] <-mcmc.table[l[[i]],paste("beta.",var.vect[v],"[",i,",",chiffre,"]",sep="")]
    #l.bet[[i]] <-mcmc.table[,paste("beta.",var.vect[v],"[",i,",",chiffre,"]",sep="")]
    #coefs[[i]]<-mean(l.bet[[i]][1:length(l.bet[[i]])])
    coefs[[i]]<-median(l.bet[[i]])
    print(paste("moy",mean(l.bet[[i]])))
    print(paste("median",median(l.bet[[i]])))
  }

  coefs<-unlist(coefs)
  coefs<-as.numeric(coefs)

  
  #naming the covariates by there name (ex: covariate 1 = l1.anch...)
  vect.name<-c()
  for (i in 1:ncol(data_covar)){
    vect.name[i]<-paste("beta.",var.vect[v],"[",i,",",chiffre,"]",sep="")
  }
  
  #creation of the data frame with probability of inclusion, coefficient, covariate name (ex: n2.sard), covariate name in the model (ex beta[5,2])     
    df <- data.frame(probs=prob.inclu, coef=coefs,covar = colnames(data_covar),beta.names=vect.name)
    #df<- df %>% arrange((coef)) 
    df<-df%>%mutate(col = ifelse(probs > 0.5, "#009900", "red"))
    #df<-df%>%mutate(shap = ifelse(probs > 0.5, 17, 19))
    
    #selection des noms des para qui sont significatifs
    collect<- grepl(df[,"col"],pattern="#009900")
    collect<-df[collect,"beta.names"]

    #collect2<-sub(".*beta", "", collect) # Extract characters after pattern beta pour pouvoir sélectionner les p_inclusions =1 des quelques para significatifs
    #collect2<-paste("p_inclusion",collect2,sep="")  
  
    print(df)
    print(collect)
   

  df.lim<-df %>% filter(covar!="NAO" , covar!="AMO")
  
 if(length(collect)!=0){  #qd il y a des effets sur M, g ou Z.SSN 
   
   # Sample vector of parameters starting with "beta"
   beta_params <- collect
   
   select_matching_iterations <- function(df, beta_params) {
     result <- list()
     for (beta_param in beta_params) {
       # Extract the string after "beta"
       matching_string <- sub("^beta\\.(.*)$", "\\1", beta_param)
       
       # Find matching p_inclusion parameter based on the matching_string
       p_inclusion_param <- paste0("p_inclusion.", matching_string)
       
       # Select Iterations where value is 1 for the p_inclusion parameter for each chain
       inclusion_iterations_chain1 <- df %>%
         filter(Parameter == p_inclusion_param & value == 1 & Chain==1)
       inclusion_iterations_chain2 <- df %>%
         filter(Parameter == p_inclusion_param & value == 1 & Chain==2)
       inclusion_iterations_chain3 <- df %>%
         filter(Parameter == p_inclusion_param & value == 1 & Chain==3)
       
       
       # Filter "beta" parameters for each chain based on the matching "p_inclusion" parameters
       beta_iterations_chain1 <- df %>%
         filter(Parameter == beta_param & Iteration %in% inclusion_iterations_chain1$Iteration) 
       beta_iterations_chain2 <- df %>%
         filter(Parameter == beta_param & Iteration %in% inclusion_iterations_chain2$Iteration) 
       beta_iterations_chain3 <- df %>%
         filter(Parameter == beta_param & Iteration %in% inclusion_iterations_chain3$Iteration) 
       
       bon<-rbind(beta_iterations_chain1,beta_iterations_chain2)
       bon<-rbind(bon,beta_iterations_chain3)
       # View(bon)
       result[[beta_param]]<-bon
     }  
     # Bind all the data frames in the result list by row
     result_combined <- do.call(rbind, result)
     result_combined <- rbind(result_combined, df%>%filter(!(Parameter %in% beta_params)))
     
     return(result_combined)
   } #end function
   
   new.outdf <- select_matching_iterations(outdf, beta_params)
   outdf<-new.outdf
   
   
 } #end if 
  
#outdf<-outdf %>% filter(value!=0)

pre_df<-outdf%>%filter(Parameter %in% df$beta.names)%>%group_by(Parameter)%>%summarise(med=median(value))
# View(pre_df)
df<-df%>%mutate("med"=pre_df$med)
print(df)
#ci(outdf%>%filter(Parameter=="beta.M3[2,1]"))

#-----------------------------------------------------------------------
#GRAPHS  

# graph options
if(str_contains(names(data_covar.M1.anch)[1],"anch")||str_contains(names(data_covar.M1.anch)[1],"sard"))
{
  effect = "intrinsic"
  subtitle = "Demographic and size variables"
}
if(!(str_contains(names(data_covar.M1.anch)[1],"anch")||str_contains(names(data_covar.M1.anch)[1],"sard")))
{
  effect = "extrinsic"
  subtitle = "Environmental covariates"
}

  
  plot.v[[v]] <- ggplot()+
      ggs_caterpillar(outdf,family=paste("^beta.",var.vect[v],sep=""),X=data.frame("Parameter"=df$beta.names,"x"=c(1:ncol(data_covar))),horizontal = FALSE, sort=FALSE,  
                    thick_ci = c(0.1, 0.90),
                    thin_ci = c(0.01, 0.99))+
      geom_vline(xintercept = 0, linetype = "dashed",size=0.7) +   
      geom_point(data=df,aes(x=med,y=covar,col=factor(col),shape=factor(col)),size=5)+
      #geom_point(data=df,aes(x=coef,y=covar,col=factor(col),shape=factor(col)),size=5)+  
      scale_color_manual(values=c("#009900"="#009900", "red"="red"))+
      scale_shape_manual(values=c("#009900"=17, "red"=19))+
      ggtitle(title.SSVS)+
      xlab("Effects") +
      ylab(subtitle)+
      coord_flip(xlim=c(-(round(max(abs(df.lim$coef)),2)+1.2), round(max(abs(df.lim$coef)),2)+1.2))+  # coord_cartesian() or coord_flip() (Flip cartesian when x and y switched) : for changing x or y axis limits without dropping data observations, see coord_cartesian()
      scale_y_discrete(limits = (df$covar),guide=guide_axis(angle=45,n.dodge=1))+
      theme(axis.text.x = element_text(size = 11,hjust = 0.5),
            text = element_text(size = 10),
            plot.title = element_text(hjust = 0.5, color = title.color),
            panel.spacing = unit(0.25, "lines"),
            panel.background = element_rect(fill = "white"),
            panel.grid.major = element_line(colour = "darkgrey", size=0.5),
            panel.grid.minor = element_line(colour = "darkgrey", size=0.2))
  
}




resol <- 8
png(filename = paste("all_covar_SSVS_",poisson,"_",(7-length(var.vect)),".png",sep=""), height = 520*resol, width = 1000*resol, res=72*resol)
def.par <- par(no.readonly = TRUE)

if(length(var.vect)==7){
  
  grid.arrange(plot.v[[1]],plot.v[[2]],plot.v[[3]],plot.v[[4]],plot.v[[5]],plot.v[[6]],plot.v[[7]],nrow=2,ncol=4,top=textGrob(paste(str_to_title(effect),"effects on natural mortality, pseudo-growth and recruitement processes in",poisson,"populations"), gp=gpar(fontsize=20)))
  
}

if(length(var.vect)==6){
  
  grid.arrange(plot.v[[1]],plot.v[[2]],plot.v[[3]],plot.v[[4]],plot.v[[5]],plot.v[[6]],nrow=2,ncol=3,top=textGrob(paste(str_to_title(effect),"effects on natural mortality and recruitement processes in",poisson,"populations"), gp=gpar(fontsize=20)))
  
}

if(length(var.vect)==5){
  
  grid.arrange(plot.v[[1]],plot.v[[2]],plot.v[[3]],plot.v[[4]],plot.v[[5]],nrow=2,ncol=3,top=textGrob(paste(str_to_title(effect),"effects on pseudo-growth process in",poisson,"populations"), gp=gpar(fontsize=20)))
  
}
   
  par(def.par)
  dev.off() 
  

}
