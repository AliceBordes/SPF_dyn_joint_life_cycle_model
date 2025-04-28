# --------------------------------------------------------
#         convergence des chaines de Markov
# --------------------------------------------------------

convergence.gelman.interactive<-function(name.mcmc.table)
{
  
  conv.C = gelman.diag(name.mcmc.table[,startsWith(varnames(name.mcmc.table),"C")], confidence = 0.95, transform = TRUE, autoburnin=TRUE)
  conv.F = gelman.diag(name.mcmc.table[,startsWith(varnames(name.mcmc.table),"F")],confidence = 0.95, transform = TRUE, autoburnin = TRUE)
  conv.M = gelman.diag(name.mcmc.table[,startsWith(varnames(name.mcmc.table),"M")],confidence = 0.95, transform = TRUE, autoburnin = TRUE)
  conv.N = gelman.diag(name.mcmc.table[,startsWith(varnames(name.mcmc.table),"N")][,-c(1:23)], confidence = 0.95, transform =TRUE, autoburnin = TRUE)
  conv.ZSSN = gelman.diag(name.mcmc.table[,startsWith(varnames(name.mcmc.table),"Z.SSN")],confidence = 0.95, transform = TRUE, autoburnin =TRUE)
  conv.NSSN = gelman.diag(name.mcmc.table[,startsWith(varnames(name.mcmc.table),"N.SSN")],confidence = 0.95, transform = TRUE, autoburnin =TRUE)
  conv.L = gelman.diag(name.mcmc.table[,startsWith(varnames(name.mcmc.table),"L")][,-c(1:23)], confidence = 0.95, transform =TRUE, autoburnin = TRUE)
  conv.g = gelman.diag(name.mcmc.table[,startsWith(varnames(name.mcmc.table),"g")][,-c(1:22)],confidence = 0.95, transform = TRUE, autoburnin =TRUE)
  
print(rownames(conv.C$psrf))

g<- ggplot()+
  geom_hline(yintercept=1.1,color="black",linetype="dashed",size=1.2)+
  geom_line(aes(x=1:length(conv.C$psrf[,1]),y=conv.C$psrf[,1],color="C"),size=1.1,na.rm=TRUE)+
  geom_line(aes(x=1:length(conv.F$psrf[,1]),y=conv.F$psrf[,1],color="F"),size=1.1,na.rm=TRUE)+
  geom_line(aes(x=1:length(conv.M$psrf[,1]),y=conv.M$psrf[,1],color="M"),size=1.1,na.rm=TRUE)+
  geom_line(aes(x=1:length(conv.N$psrf[,1]),y=conv.N$psrf[,1],color="N"),size=1.1,na.rm=TRUE)+
  geom_line(aes(x=1:length(conv.ZSSN$psrf[,1]),y=conv.ZSSN$psrf[,1],color="R(Z.SS)"),size=1.1,na.rm=TRUE)+
  geom_line(aes(x=1:length(conv.NSSN$psrf[,1]),y=conv.NSSN$psrf[,1],color="N.SS"),size=1.1,na.rm=TRUE)+
  geom_line(aes(x=1:length(conv.L$psrf[,1]),y=conv.L$psrf[,1],color="L"),size=1.1,na.rm=TRUE)+
  geom_line(aes(x=1:length(conv.g$psrf[,1]),y=conv.g$psrf[,1],color="g"),size=1.1,na.rm=TRUE)+
  scale_color_manual(name='Legend',
                     values=c("C"="black","F"="red","M"="darkblue","N"="green","R(Z.SS)"="deepskyblue3","N.SS"="yellow","L"="orange","g"="mediumvioletred"))+
  ylim(0.99,1.2)+
  ylab("Convergence value")+
  xlab("")+
  ggtitle("Markov chains convergence")+
  theme(title =element_text(size=16),axis.title.y=element_text(size=14),legend.text = element_text(size=16))

  g_interactive<- ggplot()+
    geom_hline(yintercept=1.1,color="black",linetype="dashed",size=1.2)+
    geom_point_interactive(aes(x=1:length(conv.C$psrf[,1]),y=conv.C$psrf[,1],color="C",tooltip=rownames(conv.C$psrf)),na.rm=TRUE)+
    geom_point_interactive(aes(x=1:length(conv.F$psrf[,1]),y=conv.F$psrf[,1],color="F",tooltip=rownames(conv.F$psrf)),na.rm=TRUE)+
    geom_point_interactive(aes(x=1:length(conv.M$psrf[,1]),y=conv.M$psrf[,1],color="M",tooltip=rownames(conv.M$psrf)),na.rm=TRUE)+
    geom_point_interactive(aes(x=1:length(conv.N$psrf[,1]),y=conv.N$psrf[,1],color="N",tooltip=rownames(conv.N$psrf)),na.rm=TRUE)+
    geom_point_interactive(aes(x=1:length(conv.ZSSN$psrf[,1]),y=conv.ZSSN$psrf[,1],color="R(Z.SS)",tooltip=rownames(conv.ZSSN$psrf)),na.rm=TRUE)+
    geom_point_interactive(aes(x=1:length(conv.NSSN$psrf[,1]),y=conv.NSSN$psrf[,1],color="N.SS",tooltip=rownames(conv.NSSN$psrf)),na.rm=TRUE)+
    geom_point_interactive(aes(x=1:length(conv.L$psrf[,1]),y=conv.L$psrf[,1],color="L",tooltip=rownames(conv.L$psrf)),na.rm=TRUE)+
    geom_point_interactive(aes(x=1:length(conv.g$psrf[,1]),y=conv.g$psrf[,1],color="g",tooltip=rownames(conv.g$psrf)),na.rm=TRUE)+
    
    geom_line(aes(x=1:length(conv.C$psrf[,1]),y=conv.C$psrf[,1],color="C"),na.rm=TRUE)+
    geom_line(aes(x=1:length(conv.F$psrf[,1]),y=conv.F$psrf[,1],color="F"),na.rm=TRUE)+
    geom_line(aes(x=1:length(conv.M$psrf[,1]),y=conv.M$psrf[,1],color="M"),na.rm=TRUE)+
    geom_line(aes(x=1:length(conv.N$psrf[,1]),y=conv.N$psrf[,1],color="N"),na.rm=TRUE)+
    geom_line(aes(x=1:length(conv.ZSSN$psrf[,1]),y=conv.ZSSN$psrf[,1],color="R(Z.SS)"),na.rm=TRUE)+
    geom_line(aes(x=1:length(conv.NSSN$psrf[,1]),y=conv.NSSN$psrf[,1],color="N.SS"),na.rm=TRUE)+
    geom_line(aes(x=1:length(conv.L$psrf[,1]),y=conv.L$psrf[,1],color="L"),na.rm=TRUE)+
    geom_line(aes(x=1:length(conv.g$psrf[,1]),y=conv.g$psrf[,1],color="g"),na.rm=TRUE)+
    scale_color_manual(name='Legend',
                       values=c("C"="black","F"="red","M"="darkblue","N"="green","R(Z.SS)"="deepskyblue3","N.SS"="yellow","L"="orange","g"="mediumvioletred"))+
    ylim(0.99,1.2)+
    ylab("Convergence value")+
    xlab("")+
    ggtitle("Markov chains convergence")+
    theme(title =element_text(size=16),axis.title.y=element_text(size=14))
    
  
  print(girafe(ggobj = g_interactive))
  #print(ggplotly(g,tooltip="text"))
  windows()
  print(g)
  
  
  return()
  
}
