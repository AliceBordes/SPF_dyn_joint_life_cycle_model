# --------------------------------------------------------
#         convergence des chaines de Markov
# --------------------------------------------------------

convergence.gelman<-function(name.mcmc.table)
{
 
  conv.C = gelman.diag(name.mcmc.table[,startsWith(varnames(name.mcmc.table),"C")], confidence = 0.95, transform = TRUE, autoburnin=TRUE)
  conv.F = gelman.diag(name.mcmc.table[,startsWith(varnames(name.mcmc.table),"F")],confidence = 0.95, transform = TRUE, autoburnin = TRUE)
  conv.M = gelman.diag(name.mcmc.table[,startsWith(varnames(name.mcmc.table),"M")],confidence = 0.95, transform = TRUE, autoburnin = TRUE)
  conv.N = gelman.diag(name.mcmc.table[,startsWith(varnames(name.mcmc.table),"N")][,-c(1:23)], confidence = 0.95, transform =TRUE, autoburnin = TRUE)
  conv.ZSSN = gelman.diag(name.mcmc.table[,startsWith(varnames(name.mcmc.table),"Z.SSN")],confidence = 0.95, transform = TRUE, autoburnin =TRUE)
  conv.NSSN = gelman.diag(name.mcmc.table[,startsWith(varnames(name.mcmc.table),"N.SSN")],confidence = 0.95, transform = TRUE, autoburnin =TRUE)
  conv.L = gelman.diag(name.mcmc.table[,startsWith(varnames(name.mcmc.table),"L")][,-c(1:23)], confidence = 0.95, transform =TRUE, autoburnin = TRUE)
  conv.g = gelman.diag(name.mcmc.table[,startsWith(varnames(name.mcmc.table),"g")][,-c(1:22)],confidence = 0.95, transform = TRUE, autoburnin =TRUE)
  
  resol <- 6
  png(filename = "convergence_variables.png", height = 320*resol, width = 680*resol, res=72*resol)
  def.par <- par(no.readonly = TRUE)
  
  plot(1:length(conv.C$psrf[,1]),conv.C$psrf[,1],ylim=c(0.99,1.2),type = "l",lwd=2,xlab = "", ylab = "Convergence value", main = "Markov chains convergence" )
  lines(1:length(conv.C$psrf[,1]),rep(1.1,length(conv.C$psrf[,1])),lwd=2,lty="longdash")
  lines(1:length(conv.F$psrf[,1]),conv.F$psrf[,1],col= "red",lwd=2)
  lines(1:length(conv.M$psrf[,1]),conv.M$psrf[,1],col= "darkblue",lwd=2)
  lines(1:length(conv.N$psrf[,1]),conv.N$psrf[,1],col= "green",lwd=2)
  lines(1:length(conv.ZSSN$psrf[,1]),conv.ZSSN$psrf[,1],col= "deepskyblue3",lwd=2)
  lines(1:length(conv.NSSN$psrf[,1]),conv.NSSN$psrf[,1],col= "yellow",lwd=2)
  lines(1:length(conv.L$psrf[,1]),conv.L$psrf[,1],col= "orange",lwd=2)
  lines(1:length(conv.g$psrf[,1]),conv.g$psrf[,1],col= "mediumvioletred",lwd=2)
  legend(legend=c("C","F","M","N","R(Z.SS)","N.SS","L","g"),xpd = TRUE,ncol=8, bty="n",text.width = NA, col = c("black","red","darkblue","green","deepskyblue3","yellow","orange","mediumvioletred"),x="topright", lwd=c(2,2,2,2,2,2,2,2))
  
  par(def.par)
  
  dev.off()
  
return()

}
