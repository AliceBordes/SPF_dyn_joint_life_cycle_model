#'************************************************************
#'************************************************************
# Object : RESULTS OVERVIEW
#           Analysis and visualization of the outputs of the 
#           Two-species (sardine and anchovy) life-cycle model 
#           within a statistical state‐space Bayesian framework 
#           - Analysis of abundance and size dynamics
#           - Analysis of non-stationary demographic rates (natural mortality, growth, recruitment )
#           - Analysis of density-, size-dependent and environment-dependent demographic rates
#
# Author : Alice Bordes - M2 Internship - February to August 2022  
# Data : collected during the annual spring acoustic surveys PELGAS, from 2000 to 2022
#
# Based on the work of Charlotte Andrieux's M2 internship - January 2022
#'************************************************************
#'************************************************************


# Loading libraries ----
#'************************************************************
library(here)
library(patchwork)
library(ggridges)
library(bayestestR)
library(see)
library(ggplot2)
library(dclone)
library(rjags)
library(MASS)
library(coda)
library(reshape2)
library(sjmisc)
library(stringr)
library(gridExtra)
library(grid)
library(rlist)
library(ggmcmc)
library(knitr)
library(tidyverse)
library(bayesplot)
library(rstanarm)
library(cowplot)
library(plotly)
library(ggiraph)
load.module("dic") # module permettant de stocker les paramètres (les déviances et les termes qui permettent de calculer la pénalisation par la complexité du modèle) pour le calcul du WAIC lors de l'exécution du modèle
#'************************************************************

# Settings ---- 
#'************************************************************
#name of your project file
base <- here()

name.dta.file <- "iter=30000_28_avr._2025_13.58_JointSSVScovariates" #change : name of you file containing the outputs of the model of interest
part.file.path <- paste("4_OUTPUTS/",name.dta.file,sep="")

name.Rdta <- "Joint_model"   #do not change
#'************************************************************


# Loading functions ---- 
#'************************************************************
setwd(base)
source("3_FUNCTIONS/f.convergence.gelman.R")
source("3_FUNCTIONS/f.convergence.gelman.interactive.R")
source("3_FUNCTIONS/f.priorVSposterior_joint.R")
source("3_FUNCTIONS/f.posterior_checking_predVSobs_joint.R")
source("3_FUNCTIONS/f.cohortes_joint.R")
source("3_FUNCTIONS/f.SSVS_joint2.R")
source("3_FUNCTIONS/f.extract.mortality_growth.R")
source("3_FUNCTIONS/f.loi.jointe.density.multivar.R")
source("3_FUNCTIONS/f.loi.jointe.density.bivar.R")
source("3_FUNCTIONS/f.panel.cor.R")
source("3_FUNCTIONS/f.panel.dens.R")
source("3_FUNCTIONS/f.time.serie.one_joint_optimized.R")
#'************************************************************



# Setting parameter names for functions ----
#'************************************************************
mortality <- "M"
growth <- "g"
mortality.Z.SSN <- "Z.SSN"
anchovy <- "anchovy"
sardine <- "sardine"

log <- "yes"
data.obs <- "obs"
vector.n.anch <- c("n1.pp","n2.pp","n3.pp","n4.pp")
vector.l.anch <- c("l1.pp","l2.pp","l3.pp","l4.pp")
vector.N.anch <- c("N1","N2","N3","N4","N.SSN")
vector.L.anch <- c("L1","L2","L3","L4","L.SSN")
vector.M.anch <- c("M1","M2","M3")
vector.sigma.M.anch <- c("sigma.M1","sigma.M2","sigma.M3")
vector.F.anch <- c("F1","F2","F3")
vector.Z.anch <- c("Z1","Z2","Z3")
vector.C.anch <- c("C1","C2","C3")
vector.g.anch <- c("g1","g2","g3")

vector.n.sard <- c("n1.pp","n2.pp","n3.pp","n4.pp","n5.pp","n6.pp","n7.pp")
vector.l.sard <- c("l1.pp","l2.pp","l3.pp","l4.pp","l5.pp","l6.pp","l7.pp")
vector.N.sard <- c("N1","N2","N3","N4","N5","N6","N7","N.SSN")
vector.L.sard <- c("L1","L2","L3","L4","L5","L6","L7","L.SSN")
vector.M.sard <- c("M1","M2","M3","M4","M5","M6")
vector.sigma.M.sard <- c("sigma.M1","sigma.M2","sigma.M3","sigma.M4","sigma.M5","sigma.M6")
vector.F.sard <- c("F1","F2","F3","F4","F5","F6")
vector.Z.sard <- c("Z1","Z2","Z3","Z4","Z5","Z6")
vector.C.sard <- c("C1","C2","C3","C4","C5","C6")
vector.g.sard <- c("g1","g2","g3","g4","g5","g6")


vect.SSVS.anch <- c("M1","M2","M3","Z.SSN","g1","g2","g3")
vect.SSVS.sard <- c("M1","M2","M3","Z.SSN","g1","g2","g3")
 
# vect.Z.SSN.both <- c("Z.SSN","Z.SSN")
#'************************************************************



#  Loading data and extracting estimated natural mortality and pseudo-growth ----
#'************************************************************
load(file.path(base, part.file.path, paste0(name.Rdta,".RData")))

mcmc <- res
mcmc.table <- as.data.frame(as.matrix(window(mcmc))) # transformation de l'object MCMC en tableau

data_anchovy <- read.table(file.path(base,"1_DATA","data_anchovy_NA.txt"), sep="\t", h=T) # Biological data (anchovy abundances, sizes, captures by life stage)
data_sardine <- read.table(file.path(base,"1_DATA","data_sardine_NA.txt"), sep="\t", h=T) # Biological data (sardine abundances, sizes, captures by life stage)

extract.M_g_est(mcmc.table,mortality,anchovy)
extract.M_g_est(mcmc.table,growth,anchovy)
extract.M_g_est(mcmc.table,mortality.Z.SSN,anchovy)
extract.M_g_est(mcmc.table,mortality,sardine)
extract.M_g_est(mcmc.table,growth,sardine)
extract.M_g_est(mcmc.table,mortality.Z.SSN,sardine)
#'************************************************************



#  Markov chains convergence ----
#'************************************************************

setwd(file.path(base, part.file.path, "figures"))
convergence.gelman(mcmc)
convergence.gelman.interactive(mcmc)

# DEBUG
# conv.M_bug = gelman.diag(mcmc[,startsWith(varnames(mcmc),"M4")], confidence = 0.95, transform = TRUE, autoburnin=TRUE)
# traceplot(mcmc[,'N4[21,1]'],ylab="N4[21,1]")
#'************************************************************



#  Posteriors ----
#'************************************************************

setwd(file.path(base, part.file.path, "figures", "posterior", "anchovy"))
posterior(mcmc,"Z.SSN",anchovy)
posterior(mcmc,"N.SSN",anchovy)
posterior(mcmc,"N1",anchovy)
posterior(mcmc,"N2",anchovy)
posterior(mcmc,"N3",anchovy)
posterior(mcmc,"N4",anchovy)
posterior(mcmc,"M1",anchovy)
posterior(mcmc,"M2",anchovy)
posterior(mcmc,"M3",anchovy)
posterior(mcmc,"F1",anchovy)
posterior(mcmc,"F2",anchovy)
posterior(mcmc,"F3",anchovy)
posterior(mcmc,"Z1",anchovy)
posterior(mcmc,"Z2",anchovy)
posterior(mcmc,"Z3",anchovy)
posterior(mcmc,"g1",anchovy)
posterior(mcmc,"g2",anchovy)
posterior(mcmc,"g3",anchovy)
posterior(mcmc,"L1",anchovy) 
posterior(mcmc,"L2",anchovy)
posterior(mcmc,"L3",anchovy)
posterior(mcmc,"L4",anchovy)

setwd(file.path(base, part.file.path, "figures", "posterior", "sardine"))
posterior(mcmc,"Z.SSN",sardine)
posterior(mcmc,"N.SSN",sardine)
posterior(mcmc,"N1",sardine)
posterior(mcmc,"N2",sardine)
posterior(mcmc,"N3",sardine)
posterior(mcmc,"N4",sardine)
posterior(mcmc,"N5",sardine)
posterior(mcmc,"N6",sardine)
posterior(mcmc,"N7",sardine)
posterior(mcmc,"M1",sardine)
posterior(mcmc,"M2",sardine)
posterior(mcmc,"M3",sardine)
posterior(mcmc,"M4",sardine)
posterior(mcmc,"M5",sardine)
posterior(mcmc,"M6",sardine)
posterior(mcmc,"F1",sardine)
posterior(mcmc,"F2",sardine)
posterior(mcmc,"F3",sardine)
posterior(mcmc,"F4",sardine)
posterior(mcmc,"F5",sardine)
posterior(mcmc,"F6",sardine)
posterior(mcmc,"Z1",sardine)
posterior(mcmc,"Z2",sardine)
posterior(mcmc,"Z3",sardine)
posterior(mcmc,"Z4",sardine)
posterior(mcmc,"Z5",sardine)
posterior(mcmc,"Z6",sardine)
posterior(mcmc,"g1",sardine)
posterior(mcmc,"g2",sardine)
posterior(mcmc,"g3",sardine)
posterior(mcmc,"g4",sardine)
posterior(mcmc,"g5",sardine)
posterior(mcmc,"g6",sardine)
posterior(mcmc,"L1",sardine)
posterior(mcmc,"L2",sardine)
posterior(mcmc,"L3",sardine)
posterior(mcmc,"L4",sardine)
posterior(mcmc,"L5",sardine)
posterior(mcmc,"L6",sardine)
posterior(mcmc,"L7",sardine)
#'************************************************************


#  Time series ----
#'************************************************************

#set path location to register the figure
setwd(file.path(base, part.file.path, "figures", "time_series_variables", "anchovy"))
time.series(vector.n.anch,res,2022,log,data.obs,anchovy)
#time.series(vector.n.anch,title,y.label,res,2022,"no",data.obs,anchovy)
time.series(vector.l.anch,res,2022,log,data.obs,anchovy)
time.series(vector.l.anch,res,2022,"no",data.obs,anchovy)
time.series(vector.C.anch,res,2021,"no",data.obs,anchovy)
time.series(vector.M.anch,res,2021,"no","no",anchovy)
time.series(vector.F.anch,res,2021,"no","no",anchovy)
time.series(vector.Z.anch,res,2021,"no","no",anchovy)
time.series("Z.SSN",res,2021,"no","no",anchovy)
time.series(vector.g.anch,res,2021,log,"no",anchovy)

setwd(file.path(base, part.file.path, "figures", "time_series_variables", "sardine"))
time.series(vector.n.sard,res,2022,log,data.obs,sardine)
time.series(vector.l.sard,res,2022,log,data.obs,sardine)
time.series(vector.l.sard,res,2022,"no",data.obs,sardine)
time.series(vector.C.sard,res,2021,"no",data.obs,sardine)
time.series(vector.M.sard,res,2021,"no","no",sardine)
time.series(vector.F.sard,res,2021,"no","no",sardine)
time.series(vector.Z.sard,res,2021,"no","no",sardine)
time.series(vector.g.sard,res,2021,log,"no",sardine)
#'************************************************************


#  N_Cohorts ----
#'************************************************************

#set path location to register the figure
setwd(paste(base,"/",part.file.path,"/figures",sep = ""))
cohortes(anchovy,data_anchovy)
# cohortes(sardine,data_sardine) 


#  Time series pooled on 1 graph ----
#'************************************************************

#set path location to register the figure
setwd(file.path(base, part.file.path, "figures"))
time.series.one(vector.N.anch,res,2022,log,anchovy,12,25,0.5,-0.4,0,0,0.6,0,0,0)
time.series.one(vector.N.anch,res,2022,"no",anchovy,12,25,0.5,-0.4,0,0,0.6,0,0,0)
time.series.one(vector.L.anch,res,2022,"no",anchovy,10,22.5,-0.1,0,0,0.8,0,0,0,0)
time.series.one(vector.M.anch,res,2021,"no",anchovy,0,5,-0.8,0,1,0,0,0,0,0)
time.series.one(vector.g.anch,res,2021,"no",anchovy,0.8,1.4,0,-0.5,0.2,0,0,0,0,0)
time.series.one(vector.Z.anch,res,2021,"no",anchovy,0,5.5,-0.5,0,0,0,0,0,0,0)
time.series.one(vector.F.anch,res,2021,"no",anchovy,0,3,0,0.2,0,0,0,0,0,0)
time.series.one("Z.SSN",res,2021,"no",anchovy,0,10,1,0,0,0,0,0,0,0)
#time.series.one("N.SSN",res,2022,log,anchovy,19,24,0,0,0,0,0,0,0,0)

time.series.one(vector.N.sard,res,2022,log,sardine,15,23,-0.1,0,0,0,0,0,0,0)
time.series.one(vector.N.sard,res,2022,"no",sardine,15,23,-0.1,0,0,0,0,0,0,0)
time.series.one(vector.L.sard,res,2022,"no",sardine,12,25,-0.4,0,-0.2,0,-0.1,0.3,0.8,-0.6)
time.series.one(vector.M.sard,res,2021,"no",sardine,0,3,0.3,0,-0.1,-0.2,-0.3,0,0,0)
time.series.one(vector.g.sard,res,2021,"no",sardine,0.8,1.4,0,0.4,0.1,0,-0.3,0,0,0)
time.series.one(vector.Z.sard,res,2021,"no",sardine,0,5.5,0,-0.5,0.1,0,0,0,0,0)
time.series.one(vector.F.sard,res,2021,"no",sardine,0,1.5,0,0.3,0,0,-0.2,0,0,0)
time.series.one("Z.SSN",res,2021,"no",sardine,0,2,0,0,0,0,0,0,0,0)
#time.series.one("N.SSN",res,2022,log,sardine,20,24,0,0,0,0,0,0,0,0)
#'************************************************************


#  Extrinsic effects ----
#'************************************************************
# Covariates effects on fish dynamic rates (beta)

# Need to previously run the model .Rscript until the part "Model inputs"
setwd(file.path(base, part.file.path, "figures", "coefs"))
SSVS.2(mcmc,vect.SSVS.anch,anchovy)
SSVS.2(mcmc,vect.SSVS.sard,sardine)
#'************************************************************


#  Intrinsic effects ----
#'************************************************************
# Posterior distributions of the coefficients related to process dependent latent variables of density and size : alpha, beta...)

color_scheme_set("blue")  #other scheme: mix-teal-pink
#°°°° anchovy
pM1.anch<-mcmc_areas(
  mcmc,
  point_est = c("median"), #tracer la median
  pars = c("beta_M1[1]"),     # effects to plot
  prob = 0.9)+
  xlim(-2,1)+  
  scale_y_discrete(labels = c("Age 1 anchovy size"))+
  theme(plot.margin = margin(1.2, 0.5, 1, 3.6, "cm"),
        axis.text.y = element_text(size=13,face = "bold"),
        axis.text.x = element_text(size=12,face = "plain"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(colour = "#333333"),
        plot.title = element_text(color = "black", face = "bold"))+
  geom_vline(xintercept=0, linetype="dashed",size=1) +
  ggtitle("Effects on natural mortality of age 1 anchovy")

pM2.anch<-mcmc_areas(
  mcmc,
  point_est = c("median"), #tracer la median
  pars = c("alpha.bis_M2[1]","alpha_M2[1]"),     # effetcts to plot
  prob = 0.9)+
  scale_y_discrete(labels = c("Age 1 anchovy abundance","Age 2 anchovy abundance"))+
  theme(plot.margin = margin(0.5, 0.5, 0, 2.3, "cm"),
        axis.text.y = element_text(size=13,face = "bold"),
        axis.text.x = element_text(size=12,face = "plain"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(colour = "#333333"),
        plot.title = element_text(color = "red", face = "bold"))+
  geom_vline(xintercept=0, linetype="dashed",size=1)+
  ggtitle("Effects on natural mortality of age 2 anchovy")
  #+xlim(-5,5)

pzssn.anch<-mcmc_areas(
  mcmc,
  point_est = c("median"), #tracer la median
  pars = c("alpha_Z.SSN[1]"),     # effects to plot
  prob = 0.9)+
  scale_y_discrete(labels = c("Anchovy spawning stock abundance"))+
  theme(plot.margin = margin(2, 0.5, 0, 0.1, "cm"),
        axis.text.y = element_text(size=13,face = "bold"),
        axis.text.x = element_text(size=12,face = "plain"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(colour = "#333333"),
        plot.title = element_text(color = "deeppink", face = "bold"))+
  geom_vline(xintercept=0, linetype="dashed",size=1)+
  ggtitle("Effects on recruit processes of anchovy")
#+xlim(-5,5)

# windows()
resol<-8
png(filename = paste("Intrinsic_effects_anchovy",".png",sep=""), height = 520*resol, width = 520*resol, res=72*resol)
plot_grid(nrow=3,pM1.anch,pM2.anch,pzssn.anch)
dev.off()

#°°°° sardine
color_scheme_set("green")
pM1.sard<-mcmc_areas(
  mcmc,
  point_est = c("median"), #tracer la median
  pars = c("alpha_M1[2]"),     # effects to plot
  prob = 0.9)+
  xlim(-1,6)+
  scale_y_discrete(labels = c("Age 1 sardine abundance"))+
  theme(plot.margin = margin(1, 0.5, 2, 1.2, "cm"),
        axis.text.y = element_text(size=13,face = "bold", color = "black"),
        axis.text.x = element_text(size=12,face = "plain"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(colour = "#333333"),
        plot.title = element_text(color = "black", face = "bold"))+
  ggtitle("Effects on natural mortality of sardine at age 1")+
  geom_vline(xintercept=0, linetype="dashed",size=1)


pM3.sard<-mcmc_areas(
  mcmc,
  point_est = c("median"), #tracer la median
  pars = c("alpha_M3[2]","alpha.bis_M3[2]","alpha.ter_M3[2]"),     # effetcts to plot
  prob = 0.9)+
  scale_y_discrete(labels = c("Age 1 sardine abundance","Age 2 sardine abundance","Old stages sardine abundance"))+
  theme(plot.margin = margin(0, 0.5, 0, 0.3, "cm"),
        axis.text.y = element_text(size=13,face = "bold"),
        axis.text.x = element_text(size=12,face = "plain"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(colour = "#333333"),
        plot.title = element_text(color = "blue", face = "bold"))+
  ggtitle("Effects on natural mortality of sardine at age 3")+
  geom_vline(xintercept=0, linetype="dashed",size=1)


# windows()
resol<-8
png(filename = paste("Intrinsic_effects_sardine",".png",sep=""), height = 520*resol, width = 520*resol, res=72*resol)
plot_grid(nrow=2,pM1.sard,pM3.sard)
dev.off()

# Latent intrinsic coefficient, median value
#anchovy
median(mcmc.table[,"beta_M1[1]"])
median(mcmc.table[,"alpha_M2[1]"])
median(mcmc.table[,"alpha.bis_M2[1]"])
median(mcmc.table[,"alpha_Z.SSN[1]"])

#sardine
median(mcmc.table[,"alpha_M1[2]"])
median(mcmc.table[,"alpha_M3[2]"])
median(mcmc.table[,"alpha.bis_M3[2]"])
median(mcmc.table[,"alpha.ter_M3[2]"])

# DEBUG
#plot(density(mcmc.table[,"alpha_M1[2]"]))
#traceplot(mcmc[,'alpha_M1[2]'],ylab="alpha_M1[2]")
#names(mcmc.table)
#'************************************************************
