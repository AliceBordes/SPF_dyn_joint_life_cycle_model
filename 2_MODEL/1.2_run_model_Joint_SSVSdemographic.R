#'************************************************************
#'************************************************************
# Object : Two-species (sardine and anchovy) life-cycle model 
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

rm(list=ls())

# Loading libraries ----
#'************************************************************
library(dclone)
library(rjags)
library(MASS)
library(coda)
library(reshape2)
library(tidyverse)
load.module("dic") # module permettant de stocker les paramètres (les déviances et les termes qui permettent de calculer la pénalisation par la complexité du modèle) pour le calcul du WAIC lors de l'exécution du modèle
library(here)
#'************************************************************



# Settings ---- 
#'************************************************************
base <- here()

# Name of the rjags script file
rjags_file_name = "2_rjags_model_joint_demographic.txt" # nom du modele a aller chercher, ecrit ds un fichier txt 

# File main name 
main_name = "Jointdemo"
model_run = paste0(main_name,".txt")

## Set the number of years
n_annee = 23 # 2000 to 2022
#'************************************************************


# Loading data ----
#'************************************************************
data_anchovy <- read.table(file.path(base,"1_DATA","data_anchovy_NA.txt"), sep="\t", h=T) # Biological data (anchovy abundances, sizes, captures by life stage)
data_sardine <- read.table(file.path(base,"1_DATA","data_sardine_NA.txt"), sep="\t", h=T) # Biological data (sardine abundances, sizes, captures by life stage)

maturite_anch <- as.matrix(read.table(file.path(base,"1_DATA", "maturity_anchovy.txt"), sep="\t", h=T, row.names = NULL)) # Biological data (anchovy maturity by life stage)
maturite_sard <- as.matrix(read.table(file.path(base,"1_DATA", "maturity_sardine.txt"), sep="\t", h=T, row.names = NULL)) # Biological data (sardine maturity by life stage)

data_sprat <- read.table(file.path(base,"1_DATA", "data_sprat_NA.txt"), sep="\t",dec=",", h=T) # Biological data (sprat total abundance)
data_AMO_NAO <- read.table(file.path(base,"1_DATA", "data_AMO_NAO.txt"), sep="",dec=".",fill = TRUE) # Atlantic Multi-decadal Oscillation (AMO) Index

env_anchovy_stand <- read.table(file.path(base,"1_DATA", "Env_stand_anchovy.txt"), sep="\t", h=T) # Environmental data in the ICES division relative to anchovy presence (zooplancton and temperature)
env_sardine_stand <- read.table(file.path(base,"1_DATA", "Env_stand_sardine.txt"), sep="\t", h=T) # Environmental data in the ICES division relative to sardine presence (zooplancton and temperature)
#'************************************************************


# Formatting data ----
#'***********************************************************

## Maturity matrix

mat.list <- as.vector(c(maturite_anch,maturite_sard))
mat <- array(mat.list,dim=c(23,7,2),dimnames=list(1:23,c("age1","age2","age3","age4","age5","age6", "age7plus"),c("1","2")))


### Density ###

#°°°° anchovy
n1.obs.anch.cr <- (data_anchovy$N1-mean(data_anchovy$N1[c(1:20,22:n_annee)],na.rm=T))/sd(data_anchovy$N1[c(1:20,22:n_annee)],na.rm=T) # Observed abundance at age 1
n2.obs.anch.cr <- (data_anchovy$N2-mean(data_anchovy$N2[c(1:20,22:n_annee)],na.rm=T))/sd(data_anchovy$N2[c(1:20,22:n_annee)],na.rm=T) # Observed abundance at age 2
n3.obs.anch.cr <- (data_anchovy$N3-mean(data_anchovy$N3[c(1:20,22:n_annee)],na.rm=T))/sd(data_anchovy$N3[c(1:20,22:n_annee)],na.rm=T) # Observed abundance at age 3
n4plus.obs.anch.cr <- (data_anchovy$N4plus-mean(data_anchovy$N4plus[c(1:20,22:n_annee)],na.rm=T))/sd(data_anchovy$N4plus[c(1:20,22:n_annee)],na.rm=T) # Observed abundance at age 4+
nlarge.obs.anch.cr <- ((data_anchovy$N3[c(1:20,22:n_annee)]+data_anchovy$N4plus[c(1:20,22:n_annee)])-mean((data_anchovy$N3+data_anchovy$N4plus)[c(1:20,22:n_annee)],na.rm=T))/sd((data_anchovy$N3+data_anchovy$N4plus)[c(1:20,22:n_annee)],na.rm=T) # Sum of the abundances of aged fish (age 3 and 4+)
n.ssn.obs.anch.cr <-  ((data_anchovy$N1*mat[,1,1]+data_anchovy$N2*mat[,2,1]+data_anchovy$N3*mat[,3,1]+data_anchovy$N4plus*mat[,4,1])-mean(((data_anchovy$N1*mat[,1,1]+data_anchovy$N2*mat[,2,1]+data_anchovy$N3*mat[,3,1]+data_anchovy$N4plus*mat[,4,1]))[c(1:20,22:n_annee)],na.rm=T)[1])/sd(((data_anchovy$N1*mat[,1,1]+data_anchovy$N2*mat[,2,1]+data_anchovy$N3*mat[,3,1]+data_anchovy$N4plus*mat[,4,1]))[c(1:20,22:n_annee)],na.rm=T)[1] # Abundance of the spawning stock
n.ssn.obs.anch <-  data_anchovy$N1*mat[,1,1]+data_anchovy$N2*mat[,2,1]+data_anchovy$N3*mat[,3,1]+data_anchovy$N4plus*mat[,4,1]
nlarge.obs.anch <- rowSums(data_anchovy[,c("N3","N4plus")],na.rm=T)
nlarge.obs.anch <- replace(nlarge.obs.anch,nlarge.obs.anch == 0,NA) 

#°°°° sardine
n1.obs.sard.cr <- (data_sardine$N1-mean(data_sardine$N1[c(1:20,22:n_annee)],na.rm=T))/sd(data_sardine$N1[c(1:20,22:n_annee)],na.rm=T) # Observed abundance at age 1
n2.obs.sard.cr <- (data_sardine$N2-mean(data_sardine$N2[c(1:20,22:n_annee)],na.rm=T))/sd(data_sardine$N2[c(1:20,22:n_annee)],na.rm=T) # Observed abundance at age 2
n3.obs.sard.cr <- (data_sardine$N3-mean(data_sardine$N3[c(1:20,22:n_annee)],na.rm=T))/sd(data_sardine$N3[c(1:20,22:n_annee)],na.rm=T) # Observed abundance at age 3
n4.obs.sard.cr <- (data_sardine$N4-mean(data_sardine$N4[c(1:20,22:n_annee)],na.rm=T))/sd(data_sardine$N4[c(1:20,22:n_annee)],na.rm=T) # Observed abundance at age 4
n5.obs.sard.cr <- (data_sardine$N5-mean(data_sardine$N5[c(1:20,22:n_annee)],na.rm=T))/sd(data_sardine$N5[c(1:20,22:n_annee)],na.rm=T) # Observed abundance at age 5
# Create N6 instead of N6+
n6.obs.sard.cr <- (data_sardine$N6-mean(data_sardine$N6[c(1:20,22:n_annee)],na.rm=T))/sd(data_sardine$N6[c(1:20,22:n_annee)],na.rm=T) # Observed abundance at age 6
# Create N7+
n7plus.obs.sard.cr <- (data_sardine$N7plus-mean(data_sardine$N7plus[c(1:20,22:n_annee)],na.rm=T))/sd(data_sardine$N7plus[c(1:20,22:n_annee)],na.rm=T) # Observed abundance at age 7+
# nlarge with 7+
nlarge.obs.sard.cr <- ((data_sardine$N3+data_sardine$N4+data_sardine$N5+data_sardine$N6+data_sardine$N7plus)-mean((data_sardine$N3+data_sardine$N4+data_sardine$N5+data_sardine$N6+data_sardine$N7plus)[c(1:20,22:n_annee)],na.rm=T))/sd((data_sardine$N3+data_sardine$N4+data_sardine$N5+data_sardine$N6+data_sardine$N7plus)[c(1:20,22:n_annee)],na.rm=T)
n.ssn.obs.sard.cr <- ((data_sardine$N1*mat[,1,2]+data_sardine$N2*mat[,2,2]+data_sardine$N3*mat[,3,2]+data_sardine$N4*mat[,4,2]+data_sardine$N5*mat[,5,2]+data_sardine$N6*mat[,6,2]+data_sardine$N7plus*mat[,7,2])-mean(((data_sardine$N1*mat[,1,2]+data_sardine$N2*mat[,2,2]+data_sardine$N3*mat[,3,2]+data_sardine$N4*mat[,4,2]+data_sardine$N5*mat[,5,2]+data_sardine$N6*mat[,6,2]+data_sardine$N7plus*mat[,7,2]))[c(1:20,22:n_annee)],na.rm=T)[1])/sd(((data_sardine$N1*mat[,1,2]+data_sardine$N2*mat[,2,2]+data_sardine$N3*mat[,3,2]+data_sardine$N4*mat[,4,2]+data_sardine$N5*mat[,5,2]+data_sardine$N6*mat[,6,2]+data_sardine$N7plus*mat[,7,2]))[c(1:20,22:n_annee)],na.rm=T)[1]
n.ssn.obs.sard <- data_sardine$N1*mat[,1,2]+data_sardine$N2*mat[,2,2]+data_sardine$N3*mat[,3,2]+data_sardine$N4*mat[,4,2]+data_sardine$N5*mat[,5,2]+data_sardine$N6*mat[,6,2]+data_sardine$N7plus*mat[,7,2]
nlarge.obs.sard <- rowSums(data_sardine[,c("N2","N3","N4","N5","N6","N7plus")],na.rm=T)
nlarge.obs.sard <- replace(nlarge.obs.sard,nlarge.obs.sard == 0,NA) 

#°°°° sprat
data_sprat$wabun <- as.numeric(data_sprat$wabun)
nsprat.obs.cr <- (data_sprat$wabun-mean(data_sprat$wabun[c(1:20,22:n_annee)],na.rm=T))/sd(data_sprat$wabun[c(1:20,22:n_annee)],na.rm=T)




### Size ###

#°°°° anchovy
l1.obs.anch.cr <- (data_anchovy$L1-mean(data_anchovy$L1[c(1:20,22:n_annee)],na.rm=T))/sd(data_anchovy$L1[c(1:20,22:n_annee)],na.rm=T) # Abondance observee à l'âge 1
l2.obs.anch.cr <- (data_anchovy$L2-mean(data_anchovy$L2[c(1:20,22:n_annee)],na.rm=T))/sd(data_anchovy$L2[c(1:20,22:n_annee)],na.rm=T)
l3.obs.anch.cr <- (data_anchovy$L3-mean(data_anchovy$L3[c(1:20,22:n_annee)],na.rm=T))/sd(data_anchovy$L3[c(1:20,22:n_annee)],na.rm=T)
l4.obs.anch.cr <- (data_anchovy$L4-mean(data_anchovy$L4[c(1:20,22:n_annee)],na.rm=T))/sd(data_anchovy$L4[c(1:20,22:n_annee)],na.rm=T)

l.ssn.obs.anch.cr<-((((data_anchovy$L1*(data_anchovy$N1*mat[,1,1])+
                         data_anchovy$L2*(data_anchovy$N2*mat[,2,1])+
                         data_anchovy$L3*(data_anchovy$N3*mat[,3,1])+
                         data_anchovy$L4*(data_anchovy$N4plus*mat[,4,1]))/n.ssn.obs.anch)
                     -mean(  (data_anchovy$L1[c(1:n_annee)]*(data_anchovy$N1[c(1:n_annee)]*mat[,1,1])+
                                data_anchovy$L2[c(1:n_annee)]*(data_anchovy$N2[c(1:n_annee)]*mat[,2,1])+
                                data_anchovy$L3[c(1:n_annee)]*(data_anchovy$N3[c(1:n_annee)]*mat[,3,1])+
                                data_anchovy$L4[c(1:n_annee)]*(data_anchovy$N4plus[c(1:n_annee)]*mat[,4,1]))/n.ssn.obs.anch[c(1:n_annee)],na.rm=T))
                    /sd( (data_anchovy$L1[c(1:n_annee)]*(data_anchovy$N1[c(1:n_annee)]*mat[,1,1])+
                            data_anchovy$L2[c(1:n_annee)]*(data_anchovy$N2[c(1:n_annee)]*mat[,2,1])+
                            data_anchovy$L3[c(1:n_annee)]*(data_anchovy$N3[c(1:n_annee)]*mat[,3,1])+
                            data_anchovy$L4[c(1:n_annee)]*(data_anchovy$N4plus[c(1:n_annee)]*mat[,4,1]))/n.ssn.obs.anch[c(1:n_annee)],na.rm=T))

#°°°° sardine
l1.obs.sard.cr <- (data_sardine$L1-mean(data_sardine$L1[c(1:20,22:n_annee)],na.rm=T))/sd(data_sardine$L1[c(1:20,22:n_annee)],na.rm=T) # Abondance observee à l'âge 1
l2.obs.sard.cr <- (data_sardine$L2-mean(data_sardine$L2[c(1:20,22:n_annee)],na.rm=T))/sd(data_sardine$L2[c(1:20,22:n_annee)],na.rm=T)
l3.obs.sard.cr <- (data_sardine$L3-mean(data_sardine$L3[c(1:20,22:n_annee)],na.rm=T))/sd(data_sardine$L3[c(1:20,22:n_annee)],na.rm=T)
l4.obs.sard.cr <- (data_sardine$L4-mean(data_sardine$L4[c(1:20,22:n_annee)],na.rm=T))/sd(data_sardine$L4[c(1:20,22:n_annee)],na.rm=T)
l5.obs.sard.cr <- (data_sardine$L5-mean(data_sardine$L5[c(1:20,22:n_annee)],na.rm=T))/sd(data_sardine$L5[c(1:20,22:n_annee)],na.rm=T)
l6.obs.sard.cr <- (data_sardine$L6-mean(data_sardine$L6[c(1:20,22:n_annee)],na.rm=T))/sd(data_sardine$L6[c(1:20,22:n_annee)],na.rm=T)
l7.obs.sard.cr <- (data_sardine$L7plus-mean(data_sardine$L7plus[c(1:20,22:n_annee)],na.rm=T))/sd(data_sardine$L7plus[c(1:20,22:n_annee)],na.rm=T)

l.ssn.obs.sard.cr<-((((data_sardine$L1*(data_sardine$N1*mat[,1,2])+
                         data_sardine$L2*(data_sardine$N2*mat[,2,2])+
                         data_sardine$L3*(data_sardine$N3*mat[,3,2])+
                         data_sardine$L4*(data_sardine$N4*mat[,4,2])+
                         data_sardine$L5*(data_sardine$N5*mat[,5,2])+
                         data_sardine$L6*(data_sardine$N6*mat[,6,2]) +
                         data_sardine$L7plus*(data_sardine$N7plus*mat[,7,2]))/n.ssn.obs.sard)
                     -mean(  (data_sardine$L1[c(1:n_annee)]*(data_sardine$N1[c(1:n_annee)]*mat[,1,2])+
                                data_sardine$L2[c(1:n_annee)]*(data_sardine$N2[c(1:n_annee)]*mat[,2,2])+
                                data_sardine$L3[c(1:n_annee)]*(data_sardine$N3[c(1:n_annee)]*mat[,3,2])+
                                data_sardine$L4[c(1:n_annee)]*(data_sardine$N4[c(1:n_annee)]*mat[,4,2])+
                                data_sardine$L5[c(1:n_annee)]*(data_sardine$N5[c(1:n_annee)]*mat[,5,2])+
                                data_sardine$L6[c(1:n_annee)]*(data_sardine$N6[c(1:n_annee)]*mat[,6,2])+
                                data_sardine$L7plus[c(1:n_annee)]*(data_sardine$N7plus[c(1:n_annee)]*mat[,7,2])
                     )/n.ssn.obs.sard[c(1:n_annee)],na.rm=T))
                    /sd( (data_sardine$L1[c(1:n_annee)]*(data_sardine$N1[c(1:n_annee)]*mat[,1,2])+
                            data_sardine$L2[c(1:n_annee)]*(data_sardine$N2[c(1:n_annee)]*mat[,2,2])+
                            data_sardine$L3[c(1:n_annee)]*(data_sardine$N3[c(1:n_annee)]*mat[,3,2])+
                            data_sardine$L4[c(1:n_annee)]*(data_sardine$N4[c(1:n_annee)]*mat[,4,2])+
                            data_sardine$L5[c(1:n_annee)]*(data_sardine$N5[c(1:n_annee)]*mat[,5,2])+
                            data_sardine$L6[c(1:n_annee)]*(data_sardine$N6[c(1:n_annee)]*mat[,6,2])+
                            data_sardine$L7plus[c(1:n_annee)]*(data_sardine$N7plus[c(1:n_annee)]*mat[,7,2])
                    )/n.ssn.obs.sard[c(1:n_annee)],na.rm=T))



##------- covariates -------##

data_covar.all <- data.frame( "Year"=2000:(1999+(n_annee-1)),
                              "n1.anch"=n1.obs.anch.cr[1:(n_annee-1)],"n2.anch"=n2.obs.anch.cr[1:(n_annee-1)],"n3.anch"=n3.obs.anch.cr[1:(n_annee-1)],"n4plus.anch"=n4plus.obs.anch.cr[1:(n_annee-1)],
                              "n1.sard"=n1.obs.sard.cr[1:(n_annee-1)],"n2.sard"=n2.obs.sard.cr[1:(n_annee-1)],"n3.sard"=n3.obs.sard.cr[1:(n_annee-1)],"n4.sard"=n4.obs.sard.cr[1:(n_annee-1)],"n5.sard"=n5.obs.sard.cr[1:(n_annee-1)],"n6.sard"=n6.obs.sard.cr[1:(n_annee-1)],"n7.sard"=n7plus.obs.sard.cr[1:(n_annee-1)],
                              "nlarge.anch"=nlarge.obs.anch.cr[1:(n_annee-1)],"nlarge.sard"=nlarge.obs.sard.cr[1:(n_annee-1)],"nsprat"=nsprat.obs.cr[1:(n_annee-1)],
                              "l1.anch"=l1.obs.anch.cr[1:(n_annee-1)],"l2.anch"=l2.obs.anch.cr[1:(n_annee-1)],"l3.anch"=l3.obs.anch.cr[1:(n_annee-1)],"l4.anch"=l4.obs.anch.cr[1:(n_annee-1)],
                              "l1.sard"=l1.obs.sard.cr[1:(n_annee-1)],"l2.sard"=l2.obs.sard.cr[1:(n_annee-1)],"l3.sard"=l3.obs.sard.cr[1:(n_annee-1)],"l4.sard"=l4.obs.sard.cr[1:(n_annee-1)],"l5.sard"=l5.obs.sard.cr[1:(n_annee-1)],"l6.sard"=l6.obs.sard.cr[1:(n_annee-1)],
                              "NAO"=data_AMO_NAO$NAO_index[1:(n_annee-1)],"AMO"=data_AMO_NAO$AMO_index[1:(n_annee-1)],
                              "Zoo.nov_feb"=env_anchovy_stand[1:((n_annee-1)),2],"Temp.nov_feb"=env_anchovy_stand[1:((n_annee-1)),3],"Zoo.jul_oct"=env_anchovy_stand[1:(n_annee-1),4],"Temp.jul_oct"=env_anchovy_stand[1:(n_annee-1),5],
                              "Zoo.jun_oct"=env_sardine_stand[1:(n_annee-1),4],"Temp.jun_oct"=env_sardine_stand[1:(n_annee-1),5],
                              "Zoo.apr_jun"=env_anchovy_stand[1:((n_annee-1)),6],"Temp.apr_jun"=env_anchovy_stand[1:((n_annee-1)),7],
                              "Zoo.mar_may"=env_sardine_stand[1:((n_annee-1)),6],"Temp.mar_may"=env_sardine_stand[1:((n_annee-1)),7],
                              "Zoo.nov_feb_ante"=env_sardine_stand[1:((n_annee-1)),8],"Temp.nov_feb_ante"=env_sardine_stand[1:((n_annee-1)),9],
                              "Zoo.jun_oct_ante"=env_sardine_stand[1:((n_annee-1)),10],"Temp.jun_oct_ante"=env_sardine_stand[1:((n_annee-1)),11],
                              "n.ssn.anch"=n.ssn.obs.anch.cr[1:(n_annee-1)],"n.ssn.sard"=n.ssn.obs.sard.cr[1:(n_annee-1)],
                              "l.ssn.anch"=l.ssn.obs.anch.cr[1:(n_annee-1)],"l.ssn.sard"=l.ssn.obs.sard.cr[1:(n_annee-1)])

#to replace NA in 2021 and 2022 (Temperature & Zoo) --> NA not accepted within covariates
for(i in 1:nrow(data_covar.all))
{
  for(j in 2:ncol(data_covar.all))
  {
    if (is.na(data_covar.all[i, j])) {
      data_covar.all[i, j] <- mean(data_covar.all[(i-3):(i-1), j], na.rm = TRUE)
    }
  }
}

##------------Observation data-----------##

# to replace NA in sigma.obs : cautious approach : 2020 is associated with the maximum variance of the time serie

# Replace NA with max for columns containing "sigma_N" or "sigma_L" in data_anchovy
for (col in names(data_anchovy)[grepl("sigma_N|sigma_L", names(data_anchovy))]) {
  data_anchovy[[col]][is.na(data_anchovy[[col]])] <- max(data_anchovy[[col]], na.rm = TRUE)
}
# Replace NA with max for columns containing "sigma_N" or "sigma_L" in data_sardine
for (col in names(data_sardine)[grepl("sigma_N|sigma_L", names(data_sardine))]) {
  data_sardine[[col]][is.na(data_sardine[[col]])] <- max(data_sardine[[col]], na.rm = TRUE)
}

# Defining the covariates of the model
#'************************************************************
####################################
#°°°° anchovy

# Intrinsic demographic effects on M
covar.set.M1.anch<-c("n1.anch","n2.anch","nlarge.anch","n1.sard","n2.sard","nlarge.sard","l1.anch")
covar.set.M2.anch<-c("n1.anch","n2.anch","nlarge.anch","n1.sard","n2.sard","nlarge.sard","l2.anch")
covar.set.M3.anch<-c("n1.anch","n2.anch","nlarge.anch","n1.sard","n2.sard","nlarge.sard","l3.anch")

# Intrinsic demographic effects on g
covar.set.g1.anch<-c("n1.anch","n2.anch","nlarge.anch","n1.sard","n2.sard","nlarge.sard","l1.anch")
covar.set.g2.anch<-c("n1.anch","n2.anch","nlarge.anch","n1.sard","n2.sard","nlarge.sard","l2.anch")
covar.set.g3.anch<-c("n1.anch","n2.anch","nlarge.anch","n1.sard","n2.sard","nlarge.sard","l3.anch")

# Intrinsic demographic effects on ZSSN (or Re,t)
covar.set.ZSSN.anch<-c("n.ssn.anch","n.ssn.sard","n1.sard","n2.sard","nlarge.sard","l.ssn.anch")

#°°°° sardine

# Intrinsic demographic effects on M
covar.set.M1.sard<-c("n1.anch","n2.anch","nlarge.anch","n1.sard","n2.sard","nlarge.sard","l1.sard")
covar.set.M2.sard<-c("n1.anch","n2.anch","nlarge.anch","n1.sard","n2.sard","nlarge.sard","l2.sard")
covar.set.M3.sard<-c("n1.anch","n2.anch","nlarge.anch","n1.sard","n2.sard","nlarge.sard","l3.sard")
covar.set.M4.sard<-c("n1.anch","n2.anch","nlarge.anch","n1.sard","n2.sard","nlarge.sard","l4.sard")
covar.set.M5.sard<-c("n1.anch","n2.anch","nlarge.anch","n1.sard","n2.sard","nlarge.sard","l5.sard")
covar.set.M6.sard<-c("n1.anch","n2.anch","nlarge.anch","n1.sard","n2.sard","nlarge.sard","l6.sard")

# Intrinsic demographic effects on g
covar.set.g1.sard<-c("n1.anch","n2.anch","nlarge.anch","n1.sard","n2.sard","nlarge.sard","l1.sard")
covar.set.g2.sard<-c("n1.anch","n2.anch","nlarge.anch","n1.sard","n2.sard","nlarge.sard","l2.sard")
covar.set.g3.sard<-c("n1.anch","n2.anch","nlarge.anch","n1.sard","n2.sard","nlarge.sard","l3.sard")
covar.set.g4.sard<-c("n1.anch","n2.anch","nlarge.anch","n1.sard","n2.sard","nlarge.sard","l4.sard")
covar.set.g5.sard<-c("n1.anch","n2.anch","nlarge.anch","n1.sard","n2.sard","nlarge.sard","l5.sard")
covar.set.g6.sard<-c("n1.anch","n2.anch","nlarge.anch","n1.sard","n2.sard","nlarge.sard","l6.sard")

#complet demographique sur Z.SSN
covar.set.ZSSN.sard<-c("n.ssn.anch","n.ssn.sard","n1.anch","n2.anch","nlarge.anch","l.ssn.sard")

####################################################

#°°°° anchovy
data_covar.M1.anch <- data_covar.all %>% select(covar.set.M1.anch)
data_covar.M2.anch <- data_covar.all %>% select(covar.set.M2.anch)
data_covar.M3.anch <- data_covar.all %>% select(covar.set.M3.anch)

data_covar.g1.anch <- data_covar.all %>% select(covar.set.g1.anch)
data_covar.g2.anch <- data_covar.all %>% select(covar.set.g2.anch)
data_covar.g3.anch <- data_covar.all %>% select(covar.set.g3.anch)

data_covar.Z.SSN.anch <- data_covar.all %>% select(covar.set.ZSSN.anch)


ncov.M.anch <- ncol(data_covar.M1.anch)   #nb de covar : NAO, n1.obs... = ncol()
ncov.g.anch <- ncol(data_covar.g1.anch)
ncov.zssn.anch <- ncol(data_covar.Z.SSN.anch)


X.M1.anch=as.matrix(data_covar.M1.anch[,1:ncol(data_covar.M1.anch)])
X.M2.anch=as.matrix(data_covar.M2.anch[,1:ncol(data_covar.M2.anch)])
X.M3.anch=as.matrix(data_covar.M3.anch[,1:ncol(data_covar.M3.anch)])

X.g1.anch=as.matrix(data_covar.g1.anch[,1:ncol(data_covar.g1.anch)])
X.g2.anch=as.matrix(data_covar.g2.anch[,1:ncol(data_covar.g2.anch)])
X.g3.anch=as.matrix(data_covar.g3.anch[,1:ncol(data_covar.g3.anch)])

X.ZSSN.anch=as.matrix(data_covar.Z.SSN.anch[,1:ncol(data_covar.Z.SSN.anch)])

#°°°° sardine
data_covar.M1.sard <- data_covar.all %>% select(covar.set.M1.sard)
data_covar.M2.sard <- data_covar.all %>% select(covar.set.M2.sard)
data_covar.M3.sard <- data_covar.all %>% select(covar.set.M3.sard)
data_covar.M4.sard <- data_covar.all %>% select(covar.set.M4.sard)
data_covar.M5.sard <- data_covar.all %>% select(covar.set.M5.sard)
data_covar.M6.sard <- data_covar.all %>% select(covar.set.M6.sard)

data_covar.g1.sard <- data_covar.all %>% select(covar.set.g1.sard)
data_covar.g2.sard <- data_covar.all %>% select(covar.set.g2.sard)
data_covar.g3.sard <- data_covar.all %>% select(covar.set.g3.sard)
data_covar.g4.sard <- data_covar.all %>% select(covar.set.g4.sard)
data_covar.g5.sard <- data_covar.all %>% select(covar.set.g5.sard)
data_covar.g6.sard <- data_covar.all %>% select(covar.set.g6.sard)


data_covar.Z.SSN.sard <- data_covar.all %>% select(covar.set.ZSSN.sard)


ncov.M.sard <- ncol(data_covar.M1.sard)   #nb de covar : NAO, n1.obs... = ncol()
ncov.g.sard <- ncol(data_covar.g1.sard)
ncov.zssn.sard <- ncol(data_covar.Z.SSN.sard)


X.M1.sard=as.matrix(data_covar.M1.sard[,1:ncol(data_covar.M1.sard)])
X.M2.sard=as.matrix(data_covar.M2.sard[,1:ncol(data_covar.M2.sard)])
X.M3.sard=as.matrix(data_covar.M3.sard[,1:ncol(data_covar.M3.sard)])
X.M4.sard=as.matrix(data_covar.M4.sard[,1:ncol(data_covar.M4.sard)])
X.M5.sard=as.matrix(data_covar.M5.sard[,1:ncol(data_covar.M5.sard)])
X.M6.sard=as.matrix(data_covar.M6.sard[,1:ncol(data_covar.M6.sard)])

X.g1.sard=as.matrix(data_covar.g1.sard[,1:ncol(data_covar.g1.sard)])
X.g2.sard=as.matrix(data_covar.g2.sard[,1:ncol(data_covar.g2.sard)])
X.g3.sard=as.matrix(data_covar.g3.sard[,1:ncol(data_covar.g3.sard)])
X.g4.sard=as.matrix(data_covar.g4.sard[,1:ncol(data_covar.g4.sard)])
X.g5.sard=as.matrix(data_covar.g5.sard[,1:ncol(data_covar.g5.sard)])
X.g6.sard=as.matrix(data_covar.g6.sard[,1:ncol(data_covar.g6.sard)])

X.ZSSN.sard=as.matrix(data_covar.Z.SSN.sard[,1:ncol(data_covar.Z.SSN.sard)])

#M
X.M1.list=as.vector(c(X.M1.anch,X.M1.sard))
X.M1=array(X.M1.list,dim=c((n_annee-1),ncov.M.anch,2),dimnames=list(1:(n_annee-1),c("var1","var2","var3","var4","var5","var6","var7"),c("1","2")))

X.M2.list=as.vector(c(X.M2.anch,X.M2.sard))
X.M2=array(X.M2.list,dim=c((n_annee-1),ncov.M.anch,2),dimnames=list(1:(n_annee-1),c("var1","var2","var3","var4","var5","var6","var7"),c("1","2")))

X.M3.list=as.vector(c(X.M3.anch,X.M3.sard))
X.M3=array(X.M3.list,dim=c((n_annee-1),ncov.M.anch,2),dimnames=list(1:(n_annee-1),c("var1","var2","var3","var4","var5","var6","var7"),c("1","2")))

#g
X.g1.list=as.vector(c(X.g1.anch,X.g1.sard))
X.g1=array(X.g1.list,dim=c((n_annee-1),ncov.g.anch,2),dimnames=list(1:(n_annee-1),c("var1","var2","var3","var4","var5","var6","var7"),c("1","2")))

X.g2.list=as.vector(c(X.g2.anch,X.g2.sard))
X.g2=array(X.g2.list,dim=c((n_annee-1),ncov.g.anch,2),dimnames=list(1:(n_annee-1),c("var1","var2","var3","var4","var5","var6","var7"),c("1","2")))

X.g3.list=as.vector(c(X.g3.anch,X.g3.sard))
X.g3=array(X.g3.list,dim=c((n_annee-1),ncov.g.anch,2),dimnames=list(1:(n_annee-1),c("var1","var2","var3","var4","var5","var6","var7"),c("1","2")))

X.ZSSN.list=as.vector(c(X.ZSSN.anch,X.ZSSN.sard))
X.ZSSN=array(X.ZSSN.list,dim=c((n_annee-1),ncov.zssn.anch,2),dimnames=list(1:(n_annee-1),c("var1","var2","var3","var4","var5","var6"),c("1","2")))



# Model inputs 
#'************************************************************


data <- list(
  "X.M1"=X.M1,"X.M2"=X.M2,"X.M3"=X.M3,"X.g1"=X.g1,"X.g2"=X.g2,"X.g3"=X.g3,"X.ZSSN"=X.ZSSN,
  
  "ncov.M"=c(ncov.M.anch,ncov.M.sard),"ncov.g"=c(ncov.g.anch,ncov.g.sard),"ncov.zssn"=c(ncov.zssn.anch,ncov.zssn.sard),
  
  "n_annee" = length(data_anchovy$Year), # durée de l'analyse (en années)
  # Variables observées  
  "n1.obs" = matrix(c(data_anchovy$N1,data_sardine$N1),ncol=2), # Abondance age 1
  "n2.obs" = matrix(c(data_anchovy$N2,data_sardine$N2),ncol=2), # Abondance age 2
  "n3.obs" = matrix(c(data_anchovy$N3,data_sardine$N3),ncol=2), # Abondance age 3
  "n4.obs" = matrix(c(data_anchovy$N4plus,data_sardine$N4),ncol=2), # Abondance age 4,
  "n5.obs" = matrix(c(rep(NA,23),data_sardine$N5),ncol=2), # Abondance age 5,
  "n6.obs" = matrix(c(rep(NA,23),data_sardine$N6),ncol=2), # Abondance age 6,
  "n7.obs" = matrix(c(rep(NA,23),data_sardine$N7plus),ncol=2), # Abondance age 7,
  
  "nlarge.obs"= matrix(c(nlarge.obs.anch,nlarge.obs.sard),ncol=2), # Abondance older fishes,
  "n.ssn.obs"= matrix(c(n.ssn.obs.anch,n.ssn.obs.sard),ncol=2), # Abondance mature fishes,
  
  "l1.obs" = matrix(c(data_anchovy$L1,data_sardine$L1),ncol=2), # Taille age 1
  "l2.obs" = matrix(c(data_anchovy$L2,data_sardine$L2),ncol=2), # Taille age 2
  "l3.obs" = matrix(c(data_anchovy$L3,data_sardine$L3),ncol=2), # Taille age 3
  "l4.obs" = matrix(c(data_anchovy$L4,data_sardine$L4),ncol=2), # Taille age 4
  "l5.obs" = matrix(c(rep(NA,23),data_sardine$L5),ncol=2), # Taille age 5
  "l6.obs" = matrix(c(rep(NA,23),data_sardine$L6),ncol=2), # Taille age 6
  "l7.obs" = matrix(c(rep(NA,23),data_sardine$L7plus),ncol=2), # Taille age 7
  
  "c1.obs" = matrix(c(data_anchovy$C1+1,data_sardine$C1+1),ncol=2), # Capture age 1
  "c2.obs" = matrix(c(data_anchovy$C2+1,data_sardine$C2+1),ncol=2), # Capture age 2
  "c3.obs" = matrix(c(data_anchovy$C3+1,data_sardine$C3+1),ncol=2), # Capture age 3
  "c4.obs" = matrix(c(rep(NA,23),data_sardine$C4+1),ncol=2), # Capture age 4
  "c5.obs" = matrix(c(rep(NA,23)+1,data_sardine$C5+1),ncol=2), # Capture age 5
  "c6.obs" = matrix(c(rep(NA,23)+1,data_sardine$C6+1),ncol=2), # Capture age 5
  
  "mat" =mat,  # matrice de maturite aux ages 1 a 4 anchois + matrice de maturite aux ages 1 a 6 sardines
  
  # erreur observation dans les données 
  "sigma.n.obs" = matrix(c(data_anchovy$sigma_N,data_sardine$sigma_N),ncol=2), # erreur annuelle des abondances = ecartype tot (de ts les ages) pr chaq an
  "sigma.l1.obs" = matrix(c(data_anchovy$sigma_L1,data_sardine$sigma_L1),ncol=2), # erreur  des Longueur corporelle age 1
  "sigma.l2.obs" = matrix(c(data_anchovy$sigma_L2,data_sardine$sigma_L2),ncol=2), # erreur des Longueur corporelle age 2
  "sigma.l3.obs" = matrix(c(data_anchovy$sigma_L3,data_sardine$sigma_L3),ncol=2), # erreur des Longueur corporelle age 3
  "sigma.l4.obs" = matrix(c(data_anchovy$sigma_L4,data_sardine$sigma_L4),ncol=2), # erreur des Longueur corporelle age 4
  "sigma.l5.obs" = matrix(c(rep(NA,23),data_sardine$sigma_L5),ncol=2), # erreur des Longueur corporelle age 4
  "sigma.l6.obs" = matrix(c(rep(NA,23),data_sardine$sigma_L6),ncol=2), # erreur des Longueur corporelle age 4
  "sigma.l7.obs" = matrix(c(rep(NA,23),data_sardine$sigma_L7),ncol=2) # erreur des Longueur corporelle age7 

)
# "+1" sur chaque ligne pour éviter les données de 0 pour pouvoir tirer les valerus dans une lognormale



# Model
#'************************************************************

# Reading the model to be implemented 
source(file.path(base,"2_MODEL",rjags_file_name)) # name of the model to be fetched, written in a txt file 

model <- textConnection(model_simple) # specifying to R that we are using a JAGS language



# Saved variables 
#'************************************************************

variables_set <- c(
  # State variables (estimated variables)
  "N1","N2","N3","N4","N5","N6","N7","N.SSN", # Abundance
  "I1","I2","I3","I4","I5","I6","I7","L.SSN", # Absolute abundance index
  "L1","L2","L3","L4","L5","L6","L7", # Body size (total length)
  "C1","C2","C3","C4","C5","C6", # Catches
  "M1","M2","M3","M4","M5","M6", # Natural mortality
  "F1","F2","F3","F4","F5","F6", # Fishing mortality
  "Z1","Z2","Z3","Z4","Z5","Z6","Z.SSN", # Total mortality
  "g1","g2","g3","g4","g5","g6", # Pseudo-growth
  
  # Interannual variability
  "sigma.M1","sigma.M2","sigma.M3","sigma.M4","sigma.M5","sigma.M6", # natural mortality process error
  "sigma.F1","sigma.F2","sigma.F3","sigma.F4","sigma.F5","sigma.F6", # fishing mortality process error     
  "sigma.g1","sigma.g2","sigma.g3","sigma.g4","sigma.g5","sigma.g6", # pseudo-growth process error
  "sigma.Z.SSN", # = Re,t recruitment process error
  
  "n1.pp","n2.pp","n3.pp","n4.pp","n5.pp","n6.pp","n7.pp",     # predicted abundances/Mean estimate (espérance) of predicted abundances
  "c1.pp","c2.pp","c3.pp","c4.pp","c5.pp","c6.pp",             # predicted catches/Mean estimate  (espérance) of predicted catches
  "l1.pp","l2.pp","l3.pp","l4.pp","l5.pp","l6.pp","l7.pp",     # predicted body sizes/Mean estimate  (espérance) of predicted body sizes
  
  
  # Variable set for SSVS
  "tau_in",
  
  "tau_beta.M1","tau_beta.M2","tau_beta.M3",
  "p_inclusion.M1","p_inclusion.M2","p_inclusion.M3",
  "beta.M1","beta.M2","beta.M3",
  
  "tau_beta.g1","tau_beta.g2","tau_beta.g3",
  "p_inclusion.g1","p_inclusion.g2","p_inclusion.g3",
  "beta.g1","beta.g2","beta.g3",
  
  "tau_beta.zssn",
  "p_inclusion.zssn",
  "beta.zssn"
)



# Running the model and storing outputs
#'************************************************************


n_chains = 3 # number of MCMC chains
n_adapt = 5000 # number of iterations used as burn-in (bin)

thin = 100 # a value is retrieved every 100 values 
n_samples = 3000 # number of iterations retrieved per string 
n_iter = n_samples*thin # total number of iterations performed by the model
n.burnin = 5000 # number of iterations used as burn-in (bin)


source(file.path(base,"2_MODEL",rjags_file_name)) # name of the model to be fetched, written in a txt file 
setwd(file.path(base,"2_MODEL"))


PARA = T

# RUN ITERATIONS

if(PARA==T)
{
  
  cluster <- makePSOCKcluster(names=c(n_chains)) # to run the model by dividing my 3 MCMC strings on 3 cores of my computer 
  time_compile <- system.time(
    res <- jags.parfit(cluster,
                       data = data,
                       params = variables_set,
                       model = model_run, #rjags_file_name
                       n.chains = n_chains,
                       n.adapt = n_adapt,
                       n.update = n.burnin,
                       n.iter = n_iter,
                       thin = thin,
                       inits=NULL)
    #warnings()
  )
  time_compile
}
#'************************************************************



# Saving outputs
#'************************************************************
if(!exists(file.path(base, "4_OUTPUTS"))){dir.create(file.path(base, "4_OUTPUTS"))}
DateFile = file.path(base, "4_OUTPUTS", paste0("iter=", n_iter,  "_", format(Sys.time(), "%d_%b_%Y_%H.%M"), "_",  main_name ))
dir.create(DateFile)
setwd(DateFile)
save(res, file = "Joint_model.Rdata")
write(model_simple,file(rjags_file_name))
dir.create("figures")
setwd(paste(DateFile, "/figures", sep = ""))
dir.create("posterior")
dir.create("time_series_variables")
dir.create("coefs")
setwd(paste(DateFile, "/figures/coefs", sep = ""))
dir.create("SSVS_anchovy")
dir.create("SSVS_sardine")
setwd(paste(DateFile, "/figures/posterior", sep = ""))
dir.create("anchovy")
dir.create("sardine")
setwd(paste(DateFile, "/figures/time_series_variables", sep = ""))
dir.create("anchovy")
dir.create("sardine")
#'************************************************************




# Calculate WAIC
#'************************************************************
# see also https://gist.github.com/oliviergimenez/68ad17910a62635ff6a062f8ec34292f
# Plummer - 2017 - I have added a WAIC monitor to the dic module in JAGS 4.3.0.
# Here the component "deviance" is the deviance statistic for each observed node and "WAIC" is the corresponding WAIC penalty.
# should add the two component to compute WAIC

# Note : need the module "dic"

mcmc.waic <- jags.samples(model = res, 
                          c("WAIC","deviance"), 
                          type = "mean",
                          n.iter = n_iter,
                          n.burnin = n.burnin,
                          n.thin = thin)
mcmc.waic$p_waic <- mcmc.waic$WAIC
mcmc.waic$waic <- mcmc.waic$deviance + mcmc.waic$p_waic
tmp <- sapply(mcmc.waic, sum)
waic <- round(c(waic = tmp[["waic"]], p_waic = tmp[["p_waic"]]),1)
waic
#'************************************************************

