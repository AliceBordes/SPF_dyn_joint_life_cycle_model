#'************************************************************
#'************************************************************
# Object : Visualizing data of the study system anchovy - sardine of the Bay of Biscay
#
# Author : Alice Bordes - M2 Internship - February to August 2022  
# Data : collected during the annual spring acoustic surveys PELGAS, from 2000 to 2022
#
#'************************************************************
#'************************************************************

#remove(list = ls())

# Loading libraries ----
#'************************************************************
library(tidyverse)
library(cowplot)
library(readODS)
library(mgcv)
library(MASS)
library(openxlsx)
library(patchwork)
library(ggridges)
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
library(rstanarm)
library(cowplot)
library(here)
#'************************************************************


# Loading functions ----
#'************************************************************
base <- here()
setwd(file.path(base,"3_FUNCTIONS"))
source("f.observation.data.time.serie.R")
#'************************************************************



# Loading data ----
#'************************************************************
data_anchovy <- read.table(file.path(base,"1_DATA","data_anchovy_NA.txt"), sep="\t", h=T) # Biological data (anchovy abundances, sizes, captures by life stage)
data_sardine <- read.table(file.path(base,"1_DATA","data_sardine_NA.txt"), sep="\t", h=T) # Biological data (sardine abundances, sizes, captures by life stage)

maturite_anch <- as.matrix(read.table(file.path(base,"1_DATA", "maturity_anchovy.txt"), sep="\t", h=T, row.names = NULL)) # Biological data (anchovy maturity by life stage)
maturite_sard <- as.matrix(read.table(file.path(base,"1_DATA", "maturity_sardine.txt"), sep="\t", h=T, row.names = NULL)) # Biological data (sardine maturity by life stage)

data_sprat <- read.table(file.path(base,"1_DATA", "data_sprat_NA.txt"), sep="\t",dec=",", h=T) # Biological data (sprat total abundance)
data_sprat$wabun <- as.numeric(data_sprat$wabun)
data_AMO_NAO <- read.table(file.path(base,"1_DATA", "data_AMO_NAO.txt"), sep="",dec=".",fill = TRUE) # Atlantic Multi-decadal Oscillation (AMO) Index

env_anchovy_stand <- read.table(file.path(base,"1_DATA", "Env_stand_anchovy.txt"), sep="\t", h=T) # Environmental data in the ICES division relative to anchovy presence (zooplancton and temperature)
env_sardine_stand <- read.table(file.path(base,"1_DATA", "Env_stand_sardine.txt"), sep="\t", h=T) # Environmental data in the ICES division relative to sardine presence (zooplancton and temperature)


## Maturity matrix
mat.list <- as.vector(c(maturite_anch,maturite_sard))
mat <- array(mat.list,dim=c(23,7,2),dimnames=list(1:23,c("age1","age2","age3","age4","age5","age6", "age7plus"),c("1","2")))
mat_anchovy<-as.data.frame(mat[,,1][,-c(5,6,7)])
mat_sardine<-as.data.frame(mat[,,2])
#'************************************************************


# Setting parameter names for functions ----
#'************************************************************

anchovy <- "anchovy"
sardine <- "sardine"
both <- "both"
log <- "yes"

# Set vectors of variables 
vector.N.anch <- c("N1","N2","N3","N4plus")
vector.L.anch <- c("L1","L2","L3","L4")
vector.F.anch <- c("F1","F2","F3")
vector.C.anch <- c("C1","C2","C3","C4")
vector.maturity.anch <- c("age1","age2","age3","age4")

vector.N.sard <- c("N1","N2","N3","N4","N5","N6","N7plus")
vector.L.sard <- c("L1","L2","L3","L4","L5","L6","L7")
vector.F.sard <- c("F1","F2","F3","F4","F5","F6")
vector.C.sard <- c("C1","C2","C3","C4","C5","C6")
vector.maturity.sard <- c("age1","age2","age3","age4","age5","age6","age7plus")

vector.zoo.anch <- c("Zoo.nov_feb.stand","Zoo.jul_oct.stand","Zoo.apr_jun.stand")
vector.zoo.sard <- c("Zoo.nov_feb.stand","Zoo.jun_oct.stand","Zoo.mar_may.stand")

vector.temp.anch <- c("Temp.nov_feb.stand","Temp.jul_oct.stand","Temp.apr_jun.stand")
vector.temp.sard <- c("Temp.nov_feb.stand","Temp.jun_oct.stand","Temp.mar_may.stand")

vector.AMO.NAO <- c("AMO_index","NAO_index")

# Set vectors of incertitudes
incertitudes.N.anch <- c("sigma_N","sigma_N","sigma_N","sigma_N")
incertitudes.N.sard <- c("sigma_N","sigma_N","sigma_N","sigma_N","sigma_N","sigma_N","sigma_N")

incertitudes.L.anch <- c("sigma_L1","sigma_L2","sigma_L3","sigma_L4")
incertitudes.L.sard <- c("sigma_L1","sigma_L2","sigma_L3","sigma_L4","sigma_L5","sigma_L6","sigma_L7")
#'************************************************************




# Saving graphs ----
#'************************************************************

if (!dir.exists(file.path(base, "1_DATA", "data_figures"))) {
  dir.create(file.path(base, "1_DATA", "data_figures"))
}

setwd(file.path(base, "1_DATA", "data_figures"))

obs.time.series(vector.N.anch,incertitudes.N.anch,data_anchovy,2022,"no",anchovy)
obs.time.series(vector.N.sard,incertitudes.N.sard,data_sardine,2022,"no",sardine)

obs.time.series(vector.L.anch,incertitudes.L.anch,data_anchovy,2022,"no",anchovy)
obs.time.series(vector.L.sard,incertitudes.L.sard,data_sardine,2022,"no",sardine)

obs.time.series("wabun","no",data_sprat,2022,"no","sprat")

obs.time.series(vector.zoo.anch,"no",env_anchovy_stand,2022,"no",anchovy)
obs.time.series(vector.zoo.sard,"no",env_sardine_stand,2022,"no",sardine)

obs.time.series(vector.temp.anch,"no",env_anchovy_stand,2022,"no",anchovy)
obs.time.series(vector.temp.sard,"no",env_sardine_stand,2022,"no",sardine)

obs.time.series(vector.AMO.NAO,"no",data_AMO_NAO,2022,"no","both")

obs.time.series(vector.maturity.anch,"no",mat_anchovy,2022,"no",anchovy)
obs.time.series(vector.maturity.sard,"no",mat_sardine,2022,"no",sardine)

obs.time.series(vector.C.anch,"no",data_anchovy,2022,"no",anchovy)
obs.time.series(vector.C.sard,"no",data_sardine,2022,"no",sardine)
#'************************************************************



