

library(Rcpp)
library(sf)
library(mapdata)
library(maps)
library(ggplot2)
library(classInt)
library(dplyr)
library(RColorBrewer)
library(tidyverse)
library(marmap)
library(basemaps)
library(ggOceanMaps)
library(here)

#représentation des zones de pêches de chaque période
fao<-st_read(here("4_OUTPUTS","Figure1","division_fao.shp"),crs=4326)
traitcote<-st_read(here("4_OUTPUTS","Figure1","world_borders.shp"),crs=4326)
reseau<-st_read(here("4_OUTPUTS","Figure1","reseau.shp"),crs=4326)
isobathes<-st_read(here("4_OUTPUTS","Figure1","DCSMM_isobathes100_gascogn1km_WGS84.shp"),crs=4326)

#graph
p<-ggplot(fao%>%filter(sect_cod%in%c("27.8.A","27.8.B")))+
  geom_sf(aes(fill=sect_cod),alpha=0.4,color="black")+
  scale_fill_manual(values = c("27.8.A"="#99CCFF","27.8.B"="#99CCFF"))+
  geom_sf(data=isobathes%>%filter((CONTOUR>-800)), color="#996633",alpha=0.1)+  
  geom_sf(data=traitcote, fill="#CCCCCC")+
  geom_sf(data=reseau, color="#FF3300")+  
  geom_sf_text(aes(label=sect_cod))+
  xlab("Longitude")+
  ylab("Latitude")+
  xlim(-9,-1)+
  ylim(43,49)+
  annotate(geom="text",x=-2,y=48,label="FRANCE")+
  annotate(geom="text",x=-6,y=43,label="SPAIN")+
  theme(legend.position = "none")

p


#graph

remotes::install_github("MikkoVihtakari/ggOceanMaps")
library(ggplot2)
library(ggOceanMaps)

dt <- data.frame(lon = c(-6, 2), lat = c(42.5,50))
if(requireNamespace("ggspatial", quietly = TRUE)) {
  basemap(data = dt, bathy.style = "rcb", land.col ="black",grid.col = NA) +ylab("")+xlab("")+
    ggspatial::geom_spatial_point(data = dt, aes(x = lon, y = lat)) +
    geom_sf(data= fao %>%filter(sect_cod%in%c("27.8.A","27.8.B")),mapping=aes(fill=sect_cod),alpha=0.4, color=NA)
    
    
}


p<-ggplot(fao%>%filter(sect_cod%in%c("27.8.A","27.8.B")))+
  geom_sf(aes(fill=sect_cod),alpha=0.4, color=NA)+
  scale_fill_manual(values = c("27.8.A"=alpha("",0.9),"27.8.B"=alpha("#a0f4a0",0.9)))+
  geom_sf(data=isobathes%>%filter((CONTOUR>-800)),alpha=0.1)+  
  geom_sf(data=traitcote, fill="black",color="black")+
  geom_sf(data=reseau, color="#FF3300")+  
 # geom_sf_text(aes(label=sect_cod))+
  xlab("Longitude")+
  ylab("Latitude")+
  xlim(-8,-0.5)+
  ylim(43,49)+
  annotate(geom="text",x=-2,y=48,label="FRANCE", color="white")+
  annotate(geom="text",x=-6,y=43,label="SPAIN", color="white")+
  theme(legend.position = "none",panel.background = element_rect(fill =alpha("white",0.1),color="black"))
p

ggsave("figure1.png", width = 15, height = 15, units = "cm")


basemap(c(-11, 0, 43, 49), bathymetry = TRUE,grid.col = NA) 


# Script to make beautifull maps with depth ------------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------------------------------------------------
# Tutorial https://mikkovihtakari.github.io/ggOceanMaps/articles/ggOceanMaps.html


remotes::install_github("MikkoVihtakari/ggOceanMaps")
library(ggplot2)
library(ggOceanMaps)

dt <- data.frame(lon = c(-7, 0.1), lat = c(43,50))
if(requireNamespace("ggspatial", quietly = TRUE)) {
p<-  basemap(data = dt, bathy.style = "rcb", land.col ="lightgoldenrod1",grid.col = NA) +ylab("")+xlab("")+
    ggspatial::geom_spatial_point(data = dt, aes(x = lon, y = lat)) + theme(legend.position = "none")
}
p

ggsave("figuregaspard.pdf", width = 118, height = 84, units = "cm")
