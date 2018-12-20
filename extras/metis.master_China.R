
#----------------------------
# Install necessary packages
#----------------------------
if("devtools" %in% rownames(installed.packages()) == F){install.packages("devtools")}
library(devtools)
if("metis" %in% rownames(installed.packages()) == F){install_github(repo="zarrarkhan/metis")}
library(metis)
if("rgcam" %in% rownames(installed.packages()) == F){install_github(repo="JGCRI/rgcam")}
library(rgcam)
if("tibble" %in% rownames(installed.packages()) == F){install.packages("tibble")}
library(tibble)
if("dplyr" %in% rownames(installed.packages()) == F){install.packages("dlpyr")}
library(dplyr)
if("rgdal" %in% rownames(installed.packages()) == F){install.packages("rgdal")}
library(rgdal)
if("tmap" %in% rownames(installed.packages()) == F){install.packages("tmap")}
library(tmap)
if("rgeos" %in% rownames(installed.packages()) == F){install.packages("rgeos")}
library(rgeos)

#------------
# India Polygons
#----------------

NE0<-readOGR(dsn=paste(getwd(),"/dataFiles/gis/naturalEarth",sep=""),
                       layer="ne_10m_admin_0_countries",use_iconv=T,encoding='UTF-8')

NE1<-readOGR(dsn=paste(getwd(),"/dataFiles/gis/naturalEarth",sep=""),
             layer="ne_10m_admin_1_states_provinces",use_iconv=T,encoding='UTF-8')

GCAMBasins<-readOGR(dsn=paste(getwd(),"/dataFiles/gis/basin_GCAM",sep=""),
                    layer="Global235_CLM_final_5arcmin_multipart",use_iconv=T,encoding='UTF-8')

metis.map(GCAMBasins,fillColumn = "basin_name",facetsON = F)

projX<-"+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
NE0<-spTransform(NE0,CRS(projX))
NE1<-spTransform(NE1,CRS(projX))
GCAMBasins<-spTransform(GCAMBasins,CRS(projX))

ChinaNE0<-NE0[(NE0$NAME=="China"),]
head(ChinaNE0@data)
plot(ChinaNE0)

ChinaNE1<-raster::crop(NE1,ChinaNE0)
head(ChinaNE1@data)
plot(ChinaNE1)

ChinaGCAMBasins<-raster::crop(GCAMBasins,ChinaNE0)
head(ChinaGCAMBasins@data)
plot(ChinaGCAMBasins)

# Boundaries
#-----------------


boundaries<- metis.boundaries(
                    boundaryRegShape=NE0,#
                    #boundaryRegShpFolder=paste(getwd(),"/dataFiles/gis/naturalEarth",sep=""),#
                    #boundaryRegShpFile=paste("ne_10m_admin_0_countries",sep=""),#
                    boundaryRegCol="NAME",#
                    boundaryRegionsSelect="China",#
                    subRegShape=ChinaGCAMBasins,#
                    #subRegShpFolder=paste(getwd(),"/dataFiles/gis/naturalEarth",sep=""),#
                    #subRegShpFile=paste("ne_10m_admin_1_states_provinces",sep=""),#
                    subRegCol="basin_name",#
                    #subRegionsSelect=NULL,#
                    subRegType="Basin",#
                    #dirOutputs=paste(getwd(),"/outputs",sep=""),#
                    nameAppend="_local",#
                    expandPercent=2,#
                    #overlapShape=indiaGCAMBasin,#
                    #overlapShpFile="Global235_CLM_final_5arcmin_multipart",#
                    #overlapShpFolder=paste(getwd(),"/dataFiles/gis/basin_gcam",sep=""),#
                    labelsSize=0.7,#
                    extension = T,
                    cropSubShape2Bound = F
                    )

boundaries<- metis.boundaries(
                            boundaryRegShape=NE0,#
                            #boundaryRegShpFolder=paste(getwd(),"/dataFiles/gis/naturalEarth",sep=""),#
                            #boundaryRegShpFile=paste("ne_10m_admin_0_countries",sep=""),#
                            boundaryRegCol="NAME",#
                            boundaryRegionsSelect="China",#
                            subRegShape=ChinaNE1,#
                            #subRegShpFolder=paste(getwd(),"/dataFiles/gis/naturalEarth",sep=""),#
                            #subRegShpFile=paste("ne_10m_admin_1_states_provinces",sep=""),#
                            subRegCol="name",#
                            #subRegionsSelect=NULL,#
                            subRegType="States",#
                            #dirOutputs=paste(getwd(),"/outputs",sep=""),#
                            nameAppend="_local",#
                            expandPercent=2,#
                            overlapShape=ChinaGCAMBasins,#
                            #overlapShpFile="Global235_CLM_final_5arcmin_multipart",#
                            #overlapShpFolder=paste(getwd(),"/dataFiles/gis/basin_gcam",sep=""),#
                            labelsSize=0.7,#
                            extension = T,
                            cropSubShape2Bound = F
                          )


