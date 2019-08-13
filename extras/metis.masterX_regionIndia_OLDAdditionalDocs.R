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

indiaNE0<-readOGR(dsn=paste(getwd(),"/dataFiles/gis/naturalEarth",sep=""),
                  layer="ne_10m_admin_0_countries",use_iconv=T,encoding='UTF-8')
indiaNE0<-indiaNE0[(indiaNE0$NAME=="India"),]
head(indiaNE0@data)
plot(indiaNE0)
projX<-proj4string(indiaNE0)

indiaGADM36_1<-readOGR(dsn=paste(getwd(),"/dataFiles/gis/naturalEarth",sep=""),
                       layer="ne_10m_admin_1_states_provinces",use_iconv=T,encoding='UTF-8')
indiaGADM36_1<-indiaGADM36_1[(indiaGADM36_1$NAME=="India"),]
head(indiaGADM36_1@data)
plot(indiaGADM36_1)
projX<-proj4string(indiaGADM36_1)

indiaLocal1<-readOGR(dsn=paste(getwd(),"/dataFiles/gis/admin_India",sep=""),
                     layer="IND_adm1",use_iconv=T,encoding='UTF-8')
indiaLocal1<-spTransform(indiaLocal1,CRS(projX))
indiaLocal1 <- gBuffer(indiaLocal1, byid=TRUE, width=0)
head(indiaLocal1@data)
plot(indiaLocal1)
writeOGR(obj=indiaLocal1, dsn=paste(getwd(),"/dataFiles/gis/admin_India",sep=""), layer=paste("indiaLocal1",sep=""), driver="ESRI Shapefile", overwrite_layer=TRUE)
metis.map(dataPolygon=indiaLocal1,fillColumn = "NAME_1",printFig=F)


indiaLocal0<-readOGR(dsn=paste(getwd(),"/dataFiles/gis/admin_India",sep=""),
                     layer="IND_adm0",use_iconv=T,encoding='UTF-8')
indiaLocal0<-spTransform(indiaLocal0,CRS(projX))
head(indiaLocal0@data)
plot(indiaLocal0)
writeOGR(obj=indiaLocal0, dsn=paste(getwd(),"/dataFiles/gis/admin_India",sep=""), layer=paste("indiaLocal0",sep=""), driver="ESRI Shapefile", overwrite_layer=TRUE)
metis.map(dataPolygon=indiaLocal0,fillColumn = "NAME_FAO",printFig=F)


indiaGCAMBasin<-readOGR(dsn=paste(getwd(),"/dataFiles/gis/basin_GCAM",sep=""),
                        layer="Global235_CLM_final_5arcmin_multipart",use_iconv=T,encoding='UTF-8')
indiaGCAMBasin<-spTransform(indiaGCAMBasin,CRS(projX))
indiaGCAMBasin<-raster::intersect(indiaGCAMBasin,indiaLocal0)
head(indiaGCAMBasin@data)
plot(indiaGCAMBasin)
writeOGR(obj=indiaGCAMBasin, dsn=paste(getwd(),"/dataFiles/gis/admin_India",sep=""), layer=paste("indiaGCAMBasin",sep=""), driver="ESRI Shapefile", overwrite_layer=TRUE)
metis.map(dataPolygon=indiaGCAMBasin,fillColumn = "basin_name",printFig=F)


# Boundaries
#-----------------

indiaLocal1<-readOGR(dsn=paste(getwd(),"/dataFiles/gis/admin_India",sep=""),
                     layer="indiaLocal1",use_iconv=T,encoding='UTF-8')

indiaGCAMBasin<-readOGR(dsn=paste(getwd(),"/dataFiles/gis/admin_India",sep=""),
                        layer="indiaGCAMBasin",use_iconv=T,encoding='UTF-8')

indiaStateBoundaries<- metis.boundaries(
  boundaryRegShape=NULL,#
  boundaryRegShpFolder=paste(getwd(),"/dataFiles/gis/naturalEarth",sep=""),#
  boundaryRegShpFile=paste("ne_10m_admin_0_countries",sep=""),#
  boundaryRegCol="NAME",#
  boundaryRegionsSelect="India",#
  subRegShape=indiaLocal1,#
  #subRegShpFolder=paste(getwd(),"/dataFiles/gis/naturalEarth",sep=""),#
  #subRegShpFile=paste("ne_10m_admin_1_states_provinces",sep=""),#
  subRegCol="NAME_1",#
  #subRegionsSelect=NULL,#
  subRegType="State",#
  #dirOutputs=paste(getwd(),"/outputs",sep=""),#
  nameAppend="_local",#
  expandPercent=6,#
  overlapShape=indiaGCAMBasin,#
  #overlapShpFile="Global235_CLM_final_5arcmin_multipart",#
  #overlapShpFolder=paste(getwd(),"/dataFiles/gis/basin_gcam",sep=""),#
  labelsSize=0.7,#
  extension = T,
  cropSubShape2Bound = F
)


GCAMBasin<-readOGR(dsn=paste(getwd(),"/dataFiles/gis/basin_GCAM",sep=""),
                   layer="Global235_CLM_final_5arcmin_multipart",use_iconv=T,encoding='UTF-8')
GCAMBasin<-spTransform(GCAMBasin,CRS(projX))

indiaBasinBoundaries<- metis.boundaries(
  boundaryRegShape=NULL,#
  boundaryRegShpFolder=paste(getwd(),"/dataFiles/gis/naturalEarth",sep=""),#
  boundaryRegShpFile=paste("ne_10m_admin_0_countries",sep=""),#
  boundaryRegCol="NAME",#
  boundaryRegionsSelect="India",#
  subRegShape=indiaGCAMBasin,#
  #subRegShpFolder=paste(getwd(),"/dataFiles/gis/naturalEarth",sep=""),#
  #subRegShpFile=paste("ne_10m_admin_1_states_provinces",sep=""),#
  subRegCol="basin_name",#
  #subRegionsSelect=NULL,#
  subRegType="Basin",#
  #dirOutputs=paste(getwd(),"/outputs",sep=""),#
  nameAppend="_local",#
  expandPercent=6,#
  overlapShape=indiaGCAMBasin,#
  #overlapShpFile="Global235_CLM_final_5arcmin_multipart",#
  #overlapShpFolder=paste(getwd(),"/dataFiles/gis/basin_gcam",sep=""),#
  labelsSize=0.7,#
  extension = T,
  cropSubShape2Bound = F
)

#------------
# Grid to Shape


indiaLocal1<-readOGR(dsn=paste(getwd(),"/dataFiles/gis/admin_India",sep=""),
                     layer="indiaLocal1",use_iconv=T,encoding='UTF-8')

indiaGCAMBasin<-readOGR(dsn=paste(getwd(),"/dataFiles/gis/admin_India",sep=""),
                        layer="indiaGCAMBasin",use_iconv=T,encoding='UTF-8')

gridMetisData<-paste(getwd(),"/outputs/Grids/gridMetis.RData",sep="")
load(gridMetisData) # grid is called gridMetis
gridSelect<-gridMetis%>%
  filter(!class %in% c("Non Agriculture","Total"))
head(gridSelect)

unique(gridMetis$param)
gridTethysWatWithdrawTot<-gridMetis%>%dplyr::filter(param=="tethysWatWithdraw",class=="Total")%>%
  mutate(param="tethysWatWithdrawTotal")
head(gridTethysWatWithdrawTot)

unique(gridSelect$param)
gridScarcityLim1<-gridSelect%>%dplyr::filter(param=="griddedScarcity")%>%
  mutate(value=case_when(value>1~1,TRUE~value),
         param="griddedScarcityLimit1")
head(gridScarcityLim1)

gridScarcityLim5<-gridSelect%>%dplyr::filter(param=="griddedScarcity")%>%
  mutate(value=case_when(value>5~5,TRUE~value),
         param="griddedScarcityLimit5")
head(gridScarcityLim5)

gridXanthos<-gridSelect%>%dplyr::filter(param=="xanthosRunoff")
head(gridXanthos)

gridComb<-bind_rows(gridScarcityLim5,
                    gridScarcityLim1,
                    gridTethysWatWithdrawTot,
                    gridXanthos)

head(indiaLocal1@data)
polyIndiaStates<-metis.grid2poly(grid=gridComb,
                                 boundaryRegionsSelect="India",
                                 subRegShape=indiaLocal1,
                                 #subRegShpFolder=paste(getwd(),"/dataFiles/gis/naturalEarth",sep=""),
                                 #subRegShpFile=paste("ne_10m_admin_1_states_provinces",sep=""),
                                 subRegCol="NAME_1",
                                 subRegType="State",
                                 #aggType=NULL,
                                 #dirOutputs=paste(getwd(),"/outputs",sep=""),
                                 nameAppend="_local_gridComb")

head(indiaGCAMBasin@data)
polyIndiaBasins<-metis.grid2poly(grid=gridComb,
                                 boundaryRegionsSelect="India",
                                 subRegShape=indiaGCAMBasin,
                                 #subRegShpFolder=paste(getwd(),"/dataFiles/gis/naturalEarth",sep=""),
                                 #subRegShpFile=paste("ne_10m_admin_1_states_provinces",sep=""),
                                 subRegCol="basin_name",
                                 subRegType="Basin",
                                 #aggType=NULL,
                                 #dirOutputs=paste(getwd(),"/outputs",sep=""),
                                 nameAppend="_local_gridComb")


head(indiaLocal1@data)
polyIndiaStates<-metis.grid2poly(grid=gridSelect,
                                 boundaryRegionsSelect="India",
                                 subRegShape=indiaLocal1,
                                 #subRegShpFolder=paste(getwd(),"/dataFiles/gis/naturalEarth",sep=""),
                                 #subRegShpFile=paste("ne_10m_admin_1_states_provinces",sep=""),
                                 subRegCol="NAME_1",
                                 subRegType="State",
                                 #aggType=NULL,
                                 #dirOutputs=paste(getwd(),"/outputs",sep=""),
                                 nameAppend="_local_gridSelect")

head(indiaGCAMBasin@data)
polyIndiaBasins<-metis.grid2poly(grid=gridSelect,
                                 boundaryRegionsSelect="India",
                                 subRegShape=indiaGCAMBasin,
                                 #subRegShpFolder=paste(getwd(),"/dataFiles/gis/naturalEarth",sep=""),
                                 #subRegShpFile=paste("ne_10m_admin_1_states_provinces",sep=""),
                                 subRegCol="basin_name",
                                 subRegType="Basin",
                                 #aggType=NULL,
                                 #dirOutputs=paste(getwd(),"/outputs",sep=""),
                                 nameAppend="_local_gridSelect")


#-----------------
# metis.mapProcess
#------------------

IndiaExtended<-readOGR(dsn=paste(getwd(),"/outputs/Maps/Boundaries/India",sep=""),
                       layer="India_Extended_local",use_iconv=T,encoding='UTF-8')


subRegShape=indiaLocal1
polygonDataTables=paste(getwd(),"/outputs/Maps/Tables/subReg_origData_byClass_India_State_origDownscaled_local_gridSelect.csv",sep="")
gridDataTables=paste(getwd(),"/outputs/Grids/gridCropped_India_State_local_gridSelect.csv",sep="")
#polygonDataTables=paste(getwd(),"/outputs/Maps/Tables/subReg_origData_byClass_India_State_origDownscaled_local_ScarcityLim1.csv",sep="")
#gridDataTables=paste(getwd(),"/outputs/Grids/gridCropped_India_State_local_ScarcityLim1.csv",sep="")
#polygonDataTables=paste(getwd(),"/outputs/Maps/Tables/subReg_origData_byClass_India_State_origDownscaled_local_ScarcityLim5.csv",sep="")
#gridDataTables=paste(getwd(),"/outputs/Grids/gridCropped_India_State_local_ScarcityLim5.csv",sep="")
#polygonDataTables=paste(getwd(),"/outputs/Maps/Tables/subReg_origData_byClass_India_State_origDownscaled_local_gridComb.csv",sep="")
#gridDataTables=paste(getwd(),"/outputs/Grids/gridCropped_India_State_local_gridComb.csv",sep="")


head(subRegShape@data)
metis.mapProcess(polygonDataTables=polygonDataTables,
                 gridDataTables=gridDataTables,
                 subRegShape=subRegShape,
                 xRange=c(2010,2020,2030,2040,2050),
                 boundaryRegionsSelect="India",
                 subRegCol="NAME_1",
                 subRegType="State",
                 nameAppend="_Local_all",
                 legendPosition=c("RIGHT","bottom"),
                 extension=T,
                 boundaryRegShpFolder=paste(getwd(),"/dataFiles/gis/naturalEarth",sep=""),
                 boundaryRegShpFile=paste("ne_10m_admin_0_countries",sep=""),
                 boundaryRegCol="NAME",
                 expandPercent = 6
                 #extendedShape = IndiaExtended,
                 #extendedShapeCol="NAME"
)

# polygonDataTables=polygonDataTables
# gridDataTables=gridDataTables
# subRegShape=subRegShape
# xRange=c(2010,2020,2030,2040,2050)
# boundaryRegionsSelect="India"
# subRegCol="NAME_1"
# subRegType="State"
# nameAppend="_Local"
# legendPosition=c("RIGHT","bottom")
# extension=T
# shapeExtended = IndiaExtended

subRegShape=indiaGCAMBasin
polygonDataTables=paste(getwd(),"/outputs/Maps/Tables/subReg_origData_byClass_India_Basin_origDownscaled_local_gridSelect.csv",sep="")
gridDataTables=paste(getwd(),"/outputs/Grids/gridCropped_India_Basin_local_gridSelect.csv",sep="")
#polygonDataTables=paste(getwd(),"/outputs/Maps/Tables/subReg_origData_byClass_India_Basin_origDownscaled_local_ScarcityLim1.csv",sep="")
#gridDataTables=paste(getwd(),"/outputs/Grids/gridCropped_India_Basin_local_ScarcityLim1.csv",sep="")
#polygonDataTables=paste(getwd(),"/outputs/Maps/Tables/subReg_origData_byClass_India_Basin_origDownscaled_local_ScarcityLim5.csv",sep="")
#gridDataTables=paste(getwd(),"/outputs/Grids/gridCropped_India_Basin_local_ScarcityLim5.csv",sep="")
#polygonDataTables=paste(getwd(),"/outputs/Maps/Tables/subReg_origData_byClass_India_Basin_origDownscaled_local_gridComb.csv",sep="")
#gridDataTables=paste(getwd(),"/outputs/Grids/gridCropped_India_Basin_local_gridComb.csv",sep="")


head(subRegShape@data)
metis.mapProcess(polygonDataTables=polygonDataTables,
                 #gridDataTables=gridDataTables,
                 subRegShape=subRegShape,
                 xRange=c(2010,2020,2030,2040,2050),
                 boundaryRegionsSelect="India",
                 subRegCol="basin_name",
                 subRegType="Basin",
                 nameAppend="_Local_all",
                 legendPosition=c("RIGHT","bottom"),
                 extension=T,
                 boundaryRegShpFolder=paste(getwd(),"/dataFiles/gis/naturalEarth",sep=""),
                 boundaryRegShpFile=paste("ne_10m_admin_0_countries",sep=""),
                 boundaryRegCol="NAME",
                 expandPercent = 6
                 #extendedShape = IndiaExtended,
                 #extendedShapeCol="NAME"
)
