
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

#---------------
#  Polygons
#----------------

NE0<-readOGR(dsn=paste(getwd(),"/dataFiles/gis/naturalEarth",sep=""),
                       layer="ne_10m_admin_0_countries",use_iconv=T,encoding='UTF-8')

data(World)

NE1<-readOGR(dsn=paste(getwd(),"/dataFiles/gis/naturalEarth",sep=""),
             layer="ne_10m_admin_1_states_provinces",use_iconv=T,encoding='UTF-8')

hydrobasinsLev3<-readOGR(dsn=paste(getwd(),"/dataFiles/gis/subbasin_hydrobasin",sep=""),
                         layer="hydrobasins_level_3",use_iconv=T,encoding='UTF-8')


GCAMBasins<-readOGR(dsn=paste(getwd(),"/dataFiles/gis/basin_GCAM",sep=""),
                    layer="Global235_CLM_final_5arcmin_multipart",use_iconv=T,encoding='UTF-8')
#metis.map(GCAMBasins,fillColumn = "basin_name",facetsON = F,printFig = F)

projX<-"+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
NE0<-spTransform(NE0,CRS(projX))
NE1<-spTransform(NE1,CRS(projX))
hydrobasinsLev3<-spTransform(hydrobasinsLev3,CRS(projX))
GCAMBasins<-spTransform(GCAMBasins,CRS(projX))


# Check Maps
NE0crop<-NE0[NE0$NAME=="Argentina",]
NE0crop@data<-droplevels(NE0crop@data)
plot(NE0crop)
hydrobasinsLev3Crop<-raster::crop(hydrobasinsLev3,NE0crop)
hydrobasinsLev3Crop@data<-droplevels(hydrobasinsLev3Crop@data)
head(hydrobasinsLev3Crop@data)
Argentina2Basins<-hydrobasinsLev3Crop[hydrobasinsLev3Crop$HYBAS_ID %in% c(6050933820,6050018850,6050808660),]
Argentina2Basins@data<-droplevels(Argentina2Basins@data)
qtm(Argentina2Basins,fill="HYBAS_ID",labels=T)



#---------------
#  Individual Countries
#----------------

countriesSelect=c("Brazil")


#---------------
# Boundaries
#-----------------
for(country_i in countriesSelect){

countryXNE0<-NE0[(NE0$NAME==country_i),]
head(countryXNE0@data)
plot(countryXNE0)

countryXNE1<-raster::crop(NE1,countryXNE0)
head(countryXNE1@data)
plot(countryXNE1)

countryXGCAMBasins<-raster::crop(GCAMBasins,countryXNE0)
head(countryXGCAMBasins@data)
plot(countryXGCAMBasins)


boundaries<- metis.boundaries(
                    boundaryRegShape=NE0,#
                    #boundaryRegShpFolder=paste(getwd(),"/dataFiles/gis/naturalEarth",sep=""),#
                    #boundaryRegShpFile=paste("ne_10m_admin_0_countries",sep=""),#
                    boundaryRegCol="NAME",#
                    boundaryRegionsSelect=country_i,#
                    subRegShape=countryXGCAMBasins,#
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
                    cropSubShape2Bound = F,
                    grids = c(paste(getwd(),"/dataFiles/grids/emptyGrids/grid_025.csv",sep=""),
                              paste(getwd(),"/dataFiles/grids/emptyGrids/grid_050.csv",sep=""))
                    )

boundaries<- metis.boundaries(
                            boundaryRegShape=NE0,#
                            #boundaryRegShpFolder=paste(getwd(),"/dataFiles/gis/naturalEarth",sep=""),#
                            #boundaryRegShpFile=paste("ne_10m_admin_0_countries",sep=""),#
                            boundaryRegCol="NAME",#
                            boundaryRegionsSelect=country_i,#
                            subRegShape=countryXNE1,#
                            #subRegShpFolder=paste(getwd(),"/dataFiles/gis/naturalEarth",sep=""),#
                            #subRegShpFile=paste("ne_10m_admin_1_states_provinces",sep=""),#
                            subRegCol="name",#
                            #subRegionsSelect=NULL,#
                            subRegType="States",#
                            #dirOutputs=paste(getwd(),"/outputs",sep=""),#
                            nameAppend="_local",#
                            expandPercent=2,#
                            overlapShape=countryXGCAMBasins,#
                            #overlapShpFile="Global235_CLM_final_5arcmin_multipart",#
                            #overlapShpFolder=paste(getwd(),"/dataFiles/gis/basin_gcam",sep=""),#
                            labelsSize=0.7,#
                            extension = T,
                            cropSubShape2Bound = F,
                            grids = c(paste(getwd(),"/dataFiles/grids/emptyGrids/grid_025.csv",sep=""),
                                      paste(getwd(),"/dataFiles/grids/emptyGrids/grid_050.csv",sep=""))
                          )

# boundaryRegShape=NE0 #
# #boundaryRegShpFolder=paste(getwd(),"/dataFiles/gis/naturalEarth",sep="") #
# #boundaryRegShpFile=paste("ne_10m_admin_0_countries",sep="") #
# boundaryRegCol="NAME"
# boundaryRegionsSelect=country_i #
# subRegShape=countryXNE1 #
# #subRegShpFolder=paste(getwd(),"/dataFiles/gis/naturalEarth",sep="") #
# #subRegShpFile=paste("ne_10m_admin_1_states_provinces",sep="") #
# subRegCol="name" #
# #subRegionsSelect=NULL #
# subRegType="States" #
# #dirOutputs=paste(getwd(),"/outputs",sep="") #
# nameAppend="_local" #
# expandPercent=2 #
# overlapShape=countryXGCAMBasins #
# #overlapShpFile="Global235_CLM_final_5arcmin_multipart" #
# #overlapShpFolder=paste(getwd(),"/dataFiles/gis/basin_gcam",sep="") #
# labelsSize=0.7 #
# extension = T
# cropSubShape2Bound = F
# grids = c(paste(getwd(),"/dataFiles/grids/emptyGrids/grid_025.csv",sep=""),
#           paste(getwd(),"/dataFiles/grids/emptyGrids/grid_050.csv",sep=""))

} # Close country loop



#---------------
# Boundaries combined countries
#-----------------


countriesSelectA=c("Canada","United States of America","Colombia","Argentina","Uruguay","Russia",
                  "Kazakhstan","Ukraine","China","India","Vietnam","Brazil")

countriesSelectB=c("Canada","United States of America","Colombia","Argentina","Uruguay","Russia",
                   "Kazakhstan","Ukraine","China","India","Vietnam","Brazil",
                   "Norway","Austria","Germany","Netherlands","Italy","Japan","Pakistan","South Africa")

data(World)
unique(World$name)
countriesSelectA<-gsub(" of America","",countriesSelectA)
countriesSelectB<-gsub(" of America","",countriesSelectB)
selectA<-World[World$name %in% countriesSelectA,];head(selectA)
selectB<-World[World$name %in% countriesSelectB,];head(selectB)
mapX <- World
colName <- "name"

# NE0<-readOGR(dsn=paste(getwd(),"/dataFiles/gis/naturalEarth",sep=""),
#              layer="ne_10m_admin_0_countries",use_iconv=T,encoding='UTF-8')
# selectA<-NE0[NE0$NAME %in% countriesSelectA,];head(selectA)
# selectB<-NE0[NE0$NAME %in% countriesSelectB,];head(selectB)
# mapX<-NE0
# colName="NAME"

unique(selectA[[colName]]);length(unique(selectA[[colName]]));length(countriesSelectA);
countriesSelectA[!countriesSelectA %in% unique(selectA[[colName]])]
unique(selectB[[colName]]);length(unique(selectB[[colName]]));length(countriesSelectB)
countriesSelectB[!countriesSelectB %in% unique(selectB[[colName]])]


themeMain <- tm_shape(mapX) +
  tm_borders("grey10") +
  tm_layout(bg.color = "white", inner.margins = c(0, .02, .02, .02)) +
  tm_polygons("grey95") + tm_layout(frame=F)
  #tm_grid(projection="longlat", labels.size = .5)
  #tm_compass(position = c(.1, .1), color.light = "grey90")
  #Selection

labelTextSize = 0.8

tmA<-  themeMain + tm_shape(selectA) + tm_polygons("yellow"); tmA
tmAG<-  tmA + tm_grid(projection="longlat", labels.size = .5)   ; tmAG
tmAL <- tmA +    tm_text(colName, size=labelTextSize, col="black"); tmAL

tmB<-   themeMain + tm_shape(selectB) + tm_polygons("green3"); tmB
tmBG<-  tmB + tm_grid(projection="longlat", labels.size = .5)   ; tmBG
tmBL<- tmB + tm_text(colName, size=labelTextSize, col="black"); tmBL


  folderName="JGCRICountries"
  dirOutputs=paste(getwd(),"/outputs",sep="")
  if (!dir.exists(dirOutputs)){dir.create(dirOutputs)}
  if (!dir.exists(paste(dirOutputs, "/Maps", sep = ""))){dir.create(paste(dirOutputs, "/Maps", sep = ""))}
  if (!dir.exists(paste(dirOutputs, "/Maps/Boundaries", sep = ""))){dir.create(paste(dirOutputs, "/Maps/Boundaries", sep = ""))}
  if (!dir.exists(paste(dirOutputs, "/Maps/Boundaries/",folderName, sep = ""))){dir.create(paste(dirOutputs, "/Maps/Boundaries/",folderName,sep = ""))}

  widthX = 13; heightX = 10
  tmap_save(tmA, filename = paste(dirOutputs, "/Maps/Boundaries/",folderName,"/A_JGCRICurrentWork.png", sep = ""),width = widthX, height = heightX, units = "in")
  tmap_save(tmB, filename = paste(dirOutputs, "/Maps/Boundaries/",folderName,"/B_JGCRIAciveColab.png", sep = ""),width = widthX, height = heightX, units = "in")
  tmap_save(tmAL, filename = paste(dirOutputs, "/Maps/Boundaries/",folderName,"/A_JGCRICurrentWork_Labels.png", sep = ""),width = widthX, height = heightX, units = "in")
  tmap_save(tmBL, filename = paste(dirOutputs, "/Maps/Boundaries/",folderName,"/B_JGCRIAciveColab_Labels.png", sep = ""),width = widthX, height = heightX, units = "in")
  tmap_save(tmAG, filename = paste(dirOutputs, "/Maps/Boundaries/",folderName,"/A_JGCRICurrentWork_Grids.png", sep = ""),width = widthX, height = heightX, units = "in")
  tmap_save(tmBG, filename = paste(dirOutputs, "/Maps/Boundaries/",folderName,"/B_JGCRIAciveColab_Grids.png", sep = ""),width = widthX, height = heightX, units = "in")



  #---------------
  # Random region Out and In
  #-----------------


  # Check Maps
  NE0crop<-NE0[NE0$NAME=="Argentina",]
  NE0crop@data<-droplevels(NE0crop@data)
  qtm(NE0crop)
  hydrobasinsLev3Crop<-raster::crop(hydrobasinsLev3,NE0crop)
  hydrobasinsLev3Crop@data<-droplevels(hydrobasinsLev3Crop@data)
  head(hydrobasinsLev3Crop@data)
  Argentina2Basins<-hydrobasinsLev3Crop[hydrobasinsLev3Crop$HYBAS_ID %in% c(6050933820,6050018850,6050808660),]
  Argentina2Basins@data<-droplevels(Argentina2Basins@data)
  Argentina2Basins@data<-Argentina2Basins@data%>%mutate(NAME=c("Bermejo","Colorado","Colorado"))
  head(Argentina2Basins@data)
  qtm(Argentina2Basins,fill="HYBAS_ID",labels=T)

  # Merging two polygons and creating spatialpolygonDataFrame
  Argentina2BasinsMerged<-gUnaryUnion(Argentina2Basins, id = Argentina2Basins@data$NAME)
  #https://gis.stackexchange.com/questions/141469/how-to-convert-a-spatialpolygon-to-a-spatialpolygonsdataframe-and-add-a-column-t
  p<-Argentina2BasinsMerged
  pid <- sapply(slot(p, "polygons"), function(x) slot(x, "ID"))
  # Create dataframe with correct rownames
  p.df <- data.frame( ID=1:length(p), row.names = pid)
  p <- SpatialPolygonsDataFrame(p, p.df)
  p@data<-p@data%>%mutate(NAME=row.names(p))
  p@data;row.names(p)
  Argentina2BasinsMerged<-p
  head(Argentina2BasinsMerged@data)
  qtm(Argentina2BasinsMerged,fill="NAME",labels=T)


  themeMain <- tm_shape(NE0crop) +
    tm_borders("grey10") +
    tm_layout(bg.color = "white", inner.margins = c(0, .02, .02, .02)) +
    tm_polygons("grey95") + tm_layout(frame=F)
  #tm_grid(projection="longlat", labels.size = .5)
  #tm_compass(position = c(.1, .1), color.light = "grey90")
  #Selection

  labelTextSize = 2
  colName="NAME"

  tmA<-  themeMain + tm_shape(Argentina2BasinsMerged) + tm_polygons("yellow"); tmA
  tmAL <- tmA +    tm_text(colName, size=labelTextSize, col="black"); tmAL

  folderName="Argentina2Basins"
  dirOutputs=paste(getwd(),"/outputs",sep="")
  if (!dir.exists(dirOutputs)){dir.create(dirOutputs)}
  if (!dir.exists(paste(dirOutputs, "/Maps", sep = ""))){dir.create(paste(dirOutputs, "/Maps", sep = ""))}
  if (!dir.exists(paste(dirOutputs, "/Maps/Boundaries", sep = ""))){dir.create(paste(dirOutputs, "/Maps/Boundaries", sep = ""))}
  if (!dir.exists(paste(dirOutputs, "/Maps/Boundaries/",folderName, sep = ""))){dir.create(paste(dirOutputs, "/Maps/Boundaries/",folderName,sep = ""))}

  widthX = 13; heightX = 10
  tmap_save(tmA, filename = paste(dirOutputs, "/Maps/Boundaries/",folderName,"/ArgentinaBermejoColorado.png", sep = ""),width = widthX, height = heightX, units = "in")
  tmap_save(tmAL, filename = paste(dirOutputs, "/Maps/Boundaries/",folderName,"/ArgentinaBermejoColorado_Labels.png", sep = ""),width = widthX, height = heightX, units = "in")




  # Check Argentina Subbasins

  ProvinciasCoirco<-readOGR(dsn=paste("D:/Projects/003b_IDBLAC_Argentina/Data/shapefiles",sep=""),
                            layer="ProvinciasCoirco",use_iconv=T,encoding='UTF-8')
  head(ProvinciasCoirco@data); names(ProvinciasCoirco@data)
  qtm(ProvinciasCoirco,fill="JURISDICCI",text= "JURISDICCI")
  plot(ProvinciasCoirco)

  Subcuencas<-readOGR(dsn=paste("D:/Projects/003b_IDBLAC_Argentina/Data/shapefiles",sep=""),
                      layer="Subcuencas",use_iconv=T,encoding='UTF-8')
  head(Subcuencas@data); names(Subcuencas@data)
  qtm(Subcuencas,fill="cuenca")
  plot(Subcuencas)

  projX<-"+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
  ProvinciasCoirco<-spTransform(ProvinciasCoirco,CRS(projX))
  Subcuencas<-spTransform(Subcuencas,CRS(projX))

  qtm(ProvinciasCoirco,fill="JURISDICCI",lines.col= "red") +
    qtm(Subcuencas,line="cuenca",lines.col= "blue")

  themeMain <- tm_shape(ProvinciasCoirco) +
    tm_borders("grey10") +
    tm_layout(bg.color = "white", inner.margins = c(0, .02, .02, .02)) +
    tm_polygons("grey95") + tm_layout(frame=F) +
    tm_text("JURISDICCI", size=0.7, col="grey10"); themeMain

  tmA<-  themeMain +
    tm_shape(Subcuencas) +
    tm_borders("red") +
    tm_polygons("yellow", alpha=0.2); tmA

  tmAL <- tmA +    tm_text(colName, size=0.7, col="red");
  tmAL

dirOutputs=paste(getwd(),"/outputs",sep="")
folderName="COIRCOSubCuencas"
if (!dir.exists(dirOutputs)){dir.create(dirOutputs)}
if (!dir.exists(paste(dirOutputs, "/Maps", sep = ""))){dir.create(paste(dirOutputs, "/Maps", sep = ""))}
if (!dir.exists(paste(dirOutputs, "/Maps/Boundaries", sep = ""))){dir.create(paste(dirOutputs, "/Maps/Boundaries", sep = ""))}
if (!dir.exists(paste(dirOutputs, "/Maps/Boundaries/",folderName, sep = ""))){dir.create(paste(dirOutputs, "/Maps/Boundaries/",folderName,sep = ""))}
widthX = 13; heightX = 10
tmap_save(tmAL, filename = paste(dirOutputs, "/Maps/Boundaries/",folderName,"/SubcuencasLabels.png", sep = ""),width = widthX, height = heightX, units = "in")

coircoProvinceCropped <- raster::intersect(ProvinciasCoirco,Subcuencas);
head(coircoProvinceCropped@data); plot(coircoProvinceCropped)
coircoProvinceCropped@data<-coircoProvinceCropped@data %>%
  mutate(cuenca_provincia)

#-------------COREBE

DEPARTAMENTOS_COREBE<-readOGR(dsn=paste("D:/Projects/003b_IDBLAC_Argentina/Data/shapefilesCOREBE",sep=""),
                          layer="DEPARTAMENTOS_COREBE",use_iconv=T)
head(DEPARTAMENTOS_COREBE@data); names(DEPARTAMENTOS_COREBE@data)
qtm(DEPARTAMENTOS_COREBE,fill="FNA")
plot(DEPARTAMENTOS_COREBE)

Cuenca_Bermejo_Total<-readOGR(dsn=paste("D:/Projects/003b_IDBLAC_Argentina/Data/shapefilesCOREBE",sep=""),
                    layer="Cuenca_Bermejo_Total",use_iconv=T,encoding='UTF-8')
head(Cuenca_Bermejo_Total@data); names(Cuenca_Bermejo_Total@data)
qtm(Cuenca_Bermejo_Total,fill="Descriptio")
plot(Cuenca_Bermejo_Total)

projX<-"+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
DEPARTAMENTOS_COREBE<-spTransform(DEPARTAMENTOS_COREBE,CRS(projX))
Cuenca_Bermejo_Total<-spTransform(Cuenca_Bermejo_Total,CRS(projX))

themeMain <- tm_shape(DEPARTAMENTOS_COREBE) +
  tm_borders("grey10") +
  tm_layout(bg.color = "white", inner.margins = c(0, .02, .02, .02)) +
  tm_polygons("grey95") + tm_layout(frame=F) +
  tm_text("FNA", size=0.7, col="grey10"); themeMain

tmA<-  themeMain +
  tm_shape(Cuenca_Bermejo_Total) +
  tm_borders("red") +
  tm_polygons("yellow", alpha=0.2); tmA

tmAL <- tmA +    tm_text("Descriptio", size=0.7, col="red");
tmAL

dirOutputs=paste(getwd(),"/outputs",sep="")
folderName="COIRCOSubCuencas"
if (!dir.exists(dirOutputs)){dir.create(dirOutputs)}
if (!dir.exists(paste(dirOutputs, "/Maps", sep = ""))){dir.create(paste(dirOutputs, "/Maps", sep = ""))}
if (!dir.exists(paste(dirOutputs, "/Maps/Boundaries", sep = ""))){dir.create(paste(dirOutputs, "/Maps/Boundaries", sep = ""))}
if (!dir.exists(paste(dirOutputs, "/Maps/Boundaries/",folderName, sep = ""))){dir.create(paste(dirOutputs, "/Maps/Boundaries/",folderName,sep = ""))}
widthX = 13; heightX = 10
tmap_save(tmAL, filename = paste(dirOutputs, "/Maps/Boundaries/",folderName,"/SubcuencasLabels.png", sep = ""),width = widthX, height = heightX, units = "in")


