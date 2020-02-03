#-----------------------------
# Over all steps
#-----------------------------

#----------------------------
# Install necessary packages
#----------------------------
if("devtools" %in% rownames(installed.packages()) == F){install.packages("devtools")}
library(devtools)
if("metis" %in% rownames(installed.packages()) == F){install_github(repo="JGCRI/metis")}
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
if("tools" %in% rownames(installed.packages()) == F){install.packages("tools")}
library(tools)


countryName= "Uruguay"
countryName <- tools::toTitleCase(countryName); countryName

# Local Shape
redoLocalShape = 0;
localBasinShapeFileFolderRaw = paste(getwd(),"/dataFiles/gis/other/shapefiles_Uruguay",sep="")
localBasinShapeFileRaw = "uruguay_8cuencas"
readOGR(dsn=localBasinShapeFileFolderRaw,layer=localBasinShapeFileRaw ,use_iconv=T,encoding='UTF-8')
localBasinsShapeFileColNameRaw = "code" # Will need to load the file to see which name this would be

localBasinShapeFileFolder = paste(getwd(),"/dataFiles/gis/other/shapefiles_",countryName,sep="")
localBasinShapeFile = paste(countryName,"LocalBasin",sep="")
#readOGR(dsn=localBasinShapeFileFolder,layer=localBasinShapeFile ,use_iconv=T,encoding='UTF-8')
localBasinsShapeFileColName = "code" # Will need to load the file to see which name this would be

if(file.exists(paste(getwd(),"/dataFiles/gis/other/shapefiles_",countryName,"/",countryName,"LocalBasin.shp",sep=""))){
  countryLocalBasin=readOGR(dsn=paste(getwd(),"/dataFiles/gis/other/shapefiles_",countryName,sep=""),
                            layer=paste(countryName,"LocalBasin",sep=""),use_iconv=T,encoding='UTF-8')
  metis.map(dataPolygon=countryLocalBasin,fillColumn =localBasinsShapeFileColName,printFig=F, facetsON = F, labels=T, legendStyle = "cat")
  }


#------------
# Prepare Polygons
#----------------

# Create directory for country
if (!dir.exists(paste(getwd(),"/dataFiles/gis/other/shapefiles_",countryName,sep=""))){
  dir.create(paste(getwd(),"/dataFiles/gis/other/shapefiles_",countryName,sep=""))}


# View default metis country shapefile (Natural Earth maps)
NE0<-readOGR(dsn=paste(getwd(),"/dataFiles/gis/metis/naturalEarth",sep=""),
             layer="ne_10m_admin_0_countries",use_iconv=T,encoding='UTF-8')

if(!countryName %in% unique(NE0@data$NAME)){stop(print(paste(countryName, " not in NE0 countries. Please check data.", sep="")))}

countryNE0<-readOGR(dsn=paste(getwd(),"/dataFiles/gis/metis/naturalEarth",sep=""),
                    layer="ne_10m_admin_0_countries",use_iconv=T,encoding='UTF-8')
countryNE0<-countryNE0[(countryNE0$NAME==countryName),]
head(countryNE0@data)
plot(countryNE0)
projX<-proj4string(countryNE0)

if(!file.exists(paste(getwd(),"/dataFiles/gis/other/shapefiles_",countryName,"/",countryName,"NE1.shp",sep=""))){
  # Natural earth level 1 admin boundaries
  NE1<-readOGR(dsn=paste(getwd(),"/dataFiles/gis/metis/naturalEarth",sep=""),
               layer="ne_10m_admin_1_states_provinces",use_iconv=T,encoding='UTF-8')
  if(!countryName %in% unique(NE1@data$admin)){stop(print(paste(countryName, " not in NE1 countries. Please check data.", sep="")))}
  countryNE1<-NE1[(NE1$admin==countryName),]
  # subset any islands or regions not wanted
  countryNE1<-countryNE1[(!countryNE1$name %in% "San Andrés y Providencia") & !is.na(countryNE1$name),]
  head(countryNE1@data)
  plot(countryNE1)
  countryNE1<-spTransform(countryNE1,CRS(projX))
  writeOGR(obj=countryNE1, dsn=paste(getwd(),"/dataFiles/gis/other/shapefiles_",countryName,sep=""), layer=paste(countryName,"NE1",sep=""), driver="ESRI Shapefile", overwrite_layer=TRUE)
  metis.map(dataPolygon=countryNE1,fillColumn = "name",printFig=F, facetsON = F, labels=T, legendStyle = "cat")
}else{countryNE1=readOGR(dsn=paste(getwd(),"/dataFiles/gis/other/shapefiles_",countryName,sep=""),
                         layer=paste(countryName,"NE1",sep=""),use_iconv=T,encoding='UTF-8')
metis.map(dataPolygon=countryNE1,fillColumn = "name",printFig=F, facetsON = F, labels=T, legendStyle = "cat")}

if(!file.exists(paste(getwd(),"/dataFiles/gis/other/shapefiles_",countryName,"/",countryName,"GCAMBasin.shp",sep=""))){
  # GCAM Basins
  GCAMBasin<-readOGR(dsn=paste(getwd(),"/dataFiles/gis/metis/gcam",sep=""),
                     layer="Global235_CLM_final_5arcmin_multipart",use_iconv=T,encoding='UTF-8')
  GCAMBasin<-spTransform(GCAMBasin,CRS(projX))
  countryGCAMBasin<-raster::crop(GCAMBasin,countryNE1)
  countryGCAMBasin@data <- droplevels(countryGCAMBasin@data)
  head(countryGCAMBasin@data)
  plot(countryGCAMBasin)
  writeOGR(obj=countryGCAMBasin, dsn=paste(getwd(),"/dataFiles/gis/other/shapefiles_",countryName,sep=""), layer=paste(countryName,"GCAMBasin",sep=""), driver="ESRI Shapefile", overwrite_layer=TRUE)
  metis.map(dataPolygon=countryGCAMBasin,fillColumn = "basin_name",printFig=F,facetsON = F, labels=T, legendStyle = "cat")
}else{countryGCAMBasin=readOGR(dsn=paste(getwd(),"/dataFiles/gis/other/shapefiles_",countryName,sep=""),
                               layer=paste(countryName,"GCAMBasin",sep=""),use_iconv=T,encoding='UTF-8')
metis.map(dataPolygon=countryGCAMBasin,fillColumn = "basin_name",printFig=F, facetsON = F, labels=T, legendStyle = "cat")}


if(!file.exists(paste(getwd(),"/dataFiles/gis/other/shapefiles_",countryName,"/",countryName,"LocalBasin.shp",sep=""))){
  # Local basin Shapefiles
  countryLocalBasin<-readOGR(dsn=localBasinShapeFileFolderRaw,
                             layer=localBasinShapeFileRaw,use_iconv=T,encoding='UTF-8')
  countryLocalBasin<-spTransform(countryLocalBasin,CRS(projX))
  countryLocalBasin<-raster::crop(countryLocalBasin,countryNE1)
  countryLocalBasin@data <- droplevels(countryLocalBasin@data)
  head(countryLocalBasin@data)
  plot(countryLocalBasin)
  writeOGR(obj=countryLocalBasin, dsn=paste(getwd(),"/dataFiles/gis/other/shapefiles_",countryName,sep=""), layer=paste(countryName,"LocalBasin",sep=""), driver="ESRI Shapefile", overwrite_layer=TRUE)
  metis.map(dataPolygon=countryLocalBasin,fillColumn = localBasinsShapeFileColNameRaw,printFig=F, facetsON = F, labels=T, legendStyle = "cat")
}else{countryLocalBasin=readOGR(dsn=paste(getwd(),"/dataFiles/gis/other/shapefiles_",countryName,sep=""),
                                layer=paste(countryName,"LocalBasin",sep=""),use_iconv=T,encoding='UTF-8')
metis.map(dataPolygon=countryLocalBasin,fillColumn = localBasinsShapeFileColName,printFig=F, facetsON = F, labels=T, legendStyle = "cat")}

if(redoLocalShape ==1){
  # Local basin Shapefiles
  countryLocalBasin<-readOGR(dsn=localBasinShapeFileFolderRaw,
                             layer=localBasinShapeFileRaw,use_iconv=T,encoding='UTF-8')
  countryLocalBasin<-spTransform(countryLocalBasin,CRS(projX))
  countryLocalBasin<-raster::crop(countryLocalBasin,countryNE1)
  # Remove a very small basin from Colombia which can't really be seen and throws of scales
  #countryLocalBasin<-countryLocalBasin[(countryLocalBasin$HYBAS_ID!="6050134450"),]
  countryLocalBasin@data <- droplevels(countryLocalBasin@data)
  head(countryLocalBasin@data)
  plot(countryLocalBasin)
  writeOGR(obj=countryLocalBasin, dsn=paste(getwd(),"/dataFiles/gis/other/shapefiles_",countryName,sep=""), layer=paste(countryName,"LocalBasin",sep=""), driver="ESRI Shapefile", overwrite_layer=TRUE)
  metis.map(dataPolygon=countryLocalBasin,fillColumn = localBasinsShapeFileColNameRaw,printFig=F, facetsON = F, labels=T, legendStyle = "cat")
}


# Check a subRegion
# countryLocalBasin@data%>%arrange(SUB_AREA)
# testShp<-countryLocalBasin;testShp@data%>%arrange(SUB_AREA); plot(testShp,col="gray")
# testShpX<-testShp[(testShp$HYBAS_ID=="6050089740"),];testShpX@data;plot(testShpX,col="red")
# plot(testShp,col="gray"); plot(testShpX,col="red",add=T, label)
# # Magdalena Basin: 6050089740

if(F){
metis.gridByPoly(gridDataTables = paste(getwd(),"/dataFiles/grids/emptyGrids/grid_025.csv",sep=""),
                 shapeFile = localBasinShapeFile,
                 shapeFolder = localBasinShapeFileFolder,
                 colName = localBasinsShapeFileColName,
                 saveFile = T,
                 fname = paste("gridByPoly_local0.25_",countryName,sep=""),
                 folderName = countryName)


metis.gridByPoly(gridDataTables = paste(getwd(),"/dataFiles/grids/emptyGrids/grid_050.csv",sep=""),
                 shapeFile = localBasinShapeFile,
                 shapeFolder = localBasinShapeFileFolder,
                 colName = localBasinsShapeFileColName,
                 saveFile = T,
                 fname = paste("gridByPoly_local0.50_",countryName,sep=""),
                 folderName = countryName)

# grid = paste(getwd(),"/dataFiles/grids/emptyGrids/grid_050.csv",sep="")
# shapeFile = localBasinShapeFile
# shapeFolder = localBasinShapeFileFolder
# colName = localBasinsShapeFileColName
# saveFile = T
# fname = paste("gridByPoly_local0.50_",countryName,sep="")
# folderName = "ColombiaHydroBasinsLev3"
}


#-----------
# Boundaries
#-------------
if(F){
# Plot NE admin boundaries 1
boundaryRegShape_i = NE0
boundaryRegCol_i="NAME"
boundaryRegionsSelect_i=countryName
subRegShape_i = countryNE1
subRegCol_i = "name"
subRegType_i = "state"
nameAppend_i = "_NE"
expandPercentWidth_i = 2
expandPercentHeight_i = 2
expandPercentWidthOV_i = 35
expandPercentHeightOV_i = 75
overlapShape_i = countryGCAMBasin
extension_i =  T
cropSubShape2Bound_i = T

boundariesX<- metis.boundaries(
  boundaryRegShape=boundaryRegShape_i,
  boundaryRegCol=boundaryRegCol_i,
  boundaryRegionsSelect=boundaryRegionsSelect_i,
  subRegShape=subRegShape_i,
  subRegCol=subRegCol_i,
  subRegType=subRegType_i,
  nameAppend=nameAppend_i,
  expandPercentWidth=expandPercentWidth_i,
  expandPercentHeight=expandPercentHeight_i,
  expandPercentWidthOV=expandPercentWidthOV_i,
  expandPercentHeightOV=expandPercentHeightOV_i,
  overlapShape = overlapShape_i,
  extension = extension_i,
  grids = c(paste(getwd(),"/dataFiles/grids/emptyGrids/grid_025.csv",sep=""),
            paste(getwd(),"/dataFiles/grids/emptyGrids/grid_050.csv",sep="")),
  cropSubShape2Bound=cropSubShape2Bound_i,
  compassPos = NULL,
  scalePos = c(0.6,0.2)
)


# Plot GCAM Basin boundaries
subRegShape_i = countryGCAMBasin
subRegCol_i = "basin_name"
subRegType_i = "GCAMBasin"
nameAppend_i = "_GCAMBasin"
overlapShape_i = countryNE1

boundariesX<- metis.boundaries(
  boundaryRegShape=boundaryRegShape_i,
  boundaryRegCol=boundaryRegCol_i,
  boundaryRegionsSelect=boundaryRegionsSelect_i,
  subRegShape=subRegShape_i,
  subRegCol=subRegCol_i,
  subRegType=subRegType_i,
  nameAppend=nameAppend_i,
  expandPercent=expandPercent_i,
  overlapShape = overlapShape_i,
  extension = extension_i,
  grids = c(paste(getwd(),"/dataFiles/grids/emptyGrids/grid_025.csv",sep=""),
            paste(getwd(),"/dataFiles/grids/emptyGrids/grid_050.csv",sep="")),
  cropSubShape2Bound=cropSubShape2Bound_i)

# Plot SubRegion Basin boundaries
subRegShape_i = countryLocalBasin
subRegCol_i = localBasinsShapeFileColName
subRegType_i = "subBasin"
nameAppend_i = "_localSubBasin"
overlapShape_i = countryNE1

boundariesX<- metis.boundaries(
  boundaryRegShape=boundaryRegShape_i,
  boundaryRegCol=boundaryRegCol_i,
  boundaryRegionsSelect=boundaryRegionsSelect_i,
  subRegShape=subRegShape_i,
  subRegCol=subRegCol_i,
  subRegType=subRegType_i,
  nameAppend=nameAppend_i,
  expandPercent=expandPercent_i,
  overlapShape = overlapShape_i,
  extension = extension_i,
  grids = c(paste(getwd(),"/dataFiles/grids/emptyGrids/grid_025.csv",sep=""),
            paste(getwd(),"/dataFiles/grids/emptyGrids/grid_050.csv",sep="")),
  cropSubShape2Bound=cropSubShape2Bound_i)
}

#------------------------
# Prepare Grids
#------------------------
if(F){
dirOutputs=paste(getwd(),"/outputs",sep="")
reReadData=1
# Demeter
demeterFolders=c(paste(getwd(),"/dataFiles/grids/demeter/Reference",sep=""))
demeterScenarios=c("Ref")
demeterUnits="Landuse (Fraction)"
demeterTimesteps<-seq(from=2005,to=2100,by=5)
# Tethys
tethysFolders=c(paste(getwd(),"/dataFiles/grids/tethys/Uruguay/Ref",sep=""),
                paste(getwd(),"/dataFiles/grids/tethys/Uruguay/Impacts_GFDL2p6",sep=""),
                paste(getwd(),"/dataFiles/grids/tethys/Uruguay/Impacts_GFDL8p5",sep=""),
                paste(getwd(),"/dataFiles/grids/tethys/Uruguay/Ref_IrrOilCropCost_neg25",sep=""),
                paste(getwd(),"/dataFiles/grids/tethys/Uruguay/Ref_IrrOilCropCost_neg75",sep=""),
                paste(getwd(),"/dataFiles/grids/tethys/Uruguay/Ref_LivestockPasture_neg5",sep=""),
                paste(getwd(),"/dataFiles/grids/tethys/Uruguay/Ref_LivestockPasture_neg10",sep=""),
                paste(getwd(),"/dataFiles/grids/tethys/Uruguay/Ref_RiceYield_pos10",sep=""),
                paste(getwd(),"/dataFiles/grids/tethys/Uruguay/Ref_RiceYield_pos20",sep="")
                )
tethysScenarios=c("Ref", "GFDL2p6", "GFDL8p5",
                  "PolIrrOilCropCostNeg25","PolIrrOilCropCostNeg75",
                  "PolLivePastureNeg5","PolLivePastureNeg10",
                  "PolRiceYielPos10", "PolRiceYielPos20")
tethysFiles=c(
  "wddom_km3peryr","wdelec_km3peryr","wdirr_km3peryr","wdliv_km3peryr",
              "wdmfg_km3peryr","wdmin_km3peryr","wdnonag_km3peryr",
              "wdtotal_km3peryr"
  )
tethysUnits="Water Withdrawals (km3)"
# Xanthos
xanthosFiles= c(
  paste(getwd(),"/dataFiles/grids/xanthos/clim_impacts_GFDL-ESM2M_rcp2p6/q_km3peryear_GFDL-ESM2M_rcp2p6_1950_2099.csv",sep=""),
 paste(getwd(),"/dataFiles/grids/xanthos/clim_impacts_GFDL-ESM2M_rcp4p5/q_km3peryear_GFDL-ESM2M_rcp4p5_1950_2099.csv",sep=""),
 paste(getwd(),"/dataFiles/grids/xanthos/clim_impacts_GFDL-ESM2M_rcp6p0/q_km3peryear_GFDL-ESM2M_rcp6p0_1950_2099.csv",sep=""),
 paste(getwd(),"/dataFiles/grids/xanthos/clim_impacts_GFDL-ESM2M_rcp8p5/q_km3peryear_GFDL-ESM2M_rcp8p5_1950_2099.csv",sep=""),
 paste(getwd(),"/dataFiles/grids/xanthos/clim_impacts_HadGEM2-ES_rcp2p6/q_km3peryear_HadGEM2-ES_rcp2p6_1950_2099.csv",sep=""),
 paste(getwd(),"/dataFiles/grids/xanthos/clim_impacts_HadGEM2-ES_rcp4p5/q_km3peryear_HadGEM2-ES_rcp4p5_1950_2099.csv",sep=""),
 paste(getwd(),"/dataFiles/grids/xanthos/clim_impacts_HadGEM2-ES_rcp6p0/q_km3peryear_HadGEM2-ES_rcp6p0_1950_2099.csv",sep=""),
 paste(getwd(),"/dataFiles/grids/xanthos/clim_impacts_HadGEM2-ES_rcp8p5/q_km3peryear_HadGEM2-ES_rcp8p5_1950_2099.csv",sep=""),
 paste(getwd(),"/dataFiles/grids/xanthos/clim_impacts_IPSL-CM5A-LR_rcp2p6/q_km3peryear_IPSL-CM5A-LR_rcp2p6_1950_2099.csv",sep=""),
 paste(getwd(),"/dataFiles/grids/xanthos/clim_impacts_IPSL-CM5A-LR_rcp4p5/q_km3peryear_IPSL-CM5A-LR_rcp4p5_1950_2099.csv",sep=""),
 paste(getwd(),"/dataFiles/grids/xanthos/clim_impacts_IPSL-CM5A-LR_rcp6p0/q_km3peryear_IPSL-CM5A-LR_rcp6p0_1950_2099.csv",sep=""),
 paste(getwd(),"/dataFiles/grids/xanthos/clim_impacts_IPSL-CM5A-LR_rcp8p5/q_km3peryear_IPSL-CM5A-LR_rcp8p5_1950_2099.csv",sep=""),
 paste(getwd(),"/dataFiles/grids/xanthos/clim_impacts_MIROC-ESM-CHEM_rcp2p6/q_km3peryear_MIROC-ESM-CHEM_rcp2p6_1950_2099.csv",sep=""),
 paste(getwd(),"/dataFiles/grids/xanthos/clim_impacts_MIROC-ESM-CHEM_rcp4p5/q_km3peryear_MIROC-ESM-CHEM_rcp4p5_1950_2099.csv",sep=""),
 paste(getwd(),"/dataFiles/grids/xanthos/clim_impacts_MIROC-ESM-CHEM_rcp6p0/q_km3peryear_MIROC-ESM-CHEM_rcp6p0_1950_2099.csv",sep=""),
 paste(getwd(),"/dataFiles/grids/xanthos/clim_impacts_MIROC-ESM-CHEM_rcp8p5/q_km3peryear_MIROC-ESM-CHEM_rcp8p5_1950_2099.csv",sep=""),
 paste(getwd(),"/dataFiles/grids/xanthos/clim_impacts_NorESM1-M_rcp2p6/q_km3peryear_NorESM1-M_rcp2p6_1950_2099.csv",sep=""),
 paste(getwd(),"/dataFiles/grids/xanthos/clim_impacts_NorESM1-M_rcp4p5/q_km3peryear_NorESM1-M_rcp4p5_1950_2099.csv",sep=""),
 paste(getwd(),"/dataFiles/grids/xanthos/clim_impacts_NorESM1-M_rcp6p0/q_km3peryear_NorESM1-M_rcp6p0_1950_2099.csv",sep=""),
 paste(getwd(),"/dataFiles/grids/xanthos/clim_impacts_NorESM1-M_rcp8p5/q_km3peryear_NorESM1-M_rcp8p5_1950_2099.csv",sep="")
)
xanthosScenarios = c(
 "GFDL-ESM2M_rcp2p6", "GFDL-ESM2M_rcp4p5", "GFDL-ESM2M_rcp6p0", "GFDL-ESM2M_rcp8p5",
 "HadGEM2-ES_rcp2p6", "HadGEM2-ES_rcp4p5", "HadGEM2-ES_rcp6p0", "HadGEM2-ES_rcp8p5",
 "IPSL-CM5A-LR_rcp2p6", "IPSL-CM5A-LR_rcp4p5", "IPSL-CM5A-LR_rcp6p0", "IPSL-CM5A-LR_rcp8p5",
 "MIROC-ESM-CHEM_rcp2p6", "MIROC-ESM-CHEM_rcp4p5", "MIROC-ESM-CHEM_rcp6p0", "MIROC-ESM-CHEM_rcp8p5",
 "NorESM1-M_rcp2p6", "NorESM1-M_rcp4p5", "NorESM1-M_rcp6p0", "NorESM1-M_rcp8p5"
 )
xanthosCoordinatesPath=paste(getwd(),"/dataFiles/grids/xanthosReference/coordinates.csv",sep="")
xanthosGridAreaHecsPath=paste(getwd(),"/dataFiles/grids/xanthosReference/Grid_Areas_ID.csv",sep="")
# Scarcity
scarcityXanthosRollMeanWindow=10
spanLowess=0.25
# Population
popFolder<-paste(getwd(),"/dataFiles/grids/griddedIDsPop/",sep="")
popFiles<-"grid_pop_map"
popUnits<-"person"

gridMetis<-metis.prepGrid(
  # demeterFolders=demeterFolders,
  # demeterScenarios=demeterScenarios,
  # demeterTimesteps=demeterTimesteps,
  # demeterUnits=demeterUnits,
  tethysFolders=tethysFolders,
  tethysScenarios=tethysScenarios,
  tethysFiles=tethysFiles,
  tethysUnits=tethysUnits,
  # xanthosFiles=xanthosFiles,
  # xanthosScenarios=xanthosScenarios,
  xanthosCoordinatesPath=xanthosCoordinatesPath,
  xanthosGridAreaHecsPath=xanthosGridAreaHecsPath,
  # spanLowess=spanLowess,
  # # dirOutputs=paste(getwd(),"/outputs",sep=""),
  # popFolder=popFolder,
  # popFiles=popFiles,
  # popUnits=popUnits,
  folderName=paste(countryName,sep=""),
  saveFormat = "rds",
  filterYears = seq(1950,2100,by=5))
}
#-----------
# Grid to Poly
#-------------
if(T){
grid_i <- list.files(paste(getwd(),"/outputs/prepGrid/",countryName,sep=""),full.names = T);
grid_i <- grid_i[grepl(".rds",grid_i)];grid_i
#grid_i <- grid_i[grepl("xanthos",grid_i)];grid_i
#grid_i <- grid_i[grepl("HadGEM2-ES_rcp2p6",grid_i)];grid_i
# grid_i <- grid_i[grepl("total",grid_i)];grid_i
# grid_i <- c(grid[grepl("tethys",grid) & grepl("total",grid)][1:2],grid[grepl("xanthos",grid)][1]);grid_i
paramScenariosFile <- paste(getwd(),"/outputs/prepGrid/",countryName,"/paramScenarios.csv",sep="");
paramScenarios_i <- data.table::fread(paramScenariosFile);paramScenarios_i
paramsSelect_i = unique(paramScenarios_i$param); paramsSelect_i
scenariosSelect_i = unique(paramScenarios_i$scenario); scenariosSelect_i

subRegShape_i = countryLocalBasin
subRegCol_i = localBasinsShapeFileColName
subRegType_i = "subBasin"
nameAppend_i = ""
aggType_i = NULL

grid2polyX<-metis.grid2poly(gridFiles=grid_i,
  subRegShape =subRegShape_i,
  subRegCol=subRegCol_i,
  subRegType = subRegType_i,
  aggType=aggType_i,
  nameAppend=nameAppend_i,
  paramsSelect = paramsSelect_i,
  scenariosSelect = scenariosSelect_i,
  folderName = countryName,
  regionName = countryName,
  paramScenariosFixed=paramScenarios_i,
  calculatePolyScarcity=T,
  calculatePolyScarcityOnly=T,
  xanthosFilesScarcity = c("poly_GFDL-ESM2M_rcp2p6_subBasin_xanthosRunoff.csv",
                           "poly_GFDL-ESM2M_rcp8p5_subBasin_xanthosRunoff.csv"))

# gridFiles=grid_i
# subRegShape =subRegShape_i
# subRegCol=subRegCol_i
# subRegType = subRegType_i
# aggType=aggType_i
# nameAppend=nameAppend_i
# paramsSelect = paramsSelect_i
# scenariosSelect = scenariosSelect_i
# folderName = countryName
# regionName = countryName
# paramScenariosFixed=paramScenarios_i
# calculatePolyScarcity=T
# calculatePolyScarcityOnly=T
# xanthosFilesScarcity = c("poly_GFDL-ESM2M_rcp2p6_subBasin_xanthosRunoff.csv",
#                          "poly_GFDL-ESM2M_rcp8p5_subBasin_xanthosRunoff.csv")
}

#-----------
# Mapping
#-------------

# Plot NE admin boundaries 1
boundaryRegShape_i = NE0
boundaryRegCol_i="NAME"
boundaryRegionsSelect_i=countryName
subRegShape_i = countryLocalBasin
subRegCol_i = localBasinsShapeFileColName
subRegType_i = "subBasin"
nameAppend_i = ""

legendPosition_i=c("LEFT","bottom")
legendOutsideSingle_i=T

delay_i=100
paramsSelect_i = c("All")

scaleRange_i=data.frame(param=c("griddedScarcity"),
                        maxScale=c(1),
                        minScale=c(0))
numeric2Cat_param <- list("griddedScarcity","polygonScarcity")
numeric2Cat_breaks <- list(c(-Inf, 0.1, 0.2, 0.4,Inf),c(-Inf, 0.1, 0.2, 0.4,Inf))
numeric2Cat_labels <- list(c("None (0<WSI<0.1)","Low (0.1<WSI<0.2)","Moderate (0.2<WSI<0.4)","Severe (WSI>0.4)"),
                           c("None (0<WSI<0.1)","Low (0.1<WSI<0.2)","Moderate (0.2<WSI<0.4)","Severe (WSI>0.4)"))
numeric2Cat_palette <- list(c("pal_ScarcityCat"),
                            #c("c('None (0<WSI<0.1)'='black','Low (0.1<WSI<0.2)'='blue','Moderate (0.2<WSI<0.4)'='purple','Severe (WSI>0.4)'='yellow')"),
                            c("pal_ScarcityCat")) # Can be a custom scale or an R brewer paletter or a metis.pal
numeric2Cat_legendTextSize <- list(c(0.7),c(0.7))
numeric2Cat_list <-list(numeric2Cat_param=numeric2Cat_param,
                        numeric2Cat_breaks=numeric2Cat_breaks,
                        numeric2Cat_labels=numeric2Cat_labels,
                        numeric2Cat_palette=numeric2Cat_palette,
                        numeric2Cat_legendTextSize=numeric2Cat_legendTextSize)

chosenRefMeanYears_i=c(1950:2010)



# Grid Tables
if(T){
gridFiles <- list.files(paste(getwd(),"/outputs/Grid2Poly/",countryName,sep=""),full.names = T);
gridFiles <- gridFiles[grepl(".csv",gridFiles) & grepl(".grid",gridFiles)];gridFiles
a <- tibble::tibble()
for(gridFile_i in gridFiles){
  atemp <- data.table::fread(gridFile_i)
  a <- a%>%dplyr::bind_rows(atemp)
}
a <- a %>% unique(); a
for(param_i in unique(a$param)){print(param_i);print(unique((a%>%dplyr::filter(param==param_i))$x));print(unique((a%>%dplyr::filter(param==param_i))$scenario))}
}

# PolygonTables
polyFiles <- list.files(paste(getwd(),"/outputs/Grid2Poly/",countryName,sep=""),full.names = T);
polyFiles <- polyFiles[grepl(".csv",polyFiles) & grepl(".poly",polyFiles)];polyFiles
polyFiles <- polyFiles[!grepl("Template",polyFiles)];polyFiles
b <- tibble::tibble()
for(polyFile_i in polyFiles){
  btemp <- data.table::fread(polyFile_i)%>%tibble::as_tibble() %>% mutate(subRegion=as.character(subRegion));btemp
  b <- b%>%bind_rows(btemp)%>%filter(!is.na(value))
}
b <- b %>% unique() %>% mutate(subRegion=as.character(subRegion)); b
for(param_i in unique(b$param)){print(param_i);print(unique((b%>%dplyr::filter(param==param_i))$x));print(unique((b%>%dplyr::filter(param==param_i))$scenario))}

bx<-b
unique(bx$scenario); unique(bx$param)
paramScenarios <-unique(bx%>%dplyr::select(param,scenario))%>%as.data.frame()%>%arrange(param);
paramScenarios

bx <- bx %>% mutate(value=case_when(param=="population"~value/1e+6,
                                    TRUE~value),
                    units=case_when(param=="population"~"million",
                                    TRUE~units))

ax <- a %>% mutate(value=case_when(param=="population"~value/1e+6,
                                    TRUE~value),
                    units=case_when(param=="population"~"million",
                                    TRUE~units)) %>%
  dplyr::filter(!is.na(lat),!is.na(lon))

animateOn_i=F


# Xanthos Historical Runoff
paramScenarios
diffOn_i=F
scenRefDiffIndv_i = list(param=list(c("xanthosRunoff")),
                         #scenRef=list(c("GFDL-ESM2M_rcp8p5")),
                         #scenDiff=list(c("GFDL-ESM2M_rcp2p6")),
                         scenIndv=list(c("HadGEM2-ES_rcp2p6"))
                         ); scenRefDiffIndv_i
xRange_i= seq(from=1950,to=2010,by=5)
multiFacetsOn_i=F  # Multifacets ignores all the scenarios given above and looks for a multiRef scenario in all the data provided
refMultiA_i="GFDL-ESM2M"
refMultiB_i="rcp2p6"

metis.mapsProcess(mapTitleOn=F,
                 #polygonDataTables=bx,
                 gridDataTables=ax,
                 xRange=xRange_i,
                 boundaryRegShape=boundaryRegShape_i,
                 boundaryRegCol=boundaryRegCol_i,
                 boundaryRegionsSelect=boundaryRegionsSelect_i,
                 subRegShape=subRegShape_i,
                 subRegCol=subRegCol_i,
                 nameAppend=nameAppend_i,
                 legendOutsideSingle=legendOutsideSingle_i,
                 legendPosition=legendPosition_i,
                 animateOn=animateOn_i,
                 diffOn = diffOn_i,
                 scenRefDiffIndv=scenRefDiffIndv_i,
                 extension=T,
                 expandPercent = 3,
                 figWidth=6,
                 figHeight=7,
                 paramsSelect = paramsSelect_i,
                 #scaleRange = scaleRange_i,
                 multiFacetsOn = multiFacetsOn_i,
                 multiFacetCols="scenarioMultiA",
                 multiFacetRows="scenarioMultiB",
                 legendOutsideMulti=T,
                 legendPositionMulti=NULL,
                 legendTitleSizeMulti=NULL,
                 legendTextSizeAnim=NULL,
                 legendTextSizeMulti=NULL,
                 refMultiA = refMultiA_i,
                 refMultiB = refMultiB_i,
                 chosenRefMeanYears=chosenRefMeanYears_i,
                 numeric2Cat_list=numeric2Cat_list,
                 folderName = paste(countryName,"hist",sep=""),
                 pdfpng = "png")


# Population
paramScenarios
diffOn_i=F
scenRefDiffIndv_i = list(param=list(c("population")),
                         #scenRef=list(c("GFDL-ESM2M_rcp8p5")),
                         #scenDiff=list(c("GFDL-ESM2M_rcp2p6")),
                         scenIndv=list(c("popGWP"))
); scenRefDiffIndv_i


xRange_i= seq(from=2010,to=2050,by=5)
multiFacetsOn_i=F  # Multifacets ignores all the scenarios given above and looks for a multiRef scenario in all the data provided
refMultiA_i="GFDL-ESM2M"
refMultiB_i="rcp2p6"

scaleRange_i=NULL

metis.mapsProcess(mapTitleOn=F, polygonDataTables=bx,
                  gridDataTables=ax,
                  xRange=xRange_i,
                  boundaryRegShape=boundaryRegShape_i,
                  boundaryRegCol=boundaryRegCol_i,
                  boundaryRegionsSelect=boundaryRegionsSelect_i,
                  subRegShape=subRegShape_i,
                  subRegCol=subRegCol_i,
                  nameAppend=nameAppend_i,
                  legendOutsideSingle=legendOutsideSingle_i,
                  legendPosition=legendPosition_i,
                  animateOn=animateOn_i,
                  diffOn = diffOn_i,
                  scenRefDiffIndv=scenRefDiffIndv_i,
                  extension=T,
                  expandPercent = 3,
                  figWidth=6,
                  figHeight=7,
                  paramsSelect = paramsSelect_i,
                  scaleRange = scaleRange_i,
                  multiFacetsOn = multiFacetsOn_i,
                  multiFacetCols="scenarioMultiA",
                  multiFacetRows="scenarioMultiB",
                  legendOutsideMulti=T,
                  legendPositionMulti=NULL,
                  legendTitleSizeMulti=NULL,
                  legendTextSizeAnim=NULL,
                  legendTextSizeMulti=NULL,
                  refMultiA = refMultiA_i,
                  refMultiB = refMultiB_i,
                  chosenRefMeanYears=chosenRefMeanYears_i,
                  numeric2Cat_list=numeric2Cat_list,
                  folderName = paste(countryName,sep=""),
                  pdfpng="png")


# Water Withdrawals
paramScenarios
paramScenarios%>%filter(grepl("tethys",param))
diffOn_i=T
scenRefDiffIndv_i = list(param=list(c("tethysWatWithdraw_indv")),
                         scenRef=list(c("Ref")),
                         scenDiff=list(c("Ref", "GFDL2p6", "GFDL8p5",
                                         "PolIrrOilCropCostNeg25","PolIrrOilCropCostNeg75",
                                         "PolLivePastureNeg5","PolLivePastureNeg10",
                                         "PolRiceYielPos10", "PolRiceYielPos20")),
                         scenIndv=list(c("Ref", "GFDL2p6", "GFDL8p5",
                                         "PolIrrOilCropCostNeg25","PolIrrOilCropCostNeg75",
                                         "PolLivePastureNeg5","PolLivePastureNeg10",
                                         "PolRiceYielPos10", "PolRiceYielPos20"))
); scenRefDiffIndv_i

#xRange_i= seq(from=2050,to=2050,by=10)
xRange_i= c(2010,2050)
multiFacetsOn_i=F  # Multifacets ignores all the scenarios given above and looks for a multiRef scenario in all the data provided
refMultiA_i="GFDL-ESM2M"
refMultiB_i="rcp2p6"

bx1 <- bx %>% mutate(value=ifelse(value<0.01,0,value))
ax1 <- ax %>% mutate(value=ifelse(value<0.01,0,value))

#scaleRange_i=NULL
scaleRange_i=data.frame(param=c("tethysWatWithdraw_indv"),
                         maxScale=c(10),
                         minScale=c(-15))

metis.mapsProcess(mapTitleOn=F, polygonDataTables=bx1,
                  gridDataTables=ax1,
                  xRange=xRange_i,
                  boundaryRegShape=boundaryRegShape_i,
                  boundaryRegCol=boundaryRegCol_i,
                  boundaryRegionsSelect=boundaryRegionsSelect_i,
                  subRegShape=subRegShape_i,
                  subRegCol=subRegCol_i,
                  nameAppend=nameAppend_i,
                  legendOutsideSingle=legendOutsideSingle_i,
                  legendPosition=legendPosition_i,
                  animateOn=animateOn_i,
                  diffOn = diffOn_i,
                  scenRefDiffIndv=scenRefDiffIndv_i,
                  extension=T,
                  expandPercent = 3,
                  figWidth=6,
                  figHeight=7,
                  paramsSelect = paramsSelect_i,
                  scaleRange = scaleRange_i,
                  multiFacetsOn = multiFacetsOn_i,
                  multiFacetCols="scenarioMultiA",
                  multiFacetRows="scenarioMultiB",
                  legendOutsideMulti=T,
                  legendPositionMulti=NULL,
                  legendTitleSizeMulti=NULL,
                  legendTextSizeAnim=NULL,
                  legendTextSizeMulti=NULL,
                  refMultiA = refMultiA_i,
                  refMultiB = refMultiB_i,
                  chosenRefMeanYears=chosenRefMeanYears_i,
                  numeric2Cat_list=numeric2Cat_list,
                  folderName = paste(countryName,sep=""),
                  facetCols=3,
                  pdfpng="png")

# Water Withdrawals No Diff
paramScenarios
paramScenarios%>%filter(grepl("tethys",param))
diffOn_i=F
scenRefDiffIndv_i = list(param=list(c("tethysWatWithdraw_indv")),
                         scenRef=list(c("Ref")),
                         scenDiff=list(c("Ref", "GFDL2p6", "GFDL8p5",
                                         "PolIrrOilCropCostNeg25","PolIrrOilCropCostNeg25",
                                         "PolLivePastureNeg5","PolLivePastureNeg10",
                                         "PolRiceYielPos10", "PolRiceYielPos20")),
                         scenIndv=list(c("Ref", "GFDL2p6", "GFDL8p5",
                                         "PolIrrOilCropCostNeg25","PolIrrOilCropCostNeg75",
                                         "PolLivePastureNeg5","PolLivePastureNeg10",
                                         "PolRiceYielPos10", "PolRiceYielPos20"))
); scenRefDiffIndv_i

#xRange_i= seq(from=2050,to=2050,by=10)
xRange_i= c(2010,2050)
multiFacetsOn_i=F  # Multifacets ignores all the scenarios given above and looks for a multiRef scenario in all the data provided
refMultiA_i="GFDL-ESM2M"
refMultiB_i="rcp2p6"

bx1 <- bx %>% mutate(value=ifelse(value<0.01,0,value))
ax1 <- ax %>% mutate(value=ifelse(value<0.01,0,value))

scaleRange_i=NULL
#scaleRange_i=data.frame(param=c("tethysWatWithdraw_indv"),
#                        maxScale=c(10),
#                        minScale=c(-15))

metis.mapsProcess(mapTitleOn=F, polygonDataTables=bx1,
                  gridDataTables=ax1,
                  xRange=xRange_i,
                  boundaryRegShape=boundaryRegShape_i,
                  boundaryRegCol=boundaryRegCol_i,
                  boundaryRegionsSelect=boundaryRegionsSelect_i,
                  subRegShape=subRegShape_i,
                  subRegCol=subRegCol_i,
                  nameAppend=nameAppend_i,
                  legendOutsideSingle=legendOutsideSingle_i,
                  legendPosition=legendPosition_i,
                  animateOn=animateOn_i,
                  diffOn = diffOn_i,
                  scenRefDiffIndv=scenRefDiffIndv_i,
                  extension=T,
                  expandPercent = 3,
                  figWidth=6,
                  figHeight=7,
                  paramsSelect = paramsSelect_i,
                  scaleRange = scaleRange_i,
                  multiFacetsOn = multiFacetsOn_i,
                  multiFacetCols="scenarioMultiA",
                  multiFacetRows="scenarioMultiB",
                  legendOutsideMulti=T,
                  legendPositionMulti=NULL,
                  legendTitleSizeMulti=NULL,
                  legendTextSizeAnim=NULL,
                  legendTextSizeMulti=NULL,
                  refMultiA = refMultiA_i,
                  refMultiB = refMultiB_i,
                  chosenRefMeanYears=chosenRefMeanYears_i,
                  numeric2Cat_list=numeric2Cat_list,
                  folderName = paste(countryName,sep=""),
                  facetCols=3,
                  pdfpng="png")


# Water Withdrawals total
paramScenarios
paramScenarios%>%filter(grepl("tethys",param))
diffOn_i=T
scenRefDiffIndv_i = list(param=list(c("tethysWatWithdraw_total")),
                         scenRef=list(c("Ref")),
                         scenDiff=list(c("Ref", "GFDL2p6", "GFDL8p5",
                                         "PolIrrOilCropCostNeg25","PolIrrOilCropCostNeg25",
                                         "PolLivePastureNeg5","PolLivePastureNeg10",
                                         "PolRiceYielPos10", "PolRiceYielPos20")),
                         scenIndv=list(c("Ref", "GFDL2p6", "GFDL8p5",
                                         "PolIrrOilCropCostNeg25","PolIrrOilCropCostNeg75",
                                         "PolLivePastureNeg5","PolLivePastureNeg10",
                                         "PolRiceYielPos10", "PolRiceYielPos20"))
); scenRefDiffIndv_i

#xRange_i= seq(from=2050,to=2050,by=10)
xRange_i= c(2010,2050)
multiFacetsOn_i=F  # Multifacets ignores all the scenarios given above and looks for a multiRef scenario in all the data provided
refMultiA_i="GFDL-ESM2M"
refMultiB_i="rcp2p6"

#bx1 <- bx%>%filter(grepl("total",param)); bx1
#bx1%>%filter(x==2010)%>%tidyr::spread(key="scenario",value="value")%>%mutate(diff=Ref-HADGEM2Rcp8p5)%>%filter(diff!=0)

 scaleRange_i=data.frame(param=c("tethysWatWithdraw_total"),
                          maxScale=c(10),
                         minScale=c(0))
scaleRange_i=NULL

metis.mapsProcess(mapTitleOn=F, polygonDataTables=bx1,
                  legendSingleColorOn =T,
                  gridDataTables=ax,
                  xRange=xRange_i,
                  boundaryRegShape=boundaryRegShape_i,
                  boundaryRegCol=boundaryRegCol_i,
                  boundaryRegionsSelect=boundaryRegionsSelect_i,
                  subRegShape=subRegShape_i,
                  subRegCol=subRegCol_i,
                  nameAppend=nameAppend_i,
                  legendOutsideSingle=legendOutsideSingle_i,
                  legendPosition=legendPosition_i,
                  animateOn=animateOn_i,
                  diffOn = diffOn_i,
                  scenRefDiffIndv=scenRefDiffIndv_i,
                  extension=T,
                  expandPercent = 3,
                  figWidth=6,
                  figHeight=7,
                  paramsSelect = paramsSelect_i,
                  scaleRange = scaleRange_i,
                  multiFacetsOn = multiFacetsOn_i,
                  multiFacetCols="scenarioMultiA",
                  multiFacetRows="scenarioMultiB",
                  legendOutsideMulti=T,
                  legendPositionMulti=NULL,
                  legendTitleSizeMulti=NULL,
                  legendTextSizeAnim=NULL,
                  legendTextSizeMulti=NULL,
                  refMultiA = refMultiA_i,
                  refMultiB = refMultiB_i,
                  chosenRefMeanYears=chosenRefMeanYears_i,
                  numeric2Cat_list=numeric2Cat_list,
                  folderName = paste(countryName,sep=""),
                  pdfpng="png")

# Scarcity total
paramScenarios
#paramScenarios%>%filter(grepl("Hist",scenario))
diffOn_i=F
scenRefDiffIndv_i = list(param=list(c("polygonScarcity","griddedScarcity")),
                         #scenRef=list(c("GFDL-ESM2M_rcp8p5")),
                         #scenDiff=list(c("GFDL-ESM2M_rcp2p6")),
                         scenIndv=list(c("XxanthosHist1950to2010TGFDL2p6",
                                         "XxanthosHist1950to2010TGFDL8p5",
                                         "XxanthosHist1950to2010TRef",
                                         "XGFDL-ESM2M_rcp2p6TGFDL2p6",
                                         "XGFDL-ESM2M_rcp8p5TGFDL8p5",
                                         "XGFDL-ESM2M_rcp2p6TRef",
                                         "XGFDL-ESM2M_rcp8p5TRef",
                                         "XGFDL-ESM2M_rcp2p6TPolIrrOilCropCostNeg25",
                                         "XGFDL-ESM2M_rcp8p5TPolIrrOilCropCostNeg25",
                                         "XGFDL-ESM2M_rcp2p6TPolIrrOilCropCostNeg75",
                                         "XGFDL-ESM2M_rcp8p5TPolIrrOilCropCostNeg75",
                                         "XGFDL-ESM2M_rcp2p6TPolLivePastureNeg5",
                                         "XGFDL-ESM2M_rcp8p5TPolLivePastureNeg5",
                                         "XGFDL-ESM2M_rcp2p6TPolLivePastureNeg10",
                                         "XGFDL-ESM2M_rcp8p5TPolLivePastureNeg10",
                                         "XGFDL-ESM2M_rcp2p6TPolRiceYielPos10",
                                         "XGFDL-ESM2M_rcp8p5TPolRiceYielPos10",
                                         "XGFDL-ESM2M_rcp2p6TPolRiceYielPos20",
                                         "XGFDL-ESM2M_rcp8p5TPolRiceYielPos20"))); scenRefDiffIndv_i




#xRange_i= seq(from=2050,to=2050,by=10)
xRange_i= c(2010,2050)
multiFacetsOn_i=F  # Multifacets ignores all the scenarios given above and looks for a multiRef scenario in all the data provided
refMultiA_i="GFDL-ESM2M"
refMultiB_i="rcp2p6"

bx1 <- bx %>% dplyr::mutate(classPalette="pal_hot")
ax1 <- ax %>% dplyr::mutate(classPalette="pal_hot")
# bx%>%filter(grepl("Hist",scenario),param=="polygonScarcity"); bx1
# bx1%>%filter(x==2010)%>%tidyr::spread(key="scenario",value="value")%>%mutate(diff=XxanthosHist1980to2010TRef-XxanthosHist1980to2010THADGEM2Rcp8p5)%>%filter(diff!=0)

# scaleRange_i=data.frame(param=c("polygonScarcity"),
#                         maxScale=c(0.3),
#                         minScale=c(0))

multiFacetsOn_i=T

metis.mapsProcess(mapTitleOn=F,
                  polygonDataTables=bx1,
                  legendSingleColorOn =T,
                  gridDataTables=ax1,
                  xRange=xRange_i,
                  boundaryRegShape=boundaryRegShape_i,
                  boundaryRegCol=boundaryRegCol_i,
                  boundaryRegionsSelect=boundaryRegionsSelect_i,
                  subRegShape=subRegShape_i,
                  subRegCol=subRegCol_i,
                  nameAppend=nameAppend_i,
                  legendOutsideSingle=legendOutsideSingle_i,
                  legendPosition=legendPosition_i,
                  animateOn=animateOn_i,
                  diffOn = diffOn_i,
                  scenRefDiffIndv=scenRefDiffIndv_i,
                  extension=T,
                  expandPercent = 3,
                  figWidth=6,
                  figHeight=7,
                  paramsSelect = paramsSelect_i,
                  scaleRange = scaleRange_i,
                  multiFacetsOn = multiFacetsOn_i,
                  multiFacetCols="scenarioMultiA",
                  multiFacetRows="scenarioMultiB",
                  legendOutsideMulti=T,
                  legendPositionMulti=NULL,
                  legendTitleSizeMulti=NULL,
                  legendTextSizeAnim=NULL,
                  legendTextSizeMulti=NULL,
                  refMultiA = refMultiA_i,
                  refMultiB = refMultiB_i,
                  chosenRefMeanYears=chosenRefMeanYears_i,
                  numeric2Cat_list=numeric2Cat_list,
                  folderName = paste(countryName,sep=""),
                  pdfpng="png")

# Demeter Land use
paramScenarios
paramScenarios%>%filter(grepl("demeter",param))
diffOn_i=F
scenRefDiffIndv_i = list(param=list(c("demeterLandUse")),
                         #scenRef=list(c("GFDL-ESM2M_rcp8p5")),
                         #scenDiff=list(c("GFDL-ESM2M_rcp2p6")),
                         scenIndv=list(c("Ref"))
); scenRefDiffIndv_i

#xRange_i= seq(from=2050,to=2050,by=10)
xRange_i= c(2010,2050)
multiFacetsOn_i=F  # Multifacets ignores all the scenarios given above and looks for a multiRef scenario in all the data provided
refMultiA_i="GFDL-ESM2M"
refMultiB_i="rcp2p6"

#bx1 <- bx %>% mutate(value=ifelse(value<0.01,0,value))
bx1<-bx%>%filter(param=="demeterLandUse",
                 !class %in% c("snow","sparse","water"))%>%mutate(value=ifelse(value<1e-2,0,value))

ax1<-ax%>%filter(param=="demeterLandUse",
                 !class %in% c("snow","sparse","water"))%>%mutate(value=ifelse(value<1e-2,0,value))

# scaleRange_i=data.frame(param=c("demeterLandUse"),
#                         maxScale=c(0.2),
#                         minScale=c(0))
scaleRange_i=NULL

metis.mapsProcess(mapTitleOn=F, polygonDataTables=bx1,
                  gridDataTables=ax1,
                  xRange=xRange_i,
                  boundaryRegShape=boundaryRegShape_i,
                  boundaryRegCol=boundaryRegCol_i,
                  boundaryRegionsSelect=boundaryRegionsSelect_i,
                  subRegShape=subRegShape_i,
                  subRegCol=subRegCol_i,
                  nameAppend=nameAppend_i,
                  legendOutsideSingle=legendOutsideSingle_i,
                  legendPosition=legendPosition_i,
                  animateOn=animateOn_i,
                  diffOn = diffOn_i,
                  scenRefDiffIndv=scenRefDiffIndv_i,
                  extension=T,
                  expandPercent = 3,
                  figWidth=6,
                  figHeight=7,
                  paramsSelect = paramsSelect_i,
                  scaleRange = scaleRange_i,
                  multiFacetsOn = multiFacetsOn_i,
                  multiFacetCols="scenarioMultiA",
                  multiFacetRows="scenarioMultiB",
                  legendOutsideMulti=T,
                  legendPositionMulti=NULL,
                  legendTitleSizeMulti=NULL,
                  legendTextSizeAnim=NULL,
                  legendTextSizeMulti=NULL,
                  refMultiA = refMultiA_i,
                  refMultiB = refMultiB_i,
                  chosenRefMeanYears=chosenRefMeanYears_i,
                  numeric2Cat_list=numeric2Cat_list,
                  folderName = paste(countryName,sep=""),
                  pdfpng="png",
                  facetCols = 5)

# Xanthos Multi Scenarios
paramScenarios
paramScenarios%>%filter(grepl("xanthos",param))
diffOn_i=T
scenRefDiffIndv_i = list(param=list(c("xanthosRunoff")),
                         scenRef=list(c("xanthosHist1950to2010")),
                         scenDiff=list(c("GFDL-ESM2M_rcp2p6","GFDL-ESM2M_rcp8p5")),
                         scenIndv=list(c("GFDL-ESM2M_rcp2p6","GFDL-ESM2M_rcp8p5"))); scenRefDiffIndv_i

#xRange_i= seq(from=2050,to=2050,by=10)
xRange_i= c(2010,2050)
multiFacetsOn_i=F  # Multifacets ignores all the scenarios given above and looks for a multiRef scenario in all the data provided
refMultiA_i="GFDL-ESM2M"
refMultiB_i="rcp2p6"

bx1<-bx
#bx1 <- bx %>% mutate(value=ifelse(value<0.01,0,value))
#bx1<-bx %>% filter(!grepl("Hist",scenario))

#scaleRange_i=data.frame(param=c("xanthosRunoff"),
                        maxScale=c(120),
                        minScale=c(0))
scaleRange_i=NULL

metis.mapsProcess(mapTitleOn=F, polygonDataTables=bx1,
                  #gridDataTables=a,
                  xRange=xRange_i,
                  boundaryRegShape=boundaryRegShape_i,
                  boundaryRegCol=boundaryRegCol_i,
                  boundaryRegionsSelect=boundaryRegionsSelect_i,
                  subRegShape=subRegShape_i,
                  subRegCol=subRegCol_i,
                  nameAppend=nameAppend_i,
                  legendOutsideSingle=legendOutsideSingle_i,
                  legendPosition=legendPosition_i,
                  animateOn=animateOn_i,
                  diffOn = diffOn_i,
                  scenRefDiffIndv=scenRefDiffIndv_i,
                  extension=T,
                  expandPercent = 3,
                  figWidth=6,
                  figHeight=7,
                  paramsSelect = paramsSelect_i,
                  scaleRange = scaleRange_i,
                  multiFacetsOn = multiFacetsOn_i,
                  multiFacetCols="scenarioMultiA",
                  multiFacetRows="scenarioMultiB",
                  legendOutsideMulti=T,
                  legendPositionMulti=NULL,
                  legendTitleSizeMulti=NULL,
                  legendTextSizeAnim=NULL,
                  legendTextSizeMulti=NULL,
                  refMultiA = refMultiA_i,
                  refMultiB = refMultiB_i,
                  chosenRefMeanYears=chosenRefMeanYears_i,
                  numeric2Cat_list=numeric2Cat_list,
                  folderName = paste(countryName,sep=""),
                  pdfpng="png")

