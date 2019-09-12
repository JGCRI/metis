

#-----------------------------
# Over all steps
#-----------------------------
# 1. Read GCAM Data
# 2. Create Charts
# 3. Prepare Polygon Data (Check and fix shapefiles as needed)
# 4. Plots Boudnaries
# 5. Prep grids
# 6. Grid to polygons
# 7. Produce Maps


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
if("tools" %in% rownames(installed.packages()) == F){install.packages("tools")}
library(tools)



#----------------------------
# Read GCAM Data
#---------------------------

# ?metis.readgcam # For more help

gcamdatabasePath_i <-paste(getwd(),"/dataFiles/gcam",sep="")
gcamdatabaseName_i <-"database_basexdb_Uruguay"
gcamdataProjFile_i <-"Uruguay_dataProj.proj"
regionsSelect_i <- c("Uruguay")

#dataProjLoaded <- loadProject(paste(gcamdatabasePath, "/", gcamdataProjFile, sep = ""))
#listScenarios(dataProjLoaded)  # List of Scenarios in GCAM database
#queries <- listQueries(dataProjLoaded)  # List of Queries in queryxml

dataGCAM<-metis.readgcam(reReadData=T, # Default Value is T
                                 dataProj = gcamdataProjFile_i, # Default Value is "dataProj.proj"
                                 dataProjPath = gcamdatabasePath_i,
                                 scenOrigNames=c("IDBUruguay_GCAMOrig"),
                                 scenNewNames=c("GCAMOrig"),
                                 gcamdatabasePath=gcamdatabasePath_i,
                                 gcamdatabaseName=gcamdatabaseName_i,
                                 queryxml="metisQueries.xml",  # Default Value is "metisQueries.xml"
                                 dirOutputs= paste(getwd(),"/outputs",sep=""), # Default Value is paste(getwd(),"/outputs",sep="")
                                 regionsSelect=regionsSelect_i, # Default Value is NULL
                                 paramsSelect="All" # Default value is "All"
)

# reReadData=T # Default Value is T
# dataProj = gcamdataProjFile_i # Default Value is "dataProj.proj"
# dataProjPath = gcamdatabasePath_i
# scenOrigNames=c("GCAMOrig")
# scenNewNames=c("GCAMOrig")
# gcamdatabasePath=gcamdatabasePath_i
# gcamdatabaseName=gcamdatabaseName_i
# queryxml="metisQueries.xml"  # Default Value is "metisQueries.xml"
# dirOutputs= paste(getwd(),"/outputs",sep="") # Default Value is paste(getwd()"/outputs"sep="")
# regionsSelect=regionsSelect_i # Default Value is NULL
# paramsSelect="All" # Default value is "All"



dataGCAM # To view the data read that was read.
dataGCAM$data
unique(dataGCAM$data$param)

#----------------------------
# Produce Data Charts
#---------------------------

# Choose Parameters or set to "All" for all params. For complete list see ?metis.readgcam
paramsSelect_i=c("finalNrgbySec", "primNrgConsumByFuel", "elecByTech", "elecCapBySubsector",
                 "watConsumBySec", "watWithdrawBySec", "watWithdrawByCrop", "watBioPhysCons", "irrWatWithBasin","irrWatConsBasin",
                 "gdpPerCapita", "gdp", "gdpGrowthRate", "pop", "agProdbyIrrRfd",
                 "agProdBiomass", "agProdForest", "agProdByCrop", "landIrrRfd", "aggLandAlloc",
                 "LUCemiss", "co2emission", "co2emissionByEndUse", "ghgEmissionByGHG", "ghgEmissByGHGGROUPS",
                 "finalNrgbySecDetbyFuel","finalElecbySecDet","finalElecbyServiceDet","finalNrgbySecbyFuel","finalNrgbyFuelbySec")


# Read in Tables (If exist)
dataTables_i<-c(paste(getwd(),"/outputs/readGCAMTables/Tables_Local/local_Regional_Uruguay.csv",sep=""))  # Need to create this before loading
a<-read.csv(dataTables_i); head(a); unique(a$scenario); unique(a$param); unique(a$x)

# Read in the data from the function metis.readgcam
rTable_i <- dataGCAM$data

regionsSelect_i=c("Uruguay")

charts<-metis.chartsProcess(rTable=rTable_i, # Default is NULL
                            dataTables=dataTables_i, # Default is NULL
                            paramsSelect=paramsSelect_i, # Default is "All"
                            regionsSelect=regionsSelect_i, # Default is "All"
                            xCompare=c("2010","2015","2020","2030"), # Default is c("2015","2030","2050","2100")
                            scenRef="GCAMOrig", # Default is NULL
                            dirOutputs=paste(getwd(),"/outputs",sep=""), # Default is paste(getwd(),"/outputs",sep="")
                            pdfpng="png", # Default is "png"
                            regionCompareOnly=0, # Default is "0"
                            useNewLabels=1,
                            xRange=c(2010,2015,2020,2025,2030,2035,2040,2045,2050) # Default is All
                            )

# rTable=rTable_i # Default is NULL
# dataTables=dataTables_i # Default is NULL
# paramsSelect=paramsSelect_i # Default is "All"
# regionsSelect=regionsSelect_i # Default is "All"
# xCompare=c("2010","2015","2020","2030") # Default is c("2015","2030","2050","2100")
# scenRef="GCAMOrig" # Default is NULL
# dirOutputs=paste(getwd(),"/outputs",sep="") # Default is paste(getwd(),"/outputs",sep="")
# pdfpng="png" # Default is "png"
# regionCompareOnly=0 # Default is "0"
# useNewLabels=1
# xRange=c(2010,2015,2020,2025,2030,2035,2040,2045,2050) # Default is All


#------------
# Prepare Polygons
#----------------

countryName= "Peru"
localBasinShapeFileFolder = paste(getwd(),"/dataFiles/gis/other/shapefiles_Peru",sep="")
localBasinShapeFile = "UH"
#countryLocalBasin<-readOGR(dsn=localBasinShapeFileFolder,layer=localBasinShapeFile,use_iconv=T,encoding='UTF-8')
#countryLocalBasin; head(countryLocalBasin@data);
localBasinsShapeFileColName = "FID" # Will need to load the file to see which name this would be
countryName <- tools::toTitleCase(countryName); countryName

NE0<-readOGR(dsn=paste(getwd(),"/dataFiles/gis/metis/naturalEarth",sep=""),
             layer="ne_10m_admin_0_countries",use_iconv=T,encoding='UTF-8')

# Create directory for country
if (!dir.exists(paste(getwd(),"/dataFiles/gis/other/shapefiles_",countryName,sep=""))){
  dir.create(paste(getwd(),"/dataFiles/gis/other/shapefiles_",countryName,sep=""))}

if(file.exists(paste(getwd(),"/dataFiles/gis/other/shapefiles_",countryName,"/",countryName,"NE0.shp",sep=""))){
  countryNE0 = readOGR(dsn=paste(getwd(),"/dataFiles/gis/other/shapefiles_",countryName,sep=""),
                       layer=paste(countryName,"NE0",sep=""),use_iconv=T,encoding='UTF-8')
}else{
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
writeOGR(obj=countryNE0, dsn=paste(getwd(),"/dataFiles/gis/shapefiles_",countryName,sep=""), layer=paste(countryName,"NE0",sep=""), driver="ESRI Shapefile", overwrite_layer=TRUE)
}
metis.map(dataPolygon=countryNE0,fillColumn = "ADMIN",printFig=F, facetsON = F, labels=T, legendStyle = "cat")


# Natural earth level 1 admin boundaries
if(file.exists(paste(getwd(),"/dataFiles/gis/other/shapefiles_",countryName,"/",countryName,"NE0.shp",sep=""))){
  countryNE1 = readOGR(dsn=paste(getwd(),"/dataFiles/gis/other/shapefiles_",countryName,sep=""),
                       layer=paste(countryName,"NE1",sep=""),use_iconv=T,encoding='UTF-8')
}else{
NE1<-readOGR(dsn=paste(getwd(),"/dataFiles/gis/other/naturalEarth",sep=""),
             layer="ne_10m_admin_1_states_provinces",use_iconv=T,encoding='UTF-8')
if(!countryName %in% unique(NE1@data$admin)){stop(print(paste(countryName, " not in NE1 countries. Please check data.", sep="")))}
countryNE1<-NE1[(NE1$admin==countryName),]
# subset any islands or regions not wanted
countryNE1<-countryNE1[(!countryNE1$name %in% "San Andrés y Providencia") & !is.na(countryNE1$name),]
head(countryNE1@data)
plot(countryNE1)
countryNE1<-spTransform(countryNE1,CRS(projX))
writeOGR(obj=countryNE1, dsn=paste(getwd(),"/dataFiles/gis/other/shapefiles_",countryName,sep=""), layer=paste(countryName,"NE1",sep=""), driver="ESRI Shapefile", overwrite_layer=TRUE)
}
metis.map(dataPolygon=countryNE1,fillColumn = "name",printFig=F, facetsON = F, labels=T, legendStyle = "cat")


# GCAM Basins
if(file.exists(paste(getwd(),"/dataFiles/gis/other/shapefiles_",countryName,"/",countryName,"NE0.shp",sep=""))){
  countryGCAMBasin = readOGR(dsn=paste(getwd(),"/dataFiles/gis/other/shapefiles_",countryName,sep=""),
                       layer=paste(countryName,"GCAMBasin",sep=""),use_iconv=T,encoding='UTF-8')
}else{
GCAMBasin<-readOGR(dsn=paste(getwd(),"/dataFiles/gis/meits/gcam",sep=""),
                   layer="Global235_CLM_final_5arcmin_multipart",use_iconv=T,encoding='UTF-8')
GCAMBasin<-spTransform(GCAMBasin,CRS(projX))
countryGCAMBasin<-raster::crop(GCAMBasin,countryNE1)
countryGCAMBasin@data <- droplevels(countryGCAMBasin@data)
head(countryGCAMBasin@data)
plot(countryGCAMBasin)
writeOGR(obj=countryGCAMBasin, dsn=paste(getwd(),"/dataFiles/gis/other/shapefiles_",countryName,sep=""), layer=paste(countryName,"GCAMBasin",sep=""), driver="ESRI Shapefile", overwrite_layer=TRUE)
}
metis.map(dataPolygon=countryGCAMBasin,fillColumn = "basin_name",printFig=F,facetsON = F, labels=T, legendStyle = "cat")


# Local basin Shapefiles
if(file.exists(paste(getwd(),"/dataFiles/gis/other/shapefiles_",countryName,"/",countryName,"NE0.shp",sep=""))){
  countryLocalBasin = readOGR(dsn=paste(getwd(),"/dataFiles/gis/other/shapefiles_",countryName,sep=""),
                       layer=paste(countryName,"LocalBasin",sep=""),use_iconv=T,encoding='UTF-8')
}else{
countryLocalBasin<-readOGR(dsn=localBasinShapeFileFolder,
                            layer=localBasinShapeFile,use_iconv=T,encoding='UTF-8')
countryLocalBasin<-spTransform(countryLocalBasin,CRS(projX))
countryLocalBasin<-raster::crop(countryLocalBasin,countryNE1)
countryLocalBasin@data <- droplevels(countryLocalBasin@data)
head(countryLocalBasin@data)
plot(countryLocalBasin)
writeOGR(obj=countryLocalBasin, dsn=paste(getwd(),"/dataFiles/gis/other/shapefiles_",countryName,sep=""), layer=paste(countryName,"LocalBasin",sep=""), driver="ESRI Shapefile", overwrite_layer=TRUE)
}
metis.map(dataPolygon=countryLocalBasin,fillColumn = localBasinsShapeFileColName,printFig=F, facetsON = F, labels=T)

# dataPolygon=countryLocalBasin
# fillColumn = localBasinsShapeFileColName
# printFig=F
# facetsON = F
# labels=T
# legendStyle = "cat"
# fillPalette = eval(parse(text=paste(b[1,2])))

#-----------
# Boundaries
#-------------

# Plot NE admin boundaries 1
boundaryRegShape_i = NE0
#boundaryRegShpFolder_i=paste(getwd(),"/dataFiles/gis/naturalEarth",sep="")
#boundaryRegShpFile_i=paste("ne_10m_admin_0_countries",sep="")
boundaryRegCol_i="NAME"
boundaryRegionsSelect_i=countryName
subRegShape_i = countryNE1
#subRegShpFolder_i = paste(getwd(),"/dataFiles/gis/other/shapefiles_",countryName,sep = "")
#subRegShpFile_i = paste("countryNE1",sep= "")
subRegCol_i = "name"
subRegType_i = "state"
nameAppend_i = "_NE"
expandPercent_i = 2
overlapShape_i = countryGCAMBasin
#overlapShpFile_i = "Global235_CLM_final_5arcmin_multipart"
#overlapShpFolder_i = paste(getwd(),"/dataFiles/gis/meits/gcam",sep= "")
extension_i =  T
cropSubShape2Bound_i = T

boundariesX<- metis.boundaries(
  #fillPalette = c("Accent"),
  boundaryRegShape=boundaryRegShape_i,
  #boundaryRegShpFolder=boundaryRegShpFolder_i,
  #boundaryRegShpFile=boundaryRegShpFile_i,
  boundaryRegCol=boundaryRegCol_i,
  boundaryRegionsSelect=boundaryRegionsSelect_i,
  subRegShape=subRegShape_i,
  #subRegShpFolder=subRegShpFolder_i,
  #subRegShpFile=subRegShpFile_i,
  subRegCol=subRegCol_i,
  subRegType=subRegType_i,
  nameAppend=nameAppend_i,
  expandPercent=expandPercent_i,
  overlapShape = overlapShape_i,
  #overlapShpFile=overlapShpFile_i,
  #overlapShpFolder=overlapShpFolder_i,
  extension = extension_i,
  grids = c(paste(getwd(),"/dataFiles/grids/emptyGrids/grid_025.csv",sep=""),
            paste(getwd(),"/dataFiles/grids/emptyGrids/grid_050.csv",sep="")),
  cropSubShape2Bound=cropSubShape2Bound_i
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


#------------------------
# Prepare Grids
#------------------------

dirOutputs=paste(getwd(),"/outputs",sep="")
reReadData=1
demeterFolder=paste(getwd(),"/dataFiles/grids/demeter/",sep="")
demeterScenario="Eg1"
demeterUnits="Landuse (Fraction)"
demeterTimesteps<-seq(from=2005,to=2020,by=5)
tethysFolder=paste(getwd(),"/dataFiles/grids/tethys/",sep="")
tethysScenario="Eg1"
copySingleTethysScenbyXanthos="Eg1"
tethysFiles=c("wddom","wdelec","wdirr","wdliv","wdmfg","wdmin","wdnonag","wdtotal")
tethysUnits="Water Withdrawals (mm)"
xanthosFolder=paste(getwd(),"/dataFiles/grids/xanthosRunsChris/",sep="")
#xanthosScenario="Eg1"
#xanthosUnits="Runoff (mm)"
xanthosFiles=c(
  #"pm_abcd_mrtm_gfdl-esm2m_rcp2p6_1950_2099/q_km3peryear_pm_abcd_mrtm_gfdl-esm2m_rcp2p6_1950_2099.csv",
  # "pm_abcd_mrtm_gfdl-esm2m_rcp4p5_1950_2099/q_km3peryear_pm_abcd_mrtm_gfdl-esm2m_rcp4p5_1950_2099.csv",
  # "pm_abcd_mrtm_gfdl-esm2m_rcp6p0_1950_2099/q_km3peryear_pm_abcd_mrtm_gfdl-esm2m_rcp6p0_1950_2099.csv",
  # "pm_abcd_mrtm_gfdl-esm2m_rcp8p5_1950_2099/q_km3peryear_pm_abcd_mrtm_gfdl-esm2m_rcp8p5_1950_2099.csv",
  # "pm_abcd_mrtm_hadgem2-es_rcp2p6_1950_2099/q_km3peryear_pm_abcd_mrtm_hadgem2-es_rcp2p6_1950_2099.csv",
  #  "pm_abcd_mrtm_hadgem2-es_rcp4p5_1950_2099/q_km3peryear_pm_abcd_mrtm_hadgem2-es_rcp4p5_1950_2099.csv",
  # "pm_abcd_mrtm_hadgem2-es_rcp6p0_1950_2099/q_km3peryear_pm_abcd_mrtm_hadgem2-es_rcp6p0_1950_2099.csv",
  # "pm_abcd_mrtm_hadgem2-es_rcp8p5_1950_2099/q_km3peryear_pm_abcd_mrtm_hadgem2-es_rcp8p5_1950_2099.csv",
  # "pm_abcd_mrtm_ipsl-cm5a-lr_rcp2p6_1950_2099/q_km3peryear_pm_abcd_mrtm_ipsl-cm5a-lr_rcp2p6_1950_2099.csv",
  # "pm_abcd_mrtm_ipsl-cm5a-lr_rcp4p5_1950_2099/q_km3peryear_pm_abcd_mrtm_ipsl-cm5a-lr_rcp4p5_1950_2099.csv",
  # "pm_abcd_mrtm_ipsl-cm5a-lr_rcp6p0_1950_2099/q_km3peryear_pm_abcd_mrtm_ipsl-cm5a-lr_rcp6p0_1950_2099.csv",
  # "pm_abcd_mrtm_ipsl-cm5a-lr_rcp8p5_1950_2099/q_km3peryear_pm_abcd_mrtm_ipsl-cm5a-lr_rcp8p5_1950_2099.csv",
  # "pm_abcd_mrtm_miroc-esm-chem_rcp2p6_1950_2099/q_km3peryear_pm_abcd_mrtm_miroc-esm-chem_rcp2p6_1950_2099.csv",
  # "pm_abcd_mrtm_miroc-esm-chem_rcp4p5_1950_2099/q_km3peryear_pm_abcd_mrtm_miroc-esm-chem_rcp4p5_1950_2099.csv",
  # "pm_abcd_mrtm_miroc-esm-chem_rcp6p0_1950_2099/q_km3peryear_pm_abcd_mrtm_miroc-esm-chem_rcp6p0_1950_2099.csv",
  # "pm_abcd_mrtm_miroc-esm-chem_rcp8p5_1950_2099/q_km3peryear_pm_abcd_mrtm_miroc-esm-chem_rcp8p5_1950_2099.csv",
  # "pm_abcd_mrtm_noresm1-m_rcp2p6_1950_2099/q_km3peryear_pm_abcd_mrtm_noresm1-m_rcp2p6_1950_2099.csv",
  # "pm_abcd_mrtm_noresm1-m_rcp4p5_1950_2099/q_km3peryear_pm_abcd_mrtm_noresm1-m_rcp4p5_1950_2099.csv",
  # "pm_abcd_mrtm_noresm1-m_rcp6p0_1950_2099/q_km3peryear_pm_abcd_mrtm_noresm1-m_rcp6p0_1950_2099.csv",
  "pm_abcd_mrtm_noresm1-m_rcp8p5_1950_2099/q_km3peryear_pm_abcd_mrtm_noresm1-m_rcp8p5_1950_2099.csv"
)
xanthosCoordinatesPath=paste(getwd(),"/dataFiles/grids/xanthosReference/coordinates.csv",sep="")
xanthosGridAreaHecsPath=paste(getwd(),"/dataFiles/grids/xanthosReference/Grid_Areas_ID.csv",sep="")
scarcityXanthosRollMeanWindow=10
spanLowess=0.25
popFolder<-paste(getwd(),"/dataFiles/grids/griddedIDsPop/",sep="")
popFiles<-"grid_pop_map"
popUnits<-"person"
gridMetisData=paste(dirOutputs, "/Grids/gridMetis.RData", sep = "")
sqliteUSE = T
sqliteDBNamePath =paste(getwd(),"/outputs/Grids/gridMetis.sqlite", sep = "")

gridMetis<-metis.prepGrid(
  reReadData=reReadData,
  # demeterFolder=demeterFolder,
  # demeterScenario=demeterScenario,
  # demeterTimesteps=demeterTimesteps,
  # demeterUnits=demeterUnits,
  tethysFolder=tethysFolder,
  tethysScenario=tethysScenario,
  copySingleTethysScenbyXanthos=copySingleTethysScenbyXanthos,
  tethysFiles=tethysFiles,
  tethysUnits=tethysUnits,
  # xanthosFolder=xanthosFolder,
  # xanthosFiles=xanthosFiles,
  # xanthosCoordinatesPath=xanthosCoordinatesPath,
  # xanthosGridAreaHecsPath=xanthosGridAreaHecsPath,
  # spanLowess=spanLowess,
  popFolder=popFolder,
  popFiles=popFiles,
  popUnits=popUnits,
  dirOutputs=paste(getwd(),"/outputs",sep=""),
  gridMetisData=gridMetisData,
  sqliteUSE = sqliteUSE,
  sqliteDBNamePath =sqliteDBNamePath)

# reReadData=reReadData
# demeterFolder=demeterFolder
# demeterScenario=demeterScenario
# demeterTimesteps=demeterTimesteps
# demeterUnits=demeterUnits
# tethysFolder=tethysFolder
# tethysScenario=tethysScenario
# copySingleTethysScenbyXanthos=copySingleTethysScenbyXanthos
# tethysFiles=tethysFiles
# tethysUnits=tethysUnits
# xanthosFolder=xanthosFolder
# xanthosFiles=xanthosFiles
# xanthosCoordinatesPath=xanthosCoordinatesPath
# xanthosGridAreaHecsPath=xanthosGridAreaHecsPath
# spanLowess=spanLowess
# dirOutputs=paste(getwd(),"/outputs",sep="")
# gridMetisData=gridMetisData
# popFolder=popFolder
# popFiles=popFiles
# popUnits=popUnits
# sqliteUSE = sqliteUSE
# sqliteDBNamePath =sqliteDBNamePath

#-----------
# Grid to Poly
#-------------

# Natural Earth admin1 boundaries
boundaryRegionsSelect_i=countryName
subRegShpFolder_i = paste(getwd(),"/dataFiles/gis/other/shapefiles_",countryName,sep = "")
subRegShpFile_i = paste(countryName,"NE1",sep= "")
subRegCol_i = "name"
subRegType_i = "state"
nameAppend_i = "_NE"
aggType_i = NULL
paramsSelect_i= c("population","tethysWatWithdraw_indv") #"demeterLandUse"
sqliteUSE_i = T
sqliteDBNamePath_i = paste(getwd(),"/outputs/Grids/gridMetis.sqlite", sep = "")
#grid_i=gridMetis

grid2polyX<-metis.grid2poly(
  #grid=grid_i,
  subRegShpFolder=subRegShpFolder_i,
  subRegShpFile=subRegShpFile_i,
  subRegCol=subRegCol_i,
  subRegType = subRegType_i,
  aggType=aggType_i,
  nameAppend=nameAppend_i,
  paramsSelect = paramsSelect_i,
  sqliteUSE = sqliteUSE_i,
  sqliteDBNamePath = sqliteDBNamePath_i,
  folderName = "Peru")

# subRegShpFolder=subRegShpFolder_i
# subRegShpFile=subRegShpFile_i
# subRegCol=subRegCol_i
# subRegType = subRegType_i
# aggType=aggType_i
# nameAppend=nameAppend_i
# paramsSelect = paramsSelect_i
# sqliteUSE = sqliteUSE_i
# sqliteDBNamePath = sqliteDBNamePath_i

#grid_i=gridMetis
#grid_i=paste(getwd(),"/outputs/Grids/gridMetisXanthos.RData",sep = "")
boundaryRegionsSelect_i=countryName
subRegShpFolder_i = localBasinShapeFileFolder # paste(getwd(),"/dataFiles/gis/other/shapefiles_",countryName,sep = "")
subRegShpFile_i = localBasinShapeFile # paste("colombiaLocalBasin",sep= "")
subRegCol_i = localBasinsShapeFileColName  #
subRegType_i = "subBasin"
nameAppend_i = "_local"
aggType_i = NULL
paramsSelect_i= c("population","tethysWatWithdraw_indv") #"demeterLandUse"
sqliteUSE_i = T
sqliteDBNamePath_i = paste(getwd(),"/outputs/Grids/gridMetis.sqlite", sep = "")

grid2polyX<-metis.grid2poly(#grid=grid_i,
  boundaryRegionsSelect=boundaryRegionsSelect_i,
  subRegShpFolder=subRegShpFolder_i,
  subRegShpFile=subRegShpFile_i,
  subRegCol=subRegCol_i,
  subRegType = subRegType_i,
  aggType=aggType_i,
  nameAppend=nameAppend_i,
  paramsSelect = paramsSelect_i,
  sqliteUSE = sqliteUSE_i,
  sqliteDBNamePath = sqliteDBNamePath_i,
  folderName = "Peru")



#-----------
# Mapping
#-------------

#examplePolygonTable<-paste(getwd(),"/outputs/Maps/Tables/subReg_origData_byClass_Argentina_subRegType_origDownscaled_hydrobidBermeo3.csv",sep="")

polygonDataTables_i=paste(getwd(),"/outputs/Grid2Poly/Peru/subReg_grid2poly_state_NE.csv",sep="")
a<-read.csv(polygonDataTables_i); head(a); unique(a$scenario); unique(a$param); unique(a$x)
for(param_i in unique(a$param)){print(param_i);print(unique((a%>%dplyr::filter(param==param_i))$x));print(unique((a%>%dplyr::filter(param==param_i))$scenario))}
gridDataTables_i=paste(getwd(),"/outputs/Grid2Poly/Peru/gridCropped_state_NE.csv",sep="")
b<-read.csv(gridDataTables_i); head(b); unique(b$scenario); unique(b$param); unique(b$x)
for(param_i in unique(b$param)){print(param_i);print(unique((b%>%dplyr::filter(param==param_i))$x));print(unique((b%>%dplyr::filter(param==param_i))$scenario))}
xRange_i= seq(from=2000,to=2050,by=5)
legendPosition_i=c("LEFT","bottom")
legendOutsideSingle_i=T
animateOn_i=T
delay_i=100
scenRef_i="gfdl-esm2m_rcp2p6_NA_NA"
paramsSelect_i = c("All")
indvScenarios_i = "All"
GCMRCPSSPPol_i=T


boundaryRegShape_i = NULL
boundaryRegShpFolder_i=paste(getwd(),"/dataFiles/gis/metis/naturalEarth",sep="")
boundaryRegShpFile_i=paste("ne_10m_admin_0_countries",sep="")
boundaryRegCol_i="NAME"
boundaryRegionsSelect_i=countryName

test = readOGR(dsn=boundaryRegShpFolder_i,layer=boundaryRegShpFile_i,use_iconv=T,encoding='UTF-8')
head(test@data)
metis.map(dataPolygon=test,fillColumn = boundaryRegCol_i,labels=F ,printFig=F,facetsON=F)


subRegShape_i = NULL
subRegShpFolder_i = paste(getwd(),"/dataFiles/gis/other/shapefiles_",countryName,sep = "")
subRegShpFile_i = paste(countryName,"NE1",sep= "")
subRegCol_i = "name"
subRegType_i = "state"
nameAppend_i = "_NE"

scaleRange_i=data.frame(param=c("griddedScarcity"),
                        maxScale=c(1),
                        minScale=c(0))


numeric2Cat_param <- list("griddedScarcity","param2")
numeric2Cat_breaks <- list(c(-Inf, 0.1, 0.2, 0.4,Inf),c(0,1,2))
numeric2Cat_labels <- list(c("None (0<WSI<0.1)","Low (0.1<WSI<0.2)","Moderate (0.2<WSI<0.4)","Severe (WSI>0.4)"),
                           c("a","b","c","d"))
numeric2Cat_palette <- list(c("pal_ScarcityCat"),
                            #c("c('None (0<WSI<0.1)'='black','Low (0.1<WSI<0.2)'='blue','Moderate (0.2<WSI<0.4)'='purple','Severe (WSI>0.4)'='yellow')"),
                            c("Spectral")) # Can be a custom scale or an R brewer paletter or a metis.pal
numeric2Cat_legendTextSize <- list(c(0.7),c(NULL))
numeric2Cat_list <-list(numeric2Cat_param=numeric2Cat_param,
                        numeric2Cat_breaks=numeric2Cat_breaks,
                        numeric2Cat_labels=numeric2Cat_labels,
                        numeric2Cat_palette=numeric2Cat_palette,
                        numeric2Cat_legendTextSize=numeric2Cat_legendTextSize)

list_index <- which(numeric2Cat_list$numeric2Cat_param=="griddedScarcity")
catBreaks <- numeric2Cat_list$numeric2Cat_breaks[[list_index]]; catBreaks
catLabels <- numeric2Cat_list$numeric2Cat_labels[[list_index]]; catLabels
catPalette <- numeric2Cat_list$numeric2Cat_palette[[list_index]]; catPalette
catLegendTextSize <- numeric2Cat_list$numeric2Cat_legendTextSize[[list_index]];catLegendTextSize


metis.mapsProcess(polygonDataTables=polygonDataTables_i,
                 gridDataTables=gridDataTables_i,
                 #xRange=c(2010,2020,2100),
                 folderName="Peru",
                 boundaryRegionsSelect=boundaryRegionsSelect_i,
                 boundaryRegShpFolder=boundaryRegShpFolder_i,
                 boundaryRegShpFile=boundaryRegShpFile_i,
                 boundaryRegCol=boundaryRegCol_i,
                 subRegShpFolder=subRegShpFolder_i,
                 subRegShpFile=subRegShpFile_i,
                 subRegCol=subRegCol_i,
                 nameAppend="",
                 legendPosition=c("LEFT","bottom"),
                 animateOn=T,
                 delay=100,
                 scenRef=scenRef_i,
                 extension=T,
                 diffOn = F,
                 legendOutsideSingle = T,
                 scaleRange = scaleRange_i)

polygonDataTables=polygonDataTables_i
gridDataTables=gridDataTables_i
#xRange=c(2010,2020,2100)
folderName="Peru"
boundaryRegionsSelect=boundaryRegionsSelect_i
boundaryRegShpFolder=boundaryRegShpFolder_i
boundaryRegCol=boundaryRegCol_i
boundaryRegShpFile=boundaryRegShpFile_i
subRegShpFolder=subRegShpFolder_i
subRegShpFile=subRegShpFile_i
subRegCol=subRegCol_i
nameAppend=""
legendPosition=c("LEFT","bottom")
animateOn=T
delay=100
scenRef=scenRef_i
extension=T
diffOn = F
legendOutsideSingle = T
scaleRange = scaleRange_i


metis.mapsProcess(polygonDataTables=polygonDataTables_i,
                 gridDataTables=gridDataTables_i,
                 xRange=xRange_i,
                 boundaryRegShape=boundaryRegShape_i,
                 boundaryRegShpFolder=boundaryRegShpFolder_i,
                 boundaryRegShpFile=boundaryRegShpFile_i,
                 boundaryRegCol=boundaryRegCol_i,
                 boundaryRegionsSelect=boundaryRegionsSelect_i,
                 subRegShape=subRegShape_i,
                 subRegShpFolder=subRegShpFolder_i,
                 subRegShpFile=subRegShpFile_i,
                 subRegCol=subRegCol_i,
                 nameAppend=nameAppend_i,
                 legendOutsideSingle=legendOutsideSingle_i,
                 legendPosition=legendPosition_i,
                 animateOn=animateOn_i,
                 delay=delay_i,
                 scenRef=scenRef_i,
                 extension=T,
                 expandPercent = 3,
                 figWidth=6,
                 figHeight=7,
                 paramsSelect = paramsSelect_i,
                 scaleRange = scaleRange_i,
                 indvScenarios=indvScenarios_i,
                 GCMRCPSSPPol=GCMRCPSSPPol_i,
                 multiFacetCols="scenarioRCP",
                 multiFacetRows="scenarioGCM",
                 legendOutsideMulti=T,
                 legendPositionMulti=NULL,
                 legendTitleSizeMulti=NULL,
                 legendTextSizeAnim=NULL,
                 legendTextSizeMulti=NULL,
                 refGCM="gfdl-esm2m",
                 refRCP="rcp2p6",
                 chosenRefMeanYears=c(2000:2050),
                 numeric2Cat_list=numeric2Cat_list,
                 folderName="Peru")


# polygonDataTables=polygonDataTables_i
# gridDataTables=gridDataTables_i
# xRange=xRange_i
# boundaryRegShape=boundaryRegShape_i
# boundaryRegShpFolder=boundaryRegShpFolder_i
# boundaryRegShpFile=boundaryRegShpFile_i
# boundaryRegCol=boundaryRegCol_i
# boundaryRegionsSelect=boundaryRegionsSelect_i
# subRegShape=subRegShape_i
# subRegShpFolder=subRegShpFolder_i
# subRegShpFile=subRegShpFile_i
# subRegCol=subRegCol_i
# subRegType=subRegType_i
# nameAppend=nameAppend_i
# legendOutsideSingle=legendOutsideSingle_i
# legendPosition=legendPosition_i
# animateOn=animateOn_i
# delay=delay_i
# scenRef=scenRef_i
# extension=T
# expandPercent = 3
# figWidth=6
# figHeight=7
# paramsSelect = paramsSelect_i
# scaleRange = scaleRange_i
# indvScenarios=indvScenarios_i
# GCMRCPSSPPol=T
# multiFacetCols="scenarioRCP"
# multiFacetRows="scenarioGCM"
# legendOutsideMulti=T
# legendPositionMulti=NULL
# legendTitleSizeMulti=NULL
# legendTextSizeAnim=NULL
# legendTextSizeMulti=NULL
# refGCM="gfdl-esm2m"
# refRCP="rcp2p6"
# chosenRefMeanYears=c(2000:2050)
# numeric2Cat_list=numeric2Cat_list


polygonDataTables_i=paste(getwd(),"/outputs/Maps/Tables/subReg_origData_byClass_",countryName,"_subBasin_origDownscaled_local.csv",sep="")
a<-read.csv(polygonDataTables_i); head(a); unique(a$scenario); unique(a$param); unique(a$x)
for(param_i in unique(a$param)){print(param_i);print(unique((a%>%dplyr::filter(param==param_i))$x));print(unique((a%>%dplyr::filter(param==param_i))$scenario))}

subRegShpFolder_i = paste(getwd(),"/dataFiles/gis/other/shapefiles_",countryName,sep = "")
subRegShpFile_i = localBasinShapeFile # paste("colombiaLocalBasin",sep= "")
subRegCol_i = localBasinsShapeFileColName  #
subRegType_i = "subBasin"
nameAppend_i = "_local"

metis.mapsProcess(polygonDataTables=polygonDataTables_i,
                 #gridDataTables=gridDataTables_i,
                 xRange=xRange_i,
                 boundaryRegShape=boundaryRegShape_i,
                 boundaryRegShpFolder=boundaryRegShpFolder_i,
                 boundaryRegShpFile=boundaryRegShpFile_i,
                 boundaryRegCol=boundaryRegCol_i,
                 boundaryRegionsSelect=boundaryRegionsSelect_i,
                 subRegShape=subRegShape_i,
                 subRegShpFolder=subRegShpFolder_i,
                 subRegShpFile=subRegShpFile_i,
                 subRegCol=subRegCol_i,
                 subRegType=subRegType_i,
                 nameAppend=nameAppend_i,
                 legendOutsideSingle=legendOutsideSingle_i,
                 legendPosition=legendPosition_i,
                 animateOn=animateOn_i,
                 delay=delay_i,
                 scenRef=scenRef_i,
                 extension=T,
                 expandPercent = 3,
                 figWidth=6,
                 figHeight=7,
                 paramsSelect = paramsSelect_i,
                 scaleRange = scaleRange_i,
                 indvScenarios=indvScenarios_i,
                 GCMRCPSSPPol=GCMRCPSSPPol_i,
                 multiFacetCols="scenarioRCP",
                 multiFacetRows="scenarioGCM",
                 legendOutsideMulti=T,
                 legendPositionMulti=NULL,
                 legendTitleSizeMulti=NULL,
                 legendTextSizeAnim=NULL,
                 legendTextSizeMulti=NULL,
                 refGCM="gfdl-esm2m",
                 refRCP="rcp2p6",
                 chosenRefMeanYears=c(2000:2050),
                 numeric2Cat_list=numeric2Cat_list)

# polygonDataTables=polygonDataTables_i
# gridDataTables=gridDataTables_i
# xRange=xRange_i
# boundaryRegShape=boundaryRegShape_i
# boundaryRegShpFolder=boundaryRegShpFolder_i
# boundaryRegShpFile=boundaryRegShpFile_i
# boundaryRegCol=boundaryRegCol_i
# boundaryRegionsSelect=boundaryRegionsSelect_i
# subRegShape=subRegShape_i
# subRegShpFolder=subRegShpFolder_i
# subRegShpFile=subRegShpFile_i
# subRegCol=subRegCol_i
# subRegType=subRegType_i
# nameAppend=nameAppend_i
# legendOutsideSingle=legendOutsideSingle_i
# legendPosition=legendPosition_i
# animateOn=animateOn_i
# delay=delay_i
# scenRef=scenRef_i
# extension=T
# expandPercent = 3
# figWidth=6
# figHeight=7
# paramsSelect = paramsSelect_i
# scaleRange = scaleRange_i
# indvScenarios=indvScenarios_i
# GCMRCPSSPPol=F
# multiFacetCols="scenarioRCP"
# multiFacetRows="scenarioGCM"
# legendOutsideMulti=T
# legendPositionMulti=NULL
# legendTitleSizeMulti=NULL
# legendTextSizeAnim=NULL
# legendTextSizeMulti=NULL
# refGCM="gfdl-esm2m"
# refRCP="rcp2p6"
# chosenRefMeanYears=c(2000:2050)
# numeric2Cat_list=numeric2Cat_list
