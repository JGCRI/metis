

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

countryName= "Uruguay"
countryName <- tools::toTitleCase(countryName); countryName
localBasinShapeFileFolder = paste(getwd(),"/dataFiles/gis/other/shapefiles_",countryName,sep="")
localBasinShapeFile = "uruguay_8cuencas"
localBasinsShapeFileColName = "code" # Will need to load the file to see which name this would be


#----------------------------
# Read GCAM Data
#---------------------------

# ?metis.readgcam # For more help

gcamdatabasePath_i <-paste("D:/GCAM/gcam-core_LAC/output",sep="")
gcamdatabaseName_i <-"database_basexdb"

dataProjPath_i <- paste(getwd(),"/dataFiles/gcam",sep="")
queryPath_i <-paste(getwd(),"/dataFiles/gcam",sep="")
regionsSelect_i <- c("Uruguay")

# Reference
dataProj_i <-"Uruguay_dataProj_Ref.proj"
dataProjLoaded <- loadProject(paste(dataProjPath_i, "/", dataProj_i, sep = ""))
listScenarios(dataProjLoaded)  # List of Scenarios in GCAM database
#queries <- listQueries(dataProjLoaded)  # List of Queries in queryxml

dataGCAMRef<-metis.readgcam(reReadData=F, # Default Value is T
                                 dataProj = dataProj_i, # Default Value is "dataProj.proj"
                                 dataProjPath = dataProjPath_i,
                                 scenOrigNames=c("IDBUruguay_GCAMOrig", "IDBUruguay_GCAMRef_NoImpacts"),
                                 scenNewNames=c("GCAMOrig","GCAMRef"),
                                 gcamdatabasePath=gcamdatabasePath_i,
                                 gcamdatabaseName=gcamdatabaseName_i,
                                 queryxml="metisQueries.xml",  # Default Value is "metisQueries.xml"
                                 queryPath = queryPath_i,
                                 dirOutputs= paste(getwd(),"/outputs",sep=""), # Default Value is paste(getwd(),"/outputs",sep="")
                                 regionsSelect=regionsSelect_i, # Default Value is NULL
                                 paramsSelect="All" # Default value is "All"
)

dataGCAMRef$data
unique(dataGCAMRef$data$param)
unique(dataGCAMRef$data$scenario)

# Impacts
dataProj_i <-"Uruguay_dataProj_Impacts.proj"
dataProjLoaded <- loadProject(paste(dataProjPath_i, "/", dataProj_i, sep = ""))
listScenarios(dataProjLoaded)  # List of Scenarios in GCAM database
#queries <- listQueries(dataProjLoaded)  # List of Queries in queryxml

dataGCAMImpacts<-metis.readgcam(reReadData=F, # Default Value is T
                            dataProj = dataProj_i, # Default Value is "dataProj.proj"
                            dataProjPath = dataProjPath_i,
                            scenOrigNames=c("IDBUruguay_GCAMRef_ImpactsGFDLrcp8p5","IDBUruguay_GCAMRef_ImpactsGFDLrcp2p6"),
                            scenNewNames=c("GFDL_RCP8p5","GFDL_RCP2p6"),
                            gcamdatabasePath=gcamdatabasePath_i,
                            gcamdatabaseName=gcamdatabaseName_i,
                            queryxml="metisQueries.xml",  # Default Value is "metisQueries.xml"
                            queryPath = queryPath_i,
                            dirOutputs= paste(getwd(),"/outputs",sep=""), # Default Value is paste(getwd(),"/outputs",sep="")
                            regionsSelect=regionsSelect_i, # Default Value is NULL
                            paramsSelect="All" # Default value is "All"
)

dataGCAMImpacts$data
unique(dataGCAMImpacts$data$param)
unique(dataGCAMImpacts$data$scenario)

# Save an object to a file
#saveRDS(dataGCAM$data, file = paste(getwd(),"/dataFiles/gcam/tempUruguayGCAMData.rds",sep=""))
#readRDS(file = paste(getwd(),"/dataFiles/gcam/tempUruguayGCAMData.rds",sep=""))

dataGCAM <- dplyr::bind_rows(dataGCAMRef$data,dataGCAMImpacts$data)# To view the data read that was read.
dataGCAM
#saveRDS(dataGCAM, file = paste(getwd(),"/dataFiles/gcam/tempUruguayGCAMData.rds",sep=""))
#readRDS(file = paste(getwd(),"/dataFiles/gcam/tempUruguayGCAMData.rds",sep=""))
unique(dataGCAM$param)
unique(dataGCAM$scenario)


#----------------------------
# Produce Data Charts
#---------------------------

#Choose parameters for Report
paramsSelect_i=c("finalNrgbySec", "elecByTech", "elecCapBySubsector",
                  "finalNrgbySecDetbyFuel","finalElecbySecDet","finalElecbyServiceDet","finalNrgbySecbyFuel","finalNrgbyFuelbySec",
                 "watConsumBySec", "watWithdrawBySec",
                 "gdp", "gdpGrowthRate", "pop",
                 "agProdByCrop","aggLandAlloc",
                "co2emissionBySector","nonco2emissionBySectorGWPAR5","nonco2emissionBySectorGTPAR5","nonco2emissionBySectorOrigUnits")


# Read in Tables (If exist)
dataTables_i<-c(paste(getwd(),"/dataFiles/localData/local_Regional_Uruguay.csv",sep=""))  # Need to create this before loading
a<-read.csv(dataTables_i); head(a); unique(a$scenario); unique(a$param); unique(a$x)

# Read in the data from the function metis.readgcam
rTable_i <- dataGCAM %>% dplyr::filter(value!=0)  %>%
  dplyr::mutate(class1=case_when(param=="agProdByCrop" ~ gsub("OilCrop","SoySunflower",class1),TRUE~class1),
                class1=case_when(param=="finalNrgbyFuelbySec" ~ gsub("hydrogen","Other",class1),TRUE~class1),
                class1=case_when(param=="co2emissionBySector" ~ gsub("electricity","energy",class1),TRUE~class1)) %>%
  dplyr::group_by(scenario, region, param, sources, class1, class2, x, xLabel, vintage, units,
                  aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                  origScen, origQuery, origUnits, origX)%>%
  dplyr::summarize_at(dplyr::vars("value","origValue"),dplyr::funs(sum(.,na.rm = T)))%>%
  dplyr::ungroup() %>% droplevels()

regionsSelect_i=c("Uruguay")

rTable_iMod <- rTable_i %>%
  dplyr::mutate(class1=case_when(param=="elecByTech" ~ gsub("a Coal","Fossil",class1),TRUE~class1),
                class1=case_when(param=="elecByTech" ~ gsub("c Gas","Fossil",class1),TRUE~class1),
                class1=case_when(param=="elecByTech" ~ gsub("e Oil","Fossil",class1),TRUE~class1),
                class1=case_when(param=="agProdByCrop" ~ gsub("OilCrop","SoySunflower",class1),TRUE~class1),
                class1=case_when(param=="finalNrgbyFuelbySec" ~ gsub("hydrogen","Other",class1),TRUE~class1),
                class1=case_when(param=="co2emissionBySector" ~ gsub("electricity","energy",class1),TRUE~class1)) %>%
  dplyr::group_by(scenario, region, param, sources, class1, class2, x, xLabel, vintage, units,
                  aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                  origScen, origQuery, origUnits, origX)%>%
  dplyr::summarize_at(dplyr::vars("value","origValue"),dplyr::funs(sum(.,na.rm = T)))%>%
  dplyr::ungroup() %>% droplevels()

paramsSelect_iMod=paramsSelect_i

#----------------------------
# REFERENCE
#----------------------------

scensSelect_i = c("GCAMOrig","GCAMRef","Local Data")

scaleRange_i = tibble::tribble(
  ~param,~minScale, ~maxScale,
  "watConsumBySec", 0, 10,
  "watWithdrawBySec", 0, 10)

charts<-metis.chartsProcess(rTable=rTable_i, # Default is NULL
                            dataTables=dataTables_i, # Default is NULL
                            paramsSelect=paramsSelect_i, # Default is "All"
                            regionsSelect=regionsSelect_i, # Default is "All"
                            scensSelect=scensSelect_i,
                            xCompare=c("2010","2015","2020","2030"), # Default is c("2015","2030","2050","2100")
                            scenRef="GCAMOrig", # Default is NULL
                            dirOutputs=paste(getwd(),"/outputs",sep=""), # Default is paste(getwd(),"/outputs",sep="")
                            pdfpng="png", # Default is "png"
                            regionCompareOnly=0, # Default is "0"
                            scenarioCompareOnly=1, # Default is "0"
                            useNewLabels=1,
                            xRange=c(2010:2050),
                            colOrder1 = c("GCAMOrig","GCAMRef","Local Data"),
                            colOrderName1 = "scenario",
                            folderName = "Reference",
                            scaleRange=scaleRange_i)

charts<-metis.chartsProcess(rTable=rTable_iMod, # Default is NULL
                            dataTables=dataTables_i, # Default is NULL
                            paramsSelect=paramsSelect_iMod, # Default is "All"
                            regionsSelect=regionsSelect_i, # Default is "All"
                            scensSelect=scensSelect_i,
                            xCompare=c("2010","2015","2020","2030"), # Default is c("2015","2030","2050","2100")
                            scenRef="GCAMOrig", # Default is NULL
                            dirOutputs=paste(getwd(),"/outputs",sep=""), # Default is paste(getwd(),"/outputs",sep="")
                            pdfpng="png", # Default is "png"
                            regionCompareOnly=0, # Default is "0"
                            scenarioCompareOnly=1, # Default is "0"
                            useNewLabels=1,
                            xRange=c(2010,2015,2020,2025,2030,2035,2040,2045,2050),
                            colOrder1 = c("GCAMOrig","GCAMRef","Local Data"),
                            colOrderName1 = "scenario",
                            folderName = "Reference",
                            scaleRange=scaleRange_i)


#----------------------------
# IMPACTS
#----------------------------

scensSelect_i = c("GCAMRef","GFDL_RCP2p6","GFDL_RCP8p5")

charts<-metis.chartsProcess(rTable=rTable_i, # Default is NULL
                            #dataTables=dataTables_i, # Default is NULL
                            paramsSelect=paramsSelect_i, # Default is "All"
                            regionsSelect=regionsSelect_i, # Default is "All"
                            scensSelect=scensSelect_i,
                            xCompare=c("2010","2015","2020","2030"), # Default is c("2015","2030","2050","2100")
                            scenRef="GCAMRef", # Default is NULL
                            dirOutputs=paste(getwd(),"/outputs",sep=""), # Default is paste(getwd(),"/outputs",sep="")
                            pdfpng="png", # Default is "png"
                            regionCompareOnly=0, # Default is "0"
                            scenarioCompareOnly=0, # Default is "0"
                            useNewLabels=1,
                            xRange=c(2010:2050),
                            colOrder1 = c("GCAMRef","GFDL_RCP2p6","GFDL_RCP8p5"),
                            colOrderName1 = "scenario",
                            folderName = "Impacts",
                            scaleRange=scaleRange_i)

charts<-metis.chartsProcess(rTable=rTable_iMod, # Default is NULL
                            #dataTables=dataTables_i, # Default is NULL
                            paramsSelect=paramsSelect_iMod, # Default is "All"
                            regionsSelect=regionsSelect_i, # Default is "All"
                            scensSelect=scensSelect_i,
                            xCompare=c("2010","2015","2020","2030"), # Default is c("2015","2030","2050","2100")
                            scenRef="GCAMRef", # Default is NULL
                            dirOutputs=paste(getwd(),"/outputs",sep=""), # Default is paste(getwd(),"/outputs",sep="")
                            pdfpng="png", # Default is "png"
                            regionCompareOnly=0, # Default is "0"
                            scenarioCompareOnly=1, # Default is "0"
                            useNewLabels=1,
                            xRange=c(2010,2015,2020,2025,2030,2035,2040,2045,2050),
                            colOrder1 = c("GCAMRef","GFDL_RCP2p6","GFDL_RCP8p5"),
                            colOrderName1 = "scenario",
                            folderName = "Impacts",
                            scaleRange=scaleRange_i)



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


# Local basin Shapefiles
countryLocalBasin<-readOGR(dsn=localBasinShapeFileFolder,
                            layer=localBasinShapeFile,use_iconv=T,encoding='UTF-8')
countryLocalBasin<-spTransform(countryLocalBasin,CRS(projX))
countryLocalBasin<-raster::crop(countryLocalBasin,countryNE1)
countryLocalBasin@data <- droplevels(countryLocalBasin@data)
head(countryLocalBasin@data)
plot(countryLocalBasin)
writeOGR(obj=countryLocalBasin, dsn=paste(getwd(),"/dataFiles/gis/other/shapefiles_",countryName,sep=""), layer=paste(countryName,"LocalBasin",sep=""), driver="ESRI Shapefile", overwrite_layer=TRUE)
metis.map(dataPolygon=countryLocalBasin,fillColumn = localBasinsShapeFileColName,printFig=F, facetsON = F, labels=T, legendStyle = "cat")

# dataPolygon=countryLocalBasin
# fillColumn = localBasinsShapeFileColName
# printFig=F
# facetsON = F
# labels=T
# legendStyle = "cat"
# fillPalette = eval(parse(text=paste(b[1,2])))

metis.gridByPoly(grid = paste(getwd(),"/dataFiles/grids/emptyGrids/grid_025.csv",sep=""),
                 boundaryRegShpFile = localBasinShapeFile,
                 boundaryRegShpFolder = localBasinShapeFileFolder,
                 colName = localBasinsShapeFileColName,
                 saveFile = T,
                 fname = "gridByPoly_Uruguay8cuencas_0.25")

metis.gridByPoly(grid = paste(getwd(),"/dataFiles/grids/emptyGrids/grid_050.csv",sep=""),
                 boundaryRegShpFile = localBasinShapeFile,
                 boundaryRegShpFolder = localBasinShapeFileFolder,
                 colName = localBasinsShapeFileColName,
                 saveFile = T,
                 fname = "gridByPoly_Uruguay8cuencas_0.50")

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
#subRegShpFolder_i = paste(getwd(),"/dataFiles/gis/shapefiles_",countryName,sep = "")
#subRegShpFile_i = paste("countryNE1",sep= "")
subRegCol_i = "name"
subRegType_i = "state"
nameAppend_i = "_NE"
expandPercent_i = 2
overlapShape_i = countryGCAMBasin
#overlapShpFile_i = "Global235_CLM_final_5arcmin_multipart"
#overlapShpFolder_i = paste(getwd(),"/dataFiles/gis/basin_gcam",sep= "")
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
xanthosFolder=paste("D:/Projects/003a_IDBLAC_Uruguay/CorrespondenceData/UruguayData33_XanthosRaw_5Aug2019/Xanthos_Raw",sep="")
#xanthosScenario="Eg1"
#xanthosUnits="Runoff (mm)"
xanthosFiles=c(
  "clim_impacts_GFDL-ESM2M_rcp2p6/q_km3peryear_GFDL-ESM2M_rcp2p6_1950_2099.csv",
  "clim_impacts_GFDL-ESM2M_rcp4p5/q_km3peryear_GFDL-ESM2M_rcp4p5_1950_2099.csv"
)
xanthosCoordinatesPath=paste(getwd(),"/dataFiles/grids/xanthosReference/coordinates.csv",sep="")
xanthosGridAreaHecsPath=paste(getwd(),"/dataFiles/grids/xanthosReference/Grid_Areas_ID.csv",sep="")
scarcityXanthosRollMeanWindow=10
spanLowess=0.25
popFolder<-paste(getwd(),"/dataFiles/grids/griddedIDsPop/",sep="")
popFiles<-"grid_pop_map"
popUnits<-"person"
gridMetisData=paste(dirOutputs, "/Grids/gridMetis_uruguay.RData", sep = "")
sqliteUSE = T
sqliteDBNamePath =paste(getwd(),"/outputs/Grids/gridMetis_uruguay.sqlite", sep = "")

gridMetis<-metis.prepGrid(
  reReadData=reReadData,
  demeterFolder=demeterFolder,
  demeterScenario=demeterScenario,
  demeterTimesteps=demeterTimesteps,
  demeterUnits=demeterUnits,
  tethysFolder=tethysFolder,
  tethysScenario=tethysScenario,
  copySingleTethysScenbyXanthos=copySingleTethysScenbyXanthos,
  tethysFiles=tethysFiles,
  tethysUnits=tethysUnits,
  xanthosFolder=xanthosFolder,
  xanthosFiles=xanthosFiles,
  xanthosCoordinatesPath=xanthosCoordinatesPath,
  xanthosGridAreaHecsPath=xanthosGridAreaHecsPath,
  spanLowess=spanLowess,
  dirOutputs=paste(getwd(),"/outputs",sep=""),
  gridMetisData=gridMetisData,
  popFolder=popFolder,
  popFiles=popFiles,
  popUnits=popUnits,
  sqliteUSE = sqliteUSE,
  sqliteDBNamePath =sqliteDBNamePath)

# reReadData=reReadData
# # demeterFolder=demeterFolder
# # demeterScenario=demeterScenario
# # demeterTimesteps=demeterTimesteps
# # demeterUnits=demeterUnits
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
# # popFolder=popFolder
# # popFiles=popFiles
# # popUnits=popUnits
# sqliteUSE = sqliteUSE
# sqliteDBNamePath =sqliteDBNamePath

#-----------
# Grid to Poly
#-------------

# Natural Earth admin1 boundaries
subRegShpFolder_i = paste(getwd(),"/dataFiles/gis/other/shapefiles_",countryName,sep = "")
subRegShpFile_i = paste(countryName,"NE1",sep= "")
subRegCol_i = "name"
subRegType_i = "state"
nameAppend_i = "_NE"
aggType_i = NULL
paramsSelect_i= "All" #"demeterLandUse"
sqliteUSE_i = T
sqliteDBNamePath_i = paste(getwd(),"/outputs/Grids/gridMetis_Uruguay.sqlite", sep = "")

grid2polyX<-metis.grid2poly(#grid=grid_i,
  subRegShpFolder=subRegShpFolder_i,
  subRegShpFile=subRegShpFile_i,
  subRegCol=subRegCol_i,
  subRegType = subRegType_i,
  aggType=aggType_i,
  nameAppend=nameAppend_i,
  paramsSelect = paramsSelect_i,
  sqliteUSE = sqliteUSE_i,
  sqliteDBNamePath = sqliteDBNamePath_i,
  folderName = countryName,
  regionName = countryName)

# subRegShpFolder=subRegShpFolder_i
# subRegShpFile=subRegShpFile_i
# subRegCol=subRegCol_i
# subRegType = subRegType_i
# aggType=aggType_i
# nameAppend=nameAppend_i
# paramsSelect = paramsSelect_i
# sqliteUSE = sqliteUSE_i
# sqliteDBNamePath = sqliteDBNamePath_i
# folderName = countryName
# regionName = countryName

#grid_i=gridMetis
#grid_i=paste(getwd(),"/outputs/Grids/gridMetisXanthos.RData",sep = "")
subRegShpFolder_i = localBasinShapeFileFolder # paste(getwd(),"/dataFiles/gis/shapefiles_",countryName,sep = "")
subRegShpFile_i = localBasinShapeFile # paste("colombiaLocalBasin",sep= "")
subRegCol_i = localBasinsShapeFileColName  #
subRegType_i = "subBasin"
nameAppend_i = "_local"
aggType_i = NULL
paramsSelect_i= "All" #"demeterLandUse"
sqliteUSE_i = T
sqliteDBNamePath_i = paste(getwd(),"/outputs/Grids/gridMetis_Uruguay.sqlite", sep = "")

grid2polyX<-metis.grid2poly(#grid=grid_i,
  subRegShpFolder=subRegShpFolder_i,
  subRegShpFile=subRegShpFile_i,
  subRegCol=subRegCol_i,
  subRegType = subRegType_i,
  aggType=aggType_i,
  nameAppend=nameAppend_i,
  paramsSelect = paramsSelect_i,
  sqliteUSE = sqliteUSE_i,
  sqliteDBNamePath = sqliteDBNamePath_i,
  folderName = countryName,
  regionName = countryName)


#-----------
# Mapping
#-------------

#examplePolygonTable<-paste(getwd(),"/outputs/Maps/Tables/subReg_origData_byClass_Argentina_subRegType_origDownscaled_hydrobidBermeo3.csv",sep="")

polygonDataTables_i=paste(getwd(),"/outputs/Grid2Poly/Uruguay/subReg_grid2poly_state_NE.csv",sep="")
a<-read.csv(polygonDataTables_i); head(a); unique(a$scenario); unique(a$param); unique(a$x)
for(param_i in unique(a$param)){print(param_i);print(unique((a%>%dplyr::filter(param==param_i))$x));print(unique((a%>%dplyr::filter(param==param_i))$scenario))}
gridDataTables_i=paste(getwd(),"/outputs/Grid2Poly/Uruguay/gridCropped_state_NE.csv",sep="")
b<-read.csv(gridDataTables_i); head(b); unique(b$scenario); unique(b$param); unique(b$x)
for(param_i in unique(b$param)){print(param_i);print(unique((b%>%dplyr::filter(param==param_i))$x));print(unique((b%>%dplyr::filter(param==param_i))$scenario))}
xRange_i= seq(from=2000,to=2050,by=5)
legendPosition_i=c("LEFT","bottom")
legendOutsideSingle_i=T
animateOn_i=T
delay_i=100
scenRef_i="GFDL-ESM2M_rcp2p6_NA_NA"
paramsSelect_i = c("All")
indvScenarios_i = "All"
GCMRCPSSPPol_i=T


boundaryRegShape_i = NULL
boundaryRegShpFolder_i=paste(getwd(),"/dataFiles/gis/metis/naturalEarth",sep="")
boundaryRegShpFile_i=paste("ne_10m_admin_0_countries",sep="")
boundaryRegCol_i="NAME"
boundaryRegionsSelect_i="Uruguay"
subRegShape_i = NULL
subRegShpFolder_i = paste(getwd(),"/dataFiles/gis/other/shapefiles_",countryName,sep = "")
subRegShpFile_i = paste(countryName,"NE1",sep= "")
subRegCol_i = "name"
subRegType_i = "state"
nameAppend_i = "_NE"

scaleRange_i=data.frame(param=c("griddedScarcity"),
                        maxScale=c(1),
                        minScale=c(0))


numeric2Cat_param <- list("griddedScarcity","polyScarcity")
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

list_index <- which(numeric2Cat_list$numeric2Cat_param=="griddedScarcity")
catBreaks <- numeric2Cat_list$numeric2Cat_breaks[[list_index]]; catBreaks
catLabels <- numeric2Cat_list$numeric2Cat_labels[[list_index]]; catLabels
catPalette <- numeric2Cat_list$numeric2Cat_palette[[list_index]]; catPalette
catLegendTextSize <- numeric2Cat_list$numeric2Cat_legendTextSize[[list_index]];catLegendTextSize



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
                 folderName = "Uruguay_state")


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
# nameAppend=nameAppend_i
# legendOutsideSingle=legendOutsideSingle_i
# legendPosition=legendPosition_i
# animateOn=animateOn_i
# scenRef=scenRef_i
# extension=T
# expandPercent = 3
# figWidth=6
# figHeight=7
# paramsSelect = c("griddedScarcity") # paramsSelect_i
# scaleRange = scaleRange_i
# indvScenarios=indvScenarios_i
# GCMRCPSSPPol=GCMRCPSSPPol_i
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
# folderName = "Uruguay_state"


polygonDataTables_i=paste(getwd(),"/outputs/Grid2Poly/Uruguay/subReg_grid2poly_subBasin_local.csv",sep="")
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
                  nameAppend=nameAppend_i,
                  legendOutsideSingle=legendOutsideSingle_i,
                  legendPosition=legendPosition_i,
                  animateOn=animateOn_i,
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
                  folderName = "Uruguay_localBasin")

# polygonDataTables=polygonDataTables_i
# #gridDataTables=gridDataTables_i
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
# nameAppend=nameAppend_i
# legendOutsideSingle=legendOutsideSingle_i
# legendPosition=legendPosition_i
# animateOn=animateOn_i
# scenRef=scenRef_i
# extension=T
# expandPercent = 3
# figWidth=6
# figHeight=7
# paramsSelect = paramsSelect_i
# scaleRange = scaleRange_i
# indvScenarios=indvScenarios_i
# GCMRCPSSPPol=GCMRCPSSPPol_i
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
# folderName = "Uruguay_localBasin"

