
#----------------------------
# Install necessary packages
#----------------------------
if("devtools" %in% rownames(installed.packages()) == F){install.packages("devtools")}
library(devtools)
if("metis" %in% rownames(installed.packages()) == F){install_github(repo="zarrarkhan/metis")}
library(metis)
if("rgcam" %in% rownames(installed.packages()) == F){install_github(repo="JGCRI/rgcam")}
library(rgcam)

for(package_i in c("tibble","dplyr","rgdal","tmap")){
if(package_i %in% rownames(installed.packages()) == F){install.packages(package_i)}
library(package_i,character.only = TRUE)}



#----------------------------
# Read GCAM Data
#---------------------------

gcamparamsSelect_i = c("finalNrgbySec", "primNrgConsumByFuel", "elecByTech",
                       "watConsumBySec", "watWithdrawBySec","gdp", "gdpGrowthRate", "pop",
                       "agProdByCrop", "aggLandAlloc","co2emissionByEndUse")
reReadData_i = F # Default Value is T

gcamdatabasePath_i = paste(getwd(),"/dataFiles/gcam",sep="")
gcamdatabaseName_i = NULL
gcamdataProjFile_i = "Example_dataProj.proj" # Default Value is "dataProj.proj"
scenOrigNames_i = c("ExampleScen1","ExampleScen2")
scenNewNames_i = c("Eg1","Eg2")
gcamregionsSelect_i = c("Colombia") # Default Value is NULL

#dataProjLoaded <- loadProject(paste(gcamdatabasePath_i, "/", gcamdataProjFile_i, sep = ""))
#listScenarios(dataProjLoaded)  # List of Scenarios in GCAM database
#queries <- listQueries(dataProjLoaded)  # List of Queries in queryxml


dataGCAM<-metis.readgcam(reReadData=reReadData_i, # Default Value is T
                         dataProj=gcamdataProjFile_i, # Default Value is "dataProj.proj"
                         scenOrigNames=scenOrigNames_i,
                         scenNewNames=scenNewNames_i,
                         gcamdatabasePath=gcamdatabasePath_i,
                         gcamdatabaseName=gcamdatabaseName_i,
                         queryxml="metisQueries.xml",  # Default Value is "metisQueries.xml"
                         dirOutputs= paste(getwd(),"/outputs",sep=""), # Default Value is paste(getwd(),"/outputs",sep="")
                         regionsSelect=gcamregionsSelect_i, # Default Value is NULL
                         paramsSelect=gcamparamsSelect_i, # Default value is "All"
                         queriesSelect="All" # Default is "All"
)


dataGCAM<-tibble::as_tibble(dataGCAM$data)
dataGCAM # To view the data read that was read.
unique((dataGCAM%>%filter(value>0))$param)
unique((dataGCAM%>%filter(value>0))$scenario)


#----------------------------
# Produce Data Charts
#---------------------------

# local data tables
# dataTables<-c(paste(getwd(),"/outputs/readGCAMTables/Tables_Local/local_Regional_Argentina.csv",sep=""))  # Need to create this before loading
#a<-read.csv(dataTables)

scenRef_i="Eg1"
rTable_i=dataGCAM
dataTables_i=NULL # Default is NULL
chartregionsSelect_i= "All" # Default is "All"
chartparamsSelect_i= "All" # Default is "All"
xRange_i="All"
xCompare_i= c("2015","2030","2050","2100")
regionCompareOnly_i=0


charts<-metis.chartsProcess(rTable=rTable_i, # Default is NULL
                            dataTables=dataTables_i, # Default is NULL
                            paramsSelect=chartparamsSelect_i, # Default is "All"
                            regionsSelect=chartregionsSelect_i, # Default is "All"
                            xRange = xRange_i,
                            xCompare=xCompare_i, # Default is c("2015","2030","2050","2100")
                            scenRef=scenRef_i, # Default is NULL
                            regionCompareOnly=regionCompareOnly_i, # Default is "0"
                            dirOutputs=paste(getwd(),"/outputs",sep=""), # Default is paste(getwd(),"/outputs",sep="")
                            pdfpng="png" # Default is "png"
                            )

# rTable=dataGCAM # Default is NULL
# dataTables=dataTables_i # Default is NULL
# paramsSelect=chartparamsSelect_i # Default is "All"
# regionsSelect=chartregionsSelect_i # Default is "All"
# xRange = xRange_i
# xCompare=xCompare_i # Default is c("2015","2030","2050","2100")
# scenRef=scenRef_i # Default is NULL
# regionCompareOnly=regionCompareOnly_i # Default is "0"
# dirOutputs=paste(getwd(),"/outputs",sep="") # Default is paste(getwd(),"/outputs",sep="")
# pdfpng="png" # Default is "png"


#------------
# Prepare Polygons
#----------------

NE0<-readOGR(dsn=paste(getwd(),"/dataFiles/gis/naturalEarth",sep=""),
             layer="ne_10m_admin_0_countries",use_iconv=T,encoding='UTF-8')

colombiaNE0<-readOGR(dsn=paste(getwd(),"/dataFiles/gis/naturalEarth",sep=""),
                  layer="ne_10m_admin_0_countries",use_iconv=T,encoding='UTF-8')
colombiaNE0<-colombiaNE0[(colombiaNE0$NAME=="Colombia"),]
head(colombiaNE0@data)
plot(colombiaNE0)
projX<-proj4string(colombiaNE0)

NE1<-readOGR(dsn=paste(getwd(),"/dataFiles/gis/naturalEarth",sep=""),
                       layer="ne_10m_admin_1_states_provinces",use_iconv=T,encoding='UTF-8')
colombiaNE1<-NE1[(NE1$admin=="Colombia"),]
colombiaNE1<-colombiaNE1[(!colombiaNE1$name %in% "San Andrés y Providencia") & !is.na(colombiaNE1$name),]
head(colombiaNE1@data)
plot(colombiaNE1)
colombiaNE1<-spTransform(colombiaNE1,CRS(projX))
writeOGR(obj=colombiaNE1, dsn=paste(getwd(),"/dataFiles/gis/admin_Colombia",sep=""), layer=paste("colombiaNE1",sep=""), driver="ESRI Shapefile", overwrite_layer=TRUE)
metis.map(dataPolygon=colombiaNE1,fillColumn = "name",printFig=F)



GCAMBasin<-readOGR(dsn=paste(getwd(),"/dataFiles/gis/basin_GCAM",sep=""),
                        layer="Global235_CLM_final_5arcmin_multipart",use_iconv=T,encoding='UTF-8')
GCAMBasin<-spTransform(GCAMBasin,CRS(projX))
colombiaGCAMBasin<-raster::crop(GCAMBasin,colombiaNE0)
head(colombiaGCAMBasin@data)
plot(colombiaGCAMBasin)
writeOGR(obj=colombiaGCAMBasin, dsn=paste(getwd(),"/dataFiles/gis/admin_Colombia",sep=""), layer=paste("colombiaGCAMBasin",sep=""), driver="ESRI Shapefile", overwrite_layer=TRUE)
metis.map(dataPolygon=colombiaGCAMBasin,fillColumn = "basin_name",printFig=F)

colombiaLocalBasin<-readOGR(dsn=paste(getwd(),"/dataFiles/gis/subbasin_colombia",sep=""),
                            layer="Zonificacion_hidrografica_2013",use_iconv=T,encoding='UTF-8')
colombiaLocalBasin<-spTransform(colombiaLocalBasin,CRS(projX))
head(colombiaLocalBasin@data)
plot(colombiaLocalBasin)
writeOGR(obj=colombiaLocalBasin, dsn=paste(getwd(),"/dataFiles/gis/admin_Colombia",sep=""), layer=paste("colombiaLocalBasin",sep=""), driver="ESRI Shapefile", overwrite_layer=TRUE)
metis.map(dataPolygon=colombiaLocalBasin,fillColumn = "NOM_ZH",printFig=F)


#-----------
# Boundaries
#-------------

# Explore Shape Files
examplePolyFolder<-paste(getwd(),"/dataFiles/gis/admin_Colombia",sep = "")
examplePolyFile<-paste("colombiaNE1",sep= "")
example=readOGR(dsn=examplePolyFolder,layer=examplePolyFile,use_iconv=T,encoding='UTF-8')
head(example@data)
metis.map(dataPolygon=example,fillColumn = "name",labels=T ,printFig=F,facetsON=F)

boundaryRegShape_i = NULL
boundaryRegShpFolder_i=paste(getwd(),"/dataFiles/gis/naturalEarth",sep="")
boundaryRegShpFile_i=paste("ne_10m_admin_0_countries",sep="")
boundaryRegCol_i="NAME"
boundaryRegionsSelect_i="Colombia"
subRegShape_i = NULL
subRegShpFolder_i = paste(getwd(),"/dataFiles/gis/admin_Colombia",sep = "")
subRegShpFile_i = paste("colombiaNE1",sep= "")
subRegCol_i = "name"
subRegType_i = "state"
nameAppend_i = "_NE"
expandPercent_i = 2
overlapShpFile_i = "Global235_CLM_final_5arcmin_multipart"
overlapShpFolder_i = paste(getwd(),"/dataFiles/gis/basin_gcam",sep= "")
extension_i =  T
cropSubShape2Bound_i = T


boundariesX<- metis.boundaries(
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
  expandPercent=expandPercent_i,
  overlapShpFile=overlapShpFile_i,
  overlapShpFolder=overlapShpFolder_i,
  extension = extension_i,
  grids = c(paste(getwd(),"/dataFiles/grids/emptyGrids/grid_025.csv",sep=""),
            paste(getwd(),"/dataFiles/grids/emptyGrids/grid_050.csv",sep="")),
  cropSubShape2Bound=cropSubShape2Bound_i
)

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
# expandPercent=expandPercent_i
# overlapShpFile=overlapShpFile_i
# overlapShpFolder=overlapShpFolder_i
# extension = extension_i
# grids = c(paste(getwd(),"/dataFiles/grids/emptyGrids/grid_025.csv",sep=""),
#           paste(getwd(),"/dataFiles/grids/emptyGrids/grid_050.csv",sep=""))
# cropSubShape2Bound=cropSubShape2Bound_i

subRegShpFile_i = paste("colombiaLocalBasin",sep= "")
subRegCol_i = "NOM_ZH"
subRegType_i = "subBasin"
nameAppend_i = "_colombiaLocal"

boundariesX<- metis.boundaries(
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
  expandPercent=expandPercent_i,
  overlapShpFile=overlapShpFile_i,
  overlapShpFolder=overlapShpFolder_i,
  extension = extension_i,
  grids = c(paste(getwd(),"/dataFiles/grids/emptyGrids/grid_025.csv",sep=""),
            paste(getwd(),"/dataFiles/grids/emptyGrids/grid_050.csv",sep="")),
  cropSubShape2Bound=cropSubShape2Bound_i
)


#------------------------
# Prepare Grids
#------------------------

dirOutputs=paste(getwd(),"/outputs",sep="")
reReadData=1
demeterFolder=paste(getwd(),"/dataFiles/grids/demeter/",sep="")
demeterScenario="gfdl-esm2m_rcp2p6"
demeterUnits="Landuse (Fraction)"
demeterTimesteps<-seq(from=2005,to=2020,by=5)
tethysFolder=paste(getwd(),"/dataFiles/grids/tethys/",sep="")
tethysScenario="gfdl-esm2m_rcp2p6"
tethysFiles=c("wddom","wdelec","wdirr","wdliv","wdmfg","wdmin","wdnonag","wdtotal")
tethysUnits="Water Withdrawals (mm)"
xanthosFolder=paste(getwd(),"/dataFiles/grids/xanthosRunsChris/",sep="")
#xanthosScenario="Eg1"
#xanthosUnits="Runoff (mm)"
xanthosFiles=c("pm_abcd_mrtm_gfdl-esm2m_rcp2p6_1950_2099/q_km3peryear_pm_abcd_mrtm_gfdl-esm2m_rcp2p6_1950_2099.csv",
               "pm_abcd_mrtm_gfdl-esm2m_rcp4p5_1950_2099/q_km3peryear_pm_abcd_mrtm_gfdl-esm2m_rcp4p5_1950_2099.csv",
               "pm_abcd_mrtm_gfdl-esm2m_rcp6p0_1950_2099/q_km3peryear_pm_abcd_mrtm_gfdl-esm2m_rcp6p0_1950_2099.csv"
               # "pm_abcd_mrtm_gfdl-esm2m_rcp8p5_1950_2099/q_km3peryear_pm_abcd_mrtm_gfdl-esm2m_rcp8p5_1950_2099.csv",
               # "pm_abcd_mrtm_hadgem2-es_rcp2p6_1950_2099/q_km3peryear_pm_abcd_mrtm_hadgem2-es_rcp2p6_1950_2099.csv",
               # # "pm_abcd_mrtm_hadgem2-es_rcp4p5_1950_2099/q_km3peryear_pm_abcd_mrtm_hadgem2-es_rcp4p5_1950_2099.csv",
               # "pm_abcd_mrtm_hadgem2-es_rcp6p0_1950_2099/q_km3peryear_pm_abcd_mrtm_hadgem2-es_rcp6p0_1950_2099.csv",
               # "pm_abcd_mrtm_hadgem2-es_rcp8p5_1950_2099/q_km3peryear_pm_abcd_mrtm_hadgem2-es_rcp8p5_1950_2099.csv",
               # "pm_abcd_mrtm_ipsl-cm5a-Ir_rcp2p6_1950_2099/q_km3peryear_pm_abcd_mrtm_ipsl-cm5a-Ir_rcp2p6_1950_2099.csv",
               # "pm_abcd_mrtm_ipsl-cm5a-Ir_rcp4p5_1950_2099/q_km3peryear_pm_abcd_mrtm_ipsl-cm5a-Ir_rcp4p5_1950_2099.csv",
               # "pm_abcd_mrtm_ipsl-cm5a-Ir_rcp6p0_1950_2099/q_km3peryear_pm_abcd_mrtm_ipsl-cm5a-Ir_rcp6p0_1950_2099.csv",
               # "pm_abcd_mrtm_ipsl-cm5a-Ir_rcp8p5_1950_2099/q_km3peryear_pm_abcd_mrtm_ipsl-cm5a-Ir_rcp8p5_1950_2099.csv",
               # "pm_abcd_mrtm_miroc-esm-chem_rcp2p6_1950_2099/q_km3peryear_pm_abcd_mrtm_miroc-esm-chem_rcp2p6_1950_2099.csv",
               # "pm_abcd_mrtm_miroc-esm-chem_rcp4p5_1950_2099/q_km3peryear_pm_abcd_mrtm_miroc-esm-chem_rcp4p5_1950_2099.csv",
               # "pm_abcd_mrtm_miroc-esm-chem_rcp6p0_1950_2099/q_km3peryear_pm_abcd_mrtm_miroc-esm-chem_rcp6p0_1950_2099.csv",
               # "pm_abcd_mrtm_miroc-esm-chem_rcp8p5_1950_2099/q_km3peryear_pm_abcd_mrtm_miroc-esm-chem_rcp8p5_1950_2099.csv",
               # "pm_abcd_mrtm_noresm1-m_rcp2p6_1950_2099/q_km3peryear_pm_abcd_mrtm_noresm1-m_rcp2p6_1950_2099.csv",
               # "pm_abcd_mrtm_noresm1-m_rcp4p5_1950_2099/q_km3peryear_pm_abcd_mrtm_noresm1-m_rcp4p5_1950_2099.csv",
               # "pm_abcd_mrtm_noresm1-m_rcp6p0_1950_2099/q_km3peryear_pm_abcd_mrtm_noresm1-m_rcp6p0_1950_2099.csv",
               # "pm_abcd_mrtm_noresm1-m_rcp8p5_1950_2099/q_km3peryear_pm_abcd_mrtm_noresm1-m_rcp8p5_1950_2099.csv"
               )
xanthosCoordinatesPath=paste(getwd(),"/dataFiles/grids/xanthosCoords/coordinates.csv",sep="")
xanthosGridAreaHecsPath=paste(getwd(),"/dataFiles/grids/xanthosRunsChris/reference/Grid_Areas_ID.csv",sep="")
scarcityXanthosRollMeanWindow=10
spanLowess=0.25
popFolder<-paste(getwd(),"/dataFiles/grids/griddedIDsPop/",sep="")
popFiles<-"grid_pop_map"
popUnits<-"person"
gridMetisData=paste(dirOutputs, "/Grids/gridMetisXanthos.RData", sep = "")
sqliteUSE = T
sqliteDBNamePath =paste(getwd(),"/outputs/Grids/gridMetis.sqlite", sep = "")

gridMetis<-metis.prepGrid(
  reReadData=reReadData,
  demeterFolder=demeterFolder,
  demeterScenario=demeterScenario,
  demeterTimesteps=demeterTimesteps,
  demeterUnits=demeterUnits,
  tethysFolder=tethysFolder,
  tethysScenario=tethysScenario,
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
# demeterFolder=demeterFolder
# demeterScenario=demeterScenario
# demeterTimesteps=demeterTimesteps
# demeterUnits=demeterUnits
# tethysFolder=tethysFolder
# tethysScenario=tethysScenario
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

#grid_i<-paste(getwd(),"/dataFiles/examples/example_grid_ArgentinaBermejo3_Eg1Eg2.csv",sep="")
#grid_i<-paste(getwd(),"/outputs/Grids/gridMetis.csv",sep="")
#grid_i=gridMetis
boundaryRegionsSelect_i="Colombia"
subRegShpFolder_i = paste(getwd(),"/dataFiles/gis/admin_Colombia",sep = "")
subRegShpFile_i = paste("colombiaNE1",sep= "")
subRegCol_i = "name"
subRegType_i = "state"
nameAppend_i = "_NE"
aggType_i = NULL
paramsSelect_i= "All" #"demeterLandUse"
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
                                    sqliteDBNamePath = sqliteDBNamePath_i)

# grid=grid_i
# boundaryRegionsSelect=boundaryRegionsSelect_i
# subRegShpFolder=subRegShpFolder_i
# subRegShpFile=subRegShpFile_i
# subRegCol=subRegCol_i
# aggType=aggType_i
# nameAppend=nameAppend_i
# sqliteUSE = sqliteUSE_i
# sqliteDBNamePath = sqliteDBNamePath_i

#grid_i=gridMetis
#grid_i=paste(getwd(),"/outputs/Grids/gridMetisXanthos.RData",sep = "")
boundaryRegionsSelect_i="Colombia"
subRegShpFolder_i = paste(getwd(),"/dataFiles/gis/admin_Colombia",sep = "")
subRegShpFile_i = paste("colombiaLocalBasin",sep= "")
subRegCol_i = "NOM_ZH"
subRegType_i = "subBasin"
nameAppend_i = "_local"
aggType_i = NULL
paramsSelect_i= "All" #"demeterLandUse"
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
                            sqliteDBNamePath = sqliteDBNamePath_i)

# grid=grid_i
# boundaryRegionsSelect=boundaryRegionsSelect_i
# subRegShpFolder=subRegShpFolder_i
# subRegShpFile=subRegShpFile_i
# subRegCol=subRegCol_i
# subRegType = subRegType_i
# aggType=aggType_i
# nameAppend=nameAppend_i
# paramsSelect = paramsSelect_i
# sqliteUSE = sqliteUSE_i
# sqliteDBNamePath = sqliteDBNamePath_i


#-----------
# Mapping
#-------------

#examplePolygonTable<-paste(getwd(),"/outputs/Maps/Tables/subReg_origData_byClass_Argentina_subRegType_origDownscaled_hydrobidBermeo3.csv",sep="")

polygonDataTables_i=paste(getwd(),"/outputs/Maps/Tables/subReg_origData_byClass_Colombia_state_origDownscaled_NE.csv",sep="")
a<-read.csv(polygonDataTables_i); head(a); unique(a$scenario); unique(a$param); unique(a$x)
for(param_i in unique(a$param)){print(param_i);print(unique((a%>%dplyr::filter(param==param_i))$x))}
gridDataTables_i=paste(getwd(),"/outputs/Grids/gridCropped_Colombia_state_NE.csv",sep="")
b<-read.csv(gridDataTables_i); head(b); unique(b$scenario); unique(b$param); unique(b$x)
for(param_i in unique(b$param)){print(param_i);print(unique((b%>%dplyr::filter(param==param_i))$x))}
xRange_i= seq(from=2000,to=2020,by=5)
legendPosition_i=c("LEFT","bottom")
legendOutsideSingle_i=T
animateOn_i=T
delay_i=100
scenRef_i="Eg1"
scaleRange_i=data.frame(param=c("griddedScarcity"),
                        maxScale=c(1),
                        minScale=c(0))
paramsSelect_i = c("xanthosRunoff")


boundaryRegShape_i = NULL
boundaryRegShpFolder_i=paste(getwd(),"/dataFiles/gis/naturalEarth",sep="")
boundaryRegShpFile_i=paste("ne_10m_admin_0_countries",sep="")
boundaryRegCol_i="NAME"
boundaryRegionsSelect_i="Colombia"
subRegShape_i = NULL
subRegShpFolder_i = paste(getwd(),"/dataFiles/gis/admin_Colombia",sep = "")
subRegShpFile_i = paste("colombiaNE1",sep= "")
subRegCol_i = "name"
subRegType_i = "state"
nameAppend_i = "_NE"

metis.mapProcess(polygonDataTables=polygonDataTables_i,
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
                 indvScenarios=F,
                 GCMRCPSSPPol=T,
                 multiFacetCols="scenarioRCP",
                 multiFacetRows="scenarioGCM",
                 legendOutsideMulti=NULL,
                 legendPositionMulti=NULL,
                 legendTitleSizeMulti=NULL,
                 legendTextSizeAnim=NULL,
                 legendTextSizeMulti=NULL,
                 refGCM="gfdl-esm2m",
                 refRCP="rcp2p6",
                 chosenRefMeanYears=c(2000:2020))


polygonDataTables_i=paste(getwd(),"/outputs/Maps/Tables/subReg_origData_byClass_Colombia_subBasin_origDownscaled_local.csv",sep="")
a<-read.csv(polygonDataTables_i); head(a); unique(a$scenario); unique(a$param); unique(a$x)
for(param_i in unique(a$param)){print(param_i);print(unique((a%>%dplyr::filter(param==param_i))$x))}
gridDataTables_i=paste(getwd(),"/outputs/Grids/gridCropped_Colombia_subBasin_local.csv",sep="")
b<-read.csv(gridDataTables_i); head(b); unique(b$scenario); unique(b$param); unique(b$x)
for(param_i in unique(b$param)){print(param_i);print(unique((b%>%dplyr::filter(param==param_i))$x))}
xRange_i= seq(from=2000,to=2020,by=5)
legendPosition_i=c("LEFT","bottom")
legendOutsideSingle_i=T
animateOn_i=T
delay_i=100
scenRef_i="Eg1"
scaleRange_i=data.frame(param=c("griddedScarcity"),
                        maxScale=c(1),
                        minScale=c(0))
paramsSelect_i = c("griddedScarcity")


boundaryRegShape_i = NULL
boundaryRegShpFolder_i=paste(getwd(),"/dataFiles/gis/naturalEarth",sep="")
boundaryRegShpFile_i=paste("ne_10m_admin_0_countries",sep="")
boundaryRegCol_i="NAME"
boundaryRegionsSelect_i="Colombia"
subRegShape_i = NULL
subRegShpFolder_i = paste(getwd(),"/dataFiles/gis/admin_Colombia",sep = "")
subRegShpFile_i = paste("colombiaLocalBasin",sep= "")
subRegCol_i = "NOM_ZH"
subRegType_i = "subBasin"
nameAppend_i = "_local"

c1<-readOGR(dsn=paste(getwd(),"/dataFiles/gis/admin_Colombia",sep = ""),
                     layer="colombiaLocalBasin",use_iconv=T,encoding='UTF-8')


metis.mapProcess(polygonDataTables=polygonDataTables_i,
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
                 indvScenarios=F,
                 GCMRCPSSPPol=T,
                 multiFacetCols="scenarioRCP",
                 multiFacetRows="scenarioGCM",
                 legendOutsideMulti=NULL,
                 legendPositionMulti=NULL,
                 legendTitleSizeMulti=NULL,
                 legendTextSizeAnim=NULL,
                 legendTextSizeMulti=NULL,
                 refGCM="gfdl-esm2m",
                 refRCP="rcp2p6",
                 chosenRefMeanYears=c(2000:2020))

