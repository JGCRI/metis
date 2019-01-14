
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
gcamregionsSelect_i = c("India") # Default Value is NULL

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
indiaGCAMBasin<-raster::crop(indiaGCAMBasin,indiaLocal0)
head(indiaGCAMBasin@data)
plot(indiaGCAMBasin)
writeOGR(obj=indiaGCAMBasin, dsn=paste(getwd(),"/dataFiles/gis/admin_India",sep=""), layer=paste("indiaGCAMBasin",sep=""), driver="ESRI Shapefile", overwrite_layer=TRUE)
metis.map(dataPolygon=indiaGCAMBasin,fillColumn = "basin_name",printFig=F)


#-----------
# Boundaries
#-------------

# Explore Shape Files
examplePolyFolder<-paste(getwd(),"/dataFiles/gis/admin_India",sep = "")
examplePolyFile<-paste("indiaGCAMBasin",sep= "")
example=readOGR(dsn=examplePolyFolder,layer=examplePolyFile,use_iconv=T,encoding='UTF-8')
head(example@data)
metis.map(dataPolygon=example,fillColumn = "basin_name",labels=T ,printFig=F,facetsON=F)
boundaryRegShape_i = NULL
boundaryRegShpFolder_i=paste(getwd(),"/dataFiles/gis/naturalEarth",sep="")
boundaryRegShpFile_i=paste("ne_10m_admin_0_countries",sep="")
boundaryRegCol_i="NAME"
boundaryRegionsSelect_i="India"
subRegShape_i = NULL
subRegShpFolder_i = paste(getwd(),"/dataFiles/gis/admin_India",sep = "")
subRegShpFile_i = paste("indiaLocal1",sep= "")
subRegCol_i = "NAME_1"
subRegType_i = "state"
nameAppend_i = "_indiaLocal"
expandPercent_i = 5
overlapShpFile_i = "Global235_CLM_final_5arcmin_multipart"
overlapShpFolder_i = paste(getwd(),"/dataFiles/gis/basin_gcam",sep= "")
extension_i =  T
cropSubShape2Bound_i = F


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

subRegShpFile_i = paste("indiaGCAMBasin",sep= "")
subRegCol_i = "basin_name"
subRegType_i = "basin"
nameAppend_i = "_indiaLocal"

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
reReadData=0
demeterFolder=paste(getwd(),"/dataFiles/grids/demeter/",sep="")
demeterScenario="Eg1"
demeterUnits="Landuse (Fraction)"
demeterTimesteps<-seq(from=2005,to=2020,by=5)
tethysFolder=paste(getwd(),"/dataFiles/grids/tethys/",sep="")
tethysScenario="Eg1"
tethysFiles=c("wddom","wdelec","wdirr","wdliv","wdmfg","wdmin","wdnonag","wdtotal")
tethysUnits="Water Withdrawals (mm)"
xanthosFolder=paste(getwd(),"/dataFiles/grids/xanthos/",sep="")
xanthosScenario="Eg1"
xanthosUnits="Runoff (mm)"
xanthosFiles=c("q_mmperyear_Reference")
xanthosCoordinatesPath=paste(getwd(),"/dataFiles/grids/xanthosCoords/coordinates.csv",sep="")
scarcityXanthosRollMeanWindow=10
popFolder<-paste(getwd(),"/dataFiles/grids/griddedIDsPop/",sep="")
popFiles<-"grid_pop_map"
popUnits<-"person"
gridMetisData=paste(dirOutputs, "/Grids/gridMetis.RData", sep = "")

gridMetis<-metis.prepGrid(
  demeterFolder=demeterFolder,
  demeterScenario=demeterScenario,
  demeterTimesteps=demeterTimesteps,
  demeterUnits=demeterUnits,
  tethysFolder=tethysFolder,
  tethysScenario=tethysScenario,
  tethysFiles=tethysFiles,
  tethysUnits=tethysUnits,
  xanthosFolder=xanthosFolder,
  xanthosScenario=xanthosScenario,
  xanthosUnits=xanthosUnits,
  xanthosFiles=xanthosFiles,
  xanthosCoordinatesPath=xanthosCoordinatesPath,
  scarcityXanthosRollMeanWindow=scarcityXanthosRollMeanWindow,
  dirOutputs=paste(getwd(),"/outputs",sep=""),
  reReadData=reReadData,
  gridMetisData=gridMetisData)

head(gridMetis)

#-----------
# Grid to Poly
#-------------

#grid_i<-paste(getwd(),"/dataFiles/examples/example_grid_ArgentinaBermejo3_Eg1Eg2.csv",sep="")
#grid_i<-paste(getwd(),"/outputs/Grids/gridMetis.csv",sep="")
grid_i=gridMetis

boundaryRegionsSelect_i="India"
subRegShpFolder_i = paste(getwd(),"/dataFiles/gis/admin_India",sep = "")
subRegShpFile_i = paste("indiaLocal1",sep= "")
subRegCol_i = "NAME_1"
subRegType_i = "state"
nameAppend_i = "_indiaLocal"
aggType_i = NULL
paramsSelect_i= "All" #"demeterLandUse"

grid2polyX<-metis.grid2poly(grid=grid_i,
                                    boundaryRegionsSelect=boundaryRegionsSelect_i,
                                    subRegShpFolder=subRegShpFolder_i,
                                    subRegShpFile=subRegShpFile_i,
                                    subRegCol=subRegCol_i,
                                    subRegType = subRegType_i,
                                    aggType=aggType_i,
                                    nameAppend=nameAppend_i,
                                    paramsSelect = paramsSelect_i)

# grid=grid_i
# boundaryRegionsSelect=boundaryRegionsSelect_i
# subRegShpFolder=subRegShpFolder_i
# subRegShpFile=subRegShpFile_i
# subRegCol=subRegCol_i
# aggType=aggType_i
# nameAppend=nameAppend_i

#-----------
# Mapping
#-------------

#examplePolygonTable<-paste(getwd(),"/outputs/Maps/Tables/subReg_origData_byClass_Argentina_subRegType_origDownscaled_hydrobidBermeo3.csv",sep="")

polygonDataTables_i=paste(getwd(),"/outputs/Maps/Tables/subReg_origData_byClass_India_state_origDownscaled_indiaLocal.csv",sep="")
a<-read.csv(polygonDataTables_i); head(a); unique(a$scenario); unique(a$param); unique(a$x)
gridDataTables_i=paste(getwd(),"/outputs/Grids/gridCropped_India_state_indiaLocal.csv",sep="")
xRange_i= c(2005,2010,2020,2030,2050)
legendPosition_i=c("RIGHT","top")
animateOn_i=T
delay_i=100
scenRef_i="Eg1"

boundaryRegShape_i = NULL
boundaryRegShpFolder_i=paste(getwd(),"/dataFiles/gis/naturalEarth",sep="")
boundaryRegShpFile_i=paste("ne_10m_admin_0_countries",sep="")
boundaryRegCol_i="NAME"
boundaryRegionsSelect_i="India"
subRegShape_i = NULL
subRegShpFolder_i = paste(getwd(),"/dataFiles/gis/admin_India",sep = "")
subRegShpFile_i = paste("indiaLocal1",sep= "")
subRegCol_i = "NAME_1"
subRegType_i = "state"
nameAppend_i = "_indiaLocal"

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
                 legendPosition=legendPosition_i,
                 animateOn=animateOn_i,
                 delay=delay_i,
                 scenRef=scenRef_i,
                 extension=T,
                 expandPercent = 6
                 )




