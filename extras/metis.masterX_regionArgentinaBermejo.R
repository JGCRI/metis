
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
gcamdataProjFile_i = "LAC_dataProj.proj" # Default Value is "dataProj.proj"
scenOrigNames_i = c("GCAMOrig","GCAMModified")
scenNewNames_i = c("GCAMOrig","GCAMModified")
gcamregionsSelect_i = c("Colombia","Argentina") # Default Value is NULL

#dataProjLoaded <- loadProject(paste(gcamdatabasePath_i, "/", gcamdataProjFile_i, sep = ""))
#listScenarios(dataProjLoaded)  # List of Scenarios in GCAM database
#queries <- listQueries(dataProjLoaded)  # List of Queries in queryxml


dataGCAM_LAC<-metis.readgcam(reReadData=reReadData_i, # Default Value is T
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

gcamdatabasePath_i <-paste(getwd(),"/dataFiles/gcam",sep="")
gcamdatabaseName_i <-NULL
gcamdataProjFile_i <-"Uruguay_dataProj.proj"
scenOrigNames_i = c("GCAMOrig","GCAMModified")
scenNewNames_i = c("GCAMOrig","GCAMModified")
gcamregionsSelect_i = c("Uruguay") # Default Value is NULL

dataGCAM_Uruguay<-metis.readgcam(reReadData=reReadData_i, # Default Value is T
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

dataGCAM<-bind_rows(dataGCAM_LAC$data,dataGCAM_Uruguay$data)
dataGCAM # To view the data read that was read.
unique((dataGCAM%>%filter(value>0))$param)
unique((dataGCAM%>%filter(value>0))$scenario)


#----------------------------
# Produce Data Charts
#---------------------------

# local data tables
# dataTables<-c(paste(getwd(),"/outputs/readGCAMTables/Tables_Local/local_Regional_Argentina.csv",sep=""))  # Need to create this before loading
#a<-read.csv(dataTables)

scenRef_i="GCAMOrig"
dataTables_i=NULL # Default is NULL
chartregionsSelect_i= "All" # Default is "All"
chartparamsSelect_i= "All" # Default is "All"
xRange_i="All"
xCompare_i= c("2015","2030","2050","2100")
regionCompareOnly_i=1


charts<-metis.chartsProcess(rTable=dataGCAM, # Default is NULL
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

#-----------
# Boundaries
#-------------

# Boundary/Sub-region Shape Choice
#---------------------------------

# Explore Shape Files
#examplePolyFolder<-paste(getwd(),"/dataFiles/examples",sep="")
#examplePolyFile<-paste("bermejo3Cropped",sep="")
#bermejo3Cropped=readOGR(dsn=examplePolyFolder,layer=examplePolyFile,use_iconv=T,encoding='UTF-8')
#head(bermejo3Cropped@data)
#metis.map(dataPolygon=bermejo3Cropped,fillColumn = "SUB_NAME",labels=T ,printFig=F,facetsON=F)

boundaryRegShape_i = NULL
boundaryRegShpFolder_i = paste(getwd(),"/dataFiles/gis/naturalEarth",sep = "")
boundaryRegShpFile_i = paste("ne_10m_admin_0_countries",sep= "")
boundaryRegCol_i = "NAME"
boundaryRegionsSelect_i = "Argentina"
subRegShape_i = NULL
subRegShpFolder_i = paste(getwd(),"/dataFiles/examples",sep="")
subRegShpFile_i = paste("bermejo3Cropped",sep="")
subRegCol_i = "SUB_NAME"
subRegType_i = "subBasin"
nameAppend_i = "_test"
expandPercent_i = 2
overlapShpFile_i = "Global235_CLM_final_5arcmin_multipart"
overlapShpFolder_i = paste(getwd(),"/dataFiles/gis/basin_gcam",sep= "")
extension_i =  T


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
            paste(getwd(),"/dataFiles/grids/emptyGrids/grid_050.csv",sep=""))
)

#-----------
# Grid to Poly
#-------------

grid_i<-paste(getwd(),"/dataFiles/examples/example_grid_ArgentinaBermejo3_Eg1Eg2.csv",sep="")


grid2polyX<-metis.grid2poly(grid=grid_i,
                                    boundaryRegionsSelect=boundaryRegionsSelect_i,
                                    subRegShape=subRegShape_i,
                                    subRegShpFolder=subRegShpFolder_i,
                                    subRegShpFile=subRegShpFile_i,
                                    subRegCol=subRegCol_i,
                                    aggType=aggType_i,
                                    nameAppend=nameAppend_i)

#-----------
# Mapping
#-------------

#examplePolygonTable<-paste(getwd(),"/outputs/Maps/Tables/subReg_origData_byClass_Argentina_subRegType_origDownscaled_hydrobidBermeo3.csv",sep="")

polygonDataTables_i=paste(getwd(),"/outputs/Maps/Tables/subReg_origData_byClass_Argentina_subRegType_origDownscaled_hydrobidBermeo3.csv",sep="")
gridDataTables_i=paste(getwd(),"/dataFiles/examples/example_grid_ArgentinaBermejo3_Eg1Eg2.csv",sep="")
xRange_i=c(2005,2010,2020)
legendPosition_i=c("RIGHT","top")
animateOn_i=T
delay_i=100

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
                 scenRef=scenRef_i
)
