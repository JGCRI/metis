

#----------------------------
# Install The SRN Package
#----------------------------
if("devtools" %in% rownames(installed.packages()) == F){install.packages("devtools")}
library(devtools)
if("srn" %in% rownames(installed.packages()) == F){install_github(repo="zarrarkhan/srn")}
library(srn)
if("rgcam" %in% rownames(installed.packages()) == F){install_github(repo="JGCRI/rgcam")}
library(rgcam)
library(tibble)
library(dplyr)


#----------------------------
# Tests
#----------------------------
a<-srn.colors()
testcolor<-a$pal_elec_tech_colors
pie(rep(1,length(testcolor)),label=names(testcolor),col=a$testcolor)

#----------------------------
# Read GCAM Data
#---------------------------

# ?srn.readgcam # For more help

# Choose Parameters or set to "All" for all params. For complete list see ?srn.readgcam
paramsSelect=c("finalNrgbySec", "primNrgConsumByFuel", "elecByTech",
               "watConsumBySec", "watWithdrawBySec", "watWithdrawByCrop",
               "gdpPerCapita", "gdp", "gdpGrowthRate", "pop",
               "agProdbyIrrRfd","agProdByCrop",
               "landIrrRfd", "aggLandAlloc","co2emissionByEndUse", "ghgEmissByGHGGROUPS")

gcamdatabasePath <-paste(getwd(),"/dataFiles/gcam",sep="")
gcamdatabaseName <-"database_basexdb_LAC"
gcamdataProjFile <-"LAC_dataProj.proj"
regionsSelect <- c("Colombia","Argentina")

# Use function localDBConn from package rgcam to get a list of scenarios if needed.
# localDBConn(gcamdatabasePath,gcamdatabaseName)
# dataProjLoaded <- loadProject(paste(gcamdatabasePath, "/", dataProj, sep = ""))
#  listScenarios(dataProjLoaded)  # List of Scenarios in GCAM database
# queries <- listQueries(dataProjLoaded)  # List of Queries in queryxml


dataGCAM_LAC<-srn.readgcam(reReadData=F, # Default Value is T
                       dataProj=gcamdataProjFile, # Default Value is "dataProj.proj"
                       scenOrigNames=c("GCAMOrig","GCAMModified"),
                       scenNewNames=c("GCAMOrig","GCAMModified"),
                       gcamdatabasePath=gcamdatabasePath,
                       gcamdatabaseName=gcamdatabaseName,
                       queryxml="srnQueries.xml",  # Default Value is "srnQueries.xml"
                       dirOutputs= paste(getwd(),"/outputs",sep=""), # Default Value is paste(getwd(),"/outputs",sep="")
                       regionsSelect=regionsSelect, # Default Value is NULL
                       paramsSelect=paramsSelect # Default value is "All"
                       )

gcamdatabasePath <-paste(getwd(),"/dataFiles/gcam",sep="")
gcamdatabaseName <-"database_basexdb_Uruguay"
gcamdataProjFile <-"Uruguay_dataProj.proj"
regionsSelect <- c("Uruguay")

#dataProjLoaded <- loadProject(paste(gcamdatabasePath, "/", gcamdataProjFile, sep = ""))
#listScenarios(dataProjLoaded)  # List of Scenarios in GCAM database
#queries <- listQueries(dataProjLoaded)  # List of Queries in queryxml

dataGCAM_Uruguay<-srn.readgcam(reReadData=F, # Default Value is T
                           dataProj=gcamdataProjFile, # Default Value is "dataProj.proj"
                           scenOrigNames=c("GCAMOrig"),
                           scenNewNames=c("GCAMOrig"),
                           gcamdatabasePath=gcamdatabasePath,
                           gcamdatabaseName=gcamdatabaseName,
                           queryxml="srnQueries.xml",  # Default Value is "srnQueries.xml"
                           dirOutputs= paste(getwd(),"/outputs",sep=""), # Default Value is paste(getwd(),"/outputs",sep="")
                           regionsSelect=regionsSelect, # Default Value is NULL
                           paramsSelect=paramsSelect # Default value is "All"
)

dataGCAM<-bind_rows(dataGCAM_LAC$data,dataGCAM_Uruguay$data)
dataGCAM # To view the data read that was read.
unique((dataGCAM%>%filter(value>0))$param)

#----------------------------
# Produce Data Charts
#---------------------------

# ?srn.chartsProcess # For more help on charting process

# Read in Tables (If exist)
#dataTables<-c(paste(getwd(),"/outputs/Tables/Tables_Local/local_Regional_Colombia.csv",sep=""),
#              paste(getwd(),"/outputs/Tables/Tables_Local/local_Regional_Argentina.csv",sep=""),
#              paste(getwd(),"/outputs/Tables/Tables_Local/local_Regional_Uruguay.csv",sep=""))  # Need to create this before loading

# Read in the data from the function srn.readgcam
rTable <- dataGCAM;

# Choose Parameters or set to "All" for all params. For complete list see ?srn.chartsProcess
# paramsSelect=c("finalNrgbySec", "primNrgConsumByFuel", "elecByTech",
#                "watConsumBySec", "watWithdrawBySec", "watWithdrawByCrop", "watBioPhysCons",
#                "gdpPerCapita", "gdp", "gdpGrowthRate", "pop",
#                "agProdbyIrrRfd","agProdByCrop",
#                "landIrrRfd", "aggLandAlloc","co2emissionByEndUse", "ghgEmissionByGHG")

#paramsSelect=c("elecByTech")

regionsSelect=c("Uruguay")

charts<-srn.chartsProcess(rTable=rTable, # Default is NULL
                          #dataTables=dataTables, # Default is NULL
                          paramsSelect=paramsSelect, # Default is "All"
                          regionsSelect=regionsSelect, # Default is "All"
                          xCompare=c("2015","2035","2050","2100"), # Default is c("2015","2030","2050","2100")
                          scenRef="GCAMModified", # Default is NULL
                          dirOutputs=paste(getwd(),"/outputs",sep=""), # Default is paste(getwd(),"/outputs",sep="")
                          pdfpng="png", # Default is "png"
                          regionCompareOnly=0, # Default is "0"
                          useNewLabels=1,sizeBarLines = 0,
                          xRange=c(2010:2050) # Default is All
                          )


rTable=rTable; # Default is NULL
dataTables=dataTables; # Default is NULL
paramsSelect=paramsSelect; # Default is "All"
regionsSelect=regionsSelect; # Default is "All"
xCompare=c("2015","2035","2050","2100"); # Default is c("2015";"2030";"2050";"2100")
scenRef="GCAMModified"; # Default is NULL
dirOutputs=paste(getwd(),"/outputs",sep=""); # Default is paste(getwd();"/outputs";sep="")
pdfpng="png"; # Default is "png"
regionCompareOnly=0 # Default is "0"

xData="x";
yData="value";
xLabel="xLabel";
yLabel="units";
aggregate="sum";class="class"; classPalette="pal_Basic";
useNewLabels=0
xRange=c(2010:2050)


#------------------------
# Prepare Grids
#------------------------

dirOutputs=paste(getwd(),"/outputs",sep="")
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
xanthosFiles=c("q_bced_1960_1999_ipsl-cm5a-lr_1950_2005")
xanthosCoordinatesPath=paste(getwd(),"/dataFiles/grids/xanthosCoords/coordinates.csv",sep="")
scarcityXanthosRollMeanWindow=10

grid<-srn.prepGrid(
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
             reReadData=1,
             gridSRNData=paste(dirOutputs, "/Grids/gridSRN.RData", sep = ""))


# Grid to Shape
gridSRNData<-paste(getwd(),"/outputs/Grids/gridSRN.RData",sep="")
load(gridSRNData) # grid is called gridSRN
grid<-gridSRN

#------------------------
# Prepare Poly Tables
#------------------------

polyIndiaGADM<-srn.grid2poly(grid=grid,
                           boundaryRegShape=NULL,
                           subRegShape=NULL,
                           boundaryRegShpFolder=paste(getwd(),"/dataFiles/gis/admin_gadm36",sep=""),
                           boundaryRegShpFile=paste("gadm36_0",sep=""),
                           boundaryRegCol="NAME_0",
                           boundaryRegionsSelect="India",
                           subRegShpFolder=paste(getwd(),"/dataFiles/gis/admin_gadm36",sep=""),
                           subRegShpFile=paste("gadm36_1",sep=""),
                           subRegCol="NAME_1",
                           subRegionsSelect=NULL,
                           subRegType="State",
                           aggType="depth",
                           dirOutputs=paste(getwd(),"/outputs",sep=""),
                           nameAppend="_gadm",
                           expandbboxPercent=2,
                           extension=T)


polyIndiaLocal<-srn.grid2poly(grid=grid,
                           boundaryRegShape=NULL,
                           subRegShape=NULL,
                           boundaryRegShpFolder=paste(getwd(),"/dataFiles/gis/admin_India",sep=""),
                           boundaryRegShpFile=paste("IND_adm1",sep=""),
                           boundaryRegCol="NAME_0",
                           boundaryRegionsSelect="India",
                           subRegShpFolder=NULL,
                           subRegShpFile=NULL,
                           subRegCol="NAME_1",
                           subRegionsSelect=NULL,
                           subRegType="State",
                           aggType="depth",
                           dirOutputs=paste(getwd(),"/outputs",sep=""),
                           nameAppend="_local",
                           expandbboxPercent=2,
                           extension=F)


library(rgdal)

argentina=readOGR(dsn=paste(getwd(),"/dataFiles/gis/admin_gadm36",sep=""),
                  layer="gadm36_0",use_iconv=T,encoding='UTF-8')
argentina<-argentina[which(argentina[["NAME_0"]] %in% "Argentina"),]
catchment=readOGR(dsn=paste(getwd(),"/dataFiles/gis/subbasin_hydrobid",sep=""),
                         layer="Catchment",use_iconv=T,encoding='UTF-8')
bermejo3=readOGR(dsn=paste(getwd(),"/dataFiles/gis/subbasin_hydrobid",sep=""),
                  layer=paste("Bemeo_3Subains",sep=""),use_iconv=T,encoding='UTF-8')
bermejo1<-bermejo3[which(bermejo3[["SUB_NAME"]] %in% c("Bermejo 1")),]
bermejo3Cropped= raster::intersect(bermejo3,argentina)
writeOGR(obj=bermejo3Cropped, dsn=paste(getwd(),"/dataFiles/gis/subbasin_hydrobid",sep=""), layer=paste("bermejo3Cropped",sep=""), driver="ESRI Shapefile", overwrite_layer=TRUE)

bermejo<-raster::aggregate(bermejo3, by= "MAJ_BAS")
writeOGR(obj=bermejo, dsn=paste(getwd(),"/dataFiles/gis/subbasin_hydrobid",sep=""), layer=paste("bermejo",sep=""), driver="ESRI Shapefile", overwrite_layer=TRUE)
catchmentBermejo <- raster::intersect(catchment,bermejo)
writeOGR(obj=catchmentBermejo, dsn=paste(getwd(),"/dataFiles/gis/subbasin_hydrobid",sep=""), layer=paste("catchmentBermejo",sep=""), driver="ESRI Shapefile", overwrite_layer=TRUE)
catchmentBermejo1 <- raster::intersect(catchment,bermejo1)
catchmentBermejo1@data<-catchmentBermejo1@data%>%
  mutate(random=runif(nrow(catchmentBermejo1@data)))
writeOGR(obj=catchmentBermejo1, dsn=paste(getwd(),"/dataFiles/gis/subbasin_hydrobid",sep=""), layer=paste("catchmentBermejo1",sep=""), driver="ESRI Shapefile", overwrite_layer=TRUE)

bermejo=readOGR(dsn=paste(getwd(),"/dataFiles/gis/subbasin_hydrobid",sep=""),
                 layer=paste("bermejo",sep=""),use_iconv=T,encoding='UTF-8')

catchmentBermejo1=readOGR(dsn=paste(getwd(),"/dataFiles/gis/subbasin_hydrobid",sep=""),
                  layer="catchmentBermejo1",use_iconv=T,encoding='UTF-8')

srn.map(dataPolygon=catchmentBermejo1,mapName = paste("X"),dirOutputs = dir,
        fillColumn = "random",printMap=F)
srn.map(dataPolygon=bermejo3Cropped,mapName = paste("X"),dirOutputs = dir,
        fillColumn = "SUB_NAME",printMap=F)


# Grid to Shape
gridSRNData<-paste(getwd(),"/outputs/Grids/gridSRN.RData",sep="")
load(gridSRNData) # grid is called gridSRN
grid<-gridSRN

grid<-grid%>%filter(!class %in% c("Non Agriculture","Total"))

polyBermeo3Cropped<-srn.grid2poly(grid=grid,
                           boundaryRegShape=NULL,
                           boundaryRegShpFolder=paste(getwd(),"/dataFiles/gis/admin_gadm36",sep=""),
                           boundaryRegShpFile=paste("gadm36_0",sep=""),
                           boundaryRegCol="NAME_0",
                           boundaryRegionsSelect="Argentina",
                           subRegShape=NULL,
                           subRegShpFolder=paste(getwd(),"/dataFiles/gis/subbasin_hydrobid",sep=""),
                           subRegShpFile=paste("bermejo3Cropped",sep=""),
                           subRegCol="SUB_NAME",
                           subRegionsSelect=NULL,
                           subRegType="subBasin",
                           dirOutputs=paste(getwd(),"/outputs",sep=""),
                           nameAppend="_hydrobid",
                           expandbboxPercent=2,
                           extension=F)

# grid=NULL
# boundaryRegShape=NULL
# boundaryRegShpFolder=paste(getwd(),"/dataFiles/gis/admin_gadm36",sep="")
# boundaryRegShpFile=paste("gadm36_0",sep="")
# boundaryRegCol="NAME_0"
# boundaryRegionsSelect="Argentina"
# subRegShape=NULL
# subRegShpFolder=paste(getwd(),"/dataFiles/gis/subbasin_hydrobid",sep="")
# subRegShpFile=paste("Bemeo_3Subains",sep="")
# subRegCol="SUB_NAME"
# subRegionsSelect=NULL
# subRegType="subBasin"
# dirOutputs=paste(getwd(),"/outputs",sep="")
# nameAppend="_hydrobid"
# expandbboxPercent=2
# extension=F

polyCatchmentBermejo1<-srn.grid2poly(grid=grid,
                                     boundaryRegShape=NULL,
                                     subRegShape=NULL,
                                     boundaryRegShpFolder=paste(getwd(),"/dataFiles/gis/admin_gadm36",sep=""),
                                     boundaryRegShpFile=paste("gadm36_0",sep=""),
                                     boundaryRegCol="NAME_0",
                                     boundaryRegionsSelect="Argentina",
                                     subRegShpFolder=paste(getwd(),"/dataFiles/gis/subbasin_hydrobid",sep=""),
                                     subRegShpFile=paste("catchmentBermejo1",sep=""),
                                     subRegCol="COMID",
                                     subRegionsSelect=NULL,
                                     subRegType="catchment",
                                     dirOutputs=paste(getwd(),"/outputs",sep=""),
                                     nameAppend="_hydrobidCatch",
                                     expandbboxPercent=2,
                                     extension=F)


polyCatchmentBermejo1RANDOM<-srn.grid2poly(grid=NULL,
                                     boundaryRegShape=NULL,
                                     subRegShape=NULL,
                                     boundaryRegShpFolder=paste(getwd(),"/dataFiles/gis/admin_gadm36",sep=""),
                                     boundaryRegShpFile=paste("gadm36_0",sep=""),
                                     boundaryRegCol="NAME_0",
                                     boundaryRegionsSelect="Argentina",
                                     subRegShpFolder=paste(getwd(),"/dataFiles/gis/subbasin_hydrobid",sep=""),
                                     subRegShpFile=paste("catchmentBermejo1",sep=""),
                                     subRegCol="random",
                                     subRegionsSelect=NULL,
                                     subRegType="catchment",
                                     dirOutputs=paste(getwd(),"/outputs",sep=""),
                                     nameAppend="_hydrobidCatchRandom",
                                     expandbboxPercent=2,
                                     extension=F)

#------------------------
# Process Maps
#------------------------

# Grid to Shape
gridSRNData<-paste(getwd(),"/outputs/Grids/gridSRN.RData",sep="")
load(gridSRNData) # grid is called gridSRN
grid<-gridSRN

polygonDataTables=paste(getwd(),"/outputs/Maps/Tables/subReg_origData_byClass_Argentina_subBasin_origDownscaled_hydrobid.csv",sep="")
# polygonDataTables<-utils::read.csv(paste(polygonDataTables), stringsAsFactors = F)%>%tibble::as.tibble()
#polygonDataTables=polyBermeo3

srn.mapProcess(polygonDataTables=polygonDataTables,
               gridDataTables=grid,
               dirOutputs=paste(getwd(),"/outputs",sep=""),
               xRange=c(2010),
               labels=F,
               labelsSize=1.2,
               regionsSelect="Argentina",
               subRegShape=NULL,
               subRegShpFolder=paste(getwd(),"/dataFiles/gis/subbasin_hydrobid",sep=""),
               subRegShpFile=paste("bermejo3Cropped",sep=""),
               subRegCol="SUB_NAME",
               subRegType="subBasin",
               aggType=NULL,
               nameAppend="_hydrobid",
               legendPosition=c("RIGHT","top"))

polygonDataTables=polygonDataTables
gridDataTables=grid
dirOutputs=paste(getwd(),"/outputs",sep="")
xRange="All"
labels=F
labelsSize=1.2
regionsSelect="Argentina"
subRegShape=NULL
subRegShpFolder=paste(getwd(),"/dataFiles/gis/subbasin_hydrobid",sep="")
subRegShpFile=paste("Bemeo_3Subains",sep="")
subRegCol="SUB_NAME"
subRegType="subRegType"
aggType=NULL
nameAppend="_hydrobid"

srn.mapProcess(polygonDataTables=polyCatchmentBermejo1,
               gridDataTables=NULL,
               dirOutputs=paste(getwd(),"/outputs",sep=""),
               xRange=c(2010),
               labels=F,
               labelsSize=1.2,
               regionsSelect="Argentina",
               subRegShpFolder=paste(getwd(),"/dataFiles/gis/subbasin_hydrobid",sep=""),
               subRegShpFile=paste("catchmentBermejo1",sep=""),
               subRegCol="COMID",
               subRegType="catchment",
               aggType=NULL,
               nameAppend="_hydrobidCatch",
               legendPosition=c("RIGHT","top"))

# regionsSelect_i="Argentina";scenario_i="Eg1";
# param_i="demeterLandUse";x_i=2010
# subRegType_i="catchment"
# polygonDataTables=polyCatchmentBermejo1
# gridDataTables=grid
# dirOutputs=paste(getwd(),"/outputs",sep="")
# xRange="All"
# labels=F
# labelsSize=1.2
# regionsSelect="Argentina"
# subRegShape=NULL
# subRegShpFolder=paste(getwd(),"/dataFiles/gis/subbasin_hydrobid",sep="")
# subRegShpFile=paste("catchmentBermejo1",sep="")
# subRegCol="COMID"
# subRegType="catchment"
# aggType=NULL
# nameAppend="_hydrobidCatch"
# legendPosition=c("RIGHT","top")

# Grid to Shape
gridSRNData<-paste(getwd(),"/outputs/Grids/gridSRN.RData",sep="")
load(gridSRNData) # grid is called gridSRN
grid<-gridSRN

polygonDataTables=paste(getwd(),"/outputs/Maps/Tables/subReg_origData_byClass_India_State_origDownscaled_local.csv",sep="")
polygonDataTables<-utils::read.csv(paste(polygonDataTables), stringsAsFactors = F)%>%tibble::as.tibble()
polyIndiaLocal=polygonDataTables

polyIndiaLocal1=polyIndiaLocal%>%filter(param=="xanthosRunoff")%>%mutate(classPalette="pal_hot")
grid1<-grid%>%filter(param=="xanthosRunoff")%>%mutate(classPalette="pal_hot")

srn.mapProcess(polygonDataTables=polyIndiaLocal1,
               gridDataTables=NULL,
               dirOutputs=paste(getwd(),"/outputs",sep=""),
               xRange=c(2000,2005),
               labels=F,
               labelsSize=1.2,
               regionsSelect="India",
               subRegShpFolder=paste(getwd(),"/dataFiles/gis/admin_India",sep=""),
               subRegShpFile=paste("IND_adm1",sep=""),
               subRegCol="NAME_1",
               subRegType="State",
               aggType=NULL,
               nameAppend="_IndiaLocal",
               rasterCoverNegShape=F,
               legendOutsidePosition=NULL,
               legendPosition=c("RIGHT","bottom"))

polygonDataTables=polygonDataTables
gridDataTables=grid
dirOutputs=paste(getwd(),"/outputs",sep="")
xRange=c(2000,2005)
labels=F
labelsSize=1.2
regionsSelect="India"
subRegShape=NULL
subRegShpFolder=paste(getwd(),"/dataFiles/gis/admin_India",sep="")
subRegShpFile=paste("IND_adm1",sep="")
subRegCol="NAME_1"
subRegType="State"
aggType=NULL
rasterCoverNegShape=F
nameAppend="_IndiaLocal"
legendOutsidePosition=NULL
legendPosition=c("RIGHT","bottom")

regionsSelect_i="India";scenario_i="Eg1";
param_i="xanthosRunoff";x_i=2000

#
# #srn.map
# dataPolygon=subRegShape
# dataGrid=mapx
# fillColumn = names(mapx@data)
# legendShow = T
# legendOutside = T
# facetFreeScale = F
# frameShow = T
# labels=labels
# labelsSize = labelsSize
# legendTitle =legendTitle
# legendStyle="kmeans"
# legendFixedBreaks = 10
# legendDigits = legendDigits
# legendOutsidePosition = NULL
# legendPosition = NULL
# fillPalette = fillPalette
# mapName = paste("map_",regionsSelect_i,"_raster_",param_i,"_",x_i,"_",scenario_i,nameAppend,"_KMEANS",sep="")
# dirOutputs = paste(dirOutputs,"/Maps/",regionsSelect_i,"/raster/",scenario_i,"/byYear",sep = "")
#
# library("tmap",quietly = T)
# library("tidyr",quietly = T)
# library("dplyr",quietly = T)
# library("tibble",quietly = T)
# library("rgeos",quietly = T)
