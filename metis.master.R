

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


#----------------------------
# Read GCAM Data
#---------------------------

gcamdatabasePath <-paste(getwd(),"/dataFiles/gcam",sep="")
gcamdatabaseName <-"example_database_basexdb"
gcamdataProjFile <-"Example_dataProj.proj"
regionsSelect <- c("Colombia","Argentina")
# Choose Parameters or set to "All" for all params. For complete list see ?metis.readgcam
paramsSelect=c("finalNrgbySec", "primNrgConsumByFuel", "elecByTech",
               "watConsumBySec", "watWithdrawBySec","gdp", "gdpGrowthRate", "pop",
               "agProdByCrop", "aggLandAlloc","co2emissionByEndUse")

# Use function localDBConn from package rgcam to get a list of scenarios if needed.
# localDBConn(gcamdatabasePath,gcamdatabaseName)
# dataProjLoaded <- loadProject(paste(gcamdatabasePath, "/", dataProj, sep = ""))
#  listScenarios(dataProjLoaded)  # List of Scenarios in GCAM database
# queries <- listQueries(dataProjLoaded)  # List of Queries in queryxml


dataGCAM<-metis.readgcam(reReadData=F, # Default Value is T
                       dataProj=gcamdataProjFile, # Default Value is "dataProj.proj"
                       scenOrigNames=c("ExampleScen1","ExampleScen2"),
                       scenNewNames=c("Eg1","Eg2"),
                       gcamdatabasePath=gcamdatabasePath,
                       gcamdatabaseName=gcamdatabaseName,
                       queryxml="metisQueries.xml",  # Default Value is "metisQueries.xml"
                       dirOutputs= paste(getwd(),"/outputs",sep=""), # Default Value is paste(getwd(),"/outputs",sep="")
                       regionsSelect=regionsSelect, # Default Value is NULL
                       paramsSelect=paramsSelect, # Default value is "All"
                       queriesSelect="All" # Default is "All"
                       )

dataGCAM$data # To view the data read that was read.


#----------------------------
# Produce Data Charts
#---------------------------

# Read in Tables (If exist)
# To test can create a copy of the template in ./readGCAMTables/Tables_Templates/template_Regional_Argentina.csv
# in ./readGCAMTables/Tables_Local/ and rename the file something like "local_Regional_Argentina.csv.
# dataTables<-c(paste(getwd(),"/outputs/readGCAMTables/Tables_Local/local_Regional_Argentina.csv",sep=""))  # Need to create this before loading

# Read in the data from the function metis.readgcam
rTable <- dataGCAM$data;

# Choose Parameters or set to "All" for all params. For complete list see ?metis.chartsProcess
paramsSelect=c("elecByTech","gdp")


regionsSelect=c("Argentina","Colombia")

charts<-metis.chartsProcess(rTable=rTable, # Default is NULL
                          #dataTables=dataTables, # Default is NULL
                          paramsSelect=paramsSelect, # Default is "All"
                          regionsSelect=regionsSelect, # Default is "All"
                          xCompare=c("2015","2030","2050","2100"), # Default is c("2015","2030","2050","2100")
                          scenRef="Eg1", # Default is NULL
                          dirOutputs=paste(getwd(),"/outputs",sep=""), # Default is paste(getwd(),"/outputs",sep="")
                          pdfpng="png", # Default is "png"
                          regionCompareOnly=1 # Default is "0"
                          )


#-----------
# Boundaries
#-------------

# Example Shape File
examplePolyFolder<-paste(getwd(),"/dataFiles/examples",sep="")
examplePolyFile<-paste("bermejo3Cropped",sep="")
bermejo3Cropped=readOGR(dsn=examplePolyFolder,
                        layer=examplePolyFile,use_iconv=T,encoding='UTF-8')
head(bermejo3Cropped@data)
metis.map(dataPolygon=bermejo3Cropped,fillColumn = "SUB_NAME",labels=T ,printFig=F,facetsON=F)


bermejoBoundaries<- metis.boundaries(
                            boundaryRegShape=NULL,
                            boundaryRegShpFolder=paste(getwd(),"/dataFiles/gis/naturalEarth",sep=""),
                            boundaryRegShpFile=paste("ne_10m_admin_0_countries",sep=""),
                            boundaryRegCol="NAME",
                            boundaryRegionsSelect="Argentina",
                            subRegShape=bermejo3Cropped,
                            subRegCol="SUB_NAME",
                            subRegType="subRegType",
                            nameAppend="_test",
                            expandPercent=2,
                            overlapShpFile="Global235_CLM_final_5arcmin_multipart",
                            overlapShpFolder=paste(getwd(),"/dataFiles/gis/basin_gcam",sep=""),
                            extension = T,
                            grids = c(paste(getwd(),"/dataFiles/grids/emptyGrids/grid_025.csv",sep=""),
                                      paste(getwd(),"/dataFiles/grids/emptyGrids/grid_050.csv",sep=""))
                            )

#-----------
# Grid to Poly
#-------------

# Example Grid File
gridExample<-paste(getwd(),"/dataFiles/examples/example_grid_ArgentinaBermejo3_Eg1Eg2.csv",sep="")

# Run metis.grid2poly
polyBermeo3Cropped<-metis.grid2poly(grid=gridExample,
                                    boundaryRegionsSelect="Argentina",
                                    subRegShpFolder=examplePolyFolder,
                                    subRegShpFile=examplePolyFile,
                                    subRegCol="SUB_NAME",
                                    #aggType=NULL,
                                    nameAppend="_hydrobidBermeo3")

#-----------
# Mapping
#-------------

exampleGridTable<-paste(getwd(),"/dataFiles/examples/example_grid_ArgentinaBermejo3_Eg1Eg2.csv",sep="")
examplePolygonTable<-paste(getwd(),"/dataFiles/examples/example_poly_ArgentinaBermejo3_Eg1Eg2.csv",sep="")


metis.mapProcess(polygonDataTables=examplePolygonTable,
                 gridDataTables=exampleGridTable,
                 xRange=c(2005,2010,2020),
                 boundaryRegionsSelect="Argentina",
                 subRegShape=NULL,
                 subRegShpFolder=paste(getwd(),"/dataFiles/gis/subbasin_hydrobid",sep=""),
                 subRegShpFile=paste("bermejo3Cropped",sep=""),
                 subRegCol="SUB_NAME",
                 subRegType="subBasin",
                 nameAppend="_hydrobid",
                 legendPosition=c("RIGHT","top"),
                 animateOn=T,
                 delay=100,
                 scenRef="Eg1"
                 )
