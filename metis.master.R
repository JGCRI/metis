

#----------------------------
# Install The metis Package
#----------------------------
if("devtools" %in% rownames(installed.packages()) == F){install.packages("devtools")}
library(devtools)
if("metis" %in% rownames(installed.packages()) == F){install_github(repo="zarrarkhan/metis")}
library(metis)
if("rgcam" %in% rownames(installed.packages()) == F){install_github(repo="JGCRI/rgcam")}
library(rgcam)
library(tibble)
library(dplyr)


#----------------------------
# Read GCAM Data
#---------------------------

# ?metis.readgcam # For more help

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
                          xCompare=c("2015","2035","2050","2100"), # Default is c("2015","2030","2050","2100")
                          scenRef="Eg1", # Default is NULL
                          dirOutputs=paste(getwd(),"/outputs",sep=""), # Default is paste(getwd(),"/outputs",sep="")
                          pdfpng="png", # Default is "png"
                          regionCompareOnly=0 # Default is "0"
                          )


#-----------
# Grid to Poly
#-------------

# Grid to Shape
gridMetisData<-paste(getwd(),"/outputs/Grids/gridMetis.RData",sep="")
load(gridMetisData) # grid is called gridMetis
grid<-gridMetis

grid<-paste(getwd(),"/dataFiles/examples/example_grid_ArgentinaBermejo3_Eg1Eg2.csv",sep="")


# Shape FIle
examplePolyFolder<-paste(getwd(),"/dataFiles/examples",sep="")
examplePolyFile<-paste("bermejo3Cropped",sep="")

# View and Test SHape File
library(rgdal)
bermejo3Cropped=readOGR(dsn=examplePolyFolder,
                layer=examplePolyFile,use_iconv=T,encoding='UTF-8')
metis.map(dataPolygon=bermejo3Cropped,fileName = paste("X"),dirOutputs = getwd(),
          fillColumn = "SUB_NAME",labels=T ,printFig=F)

polyBermeo3Cropped<-metis.grid2poly(grid=gridMetis,
                                    boundaryRegShape=NULL,
                                    boundaryRegShpFolder=NULL,
                                    boundaryRegShpFile=NULL,
                                    boundaryRegCol=NULL,
                                    boundaryRegionsSelect="Argentina",
                                    subRegShape=NULL,
                                    subRegShpFolder=examplePolyFolder,
                                    subRegShpFile=examplePolyFile,
                                    subRegCol="SUB_NAME",
                                    subRegionsSelect=NULL,
                                    subRegType="subBasin",
                                    dirOutputs=paste(getwd(),"/outputs",sep=""),
                                    nameAppend="_hydrobidBermeo3",
                                    expandbboxPercent=2,
                                    extension=F,
                                    gcamBasinShpFolder=paste(getwd(),"/dataFiles/gis/basin_gcam",sep=""),
                                    gcamBasinShpFile ="Global235_CLM_final_5arcmin_multipart")

grid=gridMetis
boundaryRegShape=NULL
boundaryRegShpFolder=NULL
boundaryRegShpFile=NULL
boundaryRegCol=NULL
boundaryRegionsSelect="Argentina"
subRegShape=NULL
subRegShpFolder=examplePolyFolder
subRegShpFile=examplePolyFile
subRegCol="SUB_NAME"
subRegionsSelect=NULL
subRegType="subBasin"
dirOutputs=paste(getwd(),"/outputs",sep="")
nameAppend="_hydrobidBermeo3"
expandbboxPercent=2
extension=F

#-----------
# Mapping
#-------------

exampleGridTable<-paste(getwd(),"/dataFiles/examples/example_grid_ArgentinaBermejo3_Eg1Eg2.csv",sep="")
examplePolygonTable<-paste(getwd(),"/dataFiles/examples/example_poly_ArgentinaBermejo3_Eg1Eg2.csv",sep="")


metis.mapProcess(polygonDataTables=examplePolygonTable,
                 gridDataTables=exampleGridTable,
                 dirOutputs=paste(getwd(),"/outputs",sep=""),
                 xRange=c(2005,2010,2020),
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
                 legendPosition=c("RIGHT","top"),
                 rasterCoverNegShape=T,
                 legendFixedBreaks=5,
                 animateOn=T,
                 delay=100,
                 legendTitleSize=1,
                 scenRef="Eg1")

