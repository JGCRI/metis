

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
regionsSelect <- c("Uruguay","Argentina")

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

regionsSelect=c("Uruguay","Argentina")

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

demeterFolder<-"C:/Users/khan404/Desktop/srn/dataFiles/grids/demeter/"
demeterScenario<-"Eg1"
demeterTimesteps<-seq(from=2005,to=2100,by=5)

grid<-srn.prepGrid(demeterFolder=demeterFolder,
             demeterScenario=demeterScenario,
             demeterTimesteps=demeterTimesteps,
             dirOutputs=paste(getwd(),"/outputs",sep=""),
             reReadData=1,
             gridSRNData=paste(dirOutputs, "/Grids/gridSRN.RData", sep = ""))


# Grid to Shape

grid=paste(getwd(),"/outputs/Grids/gridSRN.csv",sep="")
shpFolder=paste(getwd(),"/dataFiles/gis/admin_gadm36_1",sep="")
shpFile=paste("gadm36_1",sep="")
shpRegCol="NAME_0"
subReg="NAME_1"
aggType="vol"
regionsSelect=c("Argentina")
dirOutputs=paste(getwd(),"/outputs",sep="")
shapeFilesData=paste(dirOutputs, "/Maps/shapeFiles.RData", sep = "")
gridSRNData=paste(dirOutputs, "/Grids/gridSRN.RData", sep = "")

load(shapeFilesData)
shape<-get("gadm36_1")
shape<-shape[which(shape[[shpRegCol]] %in% regionsSelect),]
shape@data<-shape@data%>%dplyr::select(NAME_1)%>%droplevels

load(gridSRNData) # grid is called gridSRN
grid<-gridSRN

poly <- srn.grid2poly(grid=grid,
                      shape=shape,
                      subReg=subReg,
                      aggType="depth")

