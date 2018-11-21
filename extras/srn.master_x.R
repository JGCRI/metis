

#----------------------------
# Install The SRN Package
#----------------------------
if("devtools" %in% rownames(installed.packages()) == F){install.packages("devtools")}
library(devtools)
install_github(repo="zarrarkhan/srn",force=T)
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
               "watConsumBySec", "watWithdrawBySec", "watWithdrawByCrop", "watBioPhysCons",
               "gdpPerCapita", "gdp", "gdpGrowthRate", "pop",
               "agProdbyIrrRfd","agProdByCrop",
               "landIrrRfd", "aggLandAlloc","co2emissionByEndUse", "ghgEmissionByGHG", "ghgEmissByGHGGROUPS")

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
dataTables<-c(paste(getwd(),"/outputs/Tables/Tables_Local/local_Regional_Colombia.csv",sep=""),
              paste(getwd(),"/outputs/Tables/Tables_Local/local_Regional_Argentina.csv",sep=""),
              paste(getwd(),"/outputs/Tables/Tables_Local/local_Regional_Uruguay.csv",sep=""))  # Need to create this before loading

# Read in the data from the function srn.readgcam
rTable <- dataGCAM;

# Choose Parameters or set to "All" for all params. For complete list see ?srn.chartsProcess
# paramsSelect=c("finalNrgbySec", "primNrgConsumByFuel", "elecByTech",
#                "watConsumBySec", "watWithdrawBySec", "watWithdrawByCrop", "watBioPhysCons",
#                "gdpPerCapita", "gdp", "gdpGrowthRate", "pop",
#                "agProdbyIrrRfd","agProdByCrop",
#                "landIrrRfd", "aggLandAlloc","co2emissionByEndUse", "ghgEmissionByGHG", "ghgEmissByGHGGROUPS")

paramsSelect=c("elecByTech")

regionsSelect=c("Argentina","Colombia","Uruguay")

charts<-srn.chartsProcess(rTable=rTable, # Default is NULL
                          dataTables=dataTables, # Default is NULL
                          paramsSelect=paramsSelect, # Default is "All"
                          regionsSelect=regionsSelect, # Default is "All"
                          xCompare=c("2015","2035","2050","2100"), # Default is c("2015","2030","2050","2100")
                          scenRef="GCAMModified", # Default is NULL
                          dirOutputs=paste(getwd(),"/outputs",sep=""), # Default is paste(getwd(),"/outputs",sep="")
                          pdfpng="png", # Default is "png"
                          regionCompareOnly=0, # Default is "0"
                          useNewLabels=1
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

#------------------------
# Maps
#------------------------

# Get Base Shapefiles



