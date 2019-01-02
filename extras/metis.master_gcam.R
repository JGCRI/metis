
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


#----------------------------
# Read GCAM Data
#---------------------------

# ?metis.readgcam # For more help

# Choose Parameters or set to "All" for all params. For complete list see ?metis.readgcam
paramsSelect=c("finalNrgbySec", "primNrgConsumByFuel", "elecByTech",
               "watConsumBySec", "watWithdrawBySec", "watWithdrawByCrop",
               "gdpPerCapita", "gdp", "gdpGrowthRate", "pop",
               "agProdbyIrrRfd","agProdByCrop",
               "landIrrRfd", "aggLandAlloc","co2emissionByEndUse", "ghgEmissByGHGGROUPS")

gcamdatabasePath <-paste(getwd(),"/dataFiles/gcam",sep="")
#gcamdatabaseName <-"database_basexdb_LAC"
gcamdataProjFile <-"LAC_dataProj.proj"
regionsSelect <- c("Colombia","Argentina")
#regionsSelect <- NULL

# Use function localDBConn from package rgcam to get a list of scenarios if needed.
# localDBConn(gcamdatabasePath,gcamdatabaseName)
# dataProjLoaded <- loadProject(paste(gcamdatabasePath, "/", dataProj, sep = ""))
#  listScenarios(dataProjLoaded)  # List of Scenarios in GCAM database
# queries <- listQueries(dataProjLoaded)  # List of Queries in queryxml


dataGCAM_LAC<-metis.readgcam(reReadData=F, # Default Value is T
                             dataProj=gcamdataProjFile, # Default Value is "dataProj.proj"
                             scenOrigNames=c("GCAMOrig","GCAMModified"),
                             scenNewNames=c("GCAMOrig","GCAMModified"),
                             gcamdatabasePath=gcamdatabasePath,
                             gcamdatabaseName=gcamdatabaseName,
                             queryxml="metisQueries.xml",  # Default Value is "metisQueries.xml"
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

dataGCAM_Uruguay<-metis.readgcam(reReadData=F, # Default Value is T
                                 dataProj=gcamdataProjFile, # Default Value is "dataProj.proj"
                                 scenOrigNames=c("GCAMOrig"),
                                 scenNewNames=c("GCAMOrig"),
                                 gcamdatabasePath=gcamdatabasePath,
                                 gcamdatabaseName=gcamdatabaseName,
                                 queryxml="metisQueries.xml",  # Default Value is "metisQueries.xml"
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

# ?metis.chartsProcess # For more help on charting process

# Read in Tables (If exist)
dataTables<-c(
            #  paste(getwd(),"/outputs/readGCAMTables/Tables_Local/local_Regional_Colombia.csv",sep=""),
            #  paste(getwd(),"/outputs/readGCAMTables/Tables_Local/local_Regional_Argentina.csv",sep=""),
              paste(getwd(),"/outputs/readGCAMTables/Tables_Local/local_Regional_Uruguay.csv",sep=""))  # Need to create this before loading

# Read in the data from the function metis.readgcam
rTable <- dataGCAM;

# Choose Parameters or set to "All" for all params. For complete list see ?metis.chartsProcess
# paramsSelect=c("finalNrgbySec", "primNrgConsumByFuel", "elecByTech",
#                "watConsumBySec", "watWithdrawBySec", "watWithdrawByCrop", "watBioPhysCons",
#                "gdpPerCapita", "gdp", "gdpGrowthRate", "pop",
#                "agProdbyIrrRfd","agProdByCrop",
#                "landIrrRfd", "aggLandAlloc","co2emissionByEndUse", "ghgEmissionByGHG")

#paramsSelect=c("elecByTech")

regionsSelect=c("Uruguay")

charts<-metis.chartsProcess(rTable=rTable, # Default is NULL
                            dataTables=dataTables, # Default is NULL
                            paramsSelect=paramsSelect, # Default is "All"
                            regionsSelect=regionsSelect, # Default is "All"
                            xCompare=c("2015","2035","2050","2100"), # Default is c("2015","2030","2050","2100")
                            scenRef="GCAMModified", # Default is NULL
                            dirOutputs=paste(getwd(),"/outputs",sep=""), # Default is paste(getwd(),"/outputs",sep="")
                            pdfpng="png", # Default is "png"
                            regionCompareOnly=0, # Default is "0"
                            useNewLabels=0,
                            xRange=c(2010:2050) # Default is All
)


# rTable=rTable # Default is NULL
# dataTables=dataTables # Default is NULL
# paramsSelect=paramsSelect # Default is "All"
# regionsSelect=regionsSelect # Default is "All"
# xCompare=c("2015","2035","2050","2100") # Default is c("2015","2030","2050","2100")
# scenRef="GCAMModified" # Default is NULL
# dirOutputs=paste(getwd(),"/outputs",sep="") # Default is paste(getwd(),"/outputs",sep="")
# pdfpng="png" # Default is "png"
# regionCompareOnly=0 # Default is "0"
# useNewLabels=0
# xRange=c(2010:2050) # Default is All
