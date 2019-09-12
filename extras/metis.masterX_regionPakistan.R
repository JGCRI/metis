

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



#----------------------------
# Read GCAM Data
#---------------------------

# ?metis.readgcam # For more help

gcamdatabasePath_i <-paste("D:/GCAM/gcam-core_LAC/output",sep="")
gcamdatabaseName_i <-"database_basexdb"

dataProjPath_i <- paste(getwd(),"/dataFiles/gcam",sep="")
queryPath_i <-paste(getwd(),"/dataFiles/gcam",sep="")
regionsSelect_i <- c("Pakistan")

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


# reReadData=F# Default Value is T
# dataProj = dataProj_i # Default Value is "dataProj.proj"
# dataProjPath = dataProjPath_i
# scenOrigNames=c("IDBUruguay_GCAMOrig", "IDBUruguay_GCAMRef_NoImpacts")
# scenNewNames=c("GCAMOrig","GCAMRef")
# gcamdatabasePath=gcamdatabasePath_i
# gcamdatabaseName=gcamdatabaseName_i
# queryxml="metisQueries.xml"  # Default Value is "metisQueries.xml"
# queryPath = queryPath_i
# dirOutputs= paste(getwd(),"/outputs",sep="") # Default Value is paste(getwd(),"/outputs",sep="")
# regionsSelect=regionsSelect_i # Default Value is NULL
# paramsSelect="All" # Default value is "All"

dataGCAMRef$data
unique(dataGCAMRef$data$param)
unique(dataGCAMRef$data$scenario)


# Save an object to a file
#saveRDS(dataGCAM$data, file = paste(getwd(),"/dataFiles/gcam/tempUruguayGCAMData.rds",sep=""))
#readRDS(file = paste(getwd(),"/dataFiles/gcam/tempUruguayGCAMData.rds",sep=""))

dataGCAM <- dplyr::bind_rows(dataGCAMRef$data)# To view the data read that was read.
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

paramsSelect_i=c("finalNrgbySecDetbyFuel")

# Read in Tables (If exist)
dataTables_i<-c(paste(getwd(),"/dataFiles/localData/local_Regional_Pakistan.csv",sep=""))  # Need to create this before loading
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

regionsSelect_i=c("Pakistan")

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
                            scenarioCompareOnly=0, # Default is "0"
                            useNewLabels=1,
                            xRange=c(2010:2050),
                            colOrder1 = c("GCAMOrig","GCAMRef","Local Data"),
                            colOrderName1 = "scenario",
                            folderName = "Pakistan_Reference",
                            scaleRange=scaleRange_i)


