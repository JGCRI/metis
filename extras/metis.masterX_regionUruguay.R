

#-----------------------------
# Over all steps
#-----------------------------
# 1. Read GCAM Data
# 2. Create Charts
# 3. Prepare Polygon Data (Check and fix shapefiles as needed)
# 4. Plots Boudnaries
# 5. Prep grids
# 6. Grid to polygons
# 7. Produce Maps


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

countryName= "Uruguay"
countryName <- tools::toTitleCase(countryName); countryName
localBasinShapeFileFolder = paste(getwd(),"/dataFiles/gis/other/shapefiles_",countryName,sep="")
localBasinShapeFile = "uruguay_8cuencas"
localBasinsShapeFileColName = "code" # Will need to load the file to see which name this would be


#----------------------------
# Read GCAM Data
#---------------------------

regionsSelect_i <- c("Uruguay")
dataProjPath_i <- paste(getwd(),"/dataFiles/gcam",sep="")
queryPath_i <-paste(getwd(),"/dataFiles/gcam",sep="")

# ?metis.readgcam # For more help

gcamdatabasePath_i <-paste("D:/GCAM/gcam-core_LAC/output/FinalRuns",sep="")
gcamdatabaseName_i <-"IDBNexus_RefImpactsSelect"
rgcam::localDBConn(gcamdatabasePath_i,gcamdatabaseName_i) # if connecting directly to gcam database

# Reference
dataProj_i <-"Uruguay_dataProj_Ref.proj"
dataProjLoaded <- loadProject(paste(dataProjPath_i, "/", dataProj_i, sep = ""))
listScenarios(dataProjLoaded)  # List of Scenarios in GCAM database
#queries <- listQueries(dataProjLoaded)  # List of Queries in queryxml

queryPath_i <- paste(getwd(),"/dataFiles/gcam",sep="")

# Choose Query sets, individual queries or set to "All". For complete list see ?metis.readgcam
queriesSelect_i = c("All") # Query sets are c("water", "energy", "land", "emissions", "ag", "socioecon", "transport")
#queriesSelect_i = c("energy")

dataGCAMRef<-metis.readgcam(reReadData=F, # Default Value is T
                                 dataProj = dataProj_i, # Default Value is "dataProj.proj"
                                 dataProjPath = dataProjPath_i,
                                 scenOrigNames=c("GCAMOriginal", "Ref"),
                                 scenNewNames=c("GCAMOrig","GCAMRef"),
                                 gcamdatabasePath=gcamdatabasePath_i,
                                 gcamdatabaseName=gcamdatabaseName_i,
                                 queryxml="metisQueries.xml",  # Default Value is "metisQueries.xml"
                                 queryPath = queryPath_i,
                                 dirOutputs= paste(getwd(),"/outputs",sep=""), # Default Value is paste(getwd(),"/outputs",sep="")
                                 regionsSelect=regionsSelect_i, # Default Value is NULL
                                 queriesSelect=queriesSelect_i)


dataGCAMRef$data
unique(dataGCAMRef$data$param)
unique(dataGCAMRef$data$scenario)

rgcam::localDBConn(gcamdatabasePath_i,gcamdatabaseName_i) # if connecting directly to gcam database

# Impacts
dataProj_i <-"Uruguay_dataProj_Impacts.proj"
dataProjLoaded <- loadProject(paste(dataProjPath_i, "/", dataProj_i, sep = ""))
listScenarios(dataProjLoaded)  # List of Scenarios in GCAM database
#queries <- listQueries(dataProjLoaded)  # List of Queries in queryxml

dataGCAMImpacts<-metis.readgcam(reReadData=F, # Default Value is T
                            dataProj = dataProj_i, # Default Value is "dataProj.proj"
                            dataProjPath = dataProjPath_i,
                            scenOrigNames=c("Ref_ImpactsGFDLrcp8p5","Ref_ImpactsGFDLrcp2p6"),
                            scenNewNames=c("GFDL_RCP8p5","GFDL_RCP2p6"),
                            gcamdatabasePath=gcamdatabasePath_i,
                            gcamdatabaseName=gcamdatabaseName_i,
                            queryxml="metisQueries.xml",  # Default Value is "metisQueries.xml"
                            queryPath = queryPath_i,
                            dirOutputs= paste(getwd(),"/outputs",sep=""), # Default Value is paste(getwd(),"/outputs",sep="")
                            regionsSelect=regionsSelect_i, # Default Value is NULL
                            queriesSelect=queriesSelect_i
)

dataGCAMImpacts$data
unique(dataGCAMImpacts$data$param)
unique(dataGCAMImpacts$data$scenario)


# Policy Scenarios

gcamdatabasePath_i <-paste("D:/GCAM/gcam-core_LAC/output/FinalRuns",sep="")
gcamdatabaseName_i <-"IDBNexus_RefPolicySelect"
rgcam::localDBConn(gcamdatabasePath_i,gcamdatabaseName_i) # if connecting directly to gcam database

# Reference
dataProj_i <-"Uruguay_dataProj_Policy.proj"
dataProjLoaded <- loadProject(paste(dataProjPath_i, "/", dataProj_i, sep = ""))
listScenarios(dataProjLoaded)  # List of Scenarios in GCAM database
#queries <- listQueries(dataProjLoaded)  # List of Queries in queryxml

queryPath_i <- paste(getwd(),"/dataFiles/gcam",sep="")
# Choose Query sets, individual queries or set to "All". For complete list see ?metis.readgcam
queriesSelect_i = c("All") # Query sets are c("water", "energy", "land", "emissions", "ag", "socioecon", "transport")
#queriesSelect_i = c("energy")


dataGCAMPolicy<-metis.readgcam(reReadData=F, # Default Value is T
                                dataProj = dataProj_i, # Default Value is "dataProj.proj"
                                dataProjPath = dataProjPath_i,
                                scenOrigNames=c( "Ref_OilCropIrrCost_neg10",  "Ref_OilCropIrrCost_neg25", "Ref_OilCropIrrCost_neg50",
                                                "Ref_OilCropIrrCost_neg75", "Ref_RiceYield_pos10", "Ref_RiceYield_pos21p25"),
                                scenNewNames=c("OilCropIrrCost_neg10", "OilCropIrrCost_neg25", "OilCropIrrCost_neg50",
                                               "OilCropIrrCost_neg75", "RiceYield_pos10", "RiceYield_pos21p25"),
                                gcamdatabasePath=gcamdatabasePath_i,
                                gcamdatabaseName=gcamdatabaseName_i,
                                queryxml="metisQueries.xml",  # Default Value is "metisQueries.xml"
                                queryPath = queryPath_i,
                                dirOutputs= paste(getwd(),"/outputs",sep=""), # Default Value is paste(getwd(),"/outputs",sep="")
                                regionsSelect=regionsSelect_i, # Default Value is NULL
                                queriesSelect=queriesSelect_i
)

dataGCAMPolicy$data
unique(dataGCAMImpacts$data$param)
unique(dataGCAMImpacts$data$scenario)

#------------------
# Combine Scenarios
#-------------------
dataGCAM <- dplyr::bind_rows(dataGCAMRef$data,dataGCAMImpacts$data,dataGCAMPolicy$data)# To view the data read that was read.
dataGCAM <- dataGCAM %>% dplyr::filter(!is.na(scenario))
dataGCAM <- droplevels(dataGCAM)
#saveRDS(dataGCAM, file = paste(getwd(),"/dataFiles/gcam/tempUruguayGCAMData.rds",sep=""))
readRDS(file = paste(getwd(),"/dataFiles/gcam/tempUruguayGCAMData.rds",sep=""))
unique(dataGCAM$param)
unique(dataGCAM$scenario)


#----------------------------
# Produce Data Charts
#---------------------------

#Choose parameters for Report
# paramsSelect_i = c(# energy
#         "energyPrimaryByFuelEJ","energyPrimaryRefLiqProdEJ",
#         "energyFinalConsumBySecEJ","energyFinalByFuelBySectorEJ","energyFinalSubsecByFuelTranspEJ",
#         "energyFinalSubsecByFuelBuildEJ", "energyFinalSubsecByFuelIndusEJ","energyFinalSubsecBySectorBuildEJ",
#         "energyPrimaryByFuelMTOE","energyPrimaryRefLiqProdMTOE",
#         "energyFinalConsumBySecMTOE","energyFinalbyFuelMTOE","energyFinalSubsecByFuelTranspMTOE",
#         "energyFinalSubsecByFuelBuildMTOE", "energyFinalSubsecByFuelIndusMTOE","energyFinalSubsecBySectorBuildMTOE",
#         "energyPrimaryByFuelTWh","energyPrimaryRefLiqProdTWh",
#         "energyFinalConsumBySecTWh","energyFinalbyFuelTWh","energyFinalSubsecByFuelTranspTWh",
#         "energyFinalSubsecByFuelBuildTWh", "energyFinalSubsecByFuelIndusTWh","energyFinalSubsecBySectorBuildTWh",
#         # electricity
#         "elecByTechTWh","elecCapByFuel","elecFinalBySecTWh","elecFinalByFuelTWh",
#         # transport
#         "transportPassengerVMTByMode", "transportFreightVMTByMode", "transportPassengerVMTByFuel", "transportFreightVMTByFuel",
#         # water
#         "watConsumBySec", "watWithdrawBySec", "watWithdrawByCrop", "watBioPhysCons", "watIrrWithdrawBasin","watIrrConsBasin",
#         # socioecon
#         "gdpPerCapita", "gdp", "gdpGrowthRate", "pop",
#         # ag
#         "agProdbyIrrRfd","agProdBiomass", "agProdForest", "agProdByCrop",
#         # land
#         "landIrrRfd", "landAlloc","landAllocByCrop",
#         # emissions
#         "emissLUC", "emissCO2BySector","emissCO2NonCO2BySectorGWPAR5","emissCO2NonCO2BySectorGTPAR5","emissNonCO2BySectorOrigUnits",
#         "emissNonCO2ByResProdGWPAR5", "emissTotalFFIBySec","emissMethaneBySource",
#         "emissCO2BySectorNonCO2GWPAR5", "emissCO2BySectorNonCO2GWPAR5LUC", "emissTotalBySec","emissCO2BySectorNoBio")
paramsSelect_i = "All"

#paramsSelect_i = c("watWithdrawBySec")

# Read in Tables (If exist)
dataTables_i<-c(paste(getwd(),"/dataFiles/localData/local_Regional_Uruguay.csv",sep=""))  # Need to create this before loading
a<-read.csv(dataTables_i); head(a); unique(a$scenario); unique(a$param); unique(a$x)


# Read in the data from the function metis.readgcam
rTable_i <- dataGCAM %>% dplyr::filter(value!=0)  %>%
  dplyr::mutate(class1=case_when(param=="agProdByCrop" ~ gsub("OilCrop","SoySunflower",class1),TRUE~class1)) %>%
  dplyr::group_by(scenario, region, param, sources, class1, class2, x, xLabel, vintage, units,
                  aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                  origScen, origQuery, origUnits, origX)%>%
  dplyr::summarize_at(dplyr::vars("value","origValue"),list(~sum(.,na.rm = T)))%>%
  dplyr::ungroup() %>% droplevels()


rTable_iMod <- rTable_i %>%
  dplyr::filter(!grepl("intl|International",class1))%>%
  dplyr::filter(!grepl("intl|International",class2))%>%
  dplyr::filter(!grepl("CHP",class1))%>%
  dplyr::filter(!grepl("CHP",class2))%>%
  dplyr::mutate(class1=case_when(param=="elecByTechTWh" ~ gsub("a Coal","Fossil",class1),TRUE~class1),
                class1=case_when(param=="elecByTechTWh" ~ gsub("c Gas","Fossil",class1),TRUE~class1),
                class1=case_when(param=="elecByTechTWh" ~ gsub("e Oil","Fossil",class1),TRUE~class1),
                class1=case_when(param=="agProdByCrop" ~ gsub("OilCrop","SoySunflower",class1),TRUE~class1),
                class1=gsub("Refining and Hydrogen Production","Refining",class1),
                class2=gsub("Refining and Hydrogen Production","Refining",class2)) %>%
  dplyr::group_by(scenario, region, param, sources, class1, class2, x, xLabel, vintage, units,
                  aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                  origScen, origQuery, origUnits, origX)%>%
  dplyr::summarize_at(dplyr::vars("value","origValue"),list(~sum(.,na.rm = T)))%>%
  dplyr::ungroup() %>% droplevels()

paramsSelect_iMod=c(as.vector(as.character(unique(a$param))),"watConsumBySec")

#----------------------------
# REFERENCE
#----------------------------

scensSelect_i = c("GCAMOrig","GCAMRef","Local Data")

scaleRange_i = tibble::tribble(
  ~param,~minScale, ~maxScale,
  "watConsumBySec", 0, 10,
  "watWithdrawBySec", 0, 10)

# charts<-metis.chartsProcess(rTable=rTable_i, # Default is NULL
#                             dataTables=dataTables_i, # Default is NULL
#                             paramsSelect=paramsSelect_i, # Default is "All"
#                             regionsSelect=regionsSelect_i, # Default is "All"
#                             scensSelect=scensSelect_i,
#                             xCompare=c("2010","2015","2020","2030"), # Default is c("2015","2030","2050","2100")
#                             scenRef="GCAMOrig", # Default is NULL
#                             dirOutputs=paste(getwd(),"/outputs",sep=""), # Default is paste(getwd(),"/outputs",sep="")
#                             pdfpng="png", # Default is "png"
#                             regionCompareOnly=0, # Default is "0"
#                             scenarioCompareOnly=0, # Default is "0"
#                             useNewLabels=1,
#                             xRange=c(2010:2050),
#                             colOrder1 = c("GCAMOrig","GCAMRef","Local Data"),
#                             colOrderName1 = "scenario",
#                             folderName = "Reference",
#                             scaleRange=scaleRange_i)


charts<-metis.chartsProcess(rTable=rTable_iMod, # Default is NULL
                            dataTables=dataTables_i, # Default is NULL
                            paramsSelect=paramsSelect_iMod, # Default is "All"
                            regionsSelect=regionsSelect_i, # Default is "All"
                            scensSelect=scensSelect_i,
                            xCompare=c("2010","2015","2020","2030"), # Default is c("2015","2030","2050","2100")
                            scenRef="GCAMOrig", # Default is NULL
                            dirOutputs=paste(getwd(),"/outputs",sep=""), # Default is paste(getwd(),"/outputs",sep="")
                            pdfpng="png", # Default is "png"
                            regionCompareOnly=0, # Default is "0"
                            scenarioCompareOnly=1, # Default is "0"
                            useNewLabels=1,
                            xRange=c(2010,2015,2020,2025,2030,2035,2040,2045,2050),
                            colOrder1 =scensSelect_i,
                            colOrderName1 = "scenario",
                            folderName = "Reference_Mod",
                            scaleRange=scaleRange_i)


# rTable=rTable_iMod # Default is NULL
# dataTables=dataTables_i # Default is NULL
# paramsSelect=paramsSelect_iMod # Default is "All"
# regionsSelect=regionsSelect_i # Default is "All"
# scensSelect=scensSelect_i
# xCompare=c("2010","2015","2020","2030") # Default is c("2015","2030","2050","2100")
# scenRef="GCAMOrig" # Default is NULL
# dirOutputs=paste(getwd(),"/outputs",sep="") # Default is paste(getwd(),"/outputs",sep="")
# pdfpng="png" # Default is "png"
# regionCompareOnly=0 # Default is "0"
# scenarioCompareOnly=1 # Default is "0"
# useNewLabels=1
# xRange=c(2010,2015,2020,2025,2030,2035,2040,2045,2050)
# colOrder1 =scensSelect_i
# colOrderName1 = "scenario"
# folderName = "Reference_Mod"
# scaleRange=scaleRange_i


#----------------------------
# IMPACTS
#----------------------------

scensSelect_i = c("GCAMRef","GFDL_RCP2p6","GFDL_RCP8p5")

# charts<-metis.chartsProcess(rTable=rTable_i, # Default is NULL
#                             #dataTables=dataTables_i, # Default is NULL
#                             paramsSelect=paramsSelect_i, # Default is "All"
#                             regionsSelect=regionsSelect_i, # Default is "All"
#                             scensSelect=scensSelect_i,
#                             xCompare=c("2010","2015","2020","2030"), # Default is c("2015","2030","2050","2100")
#                             scenRef="GCAMRef", # Default is NULL
#                             dirOutputs=paste(getwd(),"/outputs",sep=""), # Default is paste(getwd(),"/outputs",sep="")
#                             pdfpng="png", # Default is "png"
#                             regionCompareOnly=0, # Default is "0"
#                             scenarioCompareOnly=0, # Default is "0"
#                             useNewLabels=1,
#                             xRange=c(2010:2050),
#                             colOrder1 = c("GCAMRef","GFDL_RCP2p6","GFDL_RCP8p5"),
#                             colOrderName1 = "scenario",
#                             folderName = "Impacts",
#                             scaleRange=scaleRange_i)

charts<-metis.chartsProcess(rTable=rTable_iMod, # Default is NULL
                            #dataTables=dataTables_i, # Default is NULL
                            paramsSelect=paramsSelect_iMod, # Default is "All"
                            regionsSelect=regionsSelect_i, # Default is "All"
                            scensSelect=scensSelect_i,
                            xCompare=c("2010","2015","2020","2030"), # Default is c("2015","2030","2050","2100")
                            scenRef="GCAMRef", # Default is NULL
                            dirOutputs=paste(getwd(),"/outputs",sep=""), # Default is paste(getwd(),"/outputs",sep="")
                            pdfpng="png", # Default is "png"
                            regionCompareOnly=0, # Default is "0"
                            scenarioCompareOnly=1, # Default is "0"
                            useNewLabels=1,
                            xRange=c(2010,2015,2020,2025,2030,2035,2040,2045,2050),
                            colOrder1 = c("GCAMRef","GFDL_RCP2p6","GFDL_RCP8p5"),
                            colOrderName1 = "scenario",
                            folderName = "Impacts_Mod",
                            scaleRange=scaleRange_i)



#----------------------------
# POLICY OIL Crop
#----------------------------

scensSelect_i = c("GCAMRef","OilCropIrrCost_neg25", "OilCropIrrCost_neg75")

charts<-metis.chartsProcess(rTable=rTable_i, # Default is NULL
                            #dataTables=dataTables_i, # Default is NULL
                            paramsSelect=paramsSelect_i, # Default is "All"
                            regionsSelect=regionsSelect_i, # Default is "All"
                            scensSelect=scensSelect_i,
                            xCompare=c("2010","2020","2030","2050"), # Default is c("2015","2030","2050","2100")
                            scenRef="GCAMRef", # Default is NULL
                            dirOutputs=paste(getwd(),"/outputs",sep=""), # Default is paste(getwd(),"/outputs",sep="")
                            pdfpng="png", # Default is "png"
                            regionCompareOnly=0, # Default is "0"
                            scenarioCompareOnly=0, # Default is "0"
                            useNewLabels=1,
                            xRange=c(2010,2015,2020,2025,2030,2035,2040,2045,2050),
                            #scaleRange=scaleRange_i,
                            colOrder1 = scensSelect_i,
                            colOrderName1 = "scenario",
                            folderName = "Policy_OilCropIrr")

charts<-metis.chartsProcess(rTable=rTable_iMod, # Default is NULL
                            #dataTables=dataTables_i, # Default is NULL
                            paramsSelect=paramsSelect_iMod, # Default is "All"
                            regionsSelect=regionsSelect_i, # Default is "All"
                            scensSelect=scensSelect_i,
                            xCompare=c("2010","2020","2030","2050"), # Default is c("2015","2030","2050","2100")
                            scenRef="GCAMRef", # Default is NULL
                            dirOutputs=paste(getwd(),"/outputs",sep=""), # Default is paste(getwd(),"/outputs",sep="")
                            pdfpng="png", # Default is "png"
                            regionCompareOnly=0, # Default is "0"
                            scenarioCompareOnly=0, # Default is "0"
                            useNewLabels=1,
                            xRange=c(2010,2015,2020,2025,2030,2035,2040,2045,2050),
                            #scaleRange=scaleRange_i,
                            colOrder1 =scensSelect_i ,
                            colOrderName1 = "scenario",
                            folderName = "Policy_OilCropIrr_Mod")


#----------------------------
# POLICY OIL
#----------------------------

scensSelect_i = c("GCAMRef","RiceYield_pos10", "RiceYield_pos21p25")

charts<-metis.chartsProcess(rTable=rTable_i, # Default is NULL
                            #dataTables=dataTables_i, # Default is NULL
                            paramsSelect=paramsSelect_i, # Default is "All"
                            regionsSelect=regionsSelect_i, # Default is "All"
                            scensSelect=scensSelect_i,
                            xCompare=c("2010","2020","2030","2050"), # Default is c("2015","2030","2050","2100")
                            scenRef="GCAMRef", # Default is NULL
                            dirOutputs=paste(getwd(),"/outputs",sep=""), # Default is paste(getwd(),"/outputs",sep="")
                            pdfpng="png", # Default is "png"
                            regionCompareOnly=0, # Default is "0"
                            scenarioCompareOnly=0, # Default is "0"
                            useNewLabels=1,
                            xRange=c(2010,2015,2020,2025,2030,2035,2040,2045,2050),
                            #scaleRange=scaleRange_i,
                            colOrder1 = scensSelect_i,
                            colOrderName1 = "scenario",
                            folderName = "Policy_RiceYield")

charts<-metis.chartsProcess(rTable=rTable_iMod, # Default is NULL
                            #dataTables=dataTables_i, # Default is NULL
                            paramsSelect=paramsSelect_iMod, # Default is "All"
                            regionsSelect=regionsSelect_i, # Default is "All"
                            scensSelect=scensSelect_i,
                            xCompare=c("2010","2020","2030","2050"), # Default is c("2015","2030","2050","2100")
                            scenRef="GCAMRef", # Default is NULL
                            dirOutputs=paste(getwd(),"/outputs",sep=""), # Default is paste(getwd(),"/outputs",sep="")
                            pdfpng="png", # Default is "png"
                            regionCompareOnly=0, # Default is "0"
                            scenarioCompareOnly=1, # Default is "0"
                            useNewLabels=1,
                            xRange=c(2010,2015,2020,2025,2030,2035,2040,2045,2050),
                            #scaleRange=scaleRange_i,
                            colOrder1 =scensSelect_i ,
                            colOrderName1 = "scenario",
                            folderName = "Policy_RiceYield_Mod")






