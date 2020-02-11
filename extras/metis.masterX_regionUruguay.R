
#----------------------------
# Install necessary packages
#----------------------------
if("devtools" %in% rownames(installed.packages()) == F){install.packages("devtools")}
library(devtools)
if("metis" %in% rownames(installed.packages()) == F){install_github(repo="JGCRI/metis")}
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

regionsSelect_i <- countryName
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
                                 scenOrigNames=c("GCAMOriginal","Ref"),
                                 scenNewNames=c("GCAMOrig","GCAMRef"),
                                 gcamdatabasePath=gcamdatabasePath_i,
                                 gcamdatabaseName=gcamdatabaseName_i,
                                 queryxml="metisQueries.xml",  # Default Value is "metisQueries.xml"
                                 queryPath = queryPath_i,
                                 dirOutputs= paste(getwd(),"/outputs",sep=""), # Default Value is paste(getwd(),"/outputs",sep="")
                                 regionsSelect=regionsSelect_i, # Default Value is NULL
                                 queriesSelect=queriesSelect_i)

# reReadData=F # Default Value is T
# dataProj = dataProj_i # Default Value is "dataProj.proj"
# dataProjPath = dataProjPath_i
# scenOrigNames=c("GCAMOriginal", "Ref")
# scenNewNames=c("GCAMOrig","GCAMRef")
# gcamdatabasePath=gcamdatabasePath_i
# gcamdatabaseName=gcamdatabaseName_i
# queryxml="metisQueries.xml"  # Default Value is "metisQueries.xml"
# queryPath = queryPath_i
# dirOutputs= paste(getwd(),"/outputs",sep="") # Default Value is paste(getwd(),"/outputs",sep="")
# regionsSelect=regionsSelect_i # Default Value is NULL
# queriesSelect=queriesSelect_i


dataGCAMRef$data
unique(dataGCAMRef$data$param)%>%sort()
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


#---------------------------------------------------
if(T){
# SOy Irrigated Area Expansion

gcamdatabasePath_i <-paste("D:/GCAM/gcam-core_LAC/output/FinalRuns",sep="")
gcamdatabaseName_i <-"IDBNexus_RefPolicySelectNewSoyIrrExpand"
rgcam::localDBConn(gcamdatabasePath_i,gcamdatabaseName_i) # if connecting directly to gcam database

# Reference
dataProj_i <-"Uruguay_dataProj_PolicySoyIrrExpand.proj"
dataProjLoaded <- loadProject(paste(dataProjPath_i, "/", dataProj_i, sep = ""))
listScenarios(dataProjLoaded)  # List of Scenarios in GCAM database
#queries <- listQueries(dataProjLoaded)  # List of Queries in queryxml

queryPath_i <- paste(getwd(),"/dataFiles/gcam",sep="")
# Choose Query sets, individual queries or set to "All". For complete list see ?metis.readgcam
queriesSelect_i = c("All") # Query sets are c("water", "energy", "land", "emissions", "ag", "socioecon", "transport")
#queriesSelect_i = c("energy")


dataGCAMPolicySoyIrrExpand<-metis.readgcam(reReadData=F, # Default Value is T
                                dataProj = dataProj_i, # Default Value is "dataProj.proj"
                                dataProjPath = dataProjPath_i,
                                scenOrigNames=c( "Ref_IrrSoy_pos560"),
                                scenNewNames=c("IrrSoy_pos560"),
                                gcamdatabasePath=gcamdatabasePath_i,
                                gcamdatabaseName=gcamdatabaseName_i,
                                queryxml="metisQueries.xml",  # Default Value is "metisQueries.xml"
                                queryPath = queryPath_i,
                                dirOutputs= paste(getwd(),"/outputs",sep=""), # Default Value is paste(getwd(),"/outputs",sep="")
                                regionsSelect=regionsSelect_i, # Default Value is NULL
                                queriesSelect=queriesSelect_i
)

dataGCAMPolicySoyIrrExpand$data
unique(dataGCAMPolicySoyIrrExpand$data$param)
unique(dataGCAMPolicySoyIrrExpand$data$scenario)
}
#----------------------------------------------------------------------------------------------

#---------------------------------------------------
if(T){
  # Rice Policy Scenarios

  gcamdatabasePath_i <-paste("D:/GCAM/gcam-core_LAC/output/FinalRuns",sep="")
  gcamdatabaseName_i <-"IDBNexus_RefPolicySelect"
  rgcam::localDBConn(gcamdatabasePath_i,gcamdatabaseName_i) # if connecting directly to gcam database

  # Reference
  dataProj_i <-"Uruguay_dataProj_PolicyRice.proj"
  dataProjLoaded <- loadProject(paste(dataProjPath_i, "/", dataProj_i, sep = ""))
  listScenarios(dataProjLoaded)  # List of Scenarios in GCAM database
  #queries <- listQueries(dataProjLoaded)  # List of Queries in queryxml

  queryPath_i <- paste(getwd(),"/dataFiles/gcam",sep="")
  # Choose Query sets, individual queries or set to "All". For complete list see ?metis.readgcam
  queriesSelect_i = c("All") # Query sets are c("water", "energy", "land", "emissions", "ag", "socioecon", "transport")
  #queriesSelect_i = c("energy")


  dataGCAMPolicyRiceYield<-metis.readgcam(reReadData=F, # Default Value is T
                                          dataProj = dataProj_i, # Default Value is "dataProj.proj"
                                          dataProjPath = dataProjPath_i,
                                          scenOrigNames=c( "Ref_RiceYield_pos10", "Ref_RiceYield_pos20"),
                                          scenNewNames=c("RiceYield_pos10", "RiceYield_pos20"),
                                          gcamdatabasePath=gcamdatabasePath_i,
                                          gcamdatabaseName=gcamdatabaseName_i,
                                          queryxml="metisQueries.xml",  # Default Value is "metisQueries.xml"
                                          queryPath = queryPath_i,
                                          dirOutputs= paste(getwd(),"/outputs",sep=""), # Default Value is paste(getwd(),"/outputs",sep="")
                                          regionsSelect=regionsSelect_i, # Default Value is NULL
                                          queriesSelect=queriesSelect_i
  )

  dataGCAMPolicyRiceYield$data
  unique(dataGCAMPolicyRiceYield$data$param)
  unique(dataGCAMPolicyRiceYield$data$scenario)
}
#----------------------------------------------------------------------------------------------


# Livestock Productivity

gcamdatabasePath_i <-paste("D:/GCAM/gcam-core_LAC/output/FinalRuns",sep="")
gcamdatabaseName_i <-"IDBNexus_RefPolicySelectNewPastureEmissInt_Out"
rgcam::localDBConn(gcamdatabasePath_i,gcamdatabaseName_i) # if connecting directly to gcam database

# Reference
dataProj_i <-"Uruguay_dataProj_PolicyLiveStock.proj"
dataProjLoaded <- loadProject(paste(dataProjPath_i, "/", dataProj_i, sep = ""))
listScenarios(dataProjLoaded)  # List of Scenarios in GCAM database
#queries <- listQueries(dataProjLoaded)  # List of Queries in queryxml

queryPath_i <- paste(getwd(),"/dataFiles/gcam",sep="")
# Choose Query sets, individual queries or set to "All". For complete list see ?metis.readgcam
queriesSelect_i = c("All") # Query sets are c("water", "energy", "land", "emissions", "ag", "socioecon", "transport")
#queriesSelect_i = c("energy")


dataGCAMPolicyLivestock<-metis.readgcam(reReadData=F, # Default Value is T
                               dataProj = dataProj_i, # Default Value is "dataProj.proj"
                               dataProjPath = dataProjPath_i,
                               scenOrigNames=c( "Ref_LivestockPasture_neg5_emissIntPol","Ref_LivestockPasture_neg10_emissIntPol",
                                                "Ref_LivestockPasture_neg25_emissIntPol", "Ref_LivestockPasture_neg50_emissIntPol",
                                                "Ref_LivestockPasture_neg75_emissIntPol", "Ref_LivestockPasture_neg90_emissIntPol",
                                                "Ref_LivestockPasture_neg95_emissIntPol"),
                               scenNewNames=c("Livestock_neg5", "Livestock_neg10", "Livestock_neg25", "Livestock_neg50",
                                              "Livestock_neg75", "Livestock_neg90", "Livestock_neg95"),
                               gcamdatabasePath=gcamdatabasePath_i,
                               gcamdatabaseName=gcamdatabaseName_i,
                               queryxml="metisQueries.xml",  # Default Value is "metisQueries.xml"
                               queryPath = queryPath_i,
                               dirOutputs= paste(getwd(),"/outputs",sep=""), # Default Value is paste(getwd(),"/outputs",sep="")
                               regionsSelect=regionsSelect_i, # Default Value is NULL
                               queriesSelect=queriesSelect_i
)

dataGCAMPolicyLivestock$data
unique(dataGCAMPolicyLivestock$data$param)
unique(dataGCAMPolicyLivestock$data$scenario)


# Oil Crop Scenarios

gcamdatabasePath_i <-paste("D:/GCAM/gcam-core_LAC/output/FinalRuns",sep="")
gcamdatabaseName_i <-"IDBNexus_RefPolicySelect"
rgcam::localDBConn(gcamdatabasePath_i,gcamdatabaseName_i) # if connecting directly to gcam database

# Reference
dataProj_i <-"Uruguay_dataProj_PolicyIrrigationOilCrop.proj"
dataProjLoaded <- loadProject(paste(dataProjPath_i, "/", dataProj_i, sep = ""))
listScenarios(dataProjLoaded)  # List of Scenarios in GCAM database
#queries <- listQueries(dataProjLoaded)  # List of Queries in queryxml

queryPath_i <- paste(getwd(),"/dataFiles/gcam",sep="")
# Choose Query sets, individual queries or set to "All". For complete list see ?metis.readgcam
queriesSelect_i = c("All") # Query sets are c("water", "energy", "land", "emissions", "ag", "socioecon", "transport")
#queriesSelect_i = c("energy")


dataGCAMPolicyIrrigationOilCrop<-metis.readgcam(reReadData=F, # Default Value is T
                                         dataProj = dataProj_i, # Default Value is "dataProj.proj"
                                         dataProjPath = dataProjPath_i,
                                         scenOrigNames=c( "Ref_IrrOilCropCost_neg25", "Ref_IrrOilCropCost_neg75"),
                                         scenNewNames=c("IrrCostOil_neg25", "IrrCostOil_neg75"),
                                         gcamdatabasePath=gcamdatabasePath_i,
                                         gcamdatabaseName=gcamdatabaseName_i,
                                         queryxml="metisQueries.xml",  # Default Value is "metisQueries.xml"
                                         queryPath = queryPath_i,
                                         dirOutputs= paste(getwd(),"/outputs",sep=""), # Default Value is paste(getwd(),"/outputs",sep="")
                                         regionsSelect=regionsSelect_i, # Default Value is NULL
                                         queriesSelect=queriesSelect_i
)

# reReadData=F # Default Value is T
# dataProj = dataProj_i # Default Value is "dataProj.proj"
# dataProjPath = dataProjPath_i
# scenOrigNames=c( "Ref","Ref_IrrOilCropCost_neg25", "Ref_IrrOilCropCost_neg75")
# scenNewNames=c("Ref","IrrCostOil_neg25", "IrrCostOil_neg75")
# gcamdatabasePath=gcamdatabasePath_i
# gcamdatabaseName=gcamdatabaseName_i
# queryxml="metisQueries.xml"  # Default Value is "metisQueries.xml"
# queryPath = queryPath_i
# dirOutputs= paste(getwd(),"/outputs",sep="") # Default Value is paste(getwd(),"/outputs",sep="")
# regionsSelect=regionsSelect_i # Default Value is NULL
# queriesSelect=queriesSelect_i


dataGCAMPolicyIrrigationOilCrop$data
unique(dataGCAMPolicyIrrigationOilCrop$data$param)
unique(dataGCAMPolicyIrrigationOilCrop$data$scenario)


#------------------
# Combine Scenarios
#-------------------
dataGCAM <- dplyr::bind_rows(dataGCAMRef$data,
                             dataGCAMImpacts$data,
                             dataGCAMPolicyLivestock$data,
                             dataGCAMPolicySoyIrrExpand$data,
                             dataGCAMPolicyRiceYield$data,
                             )# To view the data read that was read.
dataGCAM <- dataGCAM %>% dplyr::filter(!is.na(scenario)) %>% unique()
dataGCAM <- droplevels(dataGCAM)


saveRDS(dataGCAM, file = paste(getwd(),"/dataFiles/gcam/temp",countryName,"GCAMData.rds",sep=""))


dataGCAM<-readRDS(file = paste(getwd(),"/dataFiles/gcam/temp",countryName,"GCAMData.rds",sep=""))
unique(dataGCAM$param)
unique(dataGCAM$scenario)

#
# # Add Rice Yield Scenario results
# dataRice <- read.csv("C:/Z/GoogleDrive/00Work/000TEMP/Tables_regional_Uruguay.csv")%>%tibble::as_tibble()%>%
#   tidyr::gather(key="scenario",value="value",-region,-param,-units,-class1,-class2,-x,-vintage)%>%
#   dplyr::mutate(origValue=value, origScen="scenario", sources="sources"); dataRice
# dataJoin <- dataGCAM%>%dplyr::select((names(dataGCAM)[!names(dataGCAM) %in% names(dataRice)[!names(dataRice) %in% c("param")]]))%>%
#   dplyr::select(-origX)%>%unique(); dataJoin
# dataRice <- dataRice %>% dplyr::left_join(dataJoin)%>%unique();dataRice
# dataGCAM <- dataGCAM %>% dplyr::bind_rows(dataRice)%>%unique(); dataGCAM

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
  dplyr::mutate(class1=case_when(param=="agProdByCrop" ~ gsub("OilCrop","Soy",class1),TRUE~class1)) %>%
  dplyr::group_by(scenario, region, param, sources, class1, class2, x, xLabel, vintage, units,
                  aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                  origScen, origQuery, origUnits, origX)%>%
  dplyr::summarize_at(dplyr::vars("value","origValue"),list(~sum(.,na.rm = T)))%>%
  dplyr::ungroup() %>% droplevels()

rTable_iBeef <- rTable_i %>%
  dplyr::filter(param %in% c("livestock_MeatDairybyTechMixed"), class2=="Pasture_FodderGrass")%>%
  dplyr::filter(grepl("Beef",class1))%>%
  dplyr::mutate(units="Beef Mixed Feed (Mt)") %>%
  dplyr::bind_rows(rTable_i %>%
                     dplyr::filter(param %in% c("livestock_MeatDairybyTechPastoral",class2=="Pasture_FodderGrass"))%>%
                     dplyr::filter(grepl("Beef",class1))%>%
                     dplyr::mutate(units="Beef Pastoral (Mt)"))

rTable_iMod <- rTable_i %>%
  dplyr::filter(!grepl("livestock",param))%>%
  dplyr::bind_rows(rTable_iBeef) %>%
  dplyr::filter(!grepl("intl|International",class1))%>%
  dplyr::filter(!grepl("intl|International",class2))%>%
  dplyr::filter(!grepl("CHP",class1))%>%
  dplyr::filter(!grepl("CHP",class2))%>%
  dplyr::mutate(value=case_when(((grepl("LUC",class1)|grepl("LUC",class1)) & x==2010)~0,TRUE~value))%>%
  dplyr::mutate(class1=case_when(param=="elecByTechTWh" ~ gsub("a Coal","Fossil",class1),TRUE~class1),
                class1=case_when(param=="elecByTechTWh" ~ gsub("c Gas","Fossil",class1),TRUE~class1),
                class1=case_when(param=="elecByTechTWh" ~ gsub("e Oil","Fossil",class1),TRUE~class1),
                class1=case_when(param=="agProdByCrop" ~ gsub("OilCrop","SoySunflower",class1),TRUE~class1),
                class1=gsub("Refining and Hydrogen Production","Refining",class1),
                class2=gsub("Refining and Hydrogen Production","Refining",class2),
                value=case_when(grepl("(ha)",units)~value*10,TRUE~value),
                units=case_when(grepl("(ha)",units)~gsub("(ha)","1000 km2",units),TRUE~units),
                units=case_when(grepl("Met1000 km2ne",units)~gsub("Met1000 km2ne","Methane",units),TRUE~units),
                value=case_when((grepl("emiss",param)&!grepl("LUC",class1)&!grepl("LUC",class2)&value<0)~0,TRUE~value),
                units=case_when(units=="GDP Growth Rate (Percent)"~"GDP Growth (percent)",
                                units=="Land Allocation (1000 km2)"~"Land (1000 km2)",
                                units=="Water Withdrawals by Sector (km3)"~"Water Withdraw (km3)",
                                units=="Water Withdrawals by Crop (km3)"~"Water by Crop (km3)",
                                units=="Final Energy by Fuel (Mtoe)"~"Final Energy (Mtoe)",
                                units=="Electricity Generation by Fuel (TWh)"~"Electricity (TWh)",
                                units=="GHG Emissions GTPAR5 (MTCO2eq)"~"GHG GTP (MTCO2eq)",
                                units=="GHG Emissions GWPAR5 (MTCO2eq)"~"GHG GWP (MTCO2eq)",
                                units=="CO2 Emissions by Sector (MTCO2eq)"~"CO2 (MTCO2eq)",
                                TRUE~units)) %>%
  dplyr::group_by(scenario, region, param, sources, class1, class2, x, xLabel, vintage, units,
                  aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                  origScen, origQuery, origUnits, origX)%>%
  dplyr::summarize_at(dplyr::vars("value","origValue"),list(~sum(.,na.rm = T)))%>%
  dplyr::ungroup() %>% unique() %>% droplevels()


scaleRange_i = tibble::tribble(
  ~param,~minScale, ~maxScale,
  "watWithdrawBySec", 0, 10,
  "watWithdrawByCrop", 0, 10,
  "watConsumBySec",0,10,
  "emissByGasGWPAR5FFI",0,60,
  "emissByGasGTPAR5FFI",0,60)

scaleRange_i=NULL


mp_i<-list(paramSet=list(
  c("live"),
  c("lu"),
  c("aglu"),
  c("energy"),
  c("water"),
  c("emissByGas"),
  c("emissBySec"),
  c("socio"),
  #c("WEL"),
  c("energyDetail"),
  c("energyPrimFinalElec"),
  c("elecCapCost"),
  c("emissDetail"),
  c("WEL1"),
  c("WEL2")),
  param=list(
    c("livestock_MeatDairybyTechMixed", "livestock_MeatDairybyTechPastoral"),
    c("landIrrRfd","landIrrCrop","landRfdCrop"),
    c("landAlloc","landAllocByCrop" ,"agProdByCrop"),
    c("energyFinalByFuelBySectorMTOE","energyFinalConsumBySecMTOE","elecByTechTWh"),
    c("watWithdrawBySec","watConsumBySec","watWithdrawByCrop"),
    c("emissCO2BySectorNoBio","emissByGasGWPAR5FFI","emissByGasGTPAR5FFI"),
    c("emissCO2BySectorNoBio","emissBySectorGWPAR5FFI","emissBySectorGTPAR5FFI"),
    c("pop","gdpGrowthRate","gdpPerCapita"),
    # c("agProdByCrop","watWithdrawBySec","watConsumBySec","landAlloc",
    #   "energyFinalByFuelBySectorMTOE","energyFinalConsumBySecMTOE","elecByTechTWh"),
    c("energyFinalSubsecByFuelBuildMTOE","energyFinalSubsecByFuelIndusMTOE","energyFinalSubsecByFuelTranspMTOE"),
    c("energyPrimaryByFuelMTOE","energyFinalByFuelBySectorMTOE","elecByTechTWh"),
    c("elecNewCapGW","elecCumCapGW","elecNewCapCost","elecCumCapCost"),
    c("emissLUC", "emissMethaneBySourceGWPAR5","emissMethaneBySourceGTPAR5"),
    c("agProdByCrop","watWithdrawBySec","landAlloc",
      "energyFinalByFuelBySectorMTOE","elecByTechTWh"),
    c("agProdByCrop","watWithdrawBySec","landAllocByCrop",
      "energyFinalByFuelBySectorMTOE","elecByTechTWh")),
  nColMax=list(
    c(2),
    c(3),
    c(3),
    c(3),
    c(3),
    c(3),
    c(3),
    c(3),
    c(3),
    c(3),
    c(4),
    c(3),
    c(3),
    c(3))
)


mpParams<-unlist(mp_i$param)%>%sort(); mpParams

paramsSelect_iMod=c(as.vector(as.character(unique(a$param))),
                    mpParams[!mpParams %in% as.vector(as.character(unique(a$param)))])
paramsSelect_iMod

pointsOn_i=T

#----------------------------
# REFERENCE
#----------------------------


if(F){
scensSelect_i = c("GCAMOrig","GCAMRef","Local Data")


charts<-metis.chartsProcess(rTable=rTable_iMod, # Default is NULL
                            dataTables=dataTables_i, # Default is NULL
                            #paramsSelect=c("landAlloc","landAllocByCrop","agProdByCrop"), # Default is "All"
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
                            scaleRange=scaleRange_i,
                            multiPlotFigsOnly = T,
                            mp=mp_i, multiPlotOn = T,
                            multiPlotFigLabels=T,pointsOn=pointsOn_i)

}
#----------------------------
# IMPACTS
#----------------------------

if(F){
scensSelect_i = c("GCAMRef","GFDL_RCP2p6","GFDL_RCP8p5")


charts<-metis.chartsProcess(rTable=rTable_iMod, # Default is NULL
                            #dataTables=dataTables_i, # Default is NULL
                            paramsSelect=unlist(mp_i$param), # Default is "All"
                            #paramsSelect="landIrrRfd", # Default is "All"
                            regionsSelect=regionsSelect_i, # Default is "All"
                            scensSelect=scensSelect_i,
                            xCompare=c("2010","2015","2020","2030"), # Default is c("2015","2030","2050","2100")
                            scenRef="GCAMRef", # Default is NULL
                            dirOutputs=paste(getwd(),"/outputs",sep=""), # Default is paste(getwd(),"/outputs",sep="")
                            pdfpng="png", # Default is "png"
                            regionCompareOnly=0, # Default is "0"
                            scenarioCompareOnly=0, # Default is "0"
                            useNewLabels=1,
                            xRange=c(2010,2015,2020,2025,2030,2035,2040,2045,2050),
                            colOrder1 = c("GCAMRef","GFDL_RCP2p6","GFDL_RCP8p5"),
                            colOrderName1 = "scenario",
                            folderName = "Impacts_Mod",
                            scaleRange=scaleRange_i,
                            multiPlotFigsOnly = T,
                            mp=mp_i, multiPlotOn = T,
                            multiPlotFigLabels=T,pointsOn=pointsOn_i,
                            facetCols=3)

}


#----------------------------
# POLICY RICE
#----------------------------

if(F){

scensSelect_i = c("GCAMRef","RiceYield_pos10", "RiceYield_pos20")


charts<-metis.chartsProcess(rTable=rTable_iMod, # Default is NULL
                            #dataTables=dataTables_i, # Default is NULL
                            paramsSelect=unlist(mp_i$param), # Default is "All"
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
                            scaleRange=scaleRange_i,
                            colOrder1 =scensSelect_i ,
                            colOrderName1 = "scenario",
                            folderName = "Policy_RiceYield_Mod",
                            multiPlotFigsOnly = T,
                            mp = mp_i, multiPlotOn = T,
                            multiPlotFigLabels=T,pointsOn=pointsOn_i)

# rTable=rTable_iMod # Default is NULL
# #dataTables=dataTables_i, # Default is NULL
# paramsSelect=unlist(mp_i$param) # Default is "All"
# regionsSelect=regionsSelect_i # Default is "All"
# scensSelect=scensSelect_i
# xCompare=c("2010","2020","2030","2050") # Default is c("2015","2030","2050","2100")
# scenRef="GCAMRef" # Default is NULL
# dirOutputs=paste(getwd(),"/outputs",sep="") # Default is paste(getwd(),"/outputs",sep="")
# pdfpng="png" # Default is "png"
# regionCompareOnly=0 # Default is "0"
# scenarioCompareOnly=0 # Default is "0"
# useNewLabels=1
# xRange=c(2010,2015,2020,2025,2030,2035,2040,2045,2050)
# scaleRange=scaleRange_i
# colOrder1 =scensSelect_i
# colOrderName1 = "scenario"
# folderName = "Policy_RiceYield_Mod"
# multiPlotFigsOnly = T
# mp = mp_i
# multiPlotOn = T
# multiPlotFigLabels=T
# pointsOn=pointsOn_i

}

#----------------------------
# Livestock
#----------------------------

if(F){

  unique(dataGCAM$scenario)
  scensSelect_i = c("GCAMRef","Livestock_neg5","Livestock_neg10")

 # rTable_iMod$scenario <- factor( as.character(rTable_iMod$scenario), levels=scensSelect_i)

  charts<-metis.chartsProcess(rTable=rTable_iMod, # Default is NULL
                              #dataTables=dataTables_i, # Default is NULL
                              paramsSelect=unlist(mp_i$param), # Default is "All"
                              #paramsSelect=c("livestock_MeatDairybyTechMixed"),
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
                              scaleRange=scaleRange_i,
                              colOrder1 =scensSelect_i ,
                              colOrderName1 = "scenario",
                              folderName = "Policy_Livestock_Mod",
                              multiPlotFigsOnly = T,
                              mp = mp_i, multiPlotOn = T,
                              multiPlotFigLabels=T,pointsOn=pointsOn_i)
}


#----------------------------
# Livestock PASTURE
#----------------------------

if(F){

  unique(dataGCAM$scenario)
  scensSelect_i = c("GCAMRef","Livestock_neg10","Livestock_neg25", "Livestock_neg50","Livestock_neg75",
                    "Livestock_neg90", "Livestock_neg95")

  # rTable_iMod$scenario <- factor( as.character(rTable_iMod$scenario), levels=scensSelect_i)

  charts<-metis.chartsProcess(rTable=rTable_iMod, # Default is NULL
                              #dataTables=dataTables_i, # Default is NULL
                              paramsSelect=unlist(mp_i$param), # Default is "All"
                              #paramsSelect=c("livestock_MeatDairybyTechMixed"),
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
                              scaleRange=scaleRange_i,
                              colOrder1 =scensSelect_i ,
                              colOrderName1 = "scenario",
                              folderName = "Policy_LivestockPasture_Mod",
                              multiPlotFigsOnly = T,
                              mp = mp_i, multiPlotOn = T,
                              multiPlotFigLabels=T,pointsOn=pointsOn_i)
}


#----------------------------
# Irrigation
#----------------------------

if(F){

  unique(dataGCAM$scenario)
  scensSelect_i = c("GCAMRef","IrrCost_neg25","IrrCost_neg75","IrrCost_neg99")

  charts<-metis.chartsProcess(rTable=rTable_iMod, # Default is NULL
                              #dataTables=dataTables_i, # Default is NULL
                              paramsSelect=unlist(mp_i$param), # Default is "All"
                              #paramsSelect=c("landIrrCrop",
                              #               "landIrrRfd"),
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
                              scaleRange=scaleRange_i,
                              colOrder1 =scensSelect_i ,
                              colOrderName1 = "scenario",
                              folderName = "Policy_Irrigation_Mod",
                              multiPlotFigsOnly = T,
                              mp = mp_i, multiPlotOn = T,
                              multiPlotFigLabels=T,pointsOn=pointsOn_i)
}


#----------------------------
# Irrigation Oil Crop
#----------------------------

if(F){

  unique(dataGCAM$scenario)
  scensSelect_i = c("GCAMRef","IrrCostOil_neg25","IrrCostOil_neg75")

  charts<-metis.chartsProcess(rTable=rTable_iMod, # Default is NULL
                              #dataTables=dataTables_i, # Default is NULL
                              paramsSelect=unlist(mp_i$param), # Default is "All"
                              #paramsSelect=c("landIrrCrop",
                              #               "landIrrRfd"),
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
                              scaleRange=scaleRange_i,
                              colOrder1 =scensSelect_i ,
                              colOrderName1 = "scenario",
                              folderName = "Policy_IrrigationOil_Mod",
                              multiPlotFigsOnly = T,
                              mp = mp_i, multiPlotOn = T,
                              multiPlotFigLabels=T,pointsOn=pointsOn_i)
}


#----------------------------
# Combined Scenarios All Data
#----------------------------

if(F){

  unique(dataGCAM$scenario)
  scensSelect_i = c("GCAMOrig","GCAMRef","GFDL_RCP2p6","GFDL_RCP8p5",
                    "LivestockPasture_neg10","LivestockPasture_neg5",
                    "IrrCostOil_neg25","IrrCostOil_neg75","RiceYield_pos10","RiceYield_pos20")

  charts<-metis.chartsProcess(rTable=rTable_iMod, # Default is NULL
                              #dataTables=dataTables_i, # Default is NULL
                              paramsSelect=unlist(mp_i$param), # Default is "All"
                              #paramsSelect=c("landIrrCrop",
                              #               "landIrrRfd"),
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
                              scaleRange=scaleRange_i,
                              colOrder1 =scensSelect_i ,
                              colOrderName1 = "scenario",
                              folderName = "AllScen_AllData",
                              multiPlotFigsOnly = T,
                              mp = mp_i, multiPlotOn = T,
                              multiPlotFigLabels=T,pointsOn=pointsOn_i)
}




#----------------------------
# Combined Scenarios Just Policies
#----------------------------


if(F){


  scensSelect_i = c("GCAMRef","RiceYield_pos20","LivestockPasture_neg10","IrrCostOil_neg75")

  charts<-metis.chartsProcess(rTable=rTable_iMod, # Default is NULL
                              #dataTables=dataTables_i, # Default is NULL
                              paramsSelect=unlist(mp_i$param), # Default is "All"
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
                              scaleRange=scaleRange_i,
                              colOrder1 =scensSelect_i ,
                              colOrderName1 = "scenario",
                              folderName = "Comb_Policy_Mod",
                              multiPlotFigsOnly = T,
                              mp = mp_i, multiPlotOn = T,
                              multiPlotFigLabels=T,pointsOn=pointsOn_i)


}



#----------------------------
# Combined Scenarios
#----------------------------

# mp_i_Mod <-list(paramSet=list(
#     c("WEL1"),
#     c("WEL2")),
#     param=list(
#       c("agProdByCrop","watWithdrawBySec","landAlloc",
#         "energyFinalByFuelBySectorMTOE","elecByTechTWh"),
#       c("agProdByCrop","watWithdrawBySec","landAllocByCrop",
#         "energyFinalByFuelBySectorMTOE","elecByTechTWh")),
#     nColMax=list(
#       c(3),
#       c(3))
#   )



if(F){
  scensSelect_i = c("GCAMRef","GFDL_RCP2p6","GFDL_RCP8p5","RiceYield_pos20","LivestockPasture_neg10","IrrCostOil_neg75")

  charts<-metis.chartsProcess(rTable=rTable_iMod, # Default is NULL
                              #dataTables=dataTables_i, # Default is NULL
                              paramsSelect=unlist(mp_i_Mod$param), # Default is "All"
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
                              scaleRange=scaleRange_i,
                              colOrder1 =scensSelect_i ,
                              colOrderName1 = "scenario",
                              folderName = "Comb_PolicyImpacts_Mod",
                              multiPlotFigsOnly = T,
                              mp = mp_i_Mod, multiPlotOn = T,
                              multiPlotFigLabels=T,pointsOn=pointsOn_i)

  # rTable=rTable_iMod # Default is NULL
  # #dataTables=dataTables_i, # Default is NULL
  # paramsSelect=unlist(mp_i$param) # Default is "All"
  # regionsSelect=regionsSelect_i # Default is "All"
  # scensSelect=scensSelect_i
  # xCompare=c("2010","2020","2030","2050") # Default is c("2015","2030","2050","2100")
  # scenRef="GCAMRef" # Default is NULL
  # dirOutputs=paste(getwd(),"/outputs",sep="") # Default is paste(getwd(),"/outputs",sep="")
  # pdfpng="png" # Default is "png"
  # regionCompareOnly=0 # Default is "0"
  # scenarioCompareOnly=1 # Default is "0"
  # useNewLabels=1
  # xRange=c(2010,2015,2020,2025,2030,2035,2040,2045,2050)
  # scaleRange=scaleRange_i
  # colOrder1 =scensSelect_i
  # colOrderName1 = "scenario"
  # folderName = "Comb_PolicyImpacts_Mod"
  # multiPlotFigsOnly = T
  # mp = mp_i
  # multiPlotOn = T
  # multiPlotFigLabels=T
  # pointsOn=pointsOn_i

}

#----------------------------
# Combined Scenarios Paper ERL
#----------------------------


if(T){



  #mp_ix <- mp_i
  paramsSelect_iMod

  unique(rTable_iMod$scenario)

  rtx <- rTable_iMod %>%
    dplyr::filter(scenario %in% c("GCAMRef","GFDL_RCP8p5","RiceYield_pos20","Livestock_neg75","IrrSoy_pos560"))%>%
    dplyr::mutate(scenario=case_when(scenario=="GCAMRef"~"Reference",
                                     scenario=="GFDL_RCP8p5"~"Climate",
                                     scenario=="RiceYield_pos20"~"Rice",
                                     scenario=="Livestock_neg75"~"Beef",
                                     scenario=="IrrSoy_pos560"~"Soy",
                                     TRUE~scenario))

  scensSelect_i = c("Reference","Climate","Rice","Beef","Soy")

  #Default = NULL, "theme_gray","theme_bw","theme_linedraw","theme_light", "theme_minimal","theme_classic","theme_void","theme_dark"

  mp_ix<-list(paramSet=list(
    c("socioEcon"),
    c("WELREF"),
    c("emissDetail"),
    c("emissMethane")),
    param=list(
      c("pop","gdpGrowthRate"),
      c("landAlloc","watWithdrawBySec","energyFinalByFuelBySectorMTOE",
        "agProdByCrop","watWithdrawByCrop","elecByTechTWh",
        "emissCO2BySectorNoBio","emissByGasGWPAR5FFI","emissByGasGTPAR5FFI"),
      c("emissBySectorGWPAR5FFI","emissBySectorGTPAR5FFI"),
      c("emissMethaneBySourceGWPAR5","emissMethaneBySourceGTPAR5")),
    nColMax=list(
      c(3),
      c(3),
      c(3),
      c(3))
  )

  scaleRange_ix = scaleRange_i = tibble::tribble(
    ~param,~minScale, ~maxScale,
    "watWithdrawBySec", 0, 15,
    "watWithdrawByCrop", 0, 15,
    "watConsumBySec",0,15,
    "emissByGasGWPAR5FFI",0,60,
    "emissByGasGTPAR5FFI",0,60,
    "emissBySectorGWPAR5FFI",0,30,
    "emissBySectorGTPAR5FFI",0,30)

  charts<-metis.chartsProcess(rTable=rtx, # Default is NULL
                              #dataTables=dataTables_i, # Default is NULL
                              paramsSelect=unlist(mp_ix$param), # Default is "All"
                              regionsSelect=regionsSelect_i, # Default is "All"
                              scensSelect=scensSelect_i,
                              xCompare=c("2010","2020","2030","2050"), # Default is c("2015","2030","2050","2100")
                              scenRef="Reference", # Default is NULL
                              dirOutputs=paste(getwd(),"/outputs",sep=""), # Default is paste(getwd(),"/outputs",sep="")
                              pdfpng="png", # Default is "png"
                              regionCompareOnly=0, # Default is "0"
                              scenarioCompareOnly=0, # Default is "0"
                              useNewLabels=1,
                              xRange=c(2010,2015,2020,2025,2030,2035,2040,2045,2050),
                              scaleRange=scaleRange_ix,
                              colOrder1 =scensSelect_i ,
                              colOrderName1 = "scenario",
                              folderName = "Comb_All",
                              multiPlotFigsOnly = F,
                              mp = mp_ix, multiPlotOn = F,
                              multiPlotFigLabels=F,pointsOn=pointsOn_i,
                              facetLabelSize=25,
                              figWidth=13,figHeight=9,
                              facetLabelColor = "black", facetBGColor="transparent", facetBorderColor = "transparent")


  mp_ix<-list(paramSet=list(
    #c("WEL1"),
    c("WEL2")),
    param=list(
     # c("landAlloc","agProdByCrop","watWithdrawBySec",
     #   "energyFinalByFuelBySectorMTOE","elecByTechTWh","emissByGasGWPAR5FFI"),
      c("landAllocByCrop","agProdByCrop","watWithdrawBySec",
        "energyFinalByFuelBySectorMTOE",
        "elecByTechTWh","emissByGasGWPAR5FFI")),
    nColMax=list(
     # c(3),
      c(3))
  )

  charts<-metis.chartsProcess(rTable=rtx, # Default is NULL
                              #dataTables=dataTables_i, # Default is NULL
                              paramsSelect=unlist(mp_ix$param), # Default is "All"
                              regionsSelect=regionsSelect_i, # Default is "All"
                              scensSelect=scensSelect_i,
                              xCompare=c("2010","2020","2030","2050"), # Default is c("2015","2030","2050","2100")
                              scenRef="Reference", # Default is NULL
                              dirOutputs=paste(getwd(),"/outputs",sep=""), # Default is paste(getwd(),"/outputs",sep="")
                              pdfpng="png", # Default is "png"
                              regionCompareOnly=0, # Default is "0"
                              scenarioCompareOnly=1, # Default is "0"
                              useNewLabels=1,
                              xRange=c(2010,2015,2020,2025,2030,2035,2040,2045,2050),
                              scaleRange=scaleRange_i,
                              colOrder1 =scensSelect_i ,
                              colOrderName1 = "scenario",
                              folderName = "Comb_All",
                              multiPlotFigsOnly = T,
                              mp = mp_ix, multiPlotOn = T,
                              multiPlotFigLabels=F,pointsOn=pointsOn_i,
                              facetLabelSize=25,
                              figWidth=9,figHeight=9,
                              facetLabelColor = "black", facetBGColor="transparent", facetBorderColor = "transparent",
                              yMinDefault = 0,
                              yMaxDefault = NULL,
                              yMaxDiffAbsDefault = NULL,
                              yMinDiffAbsDefault  = NULL,
                              yMaxDiffPrcntDefault = 40,
                              yMinDiffPrcntDefault = -10)


  # rTable=rtx # Default is NULL
  # #dataTables=dataTables_i, # Default is NULL
  # paramsSelect=unlist(mp_ix$param) # Default is "All"
  # regionsSelect=regionsSelect_i # Default is "All"
  # scensSelect=scensSelect_i
  # xCompare=c("2010","2020","2030","2050") # Default is c("2015","2030","2050","2100")
  # scenRef="Reference" # Default is NULL
  # dirOutputs=paste(getwd(),"/outputs",sep="") # Default is paste(getwd(),"/outputs",sep="")
  # pdfpng="png" # Default is "png"
  # regionCompareOnly=0 # Default is "0"
  # scenarioCompareOnly=1 # Default is "0"
  # useNewLabels=1
  # xRange=c(2010,2015,2020,2025,2030,2035,2040,2045,2050)
  # scaleRange=scaleRange_i
  # colOrder1 =scensSelect_i
  # colOrderName1 = "scenario"
  # folderName = "Comb_All"
  # multiPlotFigsOnly = T
  # mp = mp_ix
  # multiPlotOn = T
  # multiPlotFigLabels=F
  # pointsOn=pointsOn_i
  # facetLabelSize=25
  # figWidth=12
  # figHeight=7
  # facetLabelColor = "black"
  # facetBGColor="transparent"
  # facetBorderColor = "transparent"
  # yMinDefault = 0
  # yMaxDefault = NULL
  # yMaxDiffAbsDefault = 2
  # yMinDiffAbsDefault  = -2
  # yMaxDiffPrcntDefault = 5
  # yMinDiffPrcntDefault = -5




  mp_ix<-list(paramSet=list(
    c("WEL1NoLabels")),
    param=list(
      c("landAlloc","agProdByCrop","watWithdrawBySec",
        "energyFinalByFuelBySectorMTOE","elecByTechTWh","emissByGasGWPAR5FFI")),
    nColMax=list(
      c(3))
  )

  scaleRange_ix = scaleRange_i = tibble::tribble(
    ~param,~minScale, ~maxScale,
    "emissByGasGWPAR5FFI",0,60,
    "emissByGasGTPAR5FFI",0,60,
    "emissBySectorGWPAR5FFI",0,30,
    "emissBySectorGTPAR5FFI",0,30)


  charts<-metis.chartsProcess(rTable=rtx, # Default is NULL
                              #dataTables=dataTables_i, # Default is NULL
                              paramsSelect=unlist(mp_ix$param), # Default is "All"
                              regionsSelect=regionsSelect_i, # Default is "All"
                              scensSelect=scensSelect_i,
                              xCompare=c("2010","2020","2030","2050"), # Default is c("2015","2030","2050","2100")
                              scenRef="Reference", # Default is NULL
                              dirOutputs=paste(getwd(),"/outputs",sep=""), # Default is paste(getwd(),"/outputs",sep="")
                              pdfpng="png", # Default is "png"
                              regionCompareOnly=0, # Default is "0"
                              scenarioCompareOnly=1, # Default is "0"
                              useNewLabels=1,
                              xRange=c(2010,2015,2020,2025,2030,2035,2040,2045,2050),
                              scaleRange=scaleRange_i,
                              colOrder1 =scensSelect_i ,
                              colOrderName1 = "scenario",
                              folderName = "Comb_All",
                              multiPlotFigsOnly = T,
                              mp = mp_ix, multiPlotOn = T,
                              multiPlotFigLabels=F,pointsOn=pointsOn_i,
                              #facetLabelSize=40,
                              figWidth=7,figHeight=4,
                              facetLabelColor = "transparent", facetBGColor="transparent", facetBorderColor = "transparent")

  mp_ix<-list(paramSet=list(
    c("WEL2NoLabels")),
    param=list(
      c("landAllocByCrop","agProdByCrop","watWithdrawBySec",
        "energyFinalByFuelBySectorMTOE","elecByTechTWh","emissByGasGWPAR5FFI")),
    nColMax=list(
      c(3))
  )

  scaleRange_ix = scaleRange_i = tibble::tribble(
    ~param,~minScale, ~maxScale,
    "emissByGasGWPAR5FFI",0,60,
    "emissByGasGTPAR5FFI",0,60,
    "emissBySectorGWPAR5FFI",0,30,
    "emissBySectorGTPAR5FFI",0,30)


  charts<-metis.chartsProcess(rTable=rtx, # Default is NULL
                              #dataTables=dataTables_i, # Default is NULL
                              paramsSelect=unlist(mp_ix$param), # Default is "All"
                              regionsSelect=regionsSelect_i, # Default is "All"
                              scensSelect=scensSelect_i,
                              xCompare=c("2010","2020","2030","2050"), # Default is c("2015","2030","2050","2100")
                              scenRef="Reference", # Default is NULL
                              dirOutputs=paste(getwd(),"/outputs",sep=""), # Default is paste(getwd(),"/outputs",sep="")
                              pdfpng="png", # Default is "png"
                              regionCompareOnly=0, # Default is "0"
                              scenarioCompareOnly=1, # Default is "0"
                              useNewLabels=1,
                              xRange=c(2010,2015,2020,2025,2030,2035,2040,2045,2050),
                              scaleRange=scaleRange_i,
                              colOrder1 =scensSelect_i ,
                              colOrderName1 = "scenario",
                              folderName = "Comb_All",
                              multiPlotFigsOnly = T,
                              mp = mp_ix, multiPlotOn = T,
                              multiPlotFigLabels=F,pointsOn=pointsOn_i,
                              #facetLabelSize=40,
                              figWidth=7,figHeight=4,
                              facetLabelColor = "transparent", facetBGColor="transparent", facetBorderColor = "transparent")


}

#----------------------------
# Reference All
#----------------------------


if(F){
  scensSelect_i = c("GCAMOrig", "GCAMRef")


  charts<-metis.chartsProcess(rTable=rTable_i, # Default is NULL
                              dataTables=dataTables_i, # Default is NULL
                              #paramsSelect=c("landAlloc","landAllocByCrop","agProdByCrop"), # Default is "All"
                              paramsSelect=paramsSelect_iMod, # Default is "All"
                              regionsSelect=regionsSelect_i, # Default is "All"
                              scensSelect=scensSelect_i,
                              xCompare=c("2010","2015","2020","2030"), # Default is c("2015","2030","2050","2100")
                              scenRef="GCAMRef", # Default is NULL
                              dirOutputs=paste(getwd(),"/outputs",sep=""), # Default is paste(getwd(),"/outputs",sep="")
                              pdfpng="png", # Default is "png"
                              regionCompareOnly=0, # Default is "0"
                              scenarioCompareOnly=0, # Default is "0"
                              useNewLabels=1,
                              xRange=c(2010,2015,2020,2025,2030,2035,2040,2045,2050),
                              colOrder1 =scensSelect_i,
                              colOrderName1 = "scenario",
                              folderName = "OrigRef_all_ERL",
                              scaleRange=scaleRange_i,
                              multiPlotFigsOnly = F,
                              mp=mp_i, multiPlotOn = T,
                              multiPlotFigLabels=T,pointsOn=pointsOn_i)


}







