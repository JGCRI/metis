
# metis.master.R
# Script to run different parts of the metis package.

#----------------------------
# Install necessary packages
#----------------------------
if("devtools" %in% rownames(installed.packages()) == F){install.packages("devtools")}
library(devtools)
if("metis" %in% rownames(installed.packages()) == F){install_github(repo="JGCRI/rgcam")}
library(metis)
if("rgcam" %in% rownames(installed.packages()) == F){install_github(repo="JGCRI/rgcam")}
library(rgcam)
if("tibble" %in% rownames(installed.packages()) == F){install.packages("tibble")}
library(tibble)
if("rgdal" %in% rownames(installed.packages()) == F){install.packages("rgdal")}
library(rgdal)
if("tmap" %in% rownames(installed.packages()) == F){install.packages("tmap")}
library(tmap)
if("dplyr" %in% rownames(installed.packages()) == F){install.packages("dplyr")}
library(dplyr)
if("zoo" %in% rownames(installed.packages()) == F){install.packages("zoo")}
library(zoo)
if("dbplyr" %in% rownames(installed.packages()) == F){install.packages("dbplyr")}
library(dbplyr)
if("RSQLite" %in% rownames(installed.packages()) == F){install.packages("RSQLite")}
library(RSQLite)
if("ggplot2" %in% rownames(installed.packages()) == F){install.packages("ggplot2")}
library(ggplot2)
if("ggalluvial" %in% rownames(installed.packages()) == F){install.packages("ggalluvial")}
library(ggalluvial)

#----------------------------
# Read GCAM Data (metis.readgcam.R)
#---------------------------

# Connect to gcam database or project
  gcamdatabasePath_i <-'G:/IDBNexus/Final' # 'C:/Users/twild/Downloads/pic'  #  # Use if gcamdatabase is needed
  gcamdatabaseName_i <-"Sep272019" # "Reference_originalSW" Use if gcamdatabse is needed
  dataProjPath_i <- paste(getwd(),"/outputs",sep="") # Path to dataProj file.
  dataProj_i <-"IDBNexusFinal.proj"  # Use if gcamdata has been saved as .proj file

# Get list of scenarios and rename if desired.
#  rgcam::localDBConn(gcamdatabasePath_i,gcamdatabaseName_i) # if connecting directly to gcam database
#  dataProjLoaded <- loadProject(paste(dataProjPath_i, "/",dataProj_i , sep = ""))
#  listScenarios(dataProjLoaded)  # List of Scenarios in GCAM database
  scenOrigNames_i = c('GCAMOriginal', "Reference", "Impacts", "Policy") #  c(, "DeepDecarb1Mkt_2DS")
  scenNewNames_i = c("Original",  "Reference", "Climate Impacts", "Climate Policy") #  c(, "DeepDecarb1Mkt_2DS"")  # Names to replace the original names for final figures.

# Choose Parameters or set to "All" for all params. For complete list see ?metis.readgcam
  paramsSelect_i = "All"

# Select regions from the 32 GCAM regions.
  regionsSelect_i <- c("Colombia", "Argentina", "Uruguay")

  # Reading in the no bio query so it works with Rgcam

  dataGCAM<-metis.readgcam(reReadData = T,  # F
                           gcamdatabasePath = gcamdatabasePath_i ,
                           gcamdatabaseName = gcamdatabaseName_i,
                           scenOrigNames = scenOrigNames_i,
                           scenNewNames = scenNewNames_i,
                           dataProj = dataProj_i,
                           dataProjPath = dataProjPath_i,
                           regionsSelect = regionsSelect_i,
                           paramsSelect=paramsSelect_i)

    # reReadData = T  # F
  # gcamdatabasePath = gcamdatabasePath_i
  # gcamdatabaseName = gcamdatabaseName_i
  # scenOrigNames = scenOrigNames_i
  # scenNewNames = scenNewNames_i
  # #dataProj = dataProj_i
  # #dataProjPath = dataProjPath_i
  # regionsSelect = regionsSelect_i
  # paramsSelect=paramsSelect_i

  dataGCAM$data # To view the data read that was read.

#------------------------------------------------------------------------------------------
# Charts Process (metis.chartsProcess.R)
#------------------------------------------------------------------------------------------

# Can also add data .csv outputs from metis.readgcam.R which are autmatically saved in
  # ./metis/outputs/readGCAMTables/Tables_gcam
  # for each of the regions selected.
  # gcamDataTable_Argentina.csv, gcamDataTable_China.csv, gcamDataTable_Pakistan.csv
  # This would be added to dataTables_i as:
  dataTables_i = c(paste(getwd(), "/outputs/readGCAMTables/Tables_local/local_Regional_Colombia.csv", sep = "")
                   #paste(getwd(), "/outputs/readGCAMTables/Tables_gcam/gcamDataTable_Colombia.csv", sep = "")
                   )

# Read in the data from the function metis.readgcam.
  rTable_i <- dataGCAM$data;

# Choose Parameters or set to "All" for all params. For complete list see ?metis.chartsProcess

  paramsSelect_i <- c("finalNrgbySec", "TranspFinalNrgByFuel", "BuildFinalNrgByFuel",
                   "IndFinalNrgByFuel", "primNrgConsumByFuel", "elecByTech", "watWithdrawBySec",
                   "aggLandAlloc", "LUCemiss", "nonco2emissionBySectorGWPAR5",
                   "finalNrgbyFuel","finalElecbySec","finalElecbyFuel",
                   "NonCo2EmissionsByResProdGWPAR5",
                   "TotalFFIEmissBySec", "CO2BySector_NonCO2Gases_GWPAR5", "CO2BySector_NonCO2Gases_GWPAR5_LUC",
                   "TotalEmissBySec", "LandAllocByCrop", "MethaneBySource", "PassengerVMTByMode", "FreightVMTByMode",
                   "BuildFinalNrgBySector",
                   "co2emissionBySectorNoBio", "PassengerVMTByFuel", "FreightVMTByFuel", "RefiningByLiq")
  paramsSelect_i <- "All"

# Select regions from the 32 GCAM regions.
# paramsSelect_i <- c('BuildFinalNrgBySector')
# Charts Process
  charts<-metis.chartsProcess(
                          rTable=rTable_i, # Default is NULL
                          #dataTables=dataTables_i, # Default is NULL
                          paramsSelect=paramsSelect_i, # Default is "All"
                          regionsSelect=regionsSelect_i, # Default is "All"
                          xCompare=c("2010","2030","2050"), # Default is c("2015","2030","2050","2100")
                          scenRef="Reference", # Default is NULL
                          dirOutputs=paste(getwd(),"/outputs",sep=""), # Default is paste(getwd(),"/outputs",sep="")
                          regionCompareOnly=0, # Default 0. If set to 1, will only run comparison plots and not individual
                          scenarioCompareOnly=0,
                          useNewLabels = 0,
                          folderName = "IDBNexusFinal",
                          xRange = c(2020, 2030, 2040, 2050),
                          colOrder1 = c("Original",  "Reference", "Climate Impacts", "Climate Policy"),
                          colOrderName1 = "scenario") # Default 0. If set to 1, will only run comparison plots and not individual
