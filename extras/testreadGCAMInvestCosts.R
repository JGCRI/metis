#----------------------------
# Paper metis USA data
#----------------------------

# Paper Repo: https://github.com/zarrarkhan/paperMetisUSA


#----------------------------
# Load Libraries
#----------------------------

library(metis); library(tidyr); library(dplyr)

#----------------------------
# Global Directories
#----------------------------

dirOutputs_i = "C:/Z/projects/metisGCAMUSA/metisOutputs"
folderName_i="testCheckInvest"

#------------------------------
# GCAM data (Water-Energy-Food by States-Basin-GLU)
#-------------------------------

gcamdatabase_i <-paste("C:/Z/projects/metisGCAMUSA/gcam-core/output/metisUSAOld",sep="")
#gcamdatabase_i <-"C:/Z/example_database_basexdb"
rgcam::localDBConn("C:/Z/projects/metisGCAMUSA/gcam-core/output","metisUSAOld") # Note names of scenarios
dataProjFile_i <- "testCheckInvest.proj"
reReadData_i <- T
scenOrigNames_i <-c("GCAMUSARef","GCAMUSAWatConst")
scenNewNames_i <-c("Ref","WatConst")
paramsSelect_i <- c("elecCumRetPrematureGW")
regionsSelect_i <- c(metis.assumptions()$US52,"USA", "Colombia")

dataGCAMx<-metis.readgcam(reReadData = reReadData_i,
                         gcamdatabase = gcamdatabase_i,
                         scenOrigNames = scenOrigNames_i,
                         scenNewNames = scenNewNames_i,
                         dataProjFile = dataProjFile_i,
                         regionsSelect = regionsSelect_i ,
                         paramsSelect=paramsSelect_i,
                         folderName = folderName_i,
                         dirOutputs = dirOutputs_i)

# reReadData = reReadData_i
# gcamdatabase = gcamdatabase_i
# scenOrigNames = scenOrigNames_i
# scenNewNames = scenNewNames_i
# dataProjFile = dataProjFile_i
# regionsSelect = regionsSelect_i
# paramsSelect=paramsSelect_i
# folderName = folderName_i,
# dirOutputs = dirOutputs_i

dfx <- dataGCAMx$data
unique(dfx$scenario)
unique(dfx$param)
unique(dfx$region)

#------------------------------
# GCAM data (Water-Energy-Food by States-Basin-GLU)
#-------------------------------

# Read in the data from the function metis.readgcam.
rTable_i <- dfx
paramsSelect_i=c("elecCumRetPrematureGW")
regionsSelect_i=c("Colombia")

# Charts Process
charts<-metis.chartsProcess(rTable=rTable_i, # Default is NULL
                            paramsSelect=paramsSelect_i, # Default is "All"
                            regionsSelect=regionsSelect_i, # Default is "All"
                            scenRef="Ref", # Default is NULL
                            dirOutputs=dirOutputs_i, # Default is paste(getwd(),"/outputs",sep="")
                            regionCompareOnly=0, # Default 0. If set to 1, will only run comparison plots and not individual
                            scenarioCompareOnly=0, # Default 0. If set to 1, will only run comparison plots and not individual
                            folderName = folderName_i)


# rTable=rTable_i # Default is NULL
# paramsSelect=paramsSelect_i  # Default is "All"
# regionsSelect=regionsSelect_i  # Default is "All"
# scenRef="Ref"  # Default is NULL
# dirOutputs=dirOutputs_i  # Default is paste(getwd(),"/outputs",sep="")
# regionCompareOnly=0  # Default 0. If set to 1, will only run comparison plots and not individual
# scenarioCompareOnly=0  # Default 0. If set to 1, will only run comparison plots and not individual
# folderName = folderName_i
