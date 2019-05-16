
#----------------------------
# Install necessary packages
#----------------------------
if("devtools" %in% rownames(installed.packages()) == F){install.packages("devtools")}
library(devtools)
#if("metis" %in% rownames(installed.packages()) == F){install_github(repo="zarrarkhan/metis")}    #andym : should this be changed to JGCRI/metis ?_?
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
if("ggplot2" %in% rownames(installed.packages()) == F){install.packages("rgeos")}
library(ggplot2)



#------------------------
# Run Bia to create distributed electricity generation
#------------------------


biaOutputsFolder=paste(getwd(),"/dataFiles/grids/bia/biaOutputs",sep="")
biaInputsFolder=paste(getwd(),"/dataFiles/grids/bia/biaInputs",sep="")


gcamdatabasePath <-paste(getwd(),"/dataFiles/gcam",sep="")
gcamdatabaseName <-"example_database_basexdb"
#gcamdatabasePath <-paste("D:/ESSIC/my_gcam_workspace/output",sep="")
#gcamdatabasePath <-paste("D:/ESSIC",sep="")
#dataProjPath<-paste("D:/ESSIC/my_gcam_workspace/output",sep="")
#queryPath<-paste(getwd(),"/dataFiles/gcam",sep="")
#queryPath<-paste("D:/ESSIC/my_gcam_workspace/output",sep="")
#gcamdatabaseName <-"database_basexdb"


dataProjPath<-gcamdatabasePath
#dataProjPath<-paste(getwd(),"/dataFiles/gcam",sep="")
queryPath<-gcamdatabasePath
gcamdataProjFile <-"Example_dataProj.proj"
#gcamdataProjFile <-"example_from_example_database_Proj.proj"    #andym
dataProj=gcamdataProjFile  #andym

scenOrigNames=c("ExampleScen1","ExampleScen2")
scenNewNames=c("Eg1","Eg2")

# Use function localDBConn from package rgcam to get a list of scenarios if needed.
# localDBConn(gcamdatabasePath,gcamdatabaseName)
#dataProjLoaded <- loadProject(paste(gcamdatabasePath, "/", dataProj, sep = ""))
#  listScenarios(dataProjLoaded)  # List of Scenarios in GCAM database
# queries <- listQueries(dataProjLoaded)  # List of Queries in queryxml

queryxml="metisQueries.xml"
queriesSelect = "All"      #andym
#regionsSelect = "All"      #andym

#regionsSelect <- c('Colombia', 'Argentina', 'Japan')     #andym used this previously for tests, until May 16 2019
regionsSelect <- c('Argentina')
#regionsSelect <- NULL
#paramsSelect<-"All"
paramsSelect<- c("elecByTech", "elecCapBySubsector")
#paramsSelect<- c("elecByTech")

reReadData=F


biaInputsFiles=c("global_power_plant_database_MW")

biaScenarioAssign="Eg1"

gridChoice<-"grid_050"
#gridChoice<-"grid_025"

sqliteUSE = F


# biaInputsFiles=c("global_power_plant_database_MW")
# #biaUnits="Capacity (MW)"   #andym   ?Should we have biaUnits?
# popFolder<-paste(getwd(),"/dataFiles/grids/griddedIDsPop/",sep="")
# popFiles<-"grid_pop_map"
# popUnits<-"person"
# #gridMetisData=paste(dirOutputs, "/Grids/gridMetisBIA.RData", sep = "")
# #sqliteUSE = T andym
# sqliteUSE = F #andym
# sqliteDBNamePath =paste(getwd(),"/outputs/Grids/gridBIA.sqlite", sep = "")



biaDistElecGen<-metis.bia(
            biaInputsFolder=biaInputsFolder,
            biaInputsFiles=biaInputsFiles,
            biaScenarioAssign=biaScenarioAssign,
            biaOutputsFolder=biaOutputsFolder,
            sqliteUSE = sqliteUSE,
            sqliteDBNamePath = sqliteDBNamePath,
            regionsSelect=regionsSelect, # Default Value is NULL
            queriesSelect = queriesSelect, # Default value is "ALL"
            reReadData=reReadData, # Default Value is T
            dataProj=dataProj, # Default Value is "dataProj.proj"
            dataProjPath=dataProjPath, #Default Value is gcamdatabasePath
            scenOrigNames=scenOrigNames,
            scenNewNames=scenNewNames,
            gcamdatabasePath=gcamdatabasePath,
            gcamdatabaseName=gcamdatabaseName,
            queryxml=queryxml,  # Default Value is "metisQueries.xml"
            paramsSelect=paramsSelect, # Default = c("elecByTech", "elecCapBySubsector")
            gridChoice = gridChoice # Default = "grid_050"

)




# biaFolder=biaFolder
# biaFiles=biaFiles
# biaScenarioAssign=biaScenarioAssign
# biaOutputsFolder=biaOutputsFolder
# reReadData=reReadData
# gridMetisData=gridMetisData
# sqliteUSE = sqliteUSE
# sqliteDBNamePath = sqliteDBNamePath
# regionsSelect = NULL
# queriesSelect="All"

##andym old stuff:
# Grid to Shape
# gridMetisData<-paste(getwd(),"/outputs/Grids/gridMetis.RData",sep="")
# load(gridMetisData) # grid is called gridMetis
# grid<-gridMetis



