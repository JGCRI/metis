
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

dirbiaOutputs=paste(getwd(),"/dataFiles/grids/bia/biaOutputs",sep="")
dirbiaInputs=paste(getwd(),"/dataFiles/grids/bia/biaInputs",sep="")

biaScenarioAssign="Eg1"
biaFiles=c("global_power_plant_database_MW")
#biaUnits="Capacity (MW)"   #andym   ?Should we have biaUnits?
popFolder<-paste(getwd(),"/dataFiles/grids/griddedIDsPop/",sep="")
popFiles<-"grid_pop_map"
popUnits<-"person"
gridMetisData=paste(dirOutputs, "/Grids/gridMetisBIA.RData", sep = "")
#sqliteUSE = T andym
sqliteUSE = F #andym
sqliteDBNamePath =paste(getwd(),"/outputs/Grids/gridBIA.sqlite", sep = "")

reReadData=T  #andym

biaDistElecGen<-metis.bia(
            biaFolder=biaFolder,
            biaFiles=biaFiles,
            biaScenarioAssign=biaScenarioAssign,
            dirOutputs=dirOutputs,
            reReadData=reReadData,
            gridMetisData=gridMetisData,
            sqliteUSE = sqliteUSE,
            sqliteDBNamePath = sqliteDBNamePath
)


# biaFolder=biaFolder
# biaFiles=biaFiles
# biaScenarioAssign=biaScenarioAssign
# dirOutputs=dirOutputs
# reReadData=reReadData
# gridMetisData=gridMetisData
# sqliteUSE = sqliteUSE
# sqliteDBNamePath = sqliteDBNamePath

# Grid to Shape
# gridMetisData<-paste(getwd(),"/outputs/Grids/gridMetis.RData",sep="")
# load(gridMetisData) # grid is called gridMetis
# grid<-gridMetis



