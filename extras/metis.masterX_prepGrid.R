
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



#------------------------
# Prepare Grids
#------------------------

dirOutputs=paste(getwd(),"/outputs",sep="")
demeterFolder=paste(getwd(),"/dataFiles/grids/demeter/",sep="")
demeterScenario="Eg1"
demeterUnits="Landuse (Fraction)"
demeterTimesteps<-seq(from=2005,to=2020,by=5)
tethysFolder=paste(getwd(),"/dataFiles/grids/tethys/",sep="")
tethysScenario="Eg1"
tethysFiles=c("wddom","wdelec","wdirr","wdliv","wdmfg","wdmin","wdnonag","wdtotal")
tethysUnits="Water Withdrawals (mm)"
xanthosFolder=paste(getwd(),"/dataFiles/grids/xanthos/",sep="")
xanthosScenarioAssign="Eg1"
xanthosFiles=c("q_mmperyear_Reference")
xanthosCoordinatesPath=paste(getwd(),"/dataFiles/grids/xanthosReference/coordinates.csv",sep="")
xanthosGridAreaHecsPath=paste(getwd(),"/dataFiles/grids/xanthosReference/Grid_Areas_ID.csv",sep="")
biaFolder=paste(getwd(),"/dataFiles/grids/bia/",sep="")
biaScenarioAssign="Eg1"
biaFiles=c("global_power_plant_database_MW")
#biaUnits="Capacity (MW)"   #andym   ?Should we have biaUnits?
spanLowess=0.25
popFolder<-paste(getwd(),"/dataFiles/grids/griddedIDsPop/",sep="")
popFiles<-"grid_pop_map"
popUnits<-"person"
gridMetisData=paste(dirOutputs, "/Grids/gridMetisXanthos.RData", sep = "")
#sqliteUSE = T andym
sqliteUSE = F #andym
sqliteDBNamePath =paste(getwd(),"/outputs/Grids/gridMetis.sqlite", sep = "")

xanthosUnits="Runoff (mm)"  #andym  I thought that xanthosUnits was supposed to be taken out of prepGrid entirely, but it seems to be needed
reReadData=T  #andym
scarcityXanthosRollMeanWindow=10   #andym

gridMetis<-metis.prepGrid(
             # demeterFolder=demeterFolder,
             # demeterScenario=demeterScenario,
             # demeterTimesteps=demeterTimesteps,
             # demeterUnits=demeterUnits,
             # tethysFolder=tethysFolder,
             # tethysScenario=tethysScenario,
             # tethysFiles=tethysFiles,
             # tethysUnits=tethysUnits,
             xanthosFolder=xanthosFolder,
             xanthosScenarioAssign=xanthosScenarioAssign,   #andym from xanthosScenario=xanthosScenario,
             #xanthosUnits=xanthosUnits,
             xanthosFiles=xanthosFiles,
             xanthosCoordinatesPath=xanthosCoordinatesPath,
             xanthosGridAreaHecsPath=xanthosGridAreaHecsPath,
             biaFolder=biaFolder,
             biaFiles=biaFiles,
             biaScenarioAssign=biaScenarioAssign,
             scarcityXanthosRollMeanWindow=scarcityXanthosRollMeanWindow,
             dirOutputs=dirOutputs,
             reReadData=reReadData,
             gridMetisData=gridMetisData,
             sqliteUSE = sqliteUSE,
             sqliteDBNamePath = sqliteDBNamePath
             )



# demeterFolder=demeterFolder
# demeterScenario=demeterScenario
# demeterTimesteps=demeterTimesteps
# demeterUnits=demeterUnits
# tethysFolder=tethysFolder
# tethysScenario=tethysScenario
# tethysFiles=tethysFiles
# tethysUnits=tethysUnits
# xanthosFolder=xanthosFolder
# xanthosScenario=xanthosScenario
# xanthosUnits=xanthosUnits
# xanthosFiles=xanthosFiles
# xanthosCoordinatesPath=xanthosCoordinatesPath
# scarcityXanthosRollMeanWindow=scarcityXanthosRollMeanWindow
# dirOutputs=dirOutputs
# reReadData=reReadData
# gridMetisData=gridMetisData


# Grid to Shape
# gridMetisData<-paste(getwd(),"/outputs/Grids/gridMetis.RData",sep="")
# load(gridMetisData) # grid is called gridMetis
# grid<-gridMetis



