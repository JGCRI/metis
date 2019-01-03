
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
xanthosScenario="Eg1"
xanthosUnits="Runoff (mm)"
xanthosFiles=c("q_mmperyear_Reference")
xanthosCoordinatesPath=paste(getwd(),"/dataFiles/grids/xanthosCoords/coordinates.csv",sep="")
scarcityXanthosRollMeanWindow=10
popFolder<-paste(getwd(),"/dataFiles/grids/griddedIDsPop/",sep="")
popFiles<-"grid_pop_map"
popUnits<-"person"
reReadData=1
gridMetisData=paste(dirOutputs, "/Grids/gridMetis.RData", sep = "")


gridMetis<-metis.prepGrid(
             demeterFolder=demeterFolder,
             demeterScenario=demeterScenario,
             demeterTimesteps=demeterTimesteps,
             demeterUnits=demeterUnits,
             tethysFolder=tethysFolder,
             tethysScenario=tethysScenario,
             tethysFiles=tethysFiles,
             tethysUnits=tethysUnits,
             xanthosFolder=xanthosFolder,
             xanthosScenario=xanthosScenario,
             xanthosUnits=xanthosUnits,
             xanthosFiles=xanthosFiles,
             xanthosCoordinatesPath=xanthosCoordinatesPath,
             scarcityXanthosRollMeanWindow=scarcityXanthosRollMeanWindow,
             dirOutputs=paste(getwd(),"/outputs",sep=""),
             reReadData=reReadData,
             gridMetisData=gridMetisData)


# Grid to Shape
# gridMetisData<-paste(getwd(),"/outputs/Grids/gridMetis.RData",sep="")
# load(gridMetisData) # grid is called gridMetis
# grid<-gridMetis



