

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
             reReadData=1,
             gridMetisData=paste(dirOutputs, "/Grids/gridMetis.RData", sep = ""))


# Grid to Shape
# gridMetisData<-paste(getwd(),"/outputs/Grids/gridMetis.RData",sep="")
# load(gridMetisData) # grid is called gridMetis
# grid<-gridMetis



