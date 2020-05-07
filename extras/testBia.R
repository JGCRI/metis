library(metis); library(dplyr); library(tidyr); library(magrittr)

#--------------------
# Run Bia
#-------------------

dirOutputs=paste(getwd(),"/outputs",sep="");
dirOutputs

if (!dir.exists(dirOutputs)){                                        #these directory checks and creation I coped out of grid2poly
  dir.create(dirOutputs)}
if (!dir.exists(paste(dirOutputs, "/Grids", sep = ""))){
  dir.create(paste(dirOutputs, "/Grids", sep = ""))}

if (!dir.exists(paste(dirOutputs, "/Grids/diagnostics",sep=""))){
  dir.create(paste(dirOutputs, "/Grids/diagnostics",sep=""))}


biaOutputsFolder=paste(getwd(),"/dataFiles/grids/bia/biaOutputs",sep="")
biaInputsFolder=paste(getwd(),"/dataFiles/grids/bia/biaInputs",sep="")

#gcamdatabasePath <-paste(getwd(),"/dataFiles/gcam",sep="")
gcamdatabasePath <-paste("C:/Z/projects/metisGCAMUSA/gcam-core/output",sep="")
gcamdatabaseName <-"metisUSA"
dataProjPath<-"C:/Z/metis/outputs/Bia"
queryPath<-"C:/Z/metis/dataFiles/gcam"
gcamdataProjFile <-"metisUSA_dataProj.proj"
dataProj=gcamdataProjFile  #andym
reReadData=F
scenOrigNames=c("GCAMUSARef","GCAMUSAWatConst")
scenNewNames=c("Ref","WatConst")
queryxml="metisQueries.xml"
queriesSelect = "electricity"      #andym
paramsSelect<- c("elecByTechTWh", "elecCapByFuel")
biaInputsFiles=c("global_power_plant_database_MW")
#gridChoice<-"grid_050"
gridChoice<-"grid_050" # grid_050 or grid_025
diagnosticsON<-T
#diagnosticsON<-T
#regionsSelect <- c('TX','China','Pakistan', 'USA')
regionsSelect <- c(metis.assumptions()$US52)
# regionsSelect <- c(metis.assumptions()$US52)
# regionsSelect <- c('Colombia')
#regionsSelect <- c('Argentina')

dataBia<-metis.bia(
  biaInputsFolder=biaInputsFolder,
  biaInputsFiles=biaInputsFiles,
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
  gridChoice = gridChoice, # Default = "grid_050"
  diagnosticsON = diagnosticsON,
  subsectorNAdistribute = "even",
  nameAppend=paste("_even",sep=""))

dataBia%>%
  dplyr::filter(region %in% metis.assumptions()$US52)%>%
  dplyr::select(scenario,region,param,class1,x,value,units)%>%
  dplyr::group_by(scenario,param,x,units)%>%
  dplyr::summarize(sumVal=sum(value))%>%dplyr::filter(x==2015)

dataBia<-metis.bia(
  biaInputsFolder=biaInputsFolder,
  biaInputsFiles=biaInputsFiles,
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
  gridChoice = gridChoice, # Default = "grid_050"
  diagnosticsON = diagnosticsON,
  subsectorNAdistribute = "totalOther",
  nameAppend="_totalOther")

dataBia%>%
  dplyr::filter(region %in% metis.assumptions()$US52)%>%
  dplyr::select(scenario,region,param,class1,x,value,units)%>%
  dplyr::group_by(scenario,param,x,units)%>%
  dplyr::summarize(sumVal=sum(value,na.rm=T))%>%dplyr::filter(x==2015)

# biaInputsFolder=biaInputsFolder
# biaInputsFiles=biaInputsFiles
# regionsSelect=regionsSelect# Default Value is NULL
# queriesSelect = queriesSelect # Default value is "ALL"
# reReadData=reReadData # Default Value is T
# dataProj=dataProj # Default Value is "dataProj.proj"
# dataProjPath=dataProjPath #Default Value is gcamdatabasePath
# scenOrigNames=scenOrigNames
# scenNewNames=scenNewNames
# gcamdatabasePath=gcamdatabasePath
# gcamdatabaseName=gcamdatabaseName
# queryxml=queryxml  # Default Value is "metisQueries.xml"
# paramsSelect=paramsSelect # Default = c("elecByTech", "elecCapBySubsector")
# gridChoice = gridChoice # Default = "grid_050"
# diagnosticsON = diagnosticsON
# subsectorNAdistribute = "even"
# nameAppend=""
# folderName=NULL
# regionsSelectDiagnostic=c("CA","TX")
