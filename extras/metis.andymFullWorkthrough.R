



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
if("ggplot2" %in% rownames(installed.packages()) == F){install.packages("ggplot2")}
library(ggplot2)



#------------------------
# Run Bia to create distributed electricity generation
#------------------------


dirOutputs=paste(getwd(),"/outputs",sep="")

if (!dir.exists(dirOutputs)){                                        #these directory checks and creation I coped out of grid2poly
  dir.create(dirOutputs)}
if (!dir.exists(paste(dirOutputs, "/Grids", sep = ""))){
  dir.create(paste(dirOutputs, "/Grids", sep = ""))}

if (!dir.exists(paste(dirOutputs, "/Grids/diagnostics",sep=""))){
  dir.create(paste(dirOutputs, "/Grids/diagnostics",sep=""))}


biaOutputsFolder=paste(getwd(),"/dataFiles/grids/bia/biaOutputs",sep="")
biaInputsFolder=paste(getwd(),"/dataFiles/grids/bia/biaInputs",sep="")

gcamdatabasePath <-paste(getwd(),"/dataFiles/gcam",sep="")
gcamdatabaseName <-"example_database_basexdb"
dataProjPath<-gcamdatabasePath
queryPath<-gcamdatabasePath
gcamdataProjFile <-"Example_dataProj.proj"
dataProj=gcamdataProjFile  #andym
scenOrigNames=c("ExampleScen1","ExampleScen2")
scenNewNames=c("Eg1","Eg2")
queryxml="metisQueries.xml"
queriesSelect = "All"      #andym
regionsSelect <- c('Argentina')
#regionsSelect <- c('Argentina', 'Colombia')
paramsSelect<- c("elecByTech", "elecCapBySubsector")

reReadData=F

biaInputsFiles=c("global_power_plant_database_MW")

biaScenarioAssign="Eg1"

#gridChoice<-"grid_050"
gridChoice<-"grid_025"

diagnosticsON<-F


dataBia1<-metis.bia(
  biaInputsFolder=biaInputsFolder,
  biaInputsFiles=biaInputsFiles,
  biaScenarioAssign=biaScenarioAssign,
  #regionsSelect=regionsSelect, # Default Value is NULL
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
  diagnosticsON = diagnosticsON
)


# biaInputsFolder=biaInputsFolder
# biaInputsFiles=biaInputsFiles
# biaScenarioAssign=biaScenarioAssign
# biaOutputsFolder=biaOutputsFolder
# regionsSelect=regionsSelect # Default Value is NULL
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


dataBia1<-dataBia1%>%select(-value, -origValue)%>%
  dplyr::mutate(aggType = "vol")%>%
  dplyr::rename(lat = gridlat, lon = gridlon, class = class1, value = valueDistrib, origValue = origValueDistrib)

dataBia1 <-  dataBia1 %>%
  ungroup() %>%
  select(-gridCellPercentage,-region,-region_32_code,-ctry_name,-ctry_code, -aggregate, -contains("orig"),-gridID)

# dataBia<-dataBia%>%select(-value, -origValue)%>%
#   dplyr::mutate(aggType = "vol")%>%
#   dplyr::rename(lat = gridlat, lon = gridlon, class = class1, value = valueDistrib, origValue = origValueDistrib)
#
# dataBia <-  dataBia %>%
#   ungroup() %>%
#   select(-gridCellPercentage,-region,-region_32_code,-ctry_name,-ctry_code, -aggregate, -contains("orig"),-gridID)



