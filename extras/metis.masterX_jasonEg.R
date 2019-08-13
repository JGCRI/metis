
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


# Read in Boundary Region
# Read in the GCAM 32 regions shapefile which comes with metis.
boundaryRegShpFolder_i <- paste(getwd(),"/dataFiles/gis/metis/gcam",sep="")
boundaryRegShpFile_i <- paste("region32_0p5deg_regions",sep="")
boundaryRegShp_i = readOGR(dsn=boundaryRegShpFolder_i,layer=boundaryRegShpFile_i,use_iconv=T,encoding='UTF-8')
head(boundaryRegShp_i@data)
boundaryRegCol_i = "region"
metis.map(dataPolygon=boundaryRegShp_i,fillColumn = boundaryRegCol_i,labels=F ,printFig=F,facetsON=F)


boundaryRegionsSelect_i = c("China") # Must be a region in the boundaryRegShp


# Read in subregion shapefile
# Read in the  SubBasin GCAM Basins shapefile which comes with metis.
subRegShpFolder_i <- paste(getwd(),"/dataFiles/gis/metis/gcam",sep="")
subRegShpFile_i <- paste("Global235_CLM_final_5arcmin_multipart",sep="")
subRegShp_i = readOGR(dsn=subRegShpFolder_i,layer=subRegShpFile_i,use_iconv=T,encoding='UTF-8')
head(subRegShp_i@data)
subRegCol_i = "basin_name"
metis.map(dataPolygon=subRegShp_i,fillColumn = subRegCol_i,labels=F ,printFig=T,facetsON=F)

# Run metis.boundaries on the two shapefiles and selected region to get the cropped shapefile.
boundaries<- metis.boundaries(
  boundaryRegShape=boundaryRegShp_i,
  boundaryRegCol=boundaryRegCol_i,
  boundaryRegionsSelect=boundaryRegionsSelect_i,
  subRegShape=subRegShp_i,
  subRegCol=subRegCol_i,
  subRegType="GCAMBasin",
  nameAppend="",
  expandPercent=2,
  #overlapShpFile="Global235_CLM_final_5arcmin_multipart",
  #overlapShpFolder=paste(getwd(),"/dataFiles/gis/metis/gcam",sep=""),
  extension = T,
  cropSubShape2Bound = T)


# The subregion shapefile created by boundaries can now be selected to be used for mapping values.
subRegShp_i_Crop = boundaries$subRegShape # or can point to the subRegShapeFolder and subRegShpFile as produced by metis.boundaries.R
head(subRegShp_i_Crop@data); levels(subRegShp_i_Crop@data[[subRegCol_i]])
metis.map(dataPolygon=subRegShp_i_Crop,fillColumn = subRegCol_i,labels=T ,printFig=F,facetsON=F)
# Sometimes the cropping results in slivers of regions left around boundaries when the boundayr and subregion shape don't line up.
# The extra regions can be removed as follows.
# Choose regions from the list of regions printed above.
# Can also subset to the regions available in the polygon data table below.(unique(polyTable$subRegion)
#********    regions_to_remove =c("Amu_Darya")
#subRegShp_i_Crop<-subRegShp_i_Crop[(!subRegShp_i_Crop[[subRegCol_i]] %in% regions_to_remove) & !is.na(subRegShp_i_Crop[[subRegCol_i]]),]
subRegShp_i_Crop@data <- droplevels(subRegShp_i_Crop@data)
head(subRegShp_i_Crop@data); levels(subRegShp_i_Crop@data[[subRegCol_i]])
metis.map(dataPolygon=subRegShp_i_Crop,fillColumn = subRegCol_i,labels=T ,printFig=F,facetsON=F)


# Read in the datatable with values by subRegion
examplePolygonTable_i<-paste(getwd(),"/dataFiles/examples/China_Withdrawal_bySource_all3.csv",sep="")
polyTable=read.csv(examplePolygonTable_i);head(polyTable)
unique(polyTable$x); # check available number of years.

# Make sure shapefile subRegions and PolygonTable subregions match
unique(polyTable$subRegion); unique(subRegShp_i_Crop@data[[subRegCol_i]])

# metis.mapsProcess(polygonDataTables=examplePolygonTable_i,
#                  #gridDataTables=exampleGridTable_i,
#                  xRange=c(2010,2030,2050,2100),
#                  mapsOutFolderName=boundaryRegionsSelect_i,
#                  boundaryRegionsSelect=boundaryRegionsSelect_i,
#                  boundaryRegShape=boundaryRegShp_i,
#                  subRegShape=subRegShp_i_Crop,
#                  subRegCol=subRegCol_i,
#                  nameAppend="",
#                  animateOn=T,
#                  delay=100,
#                  scenRef="SSP2_Ref",
#                  extension=F,
#                  diffOn = F)


# Improved map using available parameters.
# Shift legend outside and change the scale_range to get conistent scale across scenarios.

# Set scale ranges across scenarios to be the same.
# Check range of data for each param
for(param_i in unique(polyTable$param)){
  print(paste("param: ", param_i, sep=""));print("Range is:")
  print(range((polyTable%>%dplyr::filter(param==param_i))$value))}

scaleRange_i = tibble::tribble(
  ~param,~minScale, ~maxScale,
  "waterWithdraw", 0, 10)

metis.mapsProcess(polygonDataTables=examplePolygonTable_i,
                 #gridDataTables=exampleGridTable_i,
                 xRange=c(2010,2030,2050,2100),
                 mapsOutFolderName=paste(boundaryRegionsSelect_i,"_Edited",sep=""),
                 boundaryRegionsSelect=boundaryRegionsSelect_i,
                 boundaryRegShape=boundaryRegShp_i,
                 subRegShape=subRegShp_i_Crop,
                 subRegCol=subRegCol_i,
                 nameAppend="_improvedFig",
                 legendPosition=c("LEFT","bottom"),
                 animateOn=T,
                 delay=100,
                 scenRef="SSP2_Ref",
                 extension=F,
                 diffOn = F,
                 legendOutsideSingle = T,
                 scaleRange = scaleRange_i)

