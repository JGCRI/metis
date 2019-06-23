
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



#----------------------------
# Read GCAM Data (metis.readgcam.R)
#---------------------------

# Connect to gcam database or project
  # gcamdatabasePath_i <-paste(getwd(),"/dataFiles/gcam",sep="") # Use if gcamdatabase is needed
  # gcamdatabaseName_i <-"example_database_basexdb" # Use if gcamdatabse is needed
  dataProjPath_i <- paste(getwd(),"/dataFiles/gcam",sep="") # Path to dataProj file.
  dataProj_i <-"Example_dataProj.proj"  # Use if gcamdata has been saved as .proj file

# Get list of scenarios and rename if desired.
  # rgcam::localDBConn(gcamdatabasePath,gcamdatabaseName) # if connecting directly to gcam database
  dataProjLoaded <- loadProject(paste(dataProjPath_i, "/",dataProj_i , sep = ""))
  listScenarios(dataProjLoaded)  # List of Scenarios in GCAM database
  scenOrigNames_i = c("exampleScen1","ExampleScen2")
  scenNewNames_i = c("Eg1","Eg2")  # These are the names that will be used in figures

# Choose Parameters or set to "All" for all params. For complete list see ?metis.readgcam
  paramsSelect_i = "All"

# Select regions from the 32 GCAM regions.
  regionsSelect_i <- c("Colombia","Argentina")

  dataGCAM<-metis.readgcam(reReadData = T,
                         #gcamdatabasePath = NULL,
                         #gcamdatabaseName = NULL,
                         scenOrigNames = scenOrigNames_i,
                         scenNewNames = scenNewNames_i,
                         dataProj = dataProj_i,
                         dataProjPath = dataProjPath_i,
                         regionsSelect = regionsSelect_i ,
                         paramsSelect=paramsSelect_i
                       )

  dataGCAM$data # To view the data read that was read.


#----------------------------
# Charts Basic (metis.chart)
#---------------------------

# Simple example with progressively more features
   tbl <- tribble (
   ~x,     ~value,
   2010,   15,
   2020,   20,
   2030,   30
   )
   metis.chart(data = tbl, xData = "x", yData = "value", chartType = "line")
   metis.chart(data = tbl, xData = "x", yData = "value", chartType = "bar")
   metis.chart(data = tbl, xData = "x", yData = "value", chartType = "bar", color = "blue",
               yLabel = "New y Label", xLabel = "New Xlabel", printFig = T, fileName = "newFileName", title = "Title")
   # See ?metis.chart for more details on further customization eg. tick marks, title size ect.

# More detailed data with facets
   # Simple example with progressively more features
   tbl_multi <- tribble (
     ~x,     ~value, ~region,     ~scen,   ~fuel,
     2010,   25,     "region1",   "scenA",  "Oil",
     2020,   30,     "region1",   "scenA",  "Oil",
     2030,   40,     "region1",   "scenA",  "Oil",
     2010,   25,     "region2",   "scenA",  "Oil",
     2020,   10,     "region2",   "scenA",  "Oil",
     2030,   60,     "region2",   "scenA",  "Oil",
     2010,   75,     "region1",   "scenB",  "Oil",
     2020,   30,     "region1",   "scenB",  "Oil",
     2030,   20,     "region1",   "scenB",  "Oil",
     2010,   25,     "region2",   "scenB",  "Oil",
     2020,   10,     "region2",   "scenB",  "Oil",
     2030,   90,     "region2",   "scenB",  "Oil",
     2010,   55,     "region1",   "scenA",  "Gas",
     2020,   40,     "region1",   "scenA",  "Gas",
     2030,   30,     "region1",   "scenA",  "Gas",
     2010,   35,     "region2",   "scenA",  "Gas",
     2020,   30,     "region2",   "scenA",  "Gas",
     2030,   32,     "region2",   "scenA",  "Gas",
     2010,   16,     "region1",   "scenB",  "Gas",
     2020,   28,     "region1",   "scenB",  "Gas",
     2030,   39,     "region1",   "scenB",  "Gas",
     2010,   12,     "region2",   "scenB",  "Gas",
     2020,   26,     "region2",   "scenB",  "Gas",
     2030,   37,     "region2",   "scenB",  "Gas"
   )

   my_pal <- RColorBrewer::brewer.pal(9, "Set1")

   metis.chart(data = tbl_multi, xData = "x", yData = "value", class="fuel",
               chartType = "line",  classPalette=my_pal,
               facet_rows="region",facet_columns="scen")

   my_pal <- metis.colors()$pal_Basic

   metis.chart(data = tbl_multi, xData = "x", yData = "value", class="fuel", position="stack",
               group="fuel",chartType = "bar", classPalette=my_pal,
               facet_rows="region",facet_columns="scen")

   metis.chart(data = tbl_multi, xData = "x", yData = "value", class="fuel", position="dodge",
               group="fuel",chartType = "bar", classPalette=my_pal,
               facet_rows="region",facet_columns="scen")


# Sankey Diagram Example

   # Data Frame with 2 regions, 3 supply sectors and 3 demand sectors
   df <- data.frame(region = c("A","A","A","B","B","B"),
                    supplySector = c("coal","gas","wind","coal","gas","wind"),
                    demandSector = c("resid","indus","ag","resid","indus","ag"),
                    value = 10*runif(6)); df

   metis.chart(data=df, chartType="sankey", yData="value", sankeyGroupColor="supplySector",
               classLabel="From", class = "supplySector", classPalette = metis.colors()$pal_Basic,
               sankeyAxis1="supplySector",sankeyAxis2="demandSector",sankeyAxis1Label ="From",sankeyAxis2Label="To",
               facet_columns="region")


#------------------------------------------------------------------------------------------
# Process data charts for multi-scenario, multi-variable comparison (metis.chartsProcess.R)
#------------------------------------------------------------------------------------------

# Read in Tables (If exist)
# To test can create a copy of the template in ./readGCAMTables/Tables_Templates/template_Regional_Argentina.csv
# in ./readGCAMTables/Tables_Local/ and rename the file something like "local_Regional_Argentina.csv.
# dataTables<-c(paste(getwd(),"/outputs/readGCAMTables/Tables_Local/local_Regional_Argentina.csv",sep=""))  # Need to create this before loading
#a<-read.csv(dataTables)

# Read in the data from the function metis.readgcam
rTable <- dataGCAM$data;

# Choose Parameters or set to "All" for all params. For complete list see ?metis.chartsProcess
paramsSelect=c("elecByTech","gdp")


regionsSelect=c("Argentina","Colombia")

charts<-metis.chartsProcess(rTable=rTable, # Default is NULL
                          #dataTables=dataTables, # Default is NULL
                          paramsSelect=paramsSelect, # Default is "All"
                          regionsSelect=regionsSelect, # Default is "All"
                          xCompare=c("2015","2030","2050","2100"), # Default is c("2015","2030","2050","2100")
                          scenRef="Eg1", # Default is NULL
                          dirOutputs=paste(getwd(),"/outputs",sep=""), # Default is paste(getwd(),"/outputs",sep="")
                          pdfpng="png", # Default is "png"
                          regionCompareOnly=0 # Default is "0"
                          )
#
# rTable=rTable
# #dataTables=dataTables
# paramsSelect=paramsSelect
# regionsSelect=regionsSelect
# xCompare=c("2015","2030","2050","2100")
# scenRef="Eg1"
# dirOutputs=paste(getwd(),"/outputs",sep="")
# pdfpng="png"
# regionCompareOnly=0


#-----------
# Boundaries
#-------------

# Example Shape File
examplePolyFolder<-paste(getwd(),"/dataFiles/examples",sep="")
examplePolyFile<-paste("bermejo3Cropped",sep="")
bermejo3Cropped=readOGR(dsn=examplePolyFolder,
                        layer=examplePolyFile,use_iconv=T,encoding='UTF-8')
head(bermejo3Cropped@data)
metis.map(dataPolygon=bermejo3Cropped,fillColumn = "SUB_NAME",labels=T ,printFig=F,facetsON=F)


bermejoBoundaries<- metis.boundaries(
                            boundaryRegShape=NULL,
                            boundaryRegShpFolder=paste(getwd(),"/dataFiles/gis/naturalEarth",sep=""),
                            boundaryRegShpFile=paste("ne_10m_admin_0_countries",sep=""),
                            boundaryRegCol="NAME",
                            boundaryRegionsSelect="Argentina",
                            subRegShape=bermejo3Cropped,
                            subRegCol="SUB_NAME",
                            subRegType="subRegType",
                            nameAppend="_test",
                            expandPercent=2,
                            overlapShpFile="Global235_CLM_final_5arcmin_multipart",
                            overlapShpFolder=paste(getwd(),"/dataFiles/gis/basin_gcam",sep=""),
                            extension = T,
                            grids = c(paste(getwd(),"/dataFiles/grids/emptyGrids/grid_025.csv",sep=""),
                                      paste(getwd(),"/dataFiles/grids/emptyGrids/grid_050.csv",sep=""))
                            )

#-----------
# Grid to Poly
#-------------

# Example Grid File
gridExample<-paste(getwd(),"/dataFiles/examples/example_grid_ArgentinaBermejo3_Eg1Eg2.csv",sep="")

# Run metis.grid2poly
polyBermeo3Cropped<-metis.grid2poly(grid=gridExample,
                                    boundaryRegionsSelect="Argentina",
                                    subRegShpFolder=examplePolyFolder,
                                    subRegShpFile=examplePolyFile,
                                    subRegCol="SUB_NAME",
                                    #aggType=NULL,
                                    nameAppend="_hydrobidBermeo3")

# grid=gridExample
# boundaryRegionsSelect="Argentina"
# subRegShpFolder=examplePolyFolder
# subRegShpFile=examplePolyFile
# subRegCol="SUB_NAME"
# #aggType=NULL
# nameAppend="_hydrobidBermeo3"

#-----------
# Mapping
#-------------

exampleGridTable<-paste(getwd(),"/dataFiles/examples/example_grid_ArgentinaBermejo3_Eg1Eg2.csv",sep="")
examplePolygonTable<-paste(getwd(),"/dataFiles/examples/example_poly_ArgentinaBermejo3_Eg1Eg2.csv",sep="")
#examplePolygonTable<-paste(getwd(),"/outputs/Maps/Tables/subReg_origData_byClass_Argentina_subRegType_origDownscaled_hydrobidBermeo3.csv",sep="")

#gridTable=read.csv(exampleGridTable);head(gridTable)
#polyTable=read.csv(examplePolygonTable);head(polyTable)

metis.mapProcess(polygonDataTables=examplePolygonTable,
                 gridDataTables=exampleGridTable,
                 xRange=c(2005,2010,2020),
                 boundaryRegionsSelect="Argentina",
                 subRegShape=NULL,
                 subRegShpFolder=paste(getwd(),"/dataFiles/gis/subbasin_hydrobid",sep=""),
                 subRegShpFile=paste("bermejo3Cropped",sep=""),
                 subRegCol="SUB_NAME",
                 subRegType="subBasin",
                 nameAppend="_hydrobid",
                 legendPosition=c("RIGHT","top"),
                 animateOn=T,
                 delay=100,
                 scenRef="Eg1",
                 #expandPercent = 6,
                 extension=F
                 )

# polygonDataTables=examplePolygonTable
# gridDataTables=exampleGridTable
# xRange=c(2005,2010,2020)
# boundaryRegionsSelect="Argentina"
# subRegShape=NULL
# subRegShpFolder=paste(getwd(),"/dataFiles/gis/subbasin_hydrobid",sep="")
# subRegShpFile=paste("bermejo3Cropped",sep="")
# subRegCol="SUB_NAME"
# subRegType="subBasin"
# nameAppend="_hydrobid"
# legendPosition=c("RIGHT","top")
# animateOn=T
# delay=100
# scenRef="Eg1"

