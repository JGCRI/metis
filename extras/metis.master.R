
# metis.master.R
# Script to run different parts of the metis package.

#----------------------------
# Install necessary packages
#----------------------------


if("devtools" %in% rownames(installed.packages()) == F){install.packages("devtools")}; library(devtools)
# install_github(repo="JGCRI/metis"); library(metis); .rs.restartR()
if("rgcam" %in% rownames(installed.packages()) == F){install_github(repo="JGCRI/rgcam")}; library(rgcam)
if("metis" %in% rownames(installed.packages()) == F){devtools::install()}; library(metis)
library(tibble);library(dplyr);library(rgdal); library(ggalluvial);
NULL -> exampleGrid2poly->exampleBoundaries->dataGCAM->io->io_sub


#----------------------------
# Read GCAM Data (metis.readgcam.R)
#---------------------------

# There are two ways to read GCAM data
# i. gcamdatabase:        Directly connecting to a gcam database and extracting data using queries
#                         which are then saved to a .proj file
# ii. .proj or .dat file: If a proj file has alreayd been created or saved from a previous run of rgcam
#                         then metis can take connect directly to this data.

# Connect to gcam database .proj or .dat file
# This is data that has already been extracted from a gcamdatabse using queries and rgcam
# and is thus much faster to process than connecting directly to the gcamdatabase.
# The data must have been extracted using queries that are available in metisQueries.xml
dataProjFile_i <- paste(getwd(),"/dataFiles/examples/Example_dataProj.proj",sep="") # Path to dataProj file.

# Get list of scenarios and rename if desired.
dataProjLoaded <- loadProject(paste(dataProjFile_i, sep = ""))
listScenarios(dataProjLoaded)  # List of Scenarios in GCAM database
listQueries(dataProjLoaded) # List of Queries in queryxml

# Scenario names
scenOrigNames_i = c("ExampleScen1","ExampleScen2")
scenNewNames_i = c("Eg1","Eg2")  # Names to replace the original names for final figures.

# Choose Query sets, individual queries or set to "All". For complete list see ?metis.readgcam
paramsSelect_i = c("energy") # Param sets are c("water", "energy", "electricity","land", "emissions", "ag", "socioecon", "transport")

# Select regions from the 32 GCAM regions.
regionsSelect_i <- c("Argentina","Colombia")

dataGCAM<-metis.readgcam(reReadData = F,
                         #gcamdatabase = gcamdatabasePath_i,
                         scenOrigNames = NULL,
                         scenNewNames = scenNewNames_i,
                         dataProjFile = dataProjFile_i,
                         regionsSelect = regionsSelect_i ,
                         paramsSelect=paramsSelect_i,
                         folderName = "metisExample",
                         saveData = F
                         folderName = NULL
)

dataGCAM$data # To view the data read that was read.

# If connecting directly to a gcamdatabase then can use the following method.
# Uncomment the following lines of code (use ctrl+C to uncomment multiple lines together)
# # Connect directly to a gcam database and produce a .proj file for future uses.
# gcamdatabasePath_i <-paste(getwd(),"/dataFiles/gcam",sep="") # Use if gcamdatabase is needed
# gcamdatabaseName_i <-"example_database_basexdb" # Replace with your database
# rgcam::localDBConn(gcamdatabasePath_i,gcamdatabaseName_i) # Note names of scenarios
#
# # The data must have been extracted using queries that are available in metisQueries.xml
# dataProjPath_i <- paste(getwd(),"/dataFiles/gcam",sep="") # Path to dataProj file.
# dataProj_i <-"Example_dataProjGCAM.proj"  # Use if gcamdata has been saved as .proj file
#
# # Scenario names
# scenOrigNames_i = c("ExampleScen1","ExampleScen2") # make sure these exist (See outputs of the rgcam::localDBConn)
# scenNewNames_i = c("Eg1","Eg2")  # Names to replace the original names for final figures.
#
# # Choose Query sets, individual queries or set to "All". For complete list see ?metis.readgcam
# queriesSelect_i = c("All") # Query sets are c("water", "energy", "land", "emissions", "ag", "socioecon", "transport")
#
# # Select regions from the 32 GCAM regions.
# regionsSelect_i <- c("Argentina","Colombia")
#
# dataGCAM<-metis.readgcam(reReadData = T,
#                          gcamdatabasePath = gcamdatabasePath_i,
#                          gcamdatabaseName = gcamdatabaseName_i,
#                          scenOrigNames = scenOrigNames_i,
#                          scenNewNames = scenNewNames_i,
#                          dataProj = dataProj_i,
#                          dataProjPath = dataProjPath_i,
#                          regionsSelect = regionsSelect_i ,
#                          paramsSelect=paramsSelect_i
# )
#
# dataGCAM$data # To view the data read that was read.

#----------------------------
# Charts Basic (metis.chart.R)
#---------------------------

# Simple example with progressively more features
tbl <- tibble::tribble (
  ~x,     ~value,
  2010,   15,
  2020,   20,
  2030,   30
)
metis.chart(data = tbl, xData = "x", yData = "value", chartType = "line", fileName = "chart_eg_line", dirOutputs="outputs",folderName = "metisExample")
metis.chart(data = tbl, xData = "x", yData = "value", chartType = "bar", fileName = "chart_eg_bar", dirOutputs="outputs", folderName = "metisExample")
metis.chart(data = tbl, xData = "x", yData = "value", chartType = "bar", color = "blue", dirOutputs="outputs", folderName = "metisExample",
            yLabel = "New y Label", xLabel = "New X label", fileName = "chart_eg_bar_blue", title = "Title")
# See ?metis.chart for more details on further customization eg. tick marks, title size ect.

# More detailed data with facets
# Simple example with progressively more features
tbl_multi <- tibble::tribble (
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
            facet_rows="region",facet_columns="scen",
            fileName="chart_eg_line_multi_Set1",dirOutputs="outputs",folderName = "metisExample"
)


metis.chart(data = tbl_multi, xData = "x", yData = "value", class="fuel",
            chartType = "line",  classPalette=my_pal,
            facet_rows="region",facet_columns="scen",dirOutputs="outputs",folderName = "metisExample"
)

my_pal <- metis.colors()$pal_Basic

metis.chart(data = tbl_multi, xData = "x", yData = "value", class="fuel", position="stack",
            group="fuel",chartType = "bar", classPalette=my_pal,
            facet_rows="region",facet_columns="scen",
            fileName="chart_eg_barStack_multi_palBasic",dirOutputs="outputs",folderName = "metisExample")

metis.chart(data = tbl_multi, xData = "x", yData = "value", class="fuel", position="dodge",
            group="fuel",chartType = "bar", classPalette=my_pal,
            facet_rows="region",facet_columns="scen",
            fileName="chart_eg_barDodge_multi_Set1",dirOutputs="outputs",folderName = "metisExample")


# Sankey Diagram Example

# Data Frame with 2 regions, 3 supply sectors and 3 demand sectors and random values
df <- data.frame(region = c("A","A","A","A","A","A","B","B","B","B","B","B"),
                 supplySector = c("coal","coal","gas","gas","wind","wind","coal","coal","gas","gas","wind","wind"),
                 demandSector = c("resid","indus","ag","resid","ag","indus","resid","indus","ag","resid","indus","ag"),
                 value = c(4.70,7.33,9.52,6.5,6.70,3.57,8.43,1.68,7.61,1.10,1.58,3.45)); df

metis.chart(data=df, chartType="sankey", yData="value", sankeyGroupColor="supplySector",
            classLabel="From", class = "supplySector", classPalette = metis.colors()$pal_Basic,
            sankeyAxis1="supplySector",sankeyAxis2="demandSector",sankeyAxis1Label ="From",sankeyAxis2Label="To",
            facet_columns="region", fileName="chart_eg_sankey_multi",dirOutputs="outputs",folderName = "metisExample")

#------------------------------------------------------------------------------------------
# Charts Process (metis.chartsProcess.R)
#------------------------------------------------------------------------------------------

# Read in csv tables if available.
# For example metis.readgcam.R produces datatable and templates which are saved by default in
# ./metis/outputs/readGCAMTables/Tables_gcam/ for each gcam region selected
# ./metis/outputs/readGCAMTables/Tables_Templates which gives an example template to fill out with local or other data.
# Custom or local csv need to have "scenario", "region", "sources",	"param", "units",	"class",	"x",	"value"
# As an example we are creating a local scenario for Argentina and will save it into Tables_Local.

localData <- tibble::tribble (
  ~scenario,    ~region,     ~sources,	 ~param,          ~units,               ~class,             ~x,    ~value,
  "Local Data", "Argentina", "Sources", "finalNrgbySec", "Final Energy (TWh)", "building",        "2010", "50",
  "Local Data", "Argentina", "Sources", "finalNrgbySec", "Final Energy (TWh)", "industry",        "2010", "100",
  "Local Data", "Argentina", "Sources", "finalNrgbySec", "Final Energy (TWh)", "transportation",  "2010", "150",
  "Local Data", "Argentina", "Sources", "finalNrgbySec", "Final Energy (TWh)", "building",        "2015", "70",
  "Local Data", "Argentina", "Sources", "finalNrgbySec", "Final Energy (TWh)", "industry",        "2015", "60",
  "Local Data", "Argentina", "Sources", "finalNrgbySec", "Final Energy (TWh)", "transportation",  "2015", "70",
  "Local Data", "Argentina", "Sources",  "aggLandAlloc", "Land Allocation (1000 km^2)", "crops",           "2010", "100",
  "Local Data", "Argentina", "Sources",  "aggLandAlloc", "Land Allocation (1000 km^2)", "forest",          "2010", "200",
  "Local Data", "Argentina", "Sources",  "aggLandAlloc", "Land Allocation (1000 km^2)", "shrubs",          "2010", "30",
  "Local Data", "Argentina", "Sources",  "aggLandAlloc", "Land Allocation (1000 km^2)", "naturalOther",    "2010", "15",
  "Local Data", "Argentina", "Sources",  "aggLandAlloc", "Land Allocation (1000 km^2)", "pasture",         "2010", "220")

localData <- localData %>% mutate(vintage="Vintage", class2="class2")

if (!dir.exists(paste(getwd(), "/outputs", sep = ""))){dir.create(paste(getwd(), "/outputs", sep = ""))}
if (!dir.exists(paste(getwd(), "/outputs/metisExample", sep = ""))){dir.create(paste(getwd(), "/outputs/metisExample/Charts", sep = ""))}
if (!dir.exists(paste(getwd(), "/outputs/metisExample/Charts", sep = ""))){dir.create(paste(getwd(), "/outputs/metisExample/Charts", sep = ""))}
data.table::fwrite(localData, file = paste(getwd(), "/outputs/metisExample/Charts/example_localFile.csv", sep = ""),row.names = F)
dataTables_i =  c(paste(getwd(), "/outputs/metisExample/Charts/example_localFile.csv", sep = ""))

# Can also add data .csv outputs from metis.readgcam.R which are autmatically saved in
# ./metis/outputs/readGCAMTables/Tables_gcam for each of the regions selected.
# eg. gcamDataTable_Argentina.csv, gcamDataTable_China.csv, gcamDataTable_Pakistan.csv
# This would be added to dataTables_i as:
# dataTables_i = c(paste(getwd(), "/example_localFile.csv", sep = ""),
#                  paste(getwd(), "outputs/readGCAMTables/Tables_gcam/gcamDataTable_Argentina.csv", sep = ""),
#                  paste(getwd(), "outputs/readGCAMTables/Tables_gcam/gcamDataTable_China.csv", sep = ""),
#                  paste(getwd(), "outputs/readGCAMTables/Tables_gcam/gcamDataTable_Pakistan.csv", sep = ""))

# Read in the data from the function metis.readgcam.
rTable_i <- dataGCAM$data;


# Choose Parameters or set to "All" for all params. For complete list see ?metis.chartsProcess

unique(rTable_i$param)%>%sort()

paramsSelect_i=c("gdp", "gdpGrowthRate", "pop",
                 "energyFinalConsumBySecEJ","elecByTechTWh",
                 "emissCO2BySector","emissNonCO2BySectorGWPAR5",
                 "agProdByCrop","landAlloc",
                 "watConsumBySec", "watWithdrawBySec")

# Select regions from the 32 GCAM regions.
regionsSelect_i=c("Argentina","Colombia")

# Charts Process
charts<-metis.chartsProcess(rTable=rTable_i, # Default is NULL
                            dataTables=dataTables_i, # Default is NULL.
                            paramsSelect=paramsSelect_i, # Default is "All"
                            regionsSelect=regionsSelect_i, # Default is "All"
                            xCompare=c("2015","2030","2050","2100"), # Default is c("2015","2030","2050","2100")
                            scenRef="Eg1", # Default is NULL
                            dirOutputs=paste(getwd(),"/outputs",sep=""), # Default is paste(getwd(),"/outputs",sep="")
                            regionCompareOnly=0, # Default 0. If set to 1, will only run comparison plots and not individual
                            scenarioCompareOnly=0, # Default 0. If set to 1, will only run comparison plots and not individual
                            folderName = "metisExample")

#-------------------
# Maps (metis.map.R)
#-------------------

# Example 1. Using US states metis pre-loaded Shape File (mapUS49)
head(metis::mapUS49@data) # Choose the appropriate column name
colName_i = "subRegion"
# Categorical Shapefile
metis.map(dataPolygon=mapUS49,fillColumn = colName_i,labels=T ,labelsAutoPlace = F,
          printFig=F,facetsON=F, folderName="metisExample")

# Example 2. Using global country map provided with metis mapGlobalCountry
head(metis::mapCountries@data) # Choose the column name
colName_i = "subRegion"
# Crop the shapefile to desired boundary by subsetting
mapx = metis::mapCountries
mapx = mapx[mapx$subRegion=="Peru",]
mapx@data = droplevels(mapx@data)
sp::plot(mapx)
metis.map(dataPolygon=mapx,fillColumn = colName_i,labels=T ,printFig=T,facetsON=F,
          labelsAutoPlace = F, dirOutputs="outputs",folderName="metisExample")

# The cropped shapefile can be saved for later use if desired.
# Create a directory to save the file to
if (!dir.exists(paste(getwd(),"/outputs",sep=""))){dir.create(paste(getwd(),"/outputs",sep=""))}
if (!dir.exists(paste(getwd(),"/outputs/metisExample",sep=""))){dir.create(paste(getwd(),"/outputs/metisExample",sep=""))}
if (!dir.exists(paste(getwd(),"/outputs/metisExample/Maps",sep=""))){dir.create(paste(getwd(),"/outputs/metisExample/Maps",sep=""))}
if (!dir.exists(paste(getwd(),"/outputs/metisExample/Maps/ExampleShapefile",sep=""))){
  dir.create(paste(getwd(),"/outputs/metisExample/Maps/ExampleShapefile",sep=""))}
rgdal::writeOGR(obj=mapx,
                dsn=paste(getwd(),"/outputs/metisExample/Maps/ExampleShapefile",sep=""),
                layer=paste("Argentina_states_example",sep=""),
                driver="ESRI Shapefile", overwrite_layer=TRUE)

#------------
# Boundaries
#------------

# Use pre-loaded (state, basin) maps to plot boundary regions for any country

# Example 1: Peru
countryName = "Peru"
exampleSubRegion=metis::mapStates
head(exampleSubRegion@data)
subRegCol_i = "subRegion"
# Crop the shapefile to the desired subRegion
exampleSubRegionCropped<-exampleSubRegion[(exampleSubRegion$region==countryName),]
head(exampleSubRegionCropped@data)
metis.map(dataPolygon=exampleSubRegionCropped,fillColumn = subRegCol_i,labels=T ,printFig=F,facetsON=F)

# Can Further subset the shapefile if desired
chosenSubRegions = c("Cusco","Madre de Dios", "Pasco")
exampleSubRegion_chooseState<-exampleSubRegionCropped[(exampleSubRegionCropped[[subRegCol_i]] %in% chosenSubRegions),]
exampleSubRegion_chooseState@data <- droplevels(exampleSubRegion_chooseState@data)
head(exampleSubRegion_chooseState@data)
metis.map(dataPolygon=exampleSubRegion_chooseState,fillColumn = subRegCol_i,labels=T ,printFig=F,facetsON=F)

exampleBoundaries<- metis.boundaries(
  boundaryRegShape = metis::mapCountries,
  boundaryRegCol = "subRegion",
  boundaryRegionsSelect=countryName,
  subRegShape=exampleSubRegionCropped,
  subRegCol=subRegCol_i,
  subRegType="state",
  nameAppend="_example",
  expandPercentWidth = 2,
  expandPercentHeight = 2,
  overlapShpFile="Global235_CLM_final_5arcmin_multipart",
  overlapShpFolder=paste(getwd(),"/dataFiles/gis/metis/gcam",sep=""),
  extension = T,
  grids = c(paste(getwd(),"/dataFiles/grids/emptyGrids/grid_025.csv",sep=""),
            paste(getwd(),"/dataFiles/grids/emptyGrids/grid_050.csv",sep="")),
  folderName="metisExample")

# Example 2: USA
countryName = "USA"
exampleSubRegion=metis::mapUS49
head(exampleSubRegion@data)
subRegCol_i = "subRegion"
# Crop the shapefile to the desired subRegion
metis.map(dataPolygon=exampleSubRegion,fillColumn = subRegCol_i,labels=T ,printFig=F,facetsON=F, folderName="metisExample")

# Can Further subset the shapefile if desired
chosenSubRegions = c("CA","NV","UT","CO","AZ","TX","OK","WY","TX")
exampleSubRegion_chooseState<-exampleSubRegion[(exampleSubRegion[[subRegCol_i]] %in% chosenSubRegions),]
exampleSubRegion_chooseState@data <- droplevels(exampleSubRegion_chooseState@data)
head(exampleSubRegion_chooseState@data)
metis.map(dataPolygon=exampleSubRegion_chooseState,fillColumn = subRegCol_i,labels=T ,printFig=F,facetsON=F, folderName="metisExample")

exampleBoundaries<- metis.boundaries(
  regionName="USA",
  #boundaryRegShape=metis::mapUS49,
  #boundaryRegCol="subRegion",
  #boundaryRegionsSelect=countryName,
  subRegShape=exampleSubRegion,
  subRegCol=subRegCol_i,
  subRegType="state",
  nameAppend="_example",
  expandPercentWidth = 2,
  expandPercentHeight = 2,
  overlapShape = metis::mapGCAMBasins,
  #overlapShpFile="Global235_CLM_final_5arcmin_multipart",
  #overlapShpFolder=paste(getwd(),"/dataFiles/gis/metis/gcam",sep=""),
  extension = F,
  #grids = c(paste(getwd(),"/dataFiles/grids/emptyGrids/grid_025.csv",sep=""),
  #          paste(getwd(),"/dataFiles/grids/emptyGrids/grid_050.csv",sep="")),
  folderName="metisExample")

exampleBoundaries<- metis.boundaries(
  regionName="USA",
  # boundaryRegShape=metis::mapCountries,
  # boundaryRegCol="subRegion",
  # boundaryRegionsSelect=countryName,
  subRegShape=exampleSubRegion_chooseState,
  subRegCol=subRegCol_i,
  subRegType="state",
  nameAppend="_exampleSubset",
  # expandPercentWidth = 10,
  # expandPercentHeight = 10,
  overlapShape = metis::mapGCAMBasins,
  extension = F,
  grids = metis::grid050,
  folderName="metisExample")

# boundaryRegShape=metis::mapCountries
# boundaryRegCol="subRegion"
# boundaryRegionsSelect=countryName
# subRegShape=exampleSubRegion_chooseState
# subRegCol=subRegCol_i
# subRegType="state"
# nameAppend="_exampleSubset"
# expandPercentWidth = 10
# expandPercentHeight = 10
# overlapShape = metis::mapGCAMBasins
# extension = T
# grids = metis::grid050
# folderName="metisExample"

#-----------
# Grid to Poly
#-------------

# Example Grid File (csv with lats and lons that overlap the shapefile)
gridExample<-paste(getwd(),"/dataFiles/examples/example_grid_Peru.csv",sep="")

# Polygons. An example Shapefile is Provided with metis in ./metis/dataFiles/examples.
# This is the same file as produced in the boundaries example above.
examplePolyFolder_i<-paste(getwd(),"/dataFiles/examples",sep="")
examplePolyFile_i<-paste("Peru_subRegion_example",sep="")
exampleSubRegionCropped=rgdal::readOGR(dsn=examplePolyFolder_i,
                                       layer=examplePolyFile_i,use_iconv=T,encoding='UTF-8')
head(exampleSubRegionCropped@data) # TO choose subRegCol name
subRegCol_i = "name"

# Run metis.grid2poly
exampleGrid2poly<-metis.grid2poly(gridFiles=gridExample,
                                  subRegShpFolder=examplePolyFolder_i,
                                  subRegShpFile=examplePolyFile_i,
                                  subRegCol=subRegCol_i,
                                  nameAppend="_examplePeru",
                                  folderName="metisExample",
                                  regionName = "Peru")

# gridFiles=gridExample
# subRegShpFolder=examplePolyFolder_i
# subRegShpFile=examplePolyFile_i
# subRegCol=subRegCol_i
# nameAppend="_examplePeru"
# folderName="metisExample"
# regionName = "Peru"

#------------------------------
# Mapping (metis.mapsProcess.R)
#------------------------------

# Simple Example. See example csv tables provided for ideal column names needed.
exampleGridTable_i<-paste(getwd(),"/dataFiles/examples/example_grid_Peru.csv",sep="")
examplePolygonTable_i<-paste(getwd(),"/dataFiles/examples/example_poly_Peru.csv",sep="")
gridTable=read.csv(exampleGridTable_i,encoding="Latin-1");head(gridTable)
polyTable=read.csv(examplePolygonTable_i,encoding="Latin-1");head(polyTable)

examplePolyFolder_i<-paste(getwd(),"/dataFiles/examples",sep="")
examplePolyFile_i<-paste("Peru_subRegion_example",sep="")
exampleSubRegionCropped=rgdal::readOGR(dsn=examplePolyFolder_i,
                                       layer=examplePolyFile_i,use_iconv=T,encoding='UTF-8')
head(exampleSubRegionCropped@data) # TO choose subRegCol name
subRegCol_i = "name"
metis.map(dataPolygon=exampleSubRegionCropped,fillColumn = subRegCol_i,labels=F ,printFig=F,facetsON=F)

countryName= "Peru"

metis.mapsProcess(polygonDataTables=examplePolygonTable_i,
                  gridDataTables=exampleGridTable_i,
                  xRange=c(2005,2010,2015,2020,2025,2030),
                  folderName="metisExample",
                  boundaryRegShape = metis::mapCountries,
                  boundaryRegCol="subRegion",
                  boundaryRegionsSelect="Peru",
                  subRegShape=NULL,
                  subRegShpFolder=examplePolyFolder_i,
                  subRegShpFile=examplePolyFile_i,
                  subRegCol=subRegCol_i,
                  nameAppend="_exampleSubRegionMap",
                  legendPosition=c("LEFT","bottom"),
                  animateOn=T,
                  fps=1,
                  expandPercent = 2,
                  extension=T)

# polygonDataTables=examplePolygonTable_i
# gridDataTables=exampleGridTable_i
# xRange=c(2005,2010,2015,2020,2025,2030)
# folderName="metisExample"
# boundaryRegShape = metis::mapCountries
# boundaryRegCol="subRegion"
# boundaryRegionsSelect="Peru"
# subRegShape=NULL
# subRegShpFolder=examplePolyFolder_i
# subRegShpFile=examplePolyFile_i
# subRegCol=subRegCol_i
# nameAppend="_exampleSubRegionMap"
# legendPosition=c("LEFT","bottom")
# animateOn=T
# fps=1
# expandPercent = 2
# extension=T
# frameShow = T



#--------------------------------------------------
# Mapping (metis.mapsProcess.R) - Extensive Example
#--------------------------------------------------
# Steps
# Read in the boundary Shapefile to crop underlying data to.
# Choose the boundary region name or names to subset the boundary shapefile.
# Read in sub-region shape file (Example the GCAM Basins shapefile)
# Run metis.boundaries.R to crop the sub-region shapefile to the boudnary region selected.
# Read in polygon data table with data per sub-regions of interest
# Runs metis.mapsProcess.R

# Read in Boundary Region
# Read in the GCAM 32 regions shapefile which comes with metis.
boundaryRegShp_i = metis::mapGCAMReg32
head(boundaryRegShp_i@data)
boundaryRegCol_i = "subRegion"
metis.map(dataPolygon=boundaryRegShp_i,fillColumn = boundaryRegCol_i,labels=F ,printFig=F,facetsON=F)

# Choose GCAM region
# Full list: USA, Africa_Eastern, Africa_Northern, Africa_Southern, Africa_Western,
# Australia_NZ, Brazil, Canada Central America and Caribbean, Central Asia, China, EU-12,
# EU-15, Europe_Eastern, Europe_Non_EU, European Free Trade Association, India, Indonesia, Japan,
# Mexico, Middle East, Pakistan, Russia, South Africa, South America_Northern, South America_Southern,
# South Asia, South Korea, Southeast Asia,

# A similar analysis can be done using the country shapefile for any country.
# Uncomment the following lines of code and choose and appropriate region to crop to.
# boundaryRegShpFolder_i <- paste(getwd(),"/dataFiles/gis/metis/naturalEarth",sep="")
# boundaryRegShpFile_i <- paste("ne_10m_admin_0_countries",sep="")
# boundaryRegShp_i = rgdal::readOGR(dsn=boundaryRegShpFolder_i,layer=boundaryRegShpFile_i,use_iconv=T,encoding='UTF-8')
# head(boundaryRegShp_i@data)
# boundaryRegCol_i = "NAME"
# metis.map(dataPolygon=boundaryRegShp_i,fillColumn = boundaryRegCol_i,labels=F ,printFig=F,facetsON=F)
# Pick country names from the list of countries in the natural earth shapefile.
# unique(boundaryRegShp_i@data[[boundaryRegCol_i]])

boundaryRegionsSelect_i = c("China") # Must be a region in the boundaryRegShp


# Read in subregion shapefile
# Read in the  SubBasin GCAM Basins shapefile which comes with metis.
subRegShp_i = metis::mapGCAMBasins
head(subRegShp_i@data)
subRegCol_i = "subRegion"
metis.map(dataPolygon=subRegShp_i,fillColumn = subRegCol_i,labels=F ,printFig=F,facetsON=F)

# Run metis.boundaries on the two shapefiles and selected region to get the cropped shapefile.
boundaries<- metis.boundaries(
  boundaryRegShape=boundaryRegShp_i,
  boundaryRegCol=boundaryRegCol_i,
  boundaryRegionsSelect=boundaryRegionsSelect_i,
  subRegShape=subRegShp_i,
  subRegCol=subRegCol_i,
  subRegType="GCAMBasin",
  nameAppend="",
  expandPercentWidth =2,
  expandPercentHeight =2,
  #overlapShpFile="Global235_CLM_final_5arcmin_multipart",
  #overlapShpFolder=paste(getwd(),"/dataFiles/gis/metis/gcam",sep=""),
  extension = T,
  cropSubShape2Bound = T,
  folderName="metisExample")


# The subregion shapefile created by boundaries can now be selected to be used for mapping values.
subRegShp_i_Crop = boundaries$subRegShape # or can point to the subRegShapeFolder and subRegShpFile as produced by metis.boundaries.R
head(subRegShp_i_Crop@data); levels(subRegShp_i_Crop@data[[subRegCol_i]])
metis.map(dataPolygon=subRegShp_i_Crop,fillColumn = subRegCol_i,labels=T ,printFig=F,facetsON=F)
# Sometimes the cropping results in slivers of regions left around boundaries when the boundayr and subregion shape don't line up.
# The extra regions can be removed as follows.
# Choose regions from the list of regions printed above.
# Can also subset to the regions available in the polygon data table below.(unique(polyTable$subRegion)
regions_to_remove =c("Amu_Darya")
subRegShp_i_Crop<-subRegShp_i_Crop[(!subRegShp_i_Crop[[subRegCol_i]] %in% regions_to_remove) & !is.na(subRegShp_i_Crop[[subRegCol_i]]),]
subRegShp_i_Crop@data <- droplevels(subRegShp_i_Crop@data)
head(subRegShp_i_Crop@data); levels(subRegShp_i_Crop@data[[subRegCol_i]])
metis.map(dataPolygon=subRegShp_i_Crop,fillColumn = subRegCol_i,labels=T ,printFig=F,facetsON=F)


# Read in the datatable with values by subRegion
examplePolygonTable_i<-paste(getwd(),"/dataFiles/examples/example_GCAMBasins_analysis.csv",sep="")
polyTable=read.csv(examplePolygonTable_i,encoding="Latin-1");head(polyTable)
unique(polyTable$x); # check available number of years.

# Make sure shapefile subRegions and PolygonTable subregions match
unique(polyTable$subRegion); unique(subRegShp_i_Crop@data[[subRegCol_i]])

scenRefDiffIndv_i = list(param=list(c("waterConsumption")),
                         scenRef=list(c("SSP2_Ref")),
                         scenDiff=list(c("SSP2_GFDL_PDSSAT","SSP2_IPSL_PDSSAT")),
                         scenIndv=list(c("SSP2_Ref","SSP2_GFDL_PDSSAT")))

metis.mapsProcess(polygonDataTables=examplePolygonTable_i,
                  #gridDataTables=exampleGridTable_i,
                  xRange=c(2010,2020,2030,2040,2050),
                  folderName="metisExample_extended",
                  boundaryRegionsSelect=boundaryRegionsSelect_i,
                  boundaryRegShape=boundaryRegShp_i,
                  subRegShape=subRegShp_i_Crop,
                  boundaryRegCol = boundaryRegCol_i,
                  subRegCol=subRegCol_i,
                  nameAppend="",
                  animateOn=T,
                  fps=1,
                  extension=F,
                  diffOn = T,
                  scenRefDiffIndv=scenRefDiffIndv_i)

# polygonDataTables=examplePolygonTable_i
# #gridDataTables=exampleGridTable_i
# xRange=c(2010,2020,2030,2040,2050)
# folderName="metisExample_extended"
# boundaryRegionsSelect=boundaryRegionsSelect_i
# boundaryRegShape=boundaryRegShp_i
# subRegShape=subRegShp_i_Crop
# boundaryRegCol = boundaryRegCol_i
# subRegCol=subRegCol_i
# nameAppend=""
# animateOn=T
# fps=1
# extension=F
# diffOn = T

# Improved map using available parameters.
# Shift legend outside and change the scale_range to get conistent scale across scenarios.

# Set scale ranges across scenarios to be the same.
# Check range of data for each param
for(param_i in unique(polyTable$param)){
  print(paste("param: ", param_i, sep=""));print("Range is:")
  print(range((polyTable%>%dplyr::filter(param==param_i))$value))}

scaleRange_i = tibble::tribble(
  ~param,~minScale, ~maxScale,
  "waterConsumption", 0, 60)

# Select natural Earth country Map
boundaryRegShp_i = metis::mapCountries
head(boundaryRegShp_i@data)
boundaryRegCol_i = "subRegion"
metis.map(dataPolygon=boundaryRegShp_i,fillColumn = boundaryRegCol_i,labels=F ,printFig=F,facetsON=F)
boundaryRegionsSelect_i = c("China")

scenRefDiffIndv_i = list(param=list(c("waterConsumption")),
                         scenRef=list(c("SSP2_Ref")),
                         scenDiff=list(c("SSP2_GFDL_PDSSAT","SSP2_IPSL_PDSSAT")),
                         scenIndv=list(c("SSP2_Ref","SSP2_GFDL_PDSSAT")))

metis.mapsProcess(polygonDataTables=examplePolygonTable_i,
                  #gridDataTables=exampleGridTable_i,
                  xRange=c(2010,2020,2030,2040,2050),
                  folderName="metisExample_extendedRefined",
                  boundaryRegionsSelect=boundaryRegionsSelect_i,
                  boundaryRegShape=boundaryRegShp_i,
                  boundaryRegCol = boundaryRegCol_i,
                  subRegShape=subRegShp_i_Crop,
                  subRegCol=subRegCol_i,
                  nameAppend="_improvedFig",
                  legendPosition=c("LEFT","bottom"),
                  animateOn=F,
                  fps=1,
                  extension=T,
                  diffOn = T,
                  legendOutsideSingle = T,
                  scaleRange = scaleRange_i,
                  scenRefDiffIndv=scenRefDiffIndv_i)


#----------------------------
# Input/Output (metis.io.R)
#---------------------------

# IO test 1 - Simple
ioTable0=tibble::tribble( # Initial Flows
  ~supplySubSector,  ~water,  ~ag,   ~elec,  ~domestic, ~export, ~mining, ~other, ~units,
  "water",            0,    30,      5,        8,         5,       5,    5, "km3",
  "ag",               0,    0,      1,        0,          10,      0,    0, "ton",
  "elec",             3,    0.3,    0,        0.5,        1,       2,    5, "TWh",
  "livestock",        1,    0.1,    0,        0.3,        2,       3,    2, "head");ioTable0


io<-metis.io(ioTable0 = ioTable0, folderName = "metisExample",plotSankeys=T, nameAppend = "_testSimple")
# View Outputs
io$A
io$L
io$ioTbl


# IO test 2 with subSectors
ioTable0=tibble::tribble( # Initial Flows
  ~supplySubSector,  ~supplySector, ~w_GW,  ~ag_Wheat,   ~elec_Coal,  ~domestic, ~export, ~mining, ~other, ~units,
  "w_GW",            "water",     0,    10,      1,        3,         1,       2,    1, "km3",
  "w_SW",            "water",     0,    30,      5,        8,         5,       5,    5, "km3",
  "ag_Biofuel",      "ag",      0,    0,       2,        0,         8,       0,    0, "ton",
  "ag_Wheat",        "ag",      0,    0,       0,        6,         9,       0,    0, "ton",
  "ag_Rice",         "ag",     0,    0,       0,        5,         20,      0,    0, "ton",
  "elec_Coal",       "elec",     3,    0.3,     0,        0.6,       1,       3,    6, "TWh",
  "elec_Wind",       "elec",     2,    0.2,     0,        0.4,       0.677,       2,    4, "TWh",
  "elec_Oil",        "elec",     1,    0.1,     0,        0.2,       0.333,       1,    2, "TWh",
  "livestock_Cow",   "livestock",     0,    0,     0,        0,    30,       60,    0, "head",
  "livestock_Chicken", "livestock",   0,    0,     0,        0,    50,       90,    0, "head");ioTable0


io_sub<-metis.io(ioTable0 = ioTable0, folderName = "metisExample",plotSankeys=T, nameAppend = "_testSubSector")
# View Outputs
io_sub$A
io_sub$L
io_sub$ioTbl



#------------------------------
# Metis Tests (Check if outputs from metis.master.R are working as expected)
#------------------------------

if(T){
  testInstallPackages<- "Test Install Packages: Passed"
  testIO<-"Test IO: Failed"
  testReadGCAM<-"Test readGCAM: Failed"
  testChart<-"Test chart: Failed"
  testChartsProcess<-"Test chartsProcess: Failed"
  testMap<-"Test map: Failed"
  testBoundary<-"Test boundary: Failed"
  testGrid2Poly<-"Test grid2Poly: Failed"
  testMapsProcess<-"Test mapsProcess: Failed"

  # Test: Install Packages Check
  if(all(c("devtools","rgcam","metis","rgdal","magick") %in% rownames(installed.packages()))==T){
    testInstallPackages = "Test Install Packages: Passed"
  } else { testInstallPackages = "Test Install Packages: Failed"}; print(testInstallPackages)

  # Test: IO Check
  if(!is.null(io)){
    if(nrow(io$A)>1){
      testIO = "Test IO: Passed"
    }} else { testIO = "Test IO: Failed"}; print(testIO)

  # Test: readgcam Check
  if(!is.null(dataGCAM)){
    if(nrow(dataGCAM$data)>1){
      testReadGCAM = "Test readGCAM: Passed"
    }} else { testReadGCAM = "Test readGCAM: Failed"}; print(testReadGCAM)

  # Test: charts Check
  if(file.exists(paste(getwd(),"/outputs/metisExample/chart_eg_line_multi_Set1.png",sep="")) &
     file.exists(paste(getwd(),"/outputs/metisExample/chart_eg_sankey_multi.png",sep=""))){
    testChart = "Test chart: Passed"
  } else { testChart = "Test chart: Failed"}; print(testChart)

  # Test: chartsProcess Check
  if(file.exists(paste(getwd(),"/outputs/metisExample/charts/compareRegions/ArgentinaColombiaEg1/energyFinalConsumBySecEJ_figBar_Eg1_compareRegions.png",sep="")) &
     file.exists(paste(getwd(),"/outputs/metisExample/charts/compareRegions/ArgentinaColombiacompareScen/energyFinalConsumBySecEJ_figBar_compareScenRegion_xScenSelectYears.png",sep=""))){
    testChartsProcess = "Test chartsProcess: Passed"
  } else { testChartsProcess = "Test chartsProcess: Failed"}; print(testChartsProcess)

  # Test: map Check
  if(file.exists(paste(getwd(),"/outputs/metisExample/Maps/ExampleShapefile/Argentina_states_example.shp",sep=""))){
    testMap = "Test map: Passed"
  } else { testMap = "Test map: Failed"}; print(testMap)

  # Test: boundary Check
  if(file.exists(paste(getwd(),"/outputs/metisExample/Boundaries/Peru/Peru_example.png",sep=""))){
    testBoundary = "Test boundary: Passed"
  } else { testBoundary = "Test boundary: Failed"}; print(testBoundary)

  # Test: grid2Poly Check
  if(!is.null(exampleGrid2poly)){
    if(nrow(exampleGrid2poly)>1){
      testGrid2Poly = "Test grid2Poly: Passed"
    }} else { testGrid2Poly = "Test grid2Poly: Failed"}; print(testGrid2Poly)

  # Test: mapsProcess Check
  if(file.exists(paste(getwd(),"/outputs/metisExample/Maps/subRegType/population/popGWP/byYear/map_metisExample_subRegType_population_2005_popGWP_exampleSubRegionMap_KMEANS.png",sep=""))){
    testMapsProcess = "Test mapsProcess: Passed"
  } else { testMapsProcess = "Test mapsProcess: Failed"}; print(testMapsProcess)

  metisTests = data.frame(Test=c(testInstallPackages,testIO,testReadGCAM,
                                 testChart,testChartsProcess,testMap,testBoundary,
                                 testGrid2Poly,testMapsProcess))
  print(metisTests)
  if(any(grepl("Failed",metisTests$Test))){"Some metis tests failed. Please check the relevant section."} else {"All metis tests passed without issues."}
}


# Numeric2Cat
numeric2Cat_param <- list("griddedScarcity","polygonScarcity")
numeric2Cat_breaks <- list(c(-Inf, 0.1, 0.2, 0.4,Inf),c(-Inf, 0.1, 0.2, 0.4,Inf))
numeric2Cat_labels <- list(c("None (0<WSI<0.1)","Low (0.1<WSI<0.2)","Moderate (0.2<WSI<0.4)","Severe (WSI>0.4)"),
                           c("None (0<WSI<0.1)","Low (0.1<WSI<0.2)","Moderate (0.2<WSI<0.4)","Severe (WSI>0.4)"))
numeric2Cat_palette <- list(c("pal_ScarcityCat"),
                            #c("c('None (0<WSI<0.1)'='black','Low (0.1<WSI<0.2)'='blue','Moderate (0.2<WSI<0.4)'='purple','Severe (WSI>0.4)'='yellow')"),
                            c("pal_ScarcityCat")) # Can be a custom scale or an R brewer paletter or a metis.pal
numeric2Cat_legendTextSize <- list(c(0.7),c(0.7))
numeric2Cat_list <-list(numeric2Cat_param=numeric2Cat_param,
                        numeric2Cat_breaks=numeric2Cat_breaks,
                        numeric2Cat_labels=numeric2Cat_labels,
                        numeric2Cat_palette=numeric2Cat_palette,
                        numeric2Cat_legendTextSize=numeric2Cat_legendTextSize)

numeric2Cat_param <- list("param")
numeric2Cat_breaks <- list(c(-Inf, 0.1,1.1,2.1,Inf))
numeric2Cat_labels <- list(c("0","1","2","3"))
numeric2Cat_palette <- list(c("0"="red","1"="blue","2"="green"))
numeric2Cat_list <-list(numeric2Cat_param=numeric2Cat_param,
                        numeric2Cat_breaks=numeric2Cat_breaks,
                        numeric2Cat_labels=numeric2Cat_labels,
                        numeric2Cat_palette=numeric2Cat_palette,
                        numeric2Cat_legendTextSize=numeric2Cat_legendTextSize)

numeric2Cat_param <- list("param")
numeric2Cat_breaks <- list(c(-Inf, 0.1,1.1,2.1,3.1,4.1,5.1,10.1,Inf))
numeric2Cat_labels <- list(c("0","1","2","3","4","5","10",">10"))
numeric2Cat_palette <- list(c("0"="green","1"="#fee5d9","2"="#fcbba1",
                              "3"="#fc9272","4"="#fb6a4a","5"="#de2d26",
                              "10"="#a50f15",">10"="black"))
numeric2Cat_legendTextSize <- list(c(0.7),c(0.7))
numeric2Cat_list <-list(numeric2Cat_param=numeric2Cat_param,
                        numeric2Cat_breaks=numeric2Cat_breaks,
                        numeric2Cat_labels=numeric2Cat_labels,
                        numeric2Cat_palette=numeric2Cat_palette,
                        numeric2Cat_legendTextSize=numeric2Cat_legendTextSize); numeric2Cat_list

data = data.frame(subRegion=c("Negro","La_plata","Great","New_England","Indus","Zambezi"),
                  x=c(2050,2050,2050,2050,2050,2050),
                  value=c(0,1,3,20,2,1))
metis.mapsProcess(polygonTable=data,
                  folderName = "DroughtEmulationValidation",
                  mapTitleOn = F,
                  numeric2Cat_list=numeric2Cat_list)  # "GnYlRd"  #  classPalette = pal_pass_fail,

# polygonTable=data
# folderName = "DroughtEmulationValidation"
# mapTitleOn = F
# numeric2Cat_list=numeric2Cat_list
