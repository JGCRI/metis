
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
# Read GCAM Data (metis.readgcam.R)
#---------------------------

# Connect directly to a gcam database and produce a .proj file for future uses.
  # gcamdatabasePath_i <-paste(getwd(),"/dataFiles/gcam",sep="") # Use if gcamdatabase is needed
  # gcamdatabaseName_i <-"example_database_basexdb" # Use if gcamdatabse is needed
  # rgcam::localDBConn(gcamdatabasePath_i,gcamdatabaseName_i) # if connecting directly to gcam database

# Connect to gcam database or project
  dataProjPath_i <- paste(getwd(),"/dataFiles/gcam",sep="") # Path to dataProj file.
  dataProj_i <-"Example_dataProj.proj"  # Use if gcamdata has been saved as .proj file

# Get list of scenarios and rename if desired.
  dataProjLoaded <- loadProject(paste(dataProjPath_i, "/",dataProj_i , sep = ""))
  listScenarios(dataProjLoaded)  # List of Scenarios in GCAM database
  scenOrigNames_i = c("ExampleScen1","ExampleScen2")
  scenNewNames_i = c("Eg1","Eg2")  # Names to replace the original names for final figures.

# Choose Parameters or set to "All" for all params. For complete list see ?metis.readgcam
  paramsSelect_i = "All"

# Select regions from the 32 GCAM regions.
  regionsSelect_i <- c("Pakistan","China","Argentina")

  dataGCAM<-metis.readgcam(reReadData = F,
                         #gcamdatabasePath = gcamdatabasePath_i,
                         #gcamdatabaseName = gcamdatabaseName_i,
                         scenOrigNames = scenOrigNames_i,
                         scenNewNames = scenNewNames_i,
                         dataProj = dataProj_i,
                         dataProjPath = dataProjPath_i,
                         regionsSelect = regionsSelect_i ,
                         paramsSelect=paramsSelect_i
                       )

  dataGCAM$data # To view the data read that was read.


#----------------------------
# Charts Basic (metis.chart.R)
#---------------------------

# Simple example with progressively more features
   tbl <- tribble (
   ~x,     ~value,
   2010,   15,
   2020,   20,
   2030,   30
   )
   metis.chart(data = tbl, xData = "x", yData = "value", chartType = "line", fileName = "chart_eg_line")
   metis.chart(data = tbl, xData = "x", yData = "value", chartType = "bar", fileName = "chart_eg_bar")
   metis.chart(data = tbl, xData = "x", yData = "value", chartType = "bar", color = "blue",
               yLabel = "New y Label", xLabel = "New X label", fileName = "chart_eg_bar_blue", title = "Title")
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
               facet_rows="region",facet_columns="scen", fileName="chart_eg_line_multi_Set1")

   my_pal <- metis.colors()$pal_Basic

   metis.chart(data = tbl_multi, xData = "x", yData = "value", class="fuel", position="stack",
               group="fuel",chartType = "bar", classPalette=my_pal,
               facet_rows="region",facet_columns="scen", fileName="chart_eg_barStack_multi_palBasic")

   metis.chart(data = tbl_multi, xData = "x", yData = "value", class="fuel", position="dodge",
               group="fuel",chartType = "bar", classPalette=my_pal,
               facet_rows="region",facet_columns="scen", fileName="chart_eg_barDodge_multi_Set1")


# Sankey Diagram Example

   # Data Frame with 2 regions, 3 supply sectors and 3 demand sectors
   df <- data.frame(region = c("A","A","A","B","B","B"),
                    supplySector = c("coal","gas","wind","coal","gas","wind"),
                    demandSector = c("resid","indus","ag","resid","indus","ag"),
                    value = 10*runif(6)); df

   metis.chart(data=df, chartType="sankey", yData="value", sankeyGroupColor="supplySector",
               classLabel="From", class = "supplySector", classPalette = metis.colors()$pal_Basic,
               sankeyAxis1="supplySector",sankeyAxis2="demandSector",sankeyAxis1Label ="From",sankeyAxis2Label="To",
               facet_columns="region", fileName="chart_eg_sankey_multi")


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
     "Local Data", "Argentina", "Sources", "finalNrgbySec", "Final Energy (TWh)", "building",        "2010", "10",
     "Local Data", "Argentina", "Sources", "finalNrgbySec", "Final Energy (TWh)", "industry",        "2010", "20",
     "Local Data", "Argentina", "Sources", "finalNrgbySec", "Final Energy (TWh)", "transportation",  "2010", "30",
     "Local Data", "Argentina", "Sources", "finalNrgbySec", "Final Energy (TWh)", "building",        "2015", "15",
     "Local Data", "Argentina", "Sources", "finalNrgbySec", "Final Energy (TWh)", "industry",        "2015", "22",
     "Local Data", "Argentina", "Sources", "finalNrgbySec", "Final Energy (TWh)", "transportation",  "2015", "34")

   if (!dir.exists(paste(getwd(), "/outputs", sep = ""))){dir.create(paste(getwd(), "/outputs", sep = ""))}
   data.table::fwrite(localData, file = paste(getwd(), "/outputs/example_localFile.csv", sep = ""),row.names = F)
   dataTables_i =  c(paste(getwd(), "/outputs/example_localFile.csv", sep = ""))

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
  paramsSelect_i=c("All")

# Select regions from the 32 GCAM regions.
  regionsSelect_i=c("Argentina","China","Pakistan")

# Charts Process
  charts<-metis.chartsProcess(rTable=rTable_i, # Default is NULL
                          dataTables=dataTables_i, # Default is NULL.
                          paramsSelect=paramsSelect_i, # Default is "All"
                          regionsSelect=regionsSelect_i, # Default is "All"
                          xCompare=c("2015","2030","2050","2100"), # Default is c("2015","2030","2050","2100")
                          scenRef="Eg1", # Default is NULL
                          dirOutputs=paste(getwd(),"/outputs",sep=""), # Default is paste(getwd(),"/outputs",sep="")
                          regionCompareOnly=0, # Default 0. If set to 1, will only run comparison plots and not individual
                          scenarioCompareOnly=0) # Default 0. If set to 1, will only run comparison plots and not individual

#-------------------
# Maps (metis.map.R)
#-------------------

# Polygons. An example Shapefile is Provided with metis in ./metis/dataFiles/examples.
  examplePolyFolder<-paste(getwd(),"/dataFiles/examples",sep="")
  examplePolyFile<-paste("bermejo3Cropped",sep="")

  # Read in the shape file and not the column name to use for fills and labels.
  bermejo3Cropped=readOGR(dsn=examplePolyFolder,
                          layer=examplePolyFile,use_iconv=T,encoding='UTF-8')
  head(bermejo3Cropped@data) # Choose the column name

  # Categorical Shapefile
  metis.map(dataPolygon=bermejo3Cropped,fillColumn = "SUB_NAME",labels=T ,printFig=F,facetsON=F)

  # Shapefile with values
  metis.map(dataPolygon=bermejo3Cropped,fillColumn = "SUB_AREA",labels=T ,printFig=F,facetsON=T,
            legendShow = T, legendOutside = T, fillPalette = "Reds", labelsAutoPlace = F)


#------------
# Boundaries
#------------

# Example Shape File. Provided with metis in ./metis/dataFiles/examples
  examplePolyFolder_i<-paste(getwd(),"/dataFiles/examples",sep="")
  examplePolyFile_i<-paste("bermejo3Cropped",sep="")
  bermejo3Cropped=readOGR(dsn=examplePolyFolder_i,
                        layer=examplePolyFile_i,use_iconv=T,encoding='UTF-8')
  head(bermejo3Cropped@data)
  subRegCol_i = "SUB_NAME"
  metis.map(dataPolygon=bermejo3Cropped,fillColumn = subRegCol_i,labels=T ,printFig=F,facetsON=F)


  bermejoBoundaries<- metis.boundaries(
                            boundaryRegShape=NULL,
                            boundaryRegShpFolder=paste(getwd(),"/dataFiles/gis/metis/naturalEarth",sep=""),
                            boundaryRegShpFile=paste("ne_10m_admin_0_countries",sep=""),
                            boundaryRegCol="NAME",
                            boundaryRegionsSelect="Argentina",
                            subRegShape=bermejo3Cropped,
                            subRegCol=subRegCol_i,
                            subRegType="subRegType",
                            nameAppend="_test",
                            expandPercent=2,
                            overlapShpFile="Global235_CLM_final_5arcmin_multipart",
                            overlapShpFolder=paste(getwd(),"/dataFiles/gis/metis/gcam",sep=""),
                            extension = T,
                            grids = c(paste(getwd(),"/dataFiles/grids/emptyGrids/grid_025.csv",sep=""),
                                      paste(getwd(),"/dataFiles/grids/emptyGrids/grid_050.csv",sep="")))

#-----------
# Grid to Poly
#-------------

# Example Grid File (csv with lats and lons that overlap the shapefile)
    gridExample<-paste(getwd(),"/dataFiles/examples/example_grid_ArgentinaBermejo3_Eg1Eg2.csv",sep="")

# Polygons. An example Shapefile is Provided with metis in ./metis/dataFiles/examples.
    examplePolyFolder_i<-paste(getwd(),"/dataFiles/examples",sep="")
    examplePolyFile_i<-paste("bermejo3Cropped",sep="")
    bermejo3Cropped=readOGR(dsn=examplePolyFolder_i,
                            layer=examplePolyFile_i,use_iconv=T,encoding='UTF-8')
    head(bermejo3Cropped@data) # TO choose subRegCol name
    subRegCol_i = "SUB_NAME"

# Run metis.grid2poly
    polyBermeo3Cropped<-metis.grid2poly(grid=gridExample,
                                    subRegShpFolder=examplePolyFolder_i,
                                    subRegShpFile=examplePolyFile_i,
                                    subRegCol=subRegCol_i,
                                    aggType="depth", # Aggregation type. Depth or volume. See docuemntation for further details.
                                    nameAppend="_Bermeo3")


#------------------------------
# Mapping (metis.mapsProcess.R)
#------------------------------

# Simple Example. See example csv tables provided for ideal column names needed.
    exampleGridTable_i<-paste(getwd(),"/dataFiles/examples/example_grid_ArgentinaBermejo3_Eg1Eg2.csv",sep="")
    examplePolygonTable_i<-paste(getwd(),"/dataFiles/examples/example_poly_ArgentinaBermejo3_Eg1Eg2.csv",sep="")
    gridTable=read.csv(exampleGridTable_i);head(gridTable)
    polyTable=read.csv(examplePolygonTable_i);head(polyTable)

    subRegShpFolder_i <- paste(getwd(),"/dataFiles/examples",sep="")
    subRegShpFile_i <- paste("bermejo3Cropped",sep="")
    subRegShp_i = readOGR(dsn=subRegShpFolder_i,layer=subRegShpFile_i,use_iconv=T,encoding='UTF-8')
    head(subRegShp_i@data)
    subRegCol_i = "SUB_NAME"
    metis.map(dataPolygon=subRegShp_i,fillColumn = subRegCol_i,labels=F ,printFig=F,facetsON=F)

    metis.mapProcess(polygonDataTables=examplePolygonTable_i,
                 gridDataTables=exampleGridTable_i,
                 xRange=c(2005,2010,2020),
                 mapsOutFolderName="BermejoExample",
                 subRegShape=NULL,
                 subRegShpFolder=examplePolyFolder_i,
                 subRegShpFile=examplePolyFile_i,
                 subRegCol=subRegCol_i,
                 subRegType="subBasin",
                 nameAppend="_exampleSubRegionMap",
                 legendPosition=c("RIGHT","top"),
                 animateOn=T,
                 delay=100,
                 scenRef="Eg1",
                 #expandPercent = 2,
                 extension=F)

# Extended Map showing the subregion within a wider boudnary region

    boundaryRegShpFolder_i <- paste(getwd(),"/dataFiles/gis/metis/naturalEarth",sep="")
    boundaryRegShpFile_i <- paste("ne_10m_admin_0_countries",sep="")
    boundaryRegShp_i = readOGR(dsn=boundaryRegShpFolder_i,layer=boundaryRegShpFile_i,use_iconv=T,encoding='UTF-8')
    head(boundaryRegShp_i@data)
    boundaryRegCol_i = "NAME"
    metis.map(dataPolygon=boundaryRegShp_i,fillColumn = boundaryRegCol_i,labels=F ,printFig=F,facetsON=F)
    # Pick country names from the list of countries in the natural earth shapefile.
    unique(boundaryRegShp_i@data[[boundaryRegCol_i]])
    boundaryRegionsSelect_i = c("Argentina") # Must be a region in the boundaryRegShp

    metis.mapProcess(polygonDataTables=examplePolygonTable_i,
                     gridDataTables=exampleGridTable_i,
                     xRange=c(2005,2010,2020),
                     mapsOutFolderName="BermejoExampleExtended",
                     boundaryRegionsSelect=boundaryRegionsSelect_i,
                     boundaryRegShpFolder = boundaryRegShpFolder_i,
                     boundaryRegShpFile = boundaryRegShpFile_i,
                     boundaryRegCol = boundaryRegCol_i,
                     subRegShape=NULL,
                     subRegShpFolder=examplePolyFolder_i,
                     subRegShpFile=examplePolyFile_i,
                     subRegCol=subRegCol_i,
                     subRegType="subBasin",
                     nameAppend="_exampleSubRegionMapExtended",
                     legendPosition=c("RIGHT","top"),
                     animateOn=T,
                     delay=100,
                     scenRef="Eg1",
                     expandPercent = 10,
                     extension=T)


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
    boundaryRegShpFolder_i <- paste(getwd(),"/dataFiles/gis/metis/gcam",sep="")
    boundaryRegShpFile_i <- paste("region32_0p5deg_regions",sep="")
    boundaryRegShp_i = readOGR(dsn=boundaryRegShpFolder_i,layer=boundaryRegShpFile_i,use_iconv=T,encoding='UTF-8')
    head(boundaryRegShp_i@data)
    boundaryRegCol_i = "region"
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
    # boundaryRegShp_i = readOGR(dsn=boundaryRegShpFolder_i,layer=boundaryRegShpFile_i,use_iconv=T,encoding='UTF-8')
    # head(boundaryRegShp_i@data)
    # boundaryRegCol_i = "NAME"
    # metis.map(dataPolygon=boundaryRegShp_i,fillColumn = boundaryRegCol_i,labels=F ,printFig=F,facetsON=F)
    # Pick country names from the list of countries in the natural earth shapefile.
    # unique(boundaryRegShp_i@data[[boundaryRegCol_i]])

    boundaryRegionsSelect_i = c("China") # Must be a region in the boundaryRegShp


# Read in subregion shapefile
    # Read in the  SubBasin GCAM Basins shapefile which comes with metis.
    subRegShpFolder_i <- paste(getwd(),"/dataFiles/gis/metis/gcam",sep="")
    subRegShpFile_i <- paste("Global235_CLM_final_5arcmin_multipart",sep="")
    subRegShp_i = readOGR(dsn=subRegShpFolder_i,layer=subRegShpFile_i,use_iconv=T,encoding='UTF-8')
    head(subRegShp_i@data)
    subRegCol_i = "basin_name"
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
    regions_to_remove =c("Amu_Darya")
    subRegShp_i_Crop<-subRegShp_i_Crop[(!subRegShp_i_Crop[[subRegCol_i]] %in% regions_to_remove) & !is.na(subRegShp_i_Crop[[subRegCol_i]]),]
    subRegShp_i_Crop@data <- droplevels(subRegShp_i_Crop@data)
    head(subRegShp_i_Crop@data); levels(subRegShp_i_Crop@data[[subRegCol_i]])
    metis.map(dataPolygon=subRegShp_i_Crop,fillColumn = subRegCol_i,labels=T ,printFig=F,facetsON=F)


# Read in the datatable with values by subRegion
    examplePolygonTable_i<-paste(getwd(),"/dataFiles/examples/example_GCAMBasins_analysis.csv",sep="")
    polyTable=read.csv(examplePolygonTable_i);head(polyTable)
    unique(polyTable$x); # check available number of years.

    # Make sure shapefile subRegions and PolygonTable subregions match
    unique(polyTable$subRegion); unique(subRegShp_i_Crop@data[[subRegCol_i]])

    metis.mapProcess(polygonDataTables=examplePolygonTable_i,
                     #gridDataTables=exampleGridTable_i,
                     xRange=c(2010,2020,2100),
                     mapsOutFolderName=boundaryRegionsSelect_i,
                     boundaryRegionsSelect=boundaryRegionsSelect_i,
                     boundaryRegShape=boundaryRegShp_i,
                     subRegShape=subRegShp_i_Crop,
                     subRegCol=subRegCol_i,
                     subRegType="GCAMBasin",
                     nameAppend="",
                     animateOn=T,
                     delay=100,
                     scenRef="SSP2_Ref",
                     extension=F,
                     diffOn = F)


# Improved map using available parameters.
    # Shift legend outside and change the scale_range to get conistent scale across scenarios.

# Set scale ranges across scenarios to be the same.
    # Check range of data for each param
    for(param_i in unique(polyTable$param)){
      print(paste("param: ", param_i, sep=""));print("Range is:")
      print(range((polyTable%>%dplyr::filter(param==param_i))$value))}

    scaleRange_i = tibble::tribble(
      ~param,~minScale, ~maxScale,
      "waterConsumption", 0, 10)

    metis.mapProcess(polygonDataTables=examplePolygonTable_i,
                     #gridDataTables=exampleGridTable_i,
                     xRange=c(2010,2020,2100),
                     mapsOutFolderName=paste(boundaryRegionsSelect_i,"_Edited",sep=""),
                     boundaryRegionsSelect=boundaryRegionsSelect_i,
                     boundaryRegShape=boundaryRegShp_i,
                     subRegShape=subRegShp_i_Crop,
                     subRegCol=subRegCol_i,
                     subRegType="GCAMBasin",
                     nameAppend="_improvedFig",
                     legendPosition=c("LEFT","bottom"),
                     animateOn=T,
                     delay=100,
                     scenRef="SSP2_Ref",
                     extension=F,
                     diffOn = F,
                     legendOutsideSingle = T,
                     scaleRange = scaleRange_i)

