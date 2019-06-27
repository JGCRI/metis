

#--------------------------------
# Install necessary packages
#--------------------------------
if("devtools" %in% rownames(installed.packages()) == F){install.packages("devtools")}
library(devtools)
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
library(metis)


#------------------------------------------------------------------------------------------------
# Most Basic Setup. (Will run with default GCAM scenarios and produce state and GCAM basin maps)
#------------------------------------------------------------------------------------------------

# Select countries and then choose modules to run.
gcamcountryNames_i <- c("China")

# USA, Africa_Eastern, Africa_Northern, Africa_Southern, Africa_Western, Australia_NZ, Brazil, Canada
# Central America and Caribbean, Central Asia, China, EU-12, EU-15, Europe_Eastern, Europe_Non_EU,
# European Free Trade Association, India, Indonesia, Japan, Mexico, Middle East, Pakistan, Russia,
# South Africa, South America_Northern, South America_Southern, South Asia, South Korea, Southeast Asia,
# Taiwan, Argentina, Colombia, Uruguay


#----------------------------------------------------------
# Intermediate Setup (Change GCAM databases and Scenarios)
#----------------------------------------------------------

if(F)
{
  # GCAM Scenarios
  gcamdatabasePath_i <-paste("D:/GCAM/gcam-core_LAC/output",sep="")
  gcamdatabaseName_i <-"database_basexdb"
  dataProj_i <-"GCAMdataProj.proj"
  dataProjPath_i <- paste(getwd(),"/dataFiles/gcam",sep="")
  queryPath_i <-paste(getwd(),"/dataFiles/gcam",sep="")

  # If Data Project Exists
  if (file.exists(paste(dataProjPath_i, "/", dataProj_i, sep = "")))
  {
    dataProjLoaded <- rgcam::loadProject(paste(dataProjPath_i, "/", dataProj_i, sep = ""))
    # List of Scenarios in GCAM database
    scenarios <- listScenarios(dataProjLoaded); print(scenarios)
    # List of Queries in queryxml
    queries <- listQueries(dataProjLoaded); print(queries)

  }
  else
  {
    print("Check gcam database file to get list of scenario names and then set scenOrigNames and scenNewNames")
  }
  #************************************************************ JE - These must correspond to the name in model interface, correct? - yes
  scenOrigNames_i <-c("IDBUruguay_GCAMOrig", "IDBUruguay_GCAMRef")
  scenNewNames_i <- c("GCAMOrig","GCAMRef")
  scenRef_i <- c("GCAMRef") # JE - Where is this coming from, is it the ref from model Int? - from MI, needed for Dif
}

#-----------------------------------------------
# Main parameters
#-----------------------------------------------

#************************************************************ JE - what are these referring to - these are queries in metis queries file
# For GCAM and Charts
# c("finalNrgbySec", "primNrgConsumByFuel", "elecByTech",
# "watConsumBySec", "watWithdrawBySec", "watWithdrawByCrop", "watBioPhysCons", "irrWatWithBasin","irrWatConsBasin",
# "gdpPerCapita", "gdp", "gdpGrowthRate", "pop", "agProdbyIrrRfd",
# "agProdBiomass", "agProdForest", "agProdByCrop", "landIrrRfd", "aggLandAlloc",
# "LUCemiss", "co2emission", "co2emissionByEndUse", "ghgEmissionByGHG", "ghgEmissByGHGGROUPS", "basins")

paramsSelect_GCAM_i       = c("All") #queries up above
paramsSelect_Chart_i      = c("watConsumBySec", "watWithdrawBySec", "basins", "irrWatWithBasin","irrWatConsBasin") #points to queries above
paramsSelect_Map_i        = c("All") #points to queries above

polygonDataTablesCustom_i = "E:/projects/NewAmerica/Maps_Tables/Consumption_bySource_all3.csv" #need defintion in help
scaleRange_i=data.frame(param=c("waterConsumption"), maxScale=c(51), minScale=c(0)) #sets scale range for maps

#--------- Check your Basin Names-------------#
countryName="China"
# View default metis country shapefile (Natural Earth maps) - Always the same file
NE0<-readOGR(dsn=paste(getwd(),"/dataFiles/gis/naturalEarth",sep=""),
             layer="ne_10m_admin_0_countries",use_iconv=T,encoding='UTF-8')

if(!countryName %in% unique(NE0@data$NAME)){stop(print(paste(countryName, " not in NE0 countries. Please check data.", sep="")))}

countryNE0<-NE0[(NE0$NAME==countryName),]
head(countryNE0@data)
projX<-proj4string(countryNE0)

# GCAM Basins
GCAMBasin<-readOGR(dsn=paste(getwd(),"/dataFiles/gis/basin_GCAM",sep=""),
                   layer="Global235_CLM_final_5arcmin_multipart",use_iconv=T,encoding='UTF-8')
GCAMBasin<-spTransform(GCAMBasin,CRS(projX))
countryGCAMBasin<-raster::crop(GCAMBasin,countryNE0)
# subset any islands or regions not wanted
countryGCAMBasin<-countryGCAMBasin[(!countryGCAMBasin$basin_name %in% c("Amu_Darya", "Syr_Darya")),]
countryGCAMBasin@data <- droplevels(countryGCAMBasin@data) # Need to drop the removed regions
head(countryGCAMBasin@data)
countryGCAMBasin@data$basin_name
#--------------------------------------------




#---------------------------------------------------
# Local Data (Add Local Data tables and Shapefiles)
#---------------------------------------------------
# Local Data tables for national Results
#dataTablesLocal_i      = c("D:/metis/outputs/readGCAMTables/Tables_Local/local_Regional_Uruguay.csv")
dataTablesLocal_i       = NULL
localcountryName_i      = NULL
localShapeFileFolder_i  = NULL
localShapeFile_i        = NULL
localShapeFileColName_i = NULL

if(F)
{
  # Local Shape Files
  localcountryName_i      = "Argentina"
  localShapeFileFolder_i  = paste(getwd(),"/dataFiles/gis/shapefiles_Argentina",sep="")
  localShapeFile_i        = "colorado_ten_subregions_v3"
  tempShape <- readOGR(dsn=localShapeFileFolder_i, layer=localShapeFile_i,use_iconv=T,encoding='UTF-8')
  head(tempShape@data)
  # Will need to load the file to see which name this would be
  localShapeFileColName_i = "cuenca"
  localSubRegDataTables_i = paste(getwd(),"/outputs/Maps/Tables/Colorado_reference_scarcity.csv",sep="")
  #localSubRegDataTables_i = NULL
}


#-----------------------------
# Modules to Run
#-----------------------------

#connect and run to gcam
if(F)
{
  #connect and run to gcam
  #used for reading GCAM database only - reread speciices to recreate proj file
  run_readGCAM_i                = F;
  reReadDataGCAM_i              = 0; # If a dataProj file has already been created then leave this as 0.

  # run charts creates charts, region and scenario subest the charts - runs quicker if enabled
  run_charts_i                  = F;
  scenarioCompareOnly_i         = 0;
  regionCompareOnly_i           = 0;

  # creates plots/maps in the boundaries directory
  run_boundaries_i              = T;

  #downscales
  run_bia_i                     = F;

  # downscales grids, aggregating by shape file, etc
  paramsSelect_grid2poly_i      = "All";
  run_map_grid_i                = F;
  reReadDataPrepGrids_i         = F;
  run_GCAMbasin_grid2poly_i     = F;
  run_grid2poly_i               = F;
  run_localShape_grid2poly_i    = F;
  run_prepGrids_i               = F;
  run_state_grid2poly_i         = F;

  #creates the maps in maps dir - 3 local vars only for local usage
  run_map_localShapeLocalData_i = F;
  run_map_localShapeGCAMData_i  = F;
  run_map_localShapeGCAMGrid_i  = F;
  run_map_state_i               = F;
  run_maps_i                    = T;
  run_map_GCAMbasin_i           = T;
  sqliteUSE_i                   = T;

  run_io_i                      = F;
}

#-----------------------------
# Run Model
#-----------------------------

x <-metis.runAll(

  #--------------------------------------------
  # MAIN CONFIG
  #--------------------------------------------

  run_readGCAM      = run_readGCAM_i,
  # Must be GCAM regions
  gcamcountryNames  = gcamcountryNames_i,
  reReadDataGCAM    = reReadDataGCAM_i,
  gcamdatabasePath  = gcamdatabasePath_i,
  gcamdatabaseName  = gcamdatabaseName_i,
  dataProjPath      = dataProjPath_i,
  dataProj          = dataProj_i,
  scenOrigNames     = scenOrigNames_i,
  scenNewNames      = scenNewNames_i,
  paramsSelect_GCAM = paramsSelect_GCAM_i,

  #----------------------------------------
  # CHARTS
  #----------------------------------------

  dataTablesLocal     = dataTablesLocal_i,
  run_charts          = run_charts_i,
  # Default is NULL
  scenRef             = scenRef_i,
  # Default is "0"
  regionCompareOnly   = regionCompareOnly_i,
  # Default is "0"
  scenarioCompareOnly = scenarioCompareOnly_i,
  #************************************************************ JE - what is xcompare and xrange
  xCompare            = c("2010","2030","2050", "2100"),
  xRange              = c(2010,2030,2050, 2100),
  paramsSelect_Chart  = paramsSelect_Chart_i,
  #colOrder1 = c("GCAMOrig","GCAMRef","Local Data"),
  #colOrderName1 = "scenario",

  #----------------------------------------
  # BOUNDARIES
  #----------------------------------------

  run_boundaries        = run_boundaries_i,
  # Country or Region Name
  localcountryName      = localcountryName_i,
  localShapeFileFolder  = localShapeFileFolder_i ,
  localShapeFile        = localShapeFile_i,
  # Make sure this is one of the names(tempShape@data)
  localShapeFileColName = localShapeFileColName_i,
  boundaryGridsOverlap  = NULL,

  #----------------------------------------
  # BIA
  #----------------------------------------

  run_bia               = run_bia_i,
  # Choose between "totalOther" or "even"
  subsectorNAdistribute = "totalOther", #************************************************************ JE - ?

  #----------------------------------------
  # PREP GRIDS
  #----------------------------------------

  run_prepGrids            = run_prepGrids_i,
  reReadDataPrepGrids      = reReadDataPrepGrids_i,
  sqliteUSE                = sqliteUSE_i,
  sqliteDBNamePath         = paste(getwd(),"/outputs/Grids/gridMetis.sqlite", sep = ""), #************************************************************ JE - ?
  gridMetisData            = paste(getwd(),"/outputs/Grids/gridMetis.RData", sep = ""),#************************************************************ JE - ?

  # Grid2Poly
  run_grid2poly            = run_grid2poly_i,
  run_state_grid2poly      = run_state_grid2poly_i,
  run_GCAMbasin_grid2poly  = run_GCAMbasin_grid2poly_i,
  run_localShape_grid2poly = run_localShape_grid2poly_i,
  # If sqliteUSE is T above it will use the SQL produced in
  # If additional grids are to be run
  grid                     = paste(getwd(),"/outputs/Grids/gridMetis.RData", sep = ""), #************************************************************ JE - ?
  paramsSelect_grid2poly   = paramsSelect_grid2poly_i,

  #--------------------------------------------
  # MAPS
  #--------------------------------------------

  polygonDataTablesCustom     = polygonDataTablesCustom_i,
  paramsSelect_Map            = paramsSelect_Map_i,
  run_maps                    = run_maps_i,
  scaleRange_i                = scaleRange_i,
  xRangeMap                   = seq(from=2000,to=2100,by=10),
  legendPosition              = c("LEFT","bottom"),
  indvScenarios               = "All",
  GCMRCPSSPPol                = F,
  # Must be one of the GCM RCP scenario combinations
  scenRefMap                  = "gfdl-esm2m_rcp2p6_NA_NA",
  run_map_grid                = run_map_grid_i,
  run_map_state               = run_map_state_i,
  run_map_GCAMbasin           = run_map_GCAMbasin_i,
  localSubRegDataTables       = localSubRegDataTables_i,
  run_map_localShapeLocalData = run_map_localShapeLocalData_i,
  run_map_localShapeGCAMData  = run_map_localShapeGCAMData_i,
  run_map_localShapeGCAMGrid  = run_map_localShapeGCAMGrid_i,

  #--------------------------------------------
  # IO RUN
  #--------------------------------------------

  run_io = run_io_i

)

