

#----------------------------
# Install necessary packages
#----------------------------
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


#-----------------------------
# Most Basic Setup. (Will run with default GCAM scenarios and produce state and GCAM basin maps)
#-----------------------------

# Select countries and then choose modules to run.

gcamcountryNames_i <- c("Uruguay")

# USA, Africa_Eastern, Africa_Northern, Africa_Southern, Africa_Western, Australia_NZ, Brazil, Canada
# Central America and Caribbean, Central Asia, China, EU-12, EU-15, Europe_Eastern, Europe_Non_EU,
# European Free Trade Association, India, Indonesia, Japan, Mexico, Middle East, Pakistan, Russia,
# South Africa, South America_Northern, South America_Southern, South Asia, South Korea, Southeast Asia,
# Taiwan, Argentina, Colombia, Uruguay


#-----------------------------
# Intermediate Setup (Change GCAM databases and Scenarios)
#-----------------------------

if(TRUE){
# GCAM Scenarios
gcamdatabasePath_i <-paste("D:/GCAM/gcam-core_LAC/output",sep="")
gcamdatabaseName_i <-"database_basexdb"
dataProj_i <-"GCAMdataProj.proj"
dataProjPath_i <- paste(getwd(),"/dataFiles/gcam",sep="")
queryPath_i <-paste(getwd(),"/dataFiles/gcam",sep="")

# If Data Project Exists
if (file.exists(paste(dataProjPath_i, "/", dataProj_i, sep = ""))) {
dataProjLoaded <- rgcam::loadProject(paste(dataProjPath_i, "/", dataProj_i, sep = ""))
scenarios <- listScenarios(dataProjLoaded); print(scenarios)  # List of Scenarios in GCAM database
queries <- listQueries(dataProjLoaded); print(queries)  # List of Queries in queryxml
} else {print("Check gcam database file to get list of scenario names and then set scenOrigNames and scenNewNames")}

scenOrigNames_i <-c("IDBUruguay_GCAMOrig", "IDBUruguay_GCAMRef")
scenNewNames_i <- c("GCAMOrig","GCAMRef")
scenRef_i <- c("GCAMRef")
}

#-----------------------------
# Intermediate Setup (Change GCAM databases and Scenarios)
#-----------------------------
# For GCAM and Charts
# c("finalNrgbySec", "primNrgConsumByFuel", "elecByTech",
# "watConsumBySec", "watWithdrawBySec", "watWithdrawByCrop", "watBioPhysCons", "irrWatWithBasin","irrWatConsBasin",
# "gdpPerCapita", "gdp", "gdpGrowthRate", "pop", "agProdbyIrrRfd",
# "agProdBiomass", "agProdForest", "agProdByCrop", "landIrrRfd", "aggLandAlloc",
# "LUCemiss", "co2emission", "co2emissionByEndUse", "ghgEmissionByGHG", "ghgEmissByGHGGROUPS")

paramsSelect_GCAM_i = c("All")
paramsSelect_Chart_i = c("LUCemiss", "co2emission", "co2emissionByEndUse", "ghgEmissionByGHG", "ghgEmissByGHGGROUPS")


paramsSelect_Map_i = c("All")

#-----------------------------
# Local Data (Add Local Data tables and Shapefiles)
#-----------------------------

# Local Data tables for national Results
dataTablesLocal_i = c("D:/metis/outputs/readGCAMTables/Tables_Local/local_Regional_Uruguay.csv")
localcountryName_i = NULL
localShapeFileFolder_i = NULL
localShapeFile_i = NULL
localShapeFileColName_i  = NULL

if(T) {
# Local Shape Files
localcountryName_i = "Argentina"
localShapeFileFolder_i = paste(getwd(),"/dataFiles/gis/shapefiles_Argentina",sep="")
localShapeFile_i = "colorado_ten_subregions_v3"
tempShape<-readOGR(dsn=localShapeFileFolder_i,
                   layer=localShapeFile_i,use_iconv=T,encoding='UTF-8')
head(tempShape@data)
localShapeFileColName_i = "cuenca" # Will need to load the file to see which name this would be
localSubRegDataTables_i = paste(getwd(),"/outputs/Maps/Tables/Colorado_reference_scarcity.csv",sep="")
#localSubRegDataTables_i = NULL
}


#-----------------------------
# Modules to Run
#-----------------------------
if(TRUE){
run_readGCAM_i   = T; reReadDataGCAM_i    = 0; # If a dataProj file has already been created then leave this as 0.
run_charts_i     = T; regionCompareOnly_i = 0; scenarioCompareOnly_i = 1;
run_boundaries_i = F;
run_bia_i        = F;
run_prepGrids_i  = F; reReadDataPrepGrids_i = 1; sqliteUSE_i = T; paramsSelect_grid2poly_i="All";
run_grid2poly_i  = F; run_state_grid2poly_i = T; run_GCAMbasin_grid2poly_i = F; run_localShape_grid2poly_i = F;
run_maps_i       = F; run_map_state_i = T;       run_map_GCAMbasin_i = T;  run_map_grid_i = T;
                      run_map_localShapeLocalData_i = T; run_map_localShapeGCAMData_i = T; run_map_localShapeGCAMGrid_i = T;
run_io_i         = F}

#-----------------------------
# Run Model
#-----------------------------

x <-metis.runAll(run_readGCAM = run_readGCAM_i,
             gcamcountryNames = gcamcountryNames_i, # Must be GCAM regions
             reReadDataGCAM = reReadDataGCAM_i,
             gcamdatabasePath = gcamdatabasePath_i,
             gcamdatabaseName = gcamdatabaseName_i,
             dataProjPath = dataProjPath_i,
             dataProj = dataProj_i,
             scenOrigNames = scenOrigNames_i,
             scenNewNames = scenNewNames_i,
             paramsSelect_GCAM=paramsSelect_GCAM_i,
             # CHARTS
             dataTablesLocal = dataTablesLocal_i,
             run_charts = run_charts_i,
             scenRef = scenRef_i, # Default is NULL
             regionCompareOnly = regionCompareOnly_i, # Default is "0"
             scenarioCompareOnly = scenarioCompareOnly_i, # Default is "0"
             colOrder1 = c("GCAMOrig","GCAMRef","Local Data"),
             colOrderName1 = "scenario",
             xCompare = c("2010","2015","2020","2050"),
             xRange = c(2010,2015,2020,2025,2030,2035,2040,2045,2050),
             paramsSelect_Chart=paramsSelect_Chart_i,
             # BOUNDARIES
             run_boundaries = run_boundaries_i,
             localcountryName = localcountryName_i, # Country or Region Name
             localShapeFileFolder = localShapeFileFolder_i ,
             localShapeFile = localShapeFile_i,
             localShapeFileColName = localShapeFileColName_i, # Make sure this is one of the names(tempShape@data)
             # BIA
             run_bia = run_bia_i,
             subsectorNAdistribute = "totalOther", # Choose between "totalOther" or "even"
             # PREP GRIDS
             run_prepGrids = run_prepGrids_i,
             reReadDataPrepGrids=reReadDataPrepGrids_i,
             sqliteUSE = sqliteUSE_i,
             sqliteDBNamePath =paste(getwd(),"/outputs/Grids/gridMetis.sqlite", sep = ""),
             gridMetisData=paste(getwd(),"/outputs/Grids/gridMetis.RData", sep = ""),
             # Grid2Poly
             run_grid2poly = run_grid2poly_i,
             run_state_grid2poly = run_state_grid2poly_i,
             run_GCAMbasin_grid2poly = run_GCAMbasin_grid2poly_i,
             run_localShape_grid2poly = run_localShape_grid2poly_i,
             # If sqliteUSE is T above it will use the SQL produced in
             grid=paste(getwd(),"/outputs/Grids/gridMetis.RData", sep = ""), # If additional grids are to be run
             paramsSelect_grid2poly=paramsSelect_grid2poly_i,
             # MAPS
             paramsSelect_Map=paramsSelect_Map_i,
             run_maps = run_maps_i,
             xRangeMap= seq(from=2000,to=2050,by=5),
             legendPosition=c("LEFT","bottom"),
             indvScenarios = "All",
             GCMRCPSSPPol=F,
             scenRefMap="gfdl-esm2m_rcp2p6_NA_NA", # Must be one of the GCM RCP scenario combinations
             run_map_grid = run_map_grid_i,
             run_map_state = run_map_state_i,
             run_map_GCAMbasin = run_map_GCAMbasin_i,
             localSubRegDataTables = localSubRegDataTables_i,
             run_map_localShapeLocalData = run_map_localShapeLocalData_i,
             run_map_localShapeGCAMData = run_map_localShapeGCAMData_i,
             run_map_localShapeGCAMGrid = run_map_localShapeGCAMGrid_i,
             # IO RUN
             run_io = run_io_i)

