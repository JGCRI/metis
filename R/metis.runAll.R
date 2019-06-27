#' metis.runAll
#'
#' This function is the wrapper to run the full metis workflow. Defaults can be changed to modifiy the runs.
#' modules to Run
#' @param run_readGCAM Default = T,
#' @param run_charts Default = T,
#' @param run_boundaries Default = T,
#' @param run_bia Default = T,
#' @param run_prepGrids Default = T,
#' @param run_grid2poly Default = T,
#' @param run_maps Default = T,
#' @param run_io Default = T,
#' @param gcamcountryNames Default = c("Argentina","Colombia","Uruguay"),
#' @param reReadDataGCAM Default = F,
#' @param gcamdatabasePath Default =paste("D:/GCAM/gcam-core_LAC/output",sepDefault =""),
#' @param gcamdatabaseName Default ="database_basexdb",
#' @param dataProjPath Default = paste(getwd(),"/dataFiles/gcam",sepDefault =""),
#' @param queryPath Default = paste(getwd(),"/dataFiles/gcam",sepDefault =""),
#' @param queryxml Default = "metisQueries.xml",
#' @param scenOrigNames Default = c("IDBUruguay_GCAMOrig", "IDBUruguay_GCAMRef"),
#' @param scenNewNames Default = c("GCAMOrig","GCAMRef"),
#' @param dataProj Default = paste("GCAMdataProj.proj",sepDefault =""),
#' @param dataTablesLocal Default = NULL.
#' @param scenRef Default = "GCAMRef"
#' @param regionCompareOnly Default = 0, # Default is "0"
#' @param scenarioCompareOnly Default = 1, # Default is "0"
#' @param useNewLabels Default = 1,
#' @param colOrder1 Default = c("GCAMOrig","GCAMRef","Local Data"),
#' @param colOrderName1 Default = "scenario",
#' @param xCompare Default = c("2015","2030","2050","2100"),
#' @param xRange Default = c(2010,2015,2020,2025,2030,2035,2040,2045,2050),
#' @param localcountryName Default = NULL
#' @param localShapeFileFolder Default = NULL
#' @param localShapeFile Default = "NULL
#' @param localShapeFileColName Default = NULL, # Make sure this is one of the names(tempShape@data)
#' @param biaInputsFolder Default = paste(getwd(),"/dataFiles/grids/bia/biaInputs",sepDefault =""),
#' @param biaInputsFiles Default = c("global_power_plant_database_MW"),
#' @param gridChoice Default = "grid_025", # Choose between "grid_025" and "grid_050"
#' @param diagnosticsON Default = T,
#' @param subsectorNAdistribute Default = "totalOther", # Choose between "totalOther" or "even"
#' @param nameAppendBia Default = "",
#' @param reReadDataPrepGrids Default =0,
#' @param demeterFolder Default =paste(getwd(),"/dataFiles/grids/demeter/",sepDefault =""),
#' @param demeterScenario Default ="Eg1",
#' @param demeterUnits Default ="Landuse (Fraction)",
#' @param demeterTimesteps Default =seq(fromDefault =2005,toDefault =2020,byDefault =5),
#' @param tethysFolder Default =paste(getwd(),"/dataFiles/grids/tethys/",sepDefault =""),
#' @param tethysScenario Default ="Eg1",
#' @param copySingleTethysScenbyXanthos Default ="Eg1",
#' @param tethysFiles Default =c("wddom","wdelec","wdirr","wdliv","wdmfg","wdmin","wdnonag","wdtotal"),
#' @param tethysUnits Default ="Water Withdrawals (mm)",
#' @param xanthosFolder Default =paste(getwd(),"/dataFiles/grids/xanthosRunsChris/",sepDefault =""),
#' @param xanthosFiles Default = See file in code. Name too long to print here.
#' @param xanthosCoordinatesPath Default =paste(getwd(),"/dataFiles/grids/xanthosReference/coordinates.csv",sepDefault =""),
#' @param xanthosGridAreaHecsPath Default =paste(getwd(),"/dataFiles/grids/xanthosReference/Grid_Areas D.csv",sepDefault =""),
#' @param scarcityXanthosRollMeanWindow Default =10,
#' @param spanLowess Default =0.25,
#' @param popFolder Default =paste(getwd(),"/dataFiles/grids/griddedIDsPop/",sepDefault =""),
#' @param popFiles Default ="grid_pop_map",
#' @param popUnits Default ="Population",
#' @param biaFolder Default =paste(getwd(),"/dataFiles/grids/bia/biaOutputs/",sepDefault =""),
#' @param biaFiles Default =paste("dataBia",nameAppendBia,sepDefault =""),
#' @param gridMetisData Default =paste(getwd(),"/outputs/Grids/gridMetis.RData", sep Default = ""),
#' @param sqliteUSE Default = T,
#' @param sqliteDBNamePath Default =paste(getwd(),"/outputs/Grids/gridMetis.sqlite", sep Default = ""),
#' @param grid Default = NULL,
#' @param boundaryGridsOverlap Default = NULL
#' @param paramsSelect_grid2poly Default = "All"
#' @param sqliteUSEGrid2Poly Default = T,
#' @param sqliteDBNamePathGrid2Poly Default = paste(getwd(),"/outputs/Grids/gridMetis.sqlite", sep Default = ""),
#' @param run_state_grid2poly Default = F,
#' @param run_GCAMbasin_grid2poly Default = F,
#' @param run_localShape_grid2poly Default = T,
#' @param polygonDataTablesCustom Default = NULL,
#' @param scaleRange_i Default = NULL,
#' @param xRangeMap Default = seq(fromDefault =2000,toDefault =2050,byDefault =5),
#' @param legendPosition Default =c("LEFT","bottom"),
#' @param legendOutsideSingle Default =T,
#' @param animateOn Default =T,
#' @param delay Default =100,
#' @param scenRefMap Default ="gfdl-esm2m_rcp2p6_NA_NA",
#' @param paramsSelect_GCAM Default = c("All"),
#' @param paramsSelect_Chart Default = c("All"),
#' @param paramsSelect_Map Default = c("All"),
#' @param indvScenarios Default = "All",
#' @param GCMRCPSSPPol Default =F,
#' @param localSubRegDataTables Default = NULL
#' @param run_map_grid Default = T,
#' @param run_map_state Default = T,
#' @param run_map_GCAMbasin Default = T,
#' @param run_map_localShapeLocalData Default = T
#' @param run_map_localShapeGCAMData Default = T
#' @param run_map_localShapeGCAMGrid Default = T
#' @return A tibble with GCAM electricity generation distributed on a grid for selected regions
#' @keywords metis
#' @export

metis.runAll<- function(
                  # modules to Run
                  run_readGCAM = F,
                  run_charts = F,
                  run_boundaries = F,
                  run_bia = F,
                  run_prepGrids = F,
                  run_grid2poly = F,
                  run_maps = F,
                  run_io = F,
                  # 1. Read GCAM Data
                  gcamcountryNames = NULL,
                  reReadDataGCAM = F,
                  gcamdatabasePath =paste("D:/GCAM/gcam-core_LAC/output",sep=""),
                  gcamdatabaseName ="database_basexdb",
                  dataProjPath = paste(getwd(),"/dataFiles/gcam",sep=""),
                  dataProj =paste("GCAMdataProj.proj",sep=""),
                  queryPath =paste(getwd(),"/dataFiles/gcam",sep=""),
                  queryxml="metisQueries.xml",  # Default Value is "metisQueries.xml"
                  scenOrigNames = c("IDBUruguay_GCAMOrig", "IDBUruguay_GCAMRef"),
                  scenNewNames = c("GCAMOrig","GCAMRef"),
                  paramsSelect_GCAM = c("All"),
                  # 2. Create Charts
                  # GCAM Data will be produced from step 1 Read GCAM Data. It is saved as dataGCAM.
                  dataTablesLocal=NULL,
                  #a=read.csv(dataTables); head(a); unique(a$scenario); unique(a$param); unique(a$x)
                  scenRef = "GCAMRef", # Default is NULL
                  regionCompareOnly = 0, # Default is "0"
                  scenarioCompareOnly = 1, # Default is "0"
                  useNewLabels = 1,
                  colOrder1 = c("GCAMOrig","GCAMRef","Local Data"),
                  colOrderName1 = "scenario",
                  xCompare = c("2015","2030","2050","2100"),
                  xRange = c(2010,2015,2020,2025,2030,2035,2040,2045,2050),
                  paramsSelect_Chart = c("All"),
                  # 3. Prepare Polygon Data (Check and fix shapefiles as needed)
                  localcountryName = NULL, # Country or Region Name
                  localShapeFileFolder = NULL,
                  localShapeFile = NULL,
                  localShapeFileColName = NULL, # Make sure this is one of the names(tempShape@data)
                  # 4. Boundaries
                  boundaryGridsOverlap=c(paste(getwd(),"/dataFiles/grids/emptyGrids/grid_025.csv",sep=""),
                    paste(getwd(),"/dataFiles/grids/emptyGrids/grid_050.csv",sep="")),
                  # 5. Bia
                  biaInputsFolder = paste(getwd(),"/dataFiles/grids/bia/biaInputs",sep=""),
                  biaInputsFiles = c("global_power_plant_database_MW"),
                  gridChoice = "grid_025", # Choose between "grid_025" and "grid_050"
                  diagnosticsON = T,
                  subsectorNAdistribute = "totalOther", # Choose between "totalOther" or "even"
                  nameAppendBia = "",
                  # 6. Prep grids
                  reReadDataPrepGrids=0,
                  demeterFolder=paste(getwd(),"/dataFiles/grids/demeter/",sep=""),
                  demeterScenario="Eg1",
                  demeterUnits="Landuse (Fraction)",
                  demeterTimesteps=seq(from=2005,to=2020,by=5),
                  tethysFolder=paste(getwd(),"/dataFiles/grids/tethys/",sep=""),
                  tethysScenario="Eg1",
                  copySingleTethysScenbyXanthos="Eg1",
                  tethysFiles=c("wddom","wdelec","wdirr","wdliv","wdmfg","wdmin","wdnonag","wdtotal"),
                  tethysUnits="Water Withdrawals (mm)",
                  xanthosFolder=paste(getwd(),"/dataFiles/grids/xanthosRunsChris/",sep=""),
                  xanthosFiles=c("pm_abcd_mrtm_noresm1-m_rcp8p5_1950_2099/q_km3peryear_pm_abcd_mrtm_noresm1-m_rcp8p5_1950_2099.csv"),
                  xanthosCoordinatesPath=paste(getwd(),"/dataFiles/grids/xanthosReference/coordinates.csv",sep=""),
                  xanthosGridAreaHecsPath=paste(getwd(),"/dataFiles/grids/xanthosReference/Grid_Areas_ID.csv",sep=""),
                  scarcityXanthosRollMeanWindow=10,
                  spanLowess=0.25,
                  popFolder=paste(getwd(),"/dataFiles/grids/griddedIDsPop/",sep=""),
                  popFiles="grid_pop_map",
                  popUnits="Population",
                  biaFolder=paste(getwd(),"/dataFiles/grids/bia/biaOutputs/",sep=""),
                  biaFiles=paste("dataBia",nameAppendBia,sep=""),
                  gridMetisData=paste(getwd(),"/outputs/Grids/gridMetis.RData", sep = ""),
                  sqliteUSE = T,
                  sqliteDBNamePath =paste(getwd(),"/outputs/Grids/gridMetis.sqlite", sep = ""),
                  # 7. Grid to polygons
                  grid=NULL,
                  paramsSelect_grid2poly="All",
                  polygonDataTablesCustom = NULL,
                  sqliteUSEGrid2Poly = T,
                  sqliteDBNamePathGrid2Poly = paste(getwd(),"/outputs/Grids/gridMetis.sqlite", sep = ""),
                  run_state_grid2poly = T,
                  run_GCAMbasin_grid2poly = T,
                  run_localShape_grid2poly = T,
                  # 8. Produce Maps
                  scaleRange_i = NULL,
                  xRangeMap= seq(from=2000,to=2050,by=5),
                  legendPosition=c("LEFT","bottom"),
                  legendOutsideSingle=T,
                  animateOn=T,
                  delay=100,
                  scenRefMap="gfdl-esm2m_rcp2p6_NA_NA",
                  paramsSelect_Map = c("All"),
                  indvScenarios = "All",
                  GCMRCPSSPPol=F,
                  localSubRegDataTables=NULL,
                  run_map_grid = T,
                  run_map_state = T,
                  run_map_GCAMbasin = T,
                  run_map_localShapeLocalData = T,
                  run_map_localShapeGCAMData = T,
                  run_map_localShapeGCAMGrid = T
              ){


  #----------------
  # Initialize variables by setting to NULL
  #----------------

  #-----------------------------
  # Modules to Run
  #-----------------------------

 if(!is.null(localcountryName)){localcountryName = tools::toTitleCase(localcountryName)}; localcountryName


  #----------------------------
  # Prelim settings
  #---------------------------

  NULL -> dataGCAM ->  polygonDataTables_x

  #----------------------------
  # Read GCAM Data
  #---------------------------
  if(run_readGCAM==T){
    dataGCAM<-metis.readgcam(reReadData=reReadDataGCAM, # Default Value is T
                             dataProj = dataProj, # Default Value is "dataProj.proj"
                             dataProjPath = dataProjPath,
                             scenOrigNames=scenOrigNames,
                             scenNewNames=scenNewNames,
                             gcamdatabasePath=gcamdatabasePath,
                             gcamdatabaseName=gcamdatabaseName,
                             queryxml="metisQueries.xml",  # Default Value is "metisQueries.xml"
                             queryPath = queryPath,
                             dirOutputs= paste(getwd(),"/outputs",sep=""), # Default Value is paste(getwd(),"/outputs",sep="")
                             regionsSelect=gcamcountryNames, # Default Value is NULL
                             paramsSelect=paramsSelect_GCAM # Default value is "All"
    )
  }

  # dataGCAM # To view the data read that was read.
  # dataGCAM$data
  # unique(dataGCAM$data$param)
  # unique(dataGCAM$data$scenario)

  # #----------------------------
  # # Produce Data Charts
  # #---------------------------
  if(run_charts==T){
    #rTable <- dataGCAM$data ## Read in the data from the function metis.readgcam

    dataTables <- dataTablesLocal

    for(countryName_i in gcamcountryNames){
    dataTables <- c(dataTables,paste(getwd(),"/outputs/readGCAMTables/Tables_gcam/gcamDataTable_",countryName_i,".csv", sep=""))
                      }

    charts<-metis.chartsProcess(#rTable=rTable, # Default is NULL
                                dataTables=dataTables, # Default is NULL
                                paramsSelect=paramsSelect_Chart, # Default is "All"
                                regionsSelect=gcamcountryNames, # Default is "All"
                                xCompare=xCompare, # Default is c("2015","2030","2050","2100")
                                scenRef=scenRef, # Default is NULL
                                dirOutputs=paste(getwd(),"/outputs",sep=""), # Default is paste(getwd(),"/outputs",sep="")
                                pdfpng="png", # Default is "png"
                                regionCompareOnly=regionCompareOnly, # Default is "0"
                                scenarioCompareOnly=scenarioCompareOnly, # Default is "0"
                                useNewLabels=useNewLabels,
                                xRange=xRange,
                                colOrder1 = colOrder1,
                                colOrderName1 = colOrderName1)

    # # #rTable=rTable, # Default is NULL
    # dataTables=c(paste(getwd(),"/outputs/readGCAMTables/Tables_Local/local_Regional_Uruguay.csv",sep=""),
    #              paste(getwd(),"/outputs/readGCAMTables/Tables_gcam/gcamDataTable_Uruguay.csv", sep=""))
    # paramsSelect="aggLandAlloc" # Default is "All"
    # regionsSelect="uruguay" # Default is "All"
    # scenarioCompareOnly=1 # Default is "0"

  }

  #------------
  # Prepare Polygons
  #----------------
  if(run_boundaries == T |
     run_bia == T |
     run_prepGrids == T |
     run_grid2poly  == T |
     run_maps == T){

    if(!is.null(gcamcountryNames)){
    for(countryName_i in gcamcountryNames){

      countryName = countryName_i

      NE0<-rgdal::readOGR(dsn=paste(getwd(),"/dataFiles/gis/naturalEarth",sep=""),
                   layer="ne_10m_admin_0_countries",use_iconv=T,encoding='UTF-8')

      projX<-sp::proj4string(NE0)

      # Create directory for country
      if (!dir.exists(paste(getwd(),"/dataFiles/gis/shapefiles_",countryName,sep=""))){
        dir.create(paste(getwd(),"/dataFiles/gis/shapefiles_",countryName,sep=""))}

      if(file.exists(paste(getwd(),"/dataFiles/gis/shapefiles_",countryName,"/",countryName,"NE0.shp",sep=""))){
        countryNE0 = rgdal::readOGR(dsn=paste(getwd(),"/dataFiles/gis/shapefiles_",countryName,sep=""),
                             layer=paste(countryName,"NE0",sep=""),use_iconv=T,encoding='UTF-8')
      }else{
        # View default metis country shapefile (Natural Earth maps)
        if(!countryName %in% unique(NE0@data$NAME)){stop(print(paste(countryName, " not in NE0 countries. Please check data.", sep="")))}
        countryNE0<-rgdal::readOGR(dsn=paste(getwd(),"/dataFiles/gis/naturalEarth",sep=""),
                            layer="ne_10m_admin_0_countries",use_iconv=T,encoding='UTF-8')
        countryNE0<-countryNE0[(countryNE0$NAME==countryName),]
        #head(countryNE0@data)
        raster::plot(countryNE0)
        projX<-sp::proj4string(countryNE0)
        rgdal::writeOGR(obj=countryNE0, dsn=paste(getwd(),"/dataFiles/gis/shapefiles_",countryName,sep=""), layer=paste(countryName,"NE0",sep=""), driver="ESRI Shapefile", overwrite_layer=TRUE)
      }
      metis.map(dataPolygon=countryNE0,fillColumn = "ADMIN",printFig=F, facetsON = F, labels=T, legendStyle = "cat")


      # Natural earth level 1 admin boundaries
      if(file.exists(paste(getwd(),"/dataFiles/gis/shapefiles_",countryName,"/",countryName,"NE1.shp",sep=""))){
        countryNE1 = rgdal::readOGR(dsn=paste(getwd(),"/dataFiles/gis/shapefiles_",countryName,sep=""),
                             layer=paste(countryName,"NE1",sep=""),use_iconv=T,encoding='UTF-8')
        countryNE1<-countryNE1[(!countryNE1$name %in% "San Andrés y Providencia") & !is.na(countryNE1$name),]
        countryNE1@data <- droplevels(countryNE1@data)
      }else{
        NE1<-rgdal::readOGR(dsn=paste(getwd(),"/dataFiles/gis/naturalEarth",sep=""),
                     layer="ne_10m_admin_1_states_provinces",use_iconv=T,encoding='UTF-8')
        if(!countryName %in% unique(NE1@data$admin)){stop(print(paste(countryName, " not in NE1 countries. Please check data.", sep="")))}
        countryNE1<-NE1[(NE1$admin==countryName),]
        # subset any islands or regions not wanted
        countryNE1<-countryNE1[(!countryNE1$name %in% "San Andrés y Providencia") & !is.na(countryNE1$name),]
        countryNE1@data <- droplevels(countryNE1@data)
        #head(countryNE1@data)
        raster::plot(countryNE1)
        countryNE1<-sp::spTransform(countryNE1,sp::CRS(projX))
        rgdal::writeOGR(obj=countryNE1, dsn=paste(getwd(),"/dataFiles/gis/shapefiles_",countryName,sep=""), layer=paste(countryName,"NE1",sep=""), driver="ESRI Shapefile", overwrite_layer=TRUE)
      }
      metis.map(dataPolygon=countryNE1,fillColumn = "name",printFig=F, facetsON = F, labels=T, legendStyle = "cat")


      # GCAM Basins
      if(file.exists(paste(getwd(),"/dataFiles/gis/shapefiles_",countryName,"/",countryName,"GCAMBasin.shp",sep=""))){
        countryGCAMBasin = rgdal::readOGR(dsn=paste(getwd(),"/dataFiles/gis/shapefiles_",countryName,sep=""),
                                   layer=paste(countryName,"GCAMBasin",sep=""),use_iconv=T,encoding='UTF-8')
      }else{
        GCAMBasin<-rgdal::readOGR(dsn=paste(getwd(),"/dataFiles/gis/basin_GCAM",sep=""),
                           layer="Global235_CLM_final_5arcmin_multipart",use_iconv=T,encoding='UTF-8')
        GCAMBasin<-sp::spTransform(GCAMBasin,sp::CRS(projX))
        countryGCAMBasin<-raster::crop(GCAMBasin,countryNE1)
        countryGCAMBasin@data <- droplevels(countryGCAMBasin@data)
        #head(countryGCAMBasin@data)
        raster::plot(countryGCAMBasin)
        rgdal::writeOGR(obj=countryGCAMBasin, dsn=paste(getwd(),"/dataFiles/gis/shapefiles_",countryName,sep=""), layer=paste(countryName,"GCAMBasin",sep=""), driver="ESRI Shapefile", overwrite_layer=TRUE)
      }
      metis.map(dataPolygon=countryGCAMBasin,fillColumn = "basin_name",printFig=F,facetsON = F, labels=T, legendStyle = "cat")
    }
    }

    if(!is.null(localcountryName) & !is.null(localShapeFileFolder) &
       !is.null(localShapeFile) & !is.null(localShapeFileColName)){
    # Local basin Shapefiles
    if(file.exists(paste(getwd(),"/dataFiles/gis/shapefiles_",localcountryName,"/",localcountryName,"local.shp",sep=""))){
      countryLocal = rgdal::readOGR(dsn=paste(getwd(),"/dataFiles/gis/shapefiles_",localcountryName,sep=""),
                             layer=paste(localcountryName,"local",sep=""),use_iconv=T,encoding='UTF-8')
      countryLocal<-countryLocal[(!countryLocal$cuenca %in%
                                    c("media","baja","RioGrande","Barrancas")) & !is.na(countryLocal$cuenca),]
      countryLocal@data <- droplevels(countryLocal@data)
    }else{
      countryLocal<-rgdal::readOGR(dsn=localShapeFileFolder,
                            layer=localShapeFile,use_iconv=T,encoding='UTF-8')
      countryLocal<-sp::spTransform(countryLocal,sp::CRS(projX))
      countryLocal<-raster::crop(countryLocal,countryNE1)
      countryLocal<-countryLocal[(!countryLocal$cuenca %in%
                                    c("media","baja","RioGrande","Barrancas")) & !is.na(countryLocal$cuenca),]
      countryLocal@data <- droplevels(countryLocal@data)
      #head(countryLocal@data)
      raster::plot(countryLocal)
      rgdal::writeOGR(obj=countryLocal, dsn=paste(getwd(),"/dataFiles/gis/shapefiles_",localcountryName,sep=""), layer=paste(localcountryName,"local",sep=""), driver="ESRI Shapefile", overwrite_layer=TRUE)
    }
    metis.map(dataPolygon=countryLocal,fillColumn = localShapeFileColName,printFig=F, facetsON = F, labels=T)

    }
  }


  #-----------
  # Boundaries
  #-----------

  if(run_boundaries == T){

    if(!is.null(gcamcountryNames)){

    for(countryName_i in gcamcountryNames){

      countryName = countryName_i

      if(file.exists(paste(getwd(),"/dataFiles/gis/shapefiles_",countryName,"/",countryName,"NE1.shp",sep=""))){
        boundariesX<- metis.boundaries(
          #boundaryRegShape=NE0,
          boundaryRegShpFolder=paste(getwd(),"/dataFiles/gis/naturalEarth",sep=""),
          boundaryRegShpFile="ne_10m_admin_0_countries",
          boundaryRegCol="NAME",
          boundaryRegionsSelect=countryName,
          #subRegShape=subRegShape_i ,
          subRegShpFolder=paste(getwd(),"/dataFiles/gis/shapefiles_",countryName,sep=""),
          subRegShpFile=paste(countryName,"NE1",sep=""),
          subRegCol="name",
          subRegType = "state",
          nameAppend = "",
          overlapShpFolder=paste(getwd(),"/dataFiles/gis/shapefiles_",countryName,sep=""),
          overlapShpFile=paste(countryName,"GCAMBasin",sep=""),
          grids = boundaryGridsOverlap,
          cropSubShape2Bound=T)}else{
            print(paste("Shapefile for region ", countryName ,"NE1 not produced yet. Please run prepare polygons first.",sep=""))}

      # Plot GCAM Basin boundaries
      if(file.exists(paste(getwd(),"/dataFiles/gis/shapefiles_",countryName,"/",countryName,"GCAMBasin.shp",sep=""))){
        boundariesX<- metis.boundaries(
          boundaryRegShpFolder=paste(getwd(),"/dataFiles/gis/naturalEarth",sep=""),
          boundaryRegShpFile="ne_10m_admin_0_countries",
          boundaryRegCol="NAME",
          boundaryRegionsSelect=countryName,
          subRegShpFolder=paste(getwd(),"/dataFiles/gis/shapefiles_",countryName,sep=""),
          subRegShpFile=paste(countryName,"GCAMBasin",sep=""),
          subRegCol="basin_name",
          subRegType = "GCAMBasin",
          nameAppend = "",
          overlapShpFolder=paste(getwd(),"/dataFiles/gis/shapefiles_",countryName,sep=""),
          overlapShpFile=paste(countryName,"NE1",sep=""),
          grids = boundaryGridsOverlap,
          cropSubShape2Bound=T)}else{
            print(paste("Shapefile for region ", countryName ,"GCAMBasin not produced yet. Please run prepare polygons first.",sep=""))}

    }
    }

    if(!is.null(localcountryName) & !is.null(localShapeFileFolder) &
       !is.null(localShapeFile) & !is.null(localShapeFileColName)){
    # Plot SubRegion Basin boundaries
    boundariesX<- metis.boundaries(
      boundaryRegShpFolder=paste(getwd(),"/dataFiles/gis/naturalEarth",sep=""),
      boundaryRegShpFile="ne_10m_admin_0_countries",
      boundaryRegCol="NAME",
      boundaryRegionsSelect=localcountryName,
      subRegShpFolder=paste(getwd(),"/dataFiles/gis/shapefiles_",localcountryName,sep=""),
      subRegShpFile=paste(localcountryName,"local",sep=""),
      subRegCol=localShapeFileColName,
      subRegType = "localShape",
      nameAppend = "",
      grids = boundaryGridsOverlap,
      cropSubShape2Bound=T)

    }

    # boundaryRegShpFolder=paste(getwd(),"/dataFiles/gis/naturalEarth",sep="")
    # boundaryRegShpFile="ne_10m_admin_0_countries"
    # boundaryRegCol="NAME"
    # boundaryRegionsSelect=localcountryName
    # subRegShpFolder=paste(getwd(),"/dataFiles/gis/shapefiles_",localcountryName,sep="")
    # subRegShpFile=paste(localcountryName,"local",sep="")
    # subRegCol=localShapeFileColName
    # subRegType = "localShape"
    # nameAppend = ""
    # grids = c(paste(getwd(),"/dataFiles/grids/emptyGrids/grid_025.csv",sep=""),
    #           paste(getwd(),"/dataFiles/grids/emptyGrids/grid_050.csv",sep=""))
    # cropSubShape2Bound=T

  }

  #--------------------
  # Run Bia
  #-------------------

  if(run_bia == T) {
    dataBia<-metis.bia(
      biaInputsFolder=biaInputsFolder,
      biaInputsFiles=biaInputsFiles,
      regionsSelect=gcamcountryNames, # Default Value is NULL
      queriesSelect = "All", # Default value is "ALL"
      reReadData=F, # Default Value is T
      dataProj=dataProj, # Default Value is "dataProj.proj"
      dataProjPath=dataProjPath, #Default Value is gcamdatabasePath
      scenOrigNames=scenOrigNames,
      scenNewNames=scenNewNames,
      gcamdatabasePath=gcamdatabasePath,
      gcamdatabaseName=gcamdatabaseName,
      queryxml="metisQueries.xml",  # Default Value is "metisQueries.xml"
      queryPath = queryPath,
      paramsSelect=c("elecByTech", "elecCapBySubsector"), # Default = c("elecByTech", "elecCapBySubsector")
      gridChoice = gridChoice, # Default = "grid_025". Choose between "grid_025" & "grid_050"
      diagnosticsON = diagnosticsON,
      subsectorNAdistribute = subsectorNAdistribute,
      nameAppend=nameAppendBia)
  }


  #------------------------
  # Prepare Grids
  #------------------------

  if(run_prepGrids == T){
    gridMetis<-metis.prepGrid(
      reReadData=reReadDataPrepGrids,
      demeterFolder=demeterFolder,
      demeterScenario=demeterScenario,
      demeterTimesteps=demeterTimesteps,
      demeterUnits=demeterUnits,
      tethysFolder=tethysFolder,
      tethysScenario=tethysScenario,
      copySingleTethysScenbyXanthos=copySingleTethysScenbyXanthos,
      tethysFiles=tethysFiles,
      tethysUnits=tethysUnits,
      xanthosFolder=xanthosFolder,
      xanthosFiles=xanthosFiles,
      xanthosCoordinatesPath=xanthosCoordinatesPath,
      xanthosGridAreaHecsPath=xanthosGridAreaHecsPath,
      biaFolder=biaFolder,
      biaFiles=biaFiles,
      popFolder=popFolder,
      popFiles=popFiles,
      popUnits=popUnits,
      spanLowess=spanLowess,
      dirOutputs=paste(getwd(),"/outputs",sep=""),
      gridMetisData=gridMetisData,
      sqliteUSE = sqliteUSE,
      sqliteDBNamePath =sqliteDBNamePath)
  }


  # biaFolder=paste(getwd(),"/dataFiles/grids/bia/biaOutputs/",sep="")
  # biaFiles=paste("dataBia",sep="")
  # reReadDataPrepGrids=1
  # sqliteUSE = F
  # sqliteDBNamePath =paste(getwd(),"/outputs/Grids/gridMetis.sqlite", sep = "")
  # gridMetisData=paste(getwd(),"/outputs/Grids/gridMetis.RData", sep = "")


  #-----------
  # Grid to Poly
  #-------------

  if(run_grid2poly == T){

    if(!is.null(gcamcountryNames)){
    for(countryName_i in gcamcountryNames){

      countryName = countryName_i

      if(run_state_grid2poly == T){
        # Natural Earth States
        grid2polyX<-metis.grid2poly(
          #grid=grid,
          boundaryRegionsSelect=countryName,
          subRegShpFolder=paste(getwd(),"/dataFiles/gis/shapefiles_",countryName,sep=""),
          subRegShpFile=paste(countryName,"NE1",sep=""),
          subRegCol="name",
          subRegType = "state",
          nameAppend="_NEState",
          sqliteUSE = sqliteUSE,
          sqliteDBNamePath = sqliteDBNamePath,
          paramsSelect=paramsSelect_grid2poly)

        # paramsSelect_grid2poly="All"
        # paramsSelect = "All"
        # countryName="Argentina"
        # grid=paste(getwd(),"/outputs/Grids/gridMetis.RData", sep = "")
        # boundaryRegionsSelect=countryName
        # subRegShpFolder=paste(getwd(),"/dataFiles/gis/shapefiles_",countryName,sep="")
        # subRegShpFile=paste(countryName,"NE1",sep="")
        # subRegCol="name"
        # subRegType = "state"
        # nameAppend="_NEState"
        # sqliteUSE = T
        # sqliteDBNamePath = paste(getwd(),"/outputs/Grids/gridMetis.sqlite", sep = "")
        # paramsSelect="elecByTech"

      }

      if(run_GCAMbasin_grid2poly == T){
        # GCAM Basins
        grid2polyX<-metis.grid2poly(
          grid=grid,
          boundaryRegionsSelect=countryName,
          subRegShpFolder=paste(getwd(),"/dataFiles/gis/shapefiles_",countryName,sep=""),
          subRegShpFile=paste(countryName,"GCAMBasin",sep=""),
          subRegCol="basin_name",
          subRegType = "basin",
          nameAppend="_GCAMBasin",
          sqliteUSE = sqliteUSE,
          sqliteDBNamePath = sqliteDBNamePath,
          paramsSelect=paramsSelect_grid2poly)
      }
    }}

    if(run_localShape_grid2poly == T){
      if(!is.null(localcountryName) & !is.null(localShapeFileFolder) &
         !is.null(localShapeFile) & !is.null(localShapeFileColName)){
      # Local Shape
      grid2polyX<-metis.grid2poly(
        grid=grid,
        boundaryRegionsSelect=localcountryName,
        subRegShpFolder=localShapeFileFolder,
        subRegShpFile=localShapeFile,
        subRegCol=localShapeFileColName,
        subRegType = "localShape",
        nameAppend = "",
        sqliteUSE = sqliteUSE,
        sqliteDBNamePath = sqliteDBNamePath,
        paramsSelect=paramsSelect_grid2poly)
    }
    }

  }

  #-----------
  # Mapping
  #-------------

  if(run_maps == T){


    scaleRange=data.frame(param=c("griddedScarcity"),
                            maxScale=c(1),
                            minScale=c(0))

   if(!is.null(scaleRange_i)){
     scaleRange = scaleRange %>%
      dplyr::bind_rows(scaleRange_i)}

    numeric2Cat_param <- list("griddedScarcity","param2")
    numeric2Cat_breaks <- list(c(-Inf, 0.1, 0.2, 0.4,Inf),c(0,1,2))
    numeric2Cat_labels <- list(c("None (0<WSI<0.1)","Low (0.1<WSI<0.2)","Moderate (0.2<WSI<0.4)","Severe (WSI>0.4)"),
                               c("a","b","c","d"))
    numeric2Cat_palette <- list(c("pal_ScarcityCat"),
                                #c("c('None (0<WSI<0.1)'='black','Low (0.1<WSI<0.2)'='blue','Moderate (0.2<WSI<0.4)'='purple','Severe (WSI>0.4)'='yellow')"),
                                c("Spectral")) # Can be a custom scale or an R brewer paletter or a metis.pal
    numeric2Cat_legendTextSize <- list(c(0.7),c(NULL))
    numeric2Cat_list <-list(numeric2Cat_param=numeric2Cat_param,
                            numeric2Cat_breaks=numeric2Cat_breaks,
                            numeric2Cat_labels=numeric2Cat_labels,
                            numeric2Cat_palette=numeric2Cat_palette,
                            numeric2Cat_legendTextSize=numeric2Cat_legendTextSize)

    list_index <- which(numeric2Cat_list$numeric2Cat_param=="griddedScarcity")
    catBreaks <- numeric2Cat_list$numeric2Cat_breaks[[list_index]]; catBreaks
    catLabels <- numeric2Cat_list$numeric2Cat_labels[[list_index]]; catLabels
    catPalette <- numeric2Cat_list$numeric2Cat_palette[[list_index]]; catPalette
    catLegendTextSize <- numeric2Cat_list$numeric2Cat_legendTextSize[[list_index]];catLegendTextSize


    if(!is.null(gcamcountryNames)){
    for(countryName_i in gcamcountryNames){

      countryName = countryName_i


      if(run_map_grid == T){

        gridDataTables_i=paste(getwd(),"/outputs/Grids/gridCropped_",countryName,"_subBasin_local.csv",sep="")
        # b<-read.csv(gridDataTables_i); head(b); unique(b$scenario); unique(b$param); unique(b$x)
        # for(param_i in unique(b$param)){print(param_i);print(unique((b%>%dplyr::filter(param==param_i))$x));print(unique((b%>%dplyr::filter(param==param_i))$scenario))}

        metis.mapProcess(
          #polygonDataTables=polygonDataTables_i,
          gridDataTables=gridDataTables_i,
          xRange=xRangeMap,
          # boundaryRegShape=boundaryRegShape,
          boundaryRegShpFolder=paste(getwd(),"/dataFiles/gis/naturalEarth",sep=""),
          boundaryRegShpFile="ne_10m_admin_0_countries",
          boundaryRegCol="NAME",
          boundaryRegionsSelect=countryName,
          subRegShpFolder=paste(getwd(),"/dataFiles/gis/shapefiles_",countryName,sep = ""),
          subRegShpFile=paste(countryName,"NE1",sep=""),
          subRegCol="name",
          subRegType = "state",
          legendOutsideSingle=legendOutsideSingle,
          legendPosition=legendPosition,
          animateOn=animateOn,
          delay=delay,
          scenRef=scenRefMap,
          paramsSelect = paramsSelect_Map,
          scaleRange = scaleRange,
          indvScenarios=indvScenarios,
          GCMRCPSSPPol=GCMRCPSSPPol,
          multiFacetCols="scenarioRCP",
          multiFacetRows="scenarioGCM",
          refGCM="gfdl-esm2m",
          refRCP="rcp2p6",
          chosenRefMeanYears=c(2000:2050),
          numeric2Cat_list=numeric2Cat_list,
          dirNameAppend="",
          nameAppend="")
      }

      if(run_map_state == T){


        polygonDataTables_x= paste(getwd(),"/outputs/Maps/Tables/subReg_origData_byClass_",countryName,"_subBasin_origDownscaled_local.csv",sep="")

          if(file.exists(polygonDataTables_x)){
            polygonDataTables_i <- c(polygonDataTables_x,
                                     polygonDataTablesCustom)
          } else {polygonDataTables_i=polygonDataTablesCustom}


        metis.mapProcess(
          polygonDataTables=polygonDataTables_i,
          xRange=xRangeMap,
          # boundaryRegShape=boundaryRegShape,
          boundaryRegShpFolder=paste(getwd(),"/dataFiles/gis/naturalEarth",sep=""),
          boundaryRegShpFile="ne_10m_admin_0_countries",
          boundaryRegCol="NAME",
          boundaryRegionsSelect=countryName,
          subRegShpFolder=paste(getwd(),"/dataFiles/gis/shapefiles_",countryName,sep = ""),
          subRegShpFile=paste(countryName,"NE1",sep=""),
          subRegCol="name",
          subRegType = "state",
          nameAppend="",
          legendOutsideSingle=legendOutsideSingle,
          legendPosition=legendPosition,
          animateOn=animateOn,
          delay=delay,
          scenRef=scenRefMap,
          paramsSelect = paramsSelect_Map,
          scaleRange = scaleRange,
          indvScenarios=indvScenarios,
          GCMRCPSSPPol=GCMRCPSSPPol,
          multiFacetCols="scenarioRCP",
          multiFacetRows="scenarioGCM",
          refGCM="gfdl-esm2m",
          refRCP="rcp2p6",
          chosenRefMeanYears=c(2000:2050),
          numeric2Cat_list=numeric2Cat_list)
      }

      if(run_map_GCAMbasin == T){

        polygonDataTables_x=paste(getwd(),"/outputs/Maps/Tables/subReg_origData_byClass_",countryName,"_subBasin_origDownscaled_local.csv",sep="")


        if(file.exists(polygonDataTables_x)){
          polygonDataTables_i <- c(polygonDataTables_x,
                                   polygonDataTablesCustom)
        } else {polygonDataTables_i=polygonDataTablesCustom}



        metis.mapProcess(polygonDataTables=polygonDataTables_i,
                         xRange=xRangeMap,
                         # boundaryRegShape=boundaryRegShape,
                         boundaryRegShpFolder=paste(getwd(),"/dataFiles/gis/naturalEarth",sep=""),
                         boundaryRegShpFile="ne_10m_admin_0_countries",
                         boundaryRegCol="NAME",
                         boundaryRegionsSelect=countryName,
                         #subRegShape=subRegShape ,
                         subRegShpFolder=paste(getwd(),"/dataFiles/gis/shapefiles_",countryName,sep = ""),
                         subRegShpFile=paste(countryName,"GCAMBasin",sep=""),
                         subRegCol="basin_name",
                         subRegType = "basin",
                         nameAppend="",
                         legendOutsideSingle=legendOutsideSingle,
                         legendPosition=legendPosition,
                         animateOn=animateOn,
                         delay=delay,
                         scenRef=scenRefMap,
                         paramsSelect = paramsSelect_Map,
                         scaleRange = scaleRange,
                         indvScenarios=indvScenarios,
                         GCMRCPSSPPol=GCMRCPSSPPol,
                         multiFacetCols="scenarioRCP",
                         multiFacetRows="scenarioGCM",
                         refGCM="gfdl-esm2m",
                         refRCP="rcp2p6",
                         chosenRefMeanYears=c(2000:2050),
                         numeric2Cat_list=numeric2Cat_list)
      }
    }}

    if(!is.null(localcountryName) & !is.null(localShapeFileFolder) &
       !is.null(localShapeFile) & !is.null(localShapeFileColName)){

    if(run_map_localShapeLocalData == T){

      polygonDataTables_i=localSubRegDataTables

      metis.mapProcess(polygonDataTables=polygonDataTables_i,
                       xRange=xRangeMap,
                       # boundaryRegShape=boundaryRegShape,
                       boundaryRegShpFolder=paste(getwd(),"/dataFiles/gis/naturalEarth",sep=""),
                       boundaryRegShpFile="ne_10m_admin_0_countries",
                       boundaryRegCol="NAME",
                       boundaryRegionsSelect=localcountryName,
                       #subRegShape=subRegShape ,
                       subRegShpFolder=localShapeFileFolder,
                       subRegShpFile=localShapeFile,
                       subRegCol=localShapeFileColName,
                       subRegType = "localShape",
                       legendOutsideSingle=legendOutsideSingle,
                       legendPosition=legendPosition,
                       animateOn=animateOn,
                       delay=delay,
                       scenRef=scenRefMap,
                       paramsSelect = paramsSelect_Map,
                       scaleRange = scaleRange,
                       indvScenarios=indvScenarios,
                       GCMRCPSSPPol=GCMRCPSSPPol,
                       multiFacetCols="scenarioRCP",
                       multiFacetRows="scenarioGCM",
                       refGCM="gfdl-esm2m",
                       refRCP="rcp2p6",
                       chosenRefMeanYears=c(2000:2050),
                       numeric2Cat_list=numeric2Cat_list,
                       dirNameAppend="_localData",
                       nameAppend="")
    }

      if(run_map_localShapeGCAMData == T){

      polygonDataTables_x=paste(getwd(),"/outputs/Maps/Tables/subReg_origData_byClass_",localcountryName,"_subBasin_origDownscaled_local.csv",sep="")

      if(file.exists(polygonDataTables_x)){
        polygonDataTables_i <- c(polygonDataTables_x,
                                 polygonDataTablesCustom)
      } else {polygonDataTables_i=polygonDataTablesCustom}

      # a<-read.csv(polygonDataTables_i); head(a); unique(a$scenario); unique(a$param); unique(a$x)
      # for(param_i in unique(a$param)){print(param_i);print(unique((a%>%dplyr::filter(param==param_i))$x));print(unique((a%>%dplyr::filter(param==param_i))$scenario))}


      metis.mapProcess(polygonDataTables=polygonDataTables_i,
                       xRange=xRangeMap,
                       # boundaryRegShape=boundaryRegShape,
                       boundaryRegShpFolder=paste(getwd(),"/dataFiles/gis/naturalEarth",sep=""),
                       boundaryRegShpFile="ne_10m_admin_0_countries",
                       boundaryRegCol="NAME",
                       boundaryRegionsSelect=localcountryName,
                       #subRegShape=subRegShape ,
                       subRegShpFolder=localShapeFileFolder,
                       subRegShpFile=localShapeFile,
                       subRegCol=localShapeFileColName,
                       subRegType = "localShape",
                       legendOutsideSingle=legendOutsideSingle,
                       legendPosition=legendPosition,
                       animateOn=animateOn,
                       delay=delay,
                       scenRef=scenRefMap,
                       paramsSelect = paramsSelect_Map,
                       scaleRange = scaleRange,
                       indvScenarios=indvScenarios,
                       GCMRCPSSPPol=GCMRCPSSPPol,
                       multiFacetCols="scenarioRCP",
                       multiFacetRows="scenarioGCM",
                       refGCM="gfdl-esm2m",
                       refRCP="rcp2p6",
                       chosenRefMeanYears=c(2000:2050),
                       numeric2Cat_list=numeric2Cat_list,
                       dirNameAppend="_localGCAM",
                       nameAppend="")

      }

      if(run_map_localShapeGCAMGrid == T){
        gridDataTables_i=paste(getwd(),"/outputs/Grids/gridCropped_",localcountryName,"_localShape.csv",sep="")

        metis.mapProcess(
          gridDataTables=gridDataTables_i,
          xRange=xRangeMap,
          # boundaryRegShape=boundaryRegShape,
          boundaryRegShpFolder=paste(getwd(),"/dataFiles/gis/naturalEarth",sep=""),
          boundaryRegShpFile="ne_10m_admin_0_countries",
          boundaryRegCol="NAME",
          boundaryRegionsSelect=localcountryName,
          #subRegShape=subRegShape ,
          subRegShpFolder=localShapeFileFolder,
          subRegShpFile=localShapeFile,
          subRegCol=localShapeFileColName,
          subRegType = "localShape",
          legendOutsideSingle=legendOutsideSingle,
          legendPosition=legendPosition,
          animateOn=animateOn,
          delay=delay,
          scenRef=scenRefMap,
          paramsSelect = paramsSelect_Map,
          scaleRange = scaleRange,
          indvScenarios=indvScenarios,
          GCMRCPSSPPol=GCMRCPSSPPol,
          multiFacetCols="scenarioRCP",
          multiFacetRows="scenarioGCM",
          refGCM="gfdl-esm2m",
          refRCP="rcp2p6",
          chosenRefMeanYears=c(2000:2050),
          numeric2Cat_list=numeric2Cat_list,
          dirNameAppend="_localGCAM",
          nameAppend="")
      }
    }
  }
  return(list(dataGCAM))
}
