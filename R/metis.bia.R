#' metis.bia
#'
#' This function downscales GCAM electricity generation and installed capacity onto a grid, based on WRI PowerWatch dataset of present capacity
#' @param biaInputsFolder Bia Inputs Folder Path
#' @param biaInputsFiles Bia Inputs Folder Path
#' @param gcamdatabasePath Path to gcam database folder
#' @param gcamdatabaseName Name of gcam database
#' @param queryxml Full path to query.xml file
#' @param queryPath Folder that contains the query.xml file.By default it is
#' the same folder as specified by gcamdatabasePath
#' @param queriesSelect Default = "All". Vector of queries to read from the queryxml for example
#' @param scenOrigNames Original Scenarios names in GCAM database in a string vector.
#' For example c('scenario1','scenario2).
#' @param scenNewNames New Names which may be shorter and more useful for figures etc.
#' Default will use Original Names. For example c('scenario1','scenario2)
#' @param reReadData Default = 1. will read the GCAM data base and create a queryData.proj file
#' in the same folder as the GCAM database. If FALSE will load a '.proj' file if a file
#' with full path is provided otherwise it will search for a dataProj.proj file in the existing
#' folder which may have been created from an old run.
#' @param dataProj Optional. A default 'dataProj.proj' is produced if no .Proj file is specified.
#' @param dataProjPath Folder that contains the dataProj or where it will be produced.
#' @param regionsSelect The regions to analyze in a vector. Example c('Colombia','Argentina')
#' @param paramsSelect Default = c("elecByTech", "elecCapBySubsector") . Vector of parameters to be read from the GCAM database
#' @param gridChoice Default = "grid_050" . Choice of whether to use 50 km x 50 km grid cells ("grid_050") or 25 km x 25 km ("grid_025").
#' @param diagnosticsON Default = T.
#' @param subsectorNAdistribute Default = "even". Choose "even" for even distribution or "totalOther" to distribute based on sum of all other subsectors..
#' @param nameAppend Default=""
#' @return A tibble with GCAM electricity generation distributed on a grid for selected regions
#' @keywords electricity, generation, gcam, gridded, downscale, downscaling, downscaled
#' @export

metis.bia<- function(biaInputsFolder = "NA",
                     biaInputsFiles = "NA",
                     reReadData = 1,
                     regionsSelect = NULL,
                     dataProj = "dataProj.proj",
                     dataProjPath = gcamdatabasePath,
                     scenOrigNames = NULL,
                     scenNewNames = NULL,
                     gcamdatabasePath = "NA",
                     gcamdatabaseName = "NA",
                     queryxml = "metisQueries.xml",
                     queryPath = paste(getwd(),"/dataFiles/gcam",sep=""),
                     queriesSelect = "All",
                     paramsSelect = c("elecByTech", "elecCapBySubsector"),
                     gridChoice = "grid_050",
                     diagnosticsON = T,
                     subsectorNAdistribute = "even",
                     nameAppend=""
){

  # biaInputsFolder = "NA"
  # biaInputsFiles = "NA"
  # biaOutputsFolder = paste(getwd(),"/dataFiles/grids/bia/biaOutputs",sep="")
  # reReadData = 1
  # regionsSelect = NULL
  # dataProj = "dataProj.proj"
  # dataProjPath = gcamdatabasePath
  # scenOrigNames = c("GCAMOrig","GCAMModified")
  # scenNewNames = c("GCAMOrig","GCAMModified")
  # gcamdatabasePath = "NA"
  # gcamdatabaseName = "NA"
  # queryxml = "metisQueries.xml"
  # queryPath = gcamdatabasePath
  # queriesSelect = "All"
  # paramsSelect = c("elecByTech", "elecCapBySubsector")
  # gridChoice = "grid_050"
  # diagnosticsON = T
  # subsectorNAdistribute = "even"




  #----------------
  # Initialize variables by setting to NULL
  #----------------

  NULL -> lat -> lon -> latitude -> longitude -> aez_id -> region_id ->X..ID->
    ilon->ilat->param->V2->V3->scenario->classPalette->x->value->id->
    biaYears->commonYears->commonScenarios->V1->Area_hec->Area_km2->valueBia->commonYears_i->
    country->name->country_long_gppd_idnr->fuel1->fuel2->fuel3->fuel4->owner->geolocation_source->
    GCMRCP->capacity_gw->capacity_mw->cf1971to2100->class1->data_source->dataBia->est_installed_capacity->
    estimated_generation_gwh->gcamCapacityFactor->generation_gwh_2013->generation_gwh_2014->
    generation_gwh_2015->generation_gwh_2016->
    owner->region->regionsSelectAll->rowid->country_long->gppd_idnr->
    vintage -> year -> xLabel -> x -> value -> sector -> scenario -> param -> origX -> origValue ->
    origUnits -> origScen -> origQuery -> classPalette2 -> classPalette1 -> classLabel2 -> classLabel1 -> class2 ->
    class1 -> connx -> aggregate -> Units -> sources -> paramx -> technology -> input -> output -> gcamCapacityFactor ->
    gridlat -> gridlon -> gridID -> region_32_code -> ctry_name -> ctry_code -> gridCellPercentage -> aggregate ->
    valueDistrib -> origValueDistrib ->readgcamdata->gridlat->gridlon->gridCropped-> year_of_capacity_data ->
    gridCellCapacity -> regionCapSum -> Var1 -> Var2 -> gridCellIndex->commissioning_year



  if(!subsectorNAdistribute %in% c("even","totalOther")){
    print(paste("subsectorNAdistribute provided: ",subsectorNAdistribute," should be either 'even' or 'totalOther'. Setting to 'even'.",sep=""))
    subsectorNAdistribute = "even"
  }


  #------------------
  # Create folders if needed
  #------------------


  if (!dir.exists(paste(getwd(),"/dataFiles",sep=""))){
    dir.create(paste(getwd(),"/dataFiles",sep=""))}

  if (!dir.exists(paste(getwd(),"/dataFiles/grids",sep=""))){
    dir.create(paste(getwd(),"/dataFiles/grids",sep=""))}

  if (!dir.exists(paste(getwd(),"/dataFiles/grids/bia",sep=""))){
    dir.create(paste(getwd(),"/dataFiles/grids/bia",sep=""))}

  if (!dir.exists(paste(getwd(),"/dataFiles/grids/bia/biaOutputs",sep=""))){
    dir.create(paste(getwd(),"/dataFiles/grids/bia/biaOutputs",sep=""))}

  biaOutputsFolder <- paste(getwd(),"/dataFiles/grids/bia/biaOutputs",sep="")

  if (dir.exists(paste(biaOutputsFolder, "/biadiagnostics",sep=""))){
    unlink(paste(biaOutputsFolder, "/biadiagnostics",sep=""),recursive=T)}

  if (!dir.exists(paste(biaOutputsFolder, "/biadiagnostics",sep=""))){
    dir.create(paste(biaOutputsFolder, "/biadiagnostics",sep=""))}


  #------------------
  # Read GCAM data
  #------------------

  readgcamdata<-metis.readgcam(gcamdatabasePath = gcamdatabasePath, gcamdatabaseName = gcamdatabaseName,
                               queryxml = queryxml, queryPath = queryPath,
                               scenOrigNames = scenOrigNames, scenNewNames = scenNewNames, reReadData = reReadData,
                               dataProj = dataProj, dataProjPath = dataProjPath, dirOutputs = biaOutputsFolder,
                               regionsSelect = regionsSelect, queriesSelect = queriesSelect , paramsSelect = paramsSelect)


  # Save list of scenarios and queries
  scenarios <- readgcamdata$scenarios  # List of Scenarios in the GCAM database pulled in through metis.readgcam
  queries <- readgcamdata$queries  # List of Queries in the GCAM database pulled in through metis.readgcam

  if(length(queries)==0){stop("No queries found. PLease check data.")}

  dataFromGCAM <- readgcamdata$data%>%
    tibble::as_tibble()

  # Get All Regions

  #tbl <- rgcam::getQuery(dataProjLoaded, queries[1])  # Tibble
  regionsAll<-unique(dataFromGCAM$region)
  if(("All" %in% regionsSelect) | ("all" %in% regionsSelect) | is.null(regionsSelect)){regionsSelect<-regionsAll; regionsSelectAll=T}else{
    regionsSelectAll=F
  }

  #------------------
  # Load grid cell file
  #------------------

  # Loading a list that gives which of the 32 regions each country is in
  ctor<-data.table::fread(file=paste(biaInputsFolder,"/country_to_region.csv",sep=""), header=T,stringsAsFactors = F,encoding="Latin-1")%>%
    tibble::as_tibble()%>%
    dplyr::mutate(country_long=ctry_name)


  listOfGridCells<-data.table::fread(file=paste(getwd(),"/dataFiles/grids/emptyGrids/",gridChoice,".csv",sep=""), header=T,stringsAsFactors = F,
                                     encoding="Latin-1")%>%
    tibble::as_tibble()

  if(!("id" %in% names(listOfGridCells))){
    print("grid id column not found within grid file, creating a new id column...")
    listOfGridCells <- tibble::rowid_to_column(listOfGridCells, var = "id")
  }

  listOfGridCells <- dplyr::rename(listOfGridCells,
                            gridlat = lat,
                            gridlon = lon,
                            gridID = id)


  latmin<-min(listOfGridCells$gridlat)
  latmax<-max(listOfGridCells$gridlat)
  lonmin<-min(listOfGridCells$gridlon)
  lonmax<-max(listOfGridCells$gridlon)

  latranked<-listOfGridCells$gridlat[sort.list(listOfGridCells$gridlat)]%>%
    unique()
  lonranked<-listOfGridCells$gridlon[sort.list(listOfGridCells$gridlon)]%>%
    unique()

  # This assumes equally spaced grids by degree.
  gridDimlat<-min(abs(latranked[2:length(latranked)]-latranked[1:length(latranked)-1]))
  gridDimlon<-min(abs(lonranked[2:length(lonranked)]-lonranked[1:length(lonranked)-1]))

  gridShiftlat<-latranked[sort.list(abs(latranked))][1]  # The latitude of the center of the grid cells closest to the equator
  gridShiftlon<-lonranked[sort.list(abs(lonranked))][1]  # The longitude of the center of the grid cells closest to prime meridian, Greenwich Meridian


  listOfGridCells$gridlat<-round(listOfGridCells$gridlat, digits = 10)
  listOfGridCells$gridlon<-round(listOfGridCells$gridlon, digits = 10)


  if(!(sum(round(latranked, digits = 4) %in% round(seq(latmin,latmax,length.out = (round((latmax-latmin)/gridDimlat)+1)),digits = 4))==length(latranked))){
    stop(paste("grid file ", getwd(),"/dataFiles/grids/emptyGrids/",gridChoice,".csv"," does not appear to contain the centers of regurlarly-spaced lat lon grid cells.",sep=""))}


    #----------------
    # Create a distribution grid for each of the 32 regions
    #---------------


    if(!dir.exists(biaInputsFolder)){
      print(paste("bia input folder: ", biaInputsFolder ," is incorrect or doesn't exist.",sep=""))
      print(paste("Skipping electricity generation distribution with bia",sep=""))}else {

        biaYears<-numeric()

        for(biaInputsFile_i in biaInputsFiles){

          if(!grepl(".csv",biaInputsFile_i)){biaInputsFile_i=paste(biaInputsFile_i,".csv",sep="")}

          if(!file.exists(paste(biaInputsFolder,"/",biaInputsFile_i,sep=""))){
            print(paste("bia input file: ", biaInputsFolder,"/",biaInputsFile_i," is incorrect or doesn't exist.",sep=""))
            print(paste("Skipping file: ",biaInputsFolder,"/",biaInputsFile_i,sep=""))
          }else{

            print(paste("Reading bia input file: ",biaInputsFile_i,"...",sep=""))
            gridWRI<-data.table::fread(paste(biaInputsFolder,"/",biaInputsFile_i,sep=""), header=T,stringsAsFactors = F,encoding="Latin-1")

            gridWRI[gridWRI==unique(gridWRI$country_long)[grepl("United States",unique(gridWRI$country_long),ignore.case=T)]]<-
              unique(ctor$country_long)[grepl("United States",unique(ctor$country_long),ignore.case=T)]
            gridWRI[gridWRI==unique(gridWRI$country_long)[grepl("Bosnia",unique(gridWRI$country_long),ignore.case=T)]]<-
              unique(ctor$country_long)[grepl("Bosnia",unique(ctor$country_long),ignore.case=T)]
            gridWRI[gridWRI==unique(gridWRI$country_long)[grepl("Brunei",unique(gridWRI$country_long),ignore.case=T)]]<-
              unique(ctor$country_long)[grepl("Brunei",unique(ctor$country_long),ignore.case=T)]
            gridWRI[gridWRI=="Democratic Republic of the Congo"]<-"Congo DRC"
            gridWRI[gridWRI=="Congo"]<-"Congo Rep."
            gridWRI[gridWRI=="Taiwan"]<-"Taiwan China"
            ctor$country_long[ctor$region == "Taiwan"] <- "Taiwan China"
            gridWRI[gridWRI=="Congo"]<-"Congo Rep."
            gridWRI[gridWRI==unique(gridWRI$country_long)[grepl("Cote",unique(gridWRI$country_long),ignore.case=T)]]<-
              unique(ctor$country_long)[grepl("Cote",unique(ctor$country_long),ignore.case=T)]
            gridWRI[gridWRI==unique(gridWRI$country_long)[grepl("Gambia",unique(gridWRI$country_long),ignore.case=T)]]<-
              unique(ctor$country_long)[grepl("Gambia",unique(ctor$country_long),ignore.case=T)]
            # gridWRI[gridWRI==unique(gridWRI$country_long)[grepl("Kosovo",unique(gridWRI$country_long),ignore.case=T)]]<-
            #   unique(ctor$country_long)[grepl("Kosovo",unique(ctor$country_long),ignore.case=T)]
            gridWRI[gridWRI==unique(gridWRI$country_long)[grepl("Syria",unique(gridWRI$country_long),ignore.case=T)]]<-
              unique(ctor$country_long)[grepl("Syria",unique(ctor$country_long),ignore.case=T)]
            gridWRI[gridWRI==unique(gridWRI$country_long)[grepl("Taiwan",unique(gridWRI$country_long),ignore.case=T)]]<-
              unique(ctor$country_long)[grepl("Taiwan",unique(ctor$country_long),ignore.case=T)]
            gridWRI[gridWRI==unique(gridWRI$country_long)[grepl("Trinidad",unique(gridWRI$country_long),ignore.case=T)]]<-
              unique(ctor$country_long)[grepl("Trinidad",unique(ctor$country_long),ignore.case=T)]


            gridWRI<-gridWRI%>%tibble::as_tibble()%>%dplyr::select(-year_of_capacity_data,-commissioning_year,-name,-country,-gppd_idnr,-fuel2,-fuel3,-fuel4,-owner,-source,-url,-geolocation_source)%>%
              dplyr::left_join(ctor,by="country_long")

            aggType="vol"


            gridWRI<-gridWRI%>%dplyr::mutate(lat=latitude,
                                             lon=longitude,
                                             param="biaElecGen",
                                             units= "Capacity (GW)",
                                             aggType=aggType,
                                             classPalette="pal_elec_subsec",
                                             class1=fuel1,
                                             value=capacity_mw/1000,
                                             x=NA,
                                             gridlat = round(gridDimlat*round(latitude*(1/gridDimlat)-(gridShiftlat/gridDimlat))+gridShiftlat, digits = 10),
                                             gridlon = round(gridDimlon*round(longitude*(1/gridDimlon)-(gridShiftlon/gridDimlon))+gridShiftlon, digits = 10))%>%
              tibble::as_tibble()%>%
              dplyr::select(-latitude,-longitude,-fuel1,-capacity_mw,-generation_gwh_2013,-generation_gwh_2014,-generation_gwh_2015,-generation_gwh_2016,-estimated_generation_gwh,-country_long)%>%
              dplyr::left_join(listOfGridCells,by = c("gridlat","gridlon"))%>%
              dplyr::group_by(gridlat, gridlon, class1, gridID, ctry_name, ctry_code, region, region_32_code, param, units)%>%
              dplyr::summarise(gridCellCapacity = sum(value))%>%
              dplyr::ungroup() %>%
              dplyr::group_by(class1,region,region_32_code)%>%
              dplyr::mutate(regionCapSum = sum(gridCellCapacity),
                            gridCellPercentage = gridCellCapacity/regionCapSum) %>%
              dplyr::ungroup()


            gridWRI[gridWRI==unique(gridWRI$class1)[grepl("cogen",unique(gridWRI$class1),ignore.case=T)]]<-
              unique(dataFromGCAM$class1)[grepl("chp",unique(dataFromGCAM$class1),ignore.case=T)]
            gridWRI[gridWRI==unique(gridWRI$class1)[grepl("coal",unique(gridWRI$class1),ignore.case=T)]]<-
              unique(dataFromGCAM$class1)[grepl("coal",unique(dataFromGCAM$class1),ignore.case=T)]
            gridWRI[gridWRI==unique(gridWRI$class1)[grepl("Gas",unique(gridWRI$class1),ignore.case=T)]]<-
              unique(dataFromGCAM$class1)[grepl("Gas",unique(dataFromGCAM$class1),ignore.case=T)]
            gridWRI[gridWRI==unique(gridWRI$class1)[grepl("Oil",unique(gridWRI$class1),ignore.case=T)]]<-
              unique(dataFromGCAM$class1)[grepl("Oil",unique(dataFromGCAM$class1),ignore.case=T)]
            gridWRI[gridWRI==unique(gridWRI$class1)[grepl("Biomass",unique(gridWRI$class1),ignore.case=T)]]<-
              unique(dataFromGCAM$class1)[grepl("Biomass",unique(dataFromGCAM$class1),ignore.case=T)]
            gridWRI[gridWRI==unique(gridWRI$class1)[grepl("Nuclear",unique(gridWRI$class1),ignore.case=T)]]<-
              unique(dataFromGCAM$class1)[grepl("Nuclear",unique(dataFromGCAM$class1),ignore.case=T)]
            gridWRI[gridWRI==unique(gridWRI$class1)[grepl("Geothermal",unique(gridWRI$class1),ignore.case=T)]]<-
              unique(dataFromGCAM$class1)[grepl("Geothermal",unique(dataFromGCAM$class1),ignore.case=T)]
            gridWRI[gridWRI==unique(gridWRI$class1)[grepl("Hydro",unique(gridWRI$class1),ignore.case=T)]]<-
              unique(dataFromGCAM$class1)[grepl("Hydro",unique(dataFromGCAM$class1),ignore.case=T)]
            gridWRI[gridWRI==unique(gridWRI$class1)[grepl("Wind",unique(gridWRI$class1),ignore.case=T)]]<-
              unique(dataFromGCAM$class1)[grepl("Wind",unique(dataFromGCAM$class1),ignore.case=T)]
            gridWRI[gridWRI==unique(gridWRI$class1)[grepl("Solar",unique(gridWRI$class1),ignore.case=T)]]<-
              unique(dataFromGCAM$class1)[grepl("Solar",unique(dataFromGCAM$class1),ignore.case=T)]



            if(subsectorNAdistribute == "even"){

            #-------------------
            # For electricity generation subsectors not represented in power plant database, distribute evently throughout region
            #-------------------

            # Read in GCAM regions

            if(!dir.exists(paste(getwd(),"/dataFiles/gis/admin_gcam32",sep=""))){
              print(paste("GCAMRegionShapeFolder: ",paste(getwd(),"/dataFiles/gis/admin_gcam32",sep=""), " does not exist.",sep=""))
            } else{
              GCAMRegionShapeFolder <- paste(getwd(),"/dataFiles/gis/admin_gcam32",sep="")}

            if(!file.exists(paste(getwd(),"/dataFiles/gis/admin_gcam32/region32_0p5deg.shp",sep=""))){
              print(paste("GCAMRegionShapeFolder: ",paste(getwd(),"/dataFiles/gis/admin_gcam32/region32_0p5deg.shp",sep=""), " does not exist.",sep=""))
            } else{
              GCAMRegionShapeFile <- "region32_0p5deg"}


            gridCropped <- tibble::tibble(gridlat = NA, gridlon = NA, gridID = NA, region = NA)

            for(regionc in regionsSelect){
              shape=rgdal::readOGR(dsn=GCAMRegionShapeFolder,layer=GCAMRegionShapeFile,use_iconv=T,encoding='UTF-8')
              shape@data <-shape@data %>%
                dplyr::left_join(ctor %>%
                            dplyr::select(region_32_code, region) %>%
                            dplyr::distinct() %>%
                            dplyr::mutate(reg32_id=as.factor(region_32_code))%>%
                            dplyr::select(-region_32_code), by = "reg32_id")
              shape@data %>% as.data.frame()
              shape <- shape[(shape$region %in% regionc),];
              raster::plot(shape)

              # Prepare grids to be cropped
              spdf = sp::SpatialPointsDataFrame(sp::SpatialPoints(coords=(cbind(listOfGridCells$gridlon,listOfGridCells$gridlat))),data=listOfGridCells)
              sp::gridded(spdf)<-TRUE

              r<-raster::stack(spdf)
              raster::projection(r)<-sp::proj4string(shape)

              rmask<-raster::mask(r,shape)
              rmaskP<-raster::rasterToPolygons(rmask)
              gridCropped<-dplyr::bind_rows(gridCropped,dplyr::mutate(tibble::as_tibble(rmaskP@data),region = regionc))

            }

            gridCropped <- dplyr::filter(gridCropped,!is.na(region))

            dataBia<- dataFromGCAM %>%
              dplyr::left_join(
              gridWRI%>%dplyr::filter(region %in% regionsSelect)%>%
              dplyr::select(gridlat, gridlon, gridID, class1, region, region_32_code, ctry_name, ctry_code, gridCellPercentage),
              by = c("class1", "region"))%>%
              dplyr::mutate(valueDistrib = gridCellPercentage*value, origValueDistrib = gridCellPercentage*origValue)


            dataBiaNA <- dplyr::filter(dataBia,is.na(gridlat)) %>%
              dplyr::select(-gridlat, -gridlon, -gridID)

            evenDistrib <- expand.grid(unique(dataBiaNA$class1), gridCropped$gridID) %>%
              tibble::as_tibble() %>%
              dplyr::rename(class1 = Var1, gridID = Var2) %>%
              dplyr::left_join(listOfGridCells, by = "gridID") %>%
              dplyr::left_join(gridCropped, by = c("gridlat", "gridlon", "gridID"))

            evenDistrib$class1 <- as.character(evenDistrib$class1)

            evenDistrib <- dplyr::left_join(dataBiaNA,evenDistrib, by = c("class1", "region")) %>%
              dplyr::mutate(gridCellCapacity = 999) %>%
              dplyr::group_by(class1,region)%>%
              dplyr::mutate(regionCapSum = sum(gridCellCapacity),
                            gridCellPercentage = gridCellCapacity/regionCapSum) %>%
              dplyr::ungroup() %>%
              dplyr::select(-valueDistrib, -origValueDistrib) %>%
              dplyr::mutate(valueDistrib = gridCellPercentage*value, origValueDistrib = gridCellPercentage*origValue)%>%
              dplyr::select(-gridCellCapacity, -regionCapSum)


            dataBia <- dataBia %>%
              dplyr::filter(!(is.na(gridlat))) %>%
              dplyr::bind_rows(evenDistrib)
            } # Close if subsectorNAdistribute == "even"



            if(subsectorNAdistribute == "totalOther"){

            #-------------------
            # For electricity generation subsectors not represented in power plant database, distribute according to installed capacity of all subsectors
            #-------------------


            gridWRIallSubsecMixed <- gridWRI %>%
              dplyr::group_by(gridlat, gridlon, gridID, ctry_name, ctry_code, region, region_32_code, param, units)%>%
              dplyr::summarise(gridCellCapacity = sum(gridCellCapacity))%>%
              dplyr::ungroup() %>%
              dplyr::group_by(region,region_32_code)%>%
              dplyr::mutate(regionCapSum = sum(gridCellCapacity),
                            gridCellPercentage = gridCellCapacity/regionCapSum) %>%
              dplyr::ungroup() %>%
              tibble::rowid_to_column(var = "gridCellIndex") %>%
              dplyr::mutate(gridCellIndex = -gridCellIndex)



            dataBia<- dataFromGCAM %>%
              dplyr::left_join(
                gridWRI%>%dplyr::filter(region %in% regionsSelect)%>%
                  dplyr::select(gridlat, gridlon, gridID, class1, region, region_32_code, ctry_name, ctry_code, gridCellPercentage),
                by = c("class1", "region"))%>%
              dplyr::mutate(valueDistrib = gridCellPercentage*value, origValueDistrib = gridCellPercentage*origValue)



            #Find the elecricity generation subsectors that are not represented in the powerplant database, but which are predicted by GCAM
            dataBiaNA <- dplyr::filter(dataBia,is.na(gridlat)) %>%
              dplyr::select(-gridlat, -gridlon, -gridID, -region_32_code, -ctry_name, -ctry_code, -gridCellPercentage)

            distribByTotalCap <- expand.grid(unique(dataBiaNA$class1), (dplyr::filter(gridWRIallSubsecMixed, region %in% regionsSelect))$gridCellIndex) %>%
              tibble::as_tibble() %>%
              dplyr::rename(class1 = Var1, gridCellIndex = Var2) %>%
              dplyr::mutate(class1 = as.character(class1)) %>%
              dplyr::left_join(gridWRIallSubsecMixed, by = "gridCellIndex") %>%
              dplyr::select(-param, -units)


            distribByTotalCap <- dplyr::left_join(dataBiaNA, distribByTotalCap, by = c("class1", "region")) %>%
              dplyr::select(-valueDistrib, -origValueDistrib) %>%
              dplyr::mutate(valueDistrib = gridCellPercentage*value, origValueDistrib = gridCellPercentage*origValue)

            dataBia <- dataBia %>%
              dplyr::filter(!(is.na(gridlat))) %>%
              dplyr::bind_rows(dplyr::select(distribByTotalCap, -regionCapSum, -gridCellCapacity, -gridCellIndex))

            } # Close if subsectorNAdistribute == "totalOther"



          } # Close if bia file exists
        } # close bia file loops

      } # Close bia folder


    #----------------
    # Function for comparing electricity generation data
    #---------------

     if(diagnosticsON == T){

       regionsSelectCompareCap<-unique(gridWRI$region)

       biaInputsFile_i<-biaInputsFiles[1]

       ctr<-data.table::fread(file = paste(biaInputsFolder,"/country_to_region.csv",sep=""), header=T,stringsAsFactors = F,encoding="Latin-1")%>%
         tibble::as_tibble()%>%
         dplyr::mutate(country_long=ctry_name)

       gWRI<-data.table::fread(file = paste(biaInputsFolder,"/",biaInputsFile_i, '.csv',sep=""), header=T,encoding="Latin-1")

       gWRI[gWRI==unique(gWRI$country_long)[grepl("United States",unique(gWRI$country_long),ignore.case=T)]]<-
         unique(ctor$country_long)[grepl("United States",unique(ctor$country_long),ignore.case=T)]
       gWRI[gWRI==unique(gWRI$country_long)[grepl("Bosnia",unique(gWRI$country_long),ignore.case=T)]]<-
         unique(ctor$country_long)[grepl("Bosnia",unique(ctor$country_long),ignore.case=T)]
       gWRI[gWRI==unique(gWRI$country_long)[grepl("Brunei",unique(gWRI$country_long),ignore.case=T)]]<-
         unique(ctor$country_long)[grepl("Brunei",unique(ctor$country_long),ignore.case=T)]
       gWRI[gWRI=="Democratic Republic of the Congo"]<-"Congo DRC"
       gWRI[gWRI=="Congo"]<-"Congo Rep."
       gWRI[gWRI=="Taiwan"]<-"Taiwan China"
       ctor$country_long[ctor$region == "Taiwan"] <- "Taiwan China"
       gWRI[gWRI=="Congo"]<-"Congo Rep."
       gWRI[gWRI==unique(gWRI$country_long)[grepl("Cote",unique(gWRI$country_long),ignore.case=T)]]<-
         unique(ctor$country_long)[grepl("Cote",unique(ctor$country_long),ignore.case=T)]
       gWRI[gWRI==unique(gWRI$country_long)[grepl("Gambia",unique(gWRI$country_long),ignore.case=T)]]<-
         unique(ctor$country_long)[grepl("Gambia",unique(ctor$country_long),ignore.case=T)]
       # gWRI[gWRI==unique(gWRI$country_long)[grepl("Kosovo",unique(gWRI$country_long),ignore.case=T)]]<-
       #   unique(ctor$country_long)[grepl("Kosovo",unique(ctor$country_long),ignore.case=T)]
       gWRI[gWRI==unique(gWRI$country_long)[grepl("Syria",unique(gWRI$country_long),ignore.case=T)]]<-
         unique(ctor$country_long)[grepl("Syria",unique(ctor$country_long),ignore.case=T)]
       gWRI[gWRI==unique(gWRI$country_long)[grepl("Taiwan",unique(gWRI$country_long),ignore.case=T)]]<-
         unique(ctor$country_long)[grepl("Taiwan",unique(ctor$country_long),ignore.case=T)]
       gWRI[gWRI==unique(gWRI$country_long)[grepl("Trinidad",unique(gWRI$country_long),ignore.case=T)]]<-
         unique(ctor$country_long)[grepl("Trinidad",unique(ctor$country_long),ignore.case=T)]

       gWRI<-gWRI%>%tibble::as_tibble()%>%dplyr::select(-name,-country,-gppd_idnr,-fuel2,-fuel3,-fuel4,-owner,-source,-url,-geolocation_source)%>%
         dplyr::left_join(ctr,by="country_long")


       aggType="vol"

       gWRI<-gWRI%>%dplyr::mutate(lat=latitude,
                                  lon=longitude,
                                  param="biaElecGen",
                                  units= "Capacity (GW)",
                                  aggType=aggType,
                                  classPalette="pal_elec_subsec",
                                  class1=fuel1,
                                  value=capacity_mw/1000,
                                  x=NA,
                                  BackCalcCapFactr=estimated_generation_gwh/capacity_mw*(1000/(365*24)),
                                  BCCF_gen2015=generation_gwh_2015/capacity_mw*(1000/(365*24)),
                                  BCCF_gen2016=(1000/(365*24))*generation_gwh_2016/capacity_mw,
                                  est_gen_gwh=estimated_generation_gwh,
                                  gen_gwh_2013=generation_gwh_2013,
                                  gen_gwh_2014=generation_gwh_2014,
                                  gen_gwh_2015=generation_gwh_2015,
                                  gen_gwh_2016=generation_gwh_2016
                                  #region=country_long
       )%>%
         tibble::as_tibble()%>%dplyr::select(-latitude,-longitude,-fuel1,-capacity_mw,-generation_gwh_2013,-generation_gwh_2014,-generation_gwh_2015,-generation_gwh_2016,-estimated_generation_gwh,-country_long)


       gWRI <- gWRI%>%
         dplyr::group_by(region, class1)%>%
         dplyr::summarise(WRI_total_capacity=sum(value))%>%
         dplyr::filter(region %in% regionsSelectCompareCap)

       gWRI[gWRI==unique(gWRI$class1)[grepl("cogen",unique(gWRI$class1),ignore.case=T)]]<-
         unique(dataFromGCAM$class1)[grepl("chp",unique(dataFromGCAM$class1),ignore.case=T)]
       gWRI[gWRI==unique(gWRI$class1)[grepl("coal",unique(gWRI$class1),ignore.case=T)]]<-
         unique(dataFromGCAM$class1)[grepl("coal",unique(dataFromGCAM$class1),ignore.case=T)]
       gWRI[gWRI==unique(gWRI$class1)[grepl("Gas",unique(gWRI$class1),ignore.case=T)]]<-
         unique(dataFromGCAM$class1)[grepl("Gas",unique(dataFromGCAM$class1),ignore.case=T)]
       gWRI[gWRI==unique(gWRI$class1)[grepl("Oil",unique(gWRI$class1),ignore.case=T)]]<-
         unique(dataFromGCAM$class1)[grepl("Oil",unique(dataFromGCAM$class1),ignore.case=T)]
       gWRI[gWRI==unique(gWRI$class1)[grepl("Biomass",unique(gWRI$class1),ignore.case=T)]]<-
         unique(dataFromGCAM$class1)[grepl("Biomass",unique(dataFromGCAM$class1),ignore.case=T)]
       gWRI[gWRI==unique(gWRI$class1)[grepl("Nuclear",unique(gWRI$class1),ignore.case=T)]]<-
         unique(dataFromGCAM$class1)[grepl("Nuclear",unique(dataFromGCAM$class1),ignore.case=T)]
       gWRI[gWRI==unique(gWRI$class1)[grepl("Geothermal",unique(gWRI$class1),ignore.case=T)]]<-
         unique(dataFromGCAM$class1)[grepl("Geothermal",unique(dataFromGCAM$class1),ignore.case=T)]
       gWRI[gWRI==unique(gWRI$class1)[grepl("Hydro",unique(gWRI$class1),ignore.case=T)]]<-
         unique(dataFromGCAM$class1)[grepl("Hydro",unique(dataFromGCAM$class1),ignore.case=T)]
       gWRI[gWRI==unique(gWRI$class1)[grepl("Wind",unique(gWRI$class1),ignore.case=T)]]<-
         unique(dataFromGCAM$class1)[grepl("Wind",unique(dataFromGCAM$class1),ignore.case=T)]
       gWRI[gWRI==unique(gWRI$class1)[grepl("Solar",unique(gWRI$class1),ignore.case=T)]]<-
         unique(dataFromGCAM$class1)[grepl("Solar",unique(dataFromGCAM$class1),ignore.case=T)]

       readAllGCAMcapDataList<-metis.readgcam(gcamdatabasePath = gcamdatabasePath, gcamdatabaseName = gcamdatabaseName,
                                    queryxml = queryxml, queryPath = queryPath,
                                    scenOrigNames = scenOrigNames, scenNewNames = scenNewNames, reReadData = reReadData,
                                    dataProj = dataProj, dataProjPath = dataProjPath, dirOutputs = biaOutputsFolder,
                                    regionsSelect = "All", queriesSelect = queriesSelect , paramsSelect = c("elecByTech", "elecCapBySubsector"))

       readAllGCAMcapData<-readAllGCAMcapDataList$data%>%
         dplyr::filter(param=="elecCapBySubsector")

       gGCAMelecCap<-readAllGCAMcapData%>%dplyr::filter(x==2015)%>%
         dplyr::mutate(GCAM_total_capacity=value)%>%
         dplyr::select(-c(value))

       gCapComparison<-dplyr::full_join(gGCAMelecCap,gWRI, by = c("region", "class1"))%>%
         tidyr::gather(key="data_source",value="est_installed_capacity",-c("region","class1","aggregate","units","vintage","x","xLabel","class2","sources","param","scenario","origValue","origX","origUnits","origQuery","origScen","classPalette1","classLabel1","classPalette2","classLabel2"))

       for(regioni in regionsSelectCompareCap){
         gridR<-gCapComparison%>%dplyr::filter(region==regioni)
         fname=paste(unique(gridR$region),"_est_installed_capacity")
         metis.printPdfPng(figure=ggplot2::ggplot(data = gridR, ggplot2::aes(fill = data_source, x = class1, y = est_installed_capacity))+ggplot2::geom_bar(position = "dodge", stat="identity"),
                           dir=paste(biaOutputsFolder, "/biadiagnostics",sep=""),filename=fname,figWidth=9,figHeight=7,pdfpng="png")

       }     #close for loop
     } # Close if FALSE


  print("About to return data for distributed electricity generation data")

  print(paste("Bia output data written to: ",paste(getwd(),"/dataFiles/grids/bia/biaOutputs/dataBia",nameAppend,".csv",sep=""), sep = ""))


  if (file.exists(paste(getwd(),"/dataFiles/grids/bia/biaOutputs/dataBia",nameAppend,".csv",sep=""))){
    unlink(paste(getwd(),"/dataFiles/grids/bia/biaOutputs/dataBia",nameAppend,".csv",sep=""),recursive=T)}

  data.table::fwrite(dataBia,
                     file = paste(getwd(),"/dataFiles/grids/bia/biaOutputs/dataBia",nameAppend,".csv",sep=""),row.names = F, append=F)



  return(dataBia)

} # Close Function
