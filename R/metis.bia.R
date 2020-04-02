#' metis.bia
#'
#' This function downscales GCAM electricity generation and installed capacity onto a grid, based on WRI PowerWatch dataset of present capacity
#' @param biaInputsFolder Bia Inputs Folder Path
#' @param biaInputsFiles Bia Inputs Folder Path
#' @param dirOutputs  Default=paste(getwd(),"/outputs",sep  Default=""). Location for outputs.
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
#' @param dataProjPath Default = NULL. Folder that contains the dataProj or where it will be produced.
#' @param regionsSelect The regions to analyze in a vector. Example c('Colombia','Argentina')
#' @param regionsSelectDiagnostic Default = NULL (sets to regionsSelect). Choose the regions to run diagnostics.
#' @param paramsSelect Default = c("elecByTech", "elecCapBySubsector") . Vector of parameters to be read from the GCAM database
#' @param gridChoice Default = "grid_050" . Choice of whether to use 50 km x 50 km grid cells ("grid_050") or 25 km x 25 km ("grid_025").
#' @param diagnosticsON Default = T.
#' @param subsectorNAdistribute Default = "even". Choose "even" for even distribution or "totalOther" to distribute based on sum of all other subsectors..
#' @param folderName Default = NULL,
#' @param nameAppend Default=""
#' @return A tibble with GCAM electricity generation distributed on a grid for selected regions
#' @keywords electricity, generation, gcam, gridded, downscale, downscaling, downscaled
#' @export

metis.bia<- function(biaInputsFolder = "NA",
                     biaInputsFiles = "NA",
                     dirOutputs=paste(getwd(),"/outputs",sep=""),
                     reReadData = 1,
                     regionsSelect = NULL,
                     regionsSelectDiagnostic = NULL,
                     dataProj = "dataProj.proj",
                     dataProjPath = NULL,
                     scenOrigNames = NULL,
                     scenNewNames = NULL,
                     gcamdatabasePath = "NA",
                     gcamdatabaseName = "NA",
                     queryxml = "metisQueries.xml",
                     queryPath = paste(getwd(),"/dataFiles/gcam",sep=""),
                     queriesSelect = "All",
                     paramsSelect = c("elecByTech", "elecCapBySubsector"),
                     gridChoice = "grid_050",
                     diagnosticsON = F,
                     subsectorNAdistribute = "even",
                     folderName = NULL,
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
    gridCellCapacity -> regionCapSum -> Var1 -> Var2 -> gridCellIndex->commissioning_year-> gridChosen->
    subRegion->regionState->region_code->subRegionAlt->reg32_id->GCAM_total_capacity->GridByPolyID->gridCellArea->
    maxAreaDuplicates



  if(!subsectorNAdistribute %in% c("even","totalOther")){
    print(paste("subsectorNAdistribute provided: ",subsectorNAdistribute," should be either 'even' or 'totalOther'. Setting to 'even'.",sep=""))
    subsectorNAdistribute = "even"
  }


  #------------------
  # Create folders if needed
  #------------------

  if (!dir.exists(dirOutputs)){dir.create(dirOutputs)}
  if (!dir.exists(paste(dirOutputs, "/", folderName, sep = ""))){dir.create(paste(dirOutputs, "/", folderName, sep = ""))}
  if(!is.null(folderName)){
    if (!dir.exists(paste(dirOutputs, "/", folderName, "/Bia", sep = ""))){dir.create(paste(dirOutputs, "/", folderName, "/Bia", sep = ""))}
    if (!dir.exists(paste(dirOutputs, "/", folderName, "/Bia/diagnostics", sep = ""))){dir.create(paste(dirOutputs, "/", folderName, "/Bia/diagnostics",sep = ""))}
    dir=paste(dirOutputs, "/", folderName, "/Bia",sep = "")
    dirDiag=paste(dirOutputs, "/", folderName, "/Bia/diagnostics",sep = "")
  } else {
    if (!dir.exists(paste(dirOutputs, "/Bia", sep = ""))){dir.create(paste(dirOutputs, "/Bia",sep = ""))}
    if (!dir.exists(paste(dirOutputs, "/Bia/diagnostics", sep = ""))){dir.create(paste(dirOutputs, "/Bia/diagnostics",sep = ""))}
    dir=paste(dirOutputs, "/Bia",sep = "")
    dirDiag=paste(dirOutputs, "/", folderName, "/Bia/diagnostics",sep = "")
  }

  if(is.null(dataProjPath)){
    dataProjPath=dir
  }else{
    if(!dir.exists(dataProjPath)){
      print(paste("dataProjPath provided does not exist. Setting to Bia output directory: ",dir,sep=""))
      dataProjPath=dir
    }
  }

  #------------------
  # Read GCAM data
  #------------------

  print("Reading GCAM data ...")
  readgcamdata<-metis.readgcam(gcamdatabasePath = gcamdatabasePath, gcamdatabaseName = gcamdatabaseName,
                               queryxml = queryxml, queryPath = queryPath,
                               scenOrigNames = scenOrigNames, scenNewNames = scenNewNames, reReadData = reReadData,
                               dataProj = dataProj, dataProjPath = dataProjPath, dirOutputs = dir,
                               regionsSelect = regionsSelect, queriesSelect = queriesSelect , paramsSelect = paramsSelect,
                               nameAppend=nameAppend)

  print("GCAM data read.")


  if(nrow(readgcamdata$data)>0){
  # Save list of scenarios and queries
  scenarios <- readgcamdata$scenarios  # List of Scenarios in the GCAM database pulled in through metis.readgcam
  queries <- readgcamdata$queries  # List of Queries in the GCAM database pulled in through metis.readgcam

  if(length(queries)==0){stop("No queries found. Please check data.")}

  dataFromGCAM <- readgcamdata$data%>%
    tibble::as_tibble()%>%
    dplyr::select(scenario, region, param, sources,class1, x, xLabel, vintage, units,
                    aggregate, classLabel1, classPalette1,
                    origScen, origQuery, origUnits, origX,value,origValue)%>%
    dplyr::group_by(scenario, region, param, sources,class1, x, xLabel, vintage, units,
                    aggregate, classLabel1, classPalette1,
                    origScen, origQuery, origUnits, origX)%>%
    dplyr::summarize_at(dplyr::vars("value","origValue"),list(~sum(.,na.rm = T)))%>%dplyr::ungroup()%>%
    dplyr::filter(!is.na(value))


  # Check data to compare with GCAM Model interface
  # dataFromGCAM%>%dplyr::select(scenario,region,param,class1,x,value)%>%dplyr::group_by(scenario,param,x)%>%
  # dplyr::summarize(valSum=sum(value))%>%as.data.frame()%>%dplyr::filter(x==2015)

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


  if(grepl("grid_050",gridChoice,ignore.case=T)){
    listOfGridCells <- metis::grid050
  }
  if(grepl("grid_025",gridChoice,ignore.case=T)){
    listOfGridCells <- metis::grid025
  }

  gridChosen <- listOfGridCells


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

  # For GCAM USA replace region and region_code with State names and codes
  gridUS52 <- metis::metis.gridByPoly(gridDataTables =gridChosen,
                                      shape = metis::mapUS52,
                                      colName = "subRegion",
                                      saveFile = F)

  # gridDataTables =gridChosen
  # shape = metis::mapUS52
  # colName = "subRegion"
  # saveFile = F


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

          #biaInputsFile_i<-biaInputsFiles[1]
          if(!grepl(".csv",biaInputsFile_i)){biaInputsFile_i=paste(biaInputsFile_i,".csv",sep="")}

          if(!file.exists(paste(biaInputsFolder,"/",biaInputsFile_i,sep=""))){
            print(paste("bia input file: ", biaInputsFolder,"/",biaInputsFile_i," is incorrect or doesn't exist.",sep=""))
            print(paste("Skipping file: ",biaInputsFolder,"/",biaInputsFile_i,sep=""))
          }else{


            # Read in WRI Power plant distribution
            print(paste("Reading bia input file: ",biaInputsFile_i,"...",sep=""))
            gridWRI<-data.table::fread(paste(biaInputsFolder,"/",biaInputsFile_i,sep=""), header=T,stringsAsFactors = F,encoding="Latin-1")

            # Check total cap and generation for a country
            # gridWRI%>%filter(country_long=="United States of America")%>%
            # dplyr::select(country_long,capacity_mw)%>%
            # dplyr::group_by(country_long)%>%dplyr::summarize(valSumGW=sum(capacity_mw/1000))
            # gridWRI%>%filter(country_long=="United States of America")%>%
            #  dplyr::select(country_long,estimated_generation_gwh)%>%dplyr::group_by(country_long)%>%
            #  dplyr::summarize(valSumTWh=sum(estimated_generation_gwh/1000,na.rm=T))

            # Make country names consistent with ctor file
            gridWRI[gridWRI=="Democratic Republic of the Congo"]<-"Congo DRC"
            gridWRI[gridWRI=="Congo"]<-"Congo Rep."
            gridWRI[gridWRI=="Taiwan"]<-"Taiwan China"
            ctor$country_long[ctor$region == "Taiwan"] <- "Taiwan China"
            gridWRI[gridWRI=="Congo"]<-"Congo Rep."

            for(country_long_i in c("United States","Bosnia","Brunei",
                                    "Cote","Gambia","Syria","Taiwan","Trinidad")){
              if(length(unique(ctor$country_long)[grepl(country_long_i,unique(ctor$country_long),ignore.case=T)])==1){
                gridWRI <- gridWRI %>%
                  dplyr::mutate(country_long=dplyr::if_else(grepl(country_long_i,country_long,ignore.case=T),
                                               unique(ctor$country_long)[grepl(country_long_i,unique(ctor$country_long),ignore.case=T)],
                                               country_long))}}

            # Join with ctor file for original 32 GCAM regions
            gridWRI<-gridWRI%>%tibble::as_tibble()%>%
              dplyr::filter(country_long %in% unique(ctor$country_long))%>%
              dplyr::select(-year_of_capacity_data,-commissioning_year,-name,-country,-gppd_idnr,-fuel2,-fuel3,-fuel4,-owner,-source,-url,-geolocation_source)%>%
              dplyr::left_join((ctor%>%dplyr::filter(!region %in% metis.assumptions()$US52)),by="country_long")

            # Check total cap and generation for a country
            # gridWRI%>%filter(country_long=="United States")%>%
            # dplyr::select(country_long,capacity_mw)%>%
            # dplyr::group_by(country_long)%>%dplyr::summarize(valSumGW=sum(capacity_mw/1000))
            # gridWRI%>%filter(country_long=="United States")%>%
            #  dplyr::select(country_long,estimated_generation_gwh)%>%dplyr::group_by(country_long)%>%
            #  dplyr::summarize(valSumTWh=sum(estimated_generation_gwh/1000,na.rm=T))

            # Re-organizing dataset and
            gridWRI<-gridWRI%>%dplyr::mutate(lat=latitude,
                                             lon=longitude,
                                             param="biaElecGen",
                                             units= "Capacity (GW)",
                                             aggType="vol",
                                             classPalette="pal_elec_subsec",
                                             class1=fuel1,
                                             value=capacity_mw/1000,
                                             x=NA,
                                             gridlat = round(gridDimlat*round(latitude*(1/gridDimlat)-(gridShiftlat/gridDimlat))+gridShiftlat, digits = 10),
                                             gridlon = round(gridDimlon*round(longitude*(1/gridDimlon)-(gridShiftlon/gridDimlon))+gridShiftlon, digits = 10))%>%
              tibble::as_tibble()%>%
              dplyr::select(-latitude,-longitude,-fuel1,-capacity_mw,-generation_gwh_2013,-generation_gwh_2014,-generation_gwh_2015,-generation_gwh_2016,-estimated_generation_gwh,-country_long)

            # Check gridWRI cap
            # gridWRI%>%filter(ctry_name=="United States")%>%
            # dplyr::select(ctry_name,value)%>%
            # dplyr::group_by(ctry_name)%>%dplyr::summarize(valSumGW=sum(value))


            # Spread US data by State
            if(any(regionsSelect %in% metis.assumptions()$US52)){
              # Replace State grid cells for states chosen
              ctorUS <- ctor%>%dplyr::filter(grepl("United States",ctry_name,ignore.case=T))
              gridWRI <- gridWRI %>%
                dplyr::filter(ctry_name==(ctorUS$ctry_name%>%unique())[1])%>%
                dplyr::left_join(gridUS52 %>%
                                   dplyr::filter(subRegion %in% regionsSelect)%>%
                                   dplyr::select(gridlat=lat,gridlon=lon,regionState=subRegion),by=c("gridlat","gridlon"))%>%
                dplyr::mutate(region = dplyr::if_else(!is.na(regionState),as.character(regionState),region))%>%
                dplyr::select(-c(region_code,ctry_code,ctry_name,country_long,regionState))%>%
                dplyr::left_join(ctor%>%dplyr::filter(ctry_name=="United States"),by=c("region"))%>%
                dplyr::mutate(ctry_code=(ctorUS$ctry_code%>%unique())[1],
                              ctry_name=(ctorUS$ctry_name%>%unique())[1])%>%
                dplyr::mutate(region_code=dplyr::if_else(is.na(region_code),(ctorUS$region_code%>%unique())[1],region_code),
                              country_long=dplyr::if_else(is.na(country_long),(ctorUS$country_long%>%unique())[1],country_long))%>%
                dplyr::bind_rows(gridWRI %>% dplyr::filter(ctry_name!=(ctorUS$ctry_name%>%unique())[1]))
            }

            # Check total cap and generation for a country
            # gridWRI%>%filter(ctry_name=="United States")%>%
            # dplyr::select(ctry_name,value)%>%
            # dplyr::group_by(ctry_name)%>%dplyr::summarize(valSumGW=sum(value))

            # Calculate gridcell %
            gridWRI <- gridWRI %>%
              dplyr::left_join(listOfGridCells,by = c("gridlat","gridlon"))%>%
              dplyr::group_by(gridlat, gridlon, class1, gridID, ctry_name, ctry_code, region, region_code, param, units)%>%
              dplyr::summarize(gridCellCapacity = sum(value))%>%
              dplyr::ungroup() %>%
              dplyr::group_by(class1,region,region_code)%>%
              dplyr::mutate(regionCapSum = sum(gridCellCapacity),
                            gridCellPercentage = gridCellCapacity/regionCapSum) %>%
              dplyr::ungroup()

            # Check GridCell Percentages
            # (gridWRI%>%dplyr::select(region,class1,gridCellPercentage)%>%
            #   dplyr::group_by(region,class1)%>%dplyr::summarize(sumX=sum(gridCellPercentage)))$sumX%>%unique()

            # Check total cap and generation for a country
            # gridWRI%>%filter(ctry_name=="United States")%>%
            # dplyr::select(ctry_name,gridCellCapacity)%>%
            # dplyr::group_by(ctry_name)%>%dplyr::summarize(valSumGW=sum(gridCellCapacity))

           # Rename to GCAM class names
           if(length(unique(dataFromGCAM$class1)[grepl("chp",unique(dataFromGCAM$class1),ignore.case=T)])==1){
             gridWRI <- gridWRI %>%
              dplyr::mutate(class1=dplyr::if_else(grepl("cogen",class1,ignore.case=T),
                                           unique(dataFromGCAM$class1)[grepl("chp",unique(dataFromGCAM$class1),ignore.case=T)][1],
                                           class1))}

            if(length(unique(dataFromGCAM$class1)[grepl("refined liquids",unique(dataFromGCAM$class1),ignore.case=T)])==1){
              gridWRI <- gridWRI %>%
                dplyr::mutate(class1=dplyr::if_else(grepl("oil",class1,ignore.case=T),
                                                    unique(dataFromGCAM$class1)[grepl("refined liquids",unique(dataFromGCAM$class1),ignore.case=T)][1],
                                                    class1))}

            for(class1_i in c("coal","gas","oil","biomass","nuclear","geothermal","hydrogen","hydro","wind","solar")){
            if(length(unique(dataFromGCAM$class1)[grepl(class1_i,unique(dataFromGCAM$class1),ignore.case=T)])==1){
              gridWRI <- gridWRI %>%
                dplyr::mutate(class1=dplyr::if_else(grepl(class1_i,class1,ignore.case=T),
                                             unique(dataFromGCAM$class1)[grepl(class1_i,unique(dataFromGCAM$class1),ignore.case=T)][1],
                                             class1))}}

            if(subsectorNAdistribute == "even"){

            #-------------------
            # For electricity generation subsectors not represented in power plant database, distribute evently throughout region
            #-------------------

            gridCropped <- tibble::tibble(gridlat = NA, gridlon = NA, gridID = NA, region = NA)

            for(regionc in regionsSelect){

              if(regionc %in% unique(metis::mapUS52@data$subRegion)){
                shape=metis::mapUS52}else{
                  if(regionc %in% unique(metis::mapGCAMReg32@data$subRegion)){
                    shape=metis::mapGCAMReg32}else{
                      if(regionc %in% unique(metis::mapCountries@data$subRegion)){
                        shape=metis::mapCountries}
                    }
                }
             shape@data <-shape@data %>%
                dplyr::select(-region)%>%
                dplyr::rename(reg32_id=subRegionAlt,region=subRegion)%>%
                dplyr::mutate(reg32_id=as.character(reg32_id))%>%
                dplyr::left_join(ctor %>%
                            dplyr::select(region_code) %>%
                            dplyr::distinct() %>%
                            dplyr::mutate(reg32_id=as.character(region_code))%>%
                            dplyr::select(-region_code),
                            by = "reg32_id")
              shape@data %>% as.data.frame()
              shape <- shape[(shape$region %in% regionc),];
              shape@data <- droplevels(shape@data)
              raster::plot(shape)

              # Prepare grids to be cropped
              spdf = sp::SpatialPointsDataFrame(sp::SpatialPoints(coords=(cbind(listOfGridCells$gridlon,listOfGridCells$gridlat))),data=listOfGridCells)
              sp::gridded(spdf)<-TRUE

              r<-raster::stack(spdf)
              raster::projection(r)<-sp::proj4string(shape)

              rmask<-raster::mask(r,shape)
              rmaskP<-raster::rasterToPolygons(rmask)
              if(is.null(rmaskP)){
              rcrop<-raster::crop(r,shape)
              rmaskP<-raster::rasterToPolygons(rcrop)
              }

              gridCropped<-dplyr::bind_rows(gridCropped,dplyr::mutate(tibble::as_tibble(rmaskP@data),region = regionc))

            }

            rm(spdf,rmask,r); gc()

            gridCropped <- gridCropped %>%
              dplyr::filter(!is.na(region)) %>%
              unique()

            if(nrow(gridCropped)>0){

            # For each class and region attach grid cell data
            dataBia<- dataFromGCAM %>%
              dplyr::left_join(
              gridWRI%>%dplyr::filter(region %in% regionsSelect[regionsSelect %in% (dataFromGCAM$region%>%unique())])%>%
              dplyr::select(gridlat, gridlon, gridID, class1, region, region_code, ctry_name, ctry_code, gridCellPercentage),
              by = c("class1", "region"))%>%
              dplyr::mutate(valueDistrib = gridCellPercentage*value, origValueDistrib = gridCellPercentage*origValue)

            # # Check GridCell Percentages
            # (gridWRI%>%dplyr::select(region,class1,gridCellPercentage)%>%
            #   dplyr::group_by(region,class1)%>%dplyr::summarize(sumX=sum(gridCellPercentage)))$sumX%>%unique()
            # # Check data to compare with GCAM Model interface
            # dataFromGCAM%>%dplyr::select(scenario,region,param,class1,x,value)%>%dplyr::group_by(scenario,param,x)%>%
            # dplyr::summarize(valSum=sum(value))%>%as.data.frame()%>%dplyr::filter(x==2015)
            # # Check total cap and generation for a country
            # gridWRI%>%filter(ctry_name=="United States")%>%
            # dplyr::select(ctry_name,gridCellCapacity)%>%
            # dplyr::group_by(ctry_name)%>%dplyr::summarize(valSumGW=sum(gridCellCapacity))
            # # Check data to compare with GCAM Model interface
            # # Will be slightly lower becuase of some regions missing distribution
            # dataBia%>%dplyr::select(scenario,region,param,class1,x,valueDistrib)%>%dplyr::group_by(scenario,param,x)%>%
            #   dplyr::summarize(valSum=sum(valueDistrib,na.rm=T))%>%as.data.frame()%>%dplyr::filter(x==2015)

            dataBiaNA <- dplyr::filter(dataBia,is.na(gridlat)) %>%
              dplyr::select(-gridlat, -gridlon, -gridID)

            # # Check Missing Data
            # dataBiaNA%>%dplyr::select(scenario,region,param,class1,x,value)%>%dplyr::group_by(scenario,param,x)%>%
            #   dplyr::summarize(valSum=sum(value,na.rm=T))%>%as.data.frame()%>%dplyr::filter(x==2015)

            evenDistrib <- expand.grid(unique(dataBiaNA$class1), gridCropped$gridID) %>%
              tibble::as_tibble() %>%
              dplyr::rename(class1 = Var1, gridID = Var2) %>%
              dplyr::left_join(listOfGridCells, by = "gridID") %>%
              dplyr::left_join(gridCropped, by = c("gridlat", "gridlon", "gridID"))

            evenDistrib$class1 <- as.character(evenDistrib$class1)

            evenDistrib <- dataBiaNA %>%
              dplyr::left_join(evenDistrib, by = c("class1", "region")) %>%
              dplyr::mutate(gridCellCapacity = 999) %>%
              dplyr::group_by(scenario,param,x,class1,region)%>%
              dplyr::mutate(regionCapSum = sum(gridCellCapacity),
                            gridCellPercentage = gridCellCapacity/regionCapSum) %>%
              dplyr::ungroup() %>%
              dplyr::select(-valueDistrib, -origValueDistrib) %>%
              dplyr::mutate(valueDistrib = gridCellPercentage*value, origValueDistrib = gridCellPercentage*origValue)%>%
              dplyr::select(-gridCellCapacity, -regionCapSum)

            # # Check GridCell Percentages
            # (evenDistrib%>%dplyr::select(scenario,param,x,region,class1,gridCellPercentage)%>%
            #   dplyr::group_by(scenario,param,x,region,class1)%>%dplyr::summarize(sumX=sum(gridCellPercentage)))$sumX%>%unique()
            # # Check total cap and generation for a country (May lose some capacity because of grid cells)
            # evenDistrib%>%as.data.frame()%>%head();
            # evenDistrib%>%dplyr::filter(region %in% metis.assumptions()$US52)%>%
            #   dplyr::select(scenario,param,x,units,valueDistrib)%>%
            #   dplyr::group_by(scenario,param,x,units,)%>%dplyr::summarize(valSum=sum(valueDistrib,na.rm=T))%>%
            #   dplyr::filter(x==2015)

            dataBia <- dataBia %>%
              dplyr::filter(!(is.na(gridlat))) %>%
              dplyr::bind_rows(evenDistrib)

            # # Check total cap and generation for a country
            # dataBia%>%as.data.frame()%>%head();
            # dataBia%>%dplyr::filter(region %in% metis.assumptions()$US52)%>%
            # dplyr::select(scenario,param,x,units,valueDistrib)%>%
            # dplyr::group_by(scenario,param,x,units,)%>%dplyr::summarize(valSum=sum(valueDistrib,na.rm=T))%>%
            #   dplyr::filter(x==2015)
            # # Check data to compare with GCAM Model interface
            # dataFromGCAM%>%dplyr::select(scenario,region,param,class1,x,value)%>%dplyr::group_by(scenario,param,x)%>%
            # dplyr::summarize(valSum=sum(value))%>%as.data.frame()%>%dplyr::filter(x==2015)


            } else {
              print(paste("No data for chosen regions: ", paste(regionsSelect,collapse=", "),sep=""))
            }

            } # Close if subsectorNAdistribute == "even"



            if(subsectorNAdistribute == "totalOther"){

            #-------------------
            # For electricity generation subsectors not represented in power plant database, distribute according to installed capacity of all subsectors
            #-------------------


            gridWRIallSubsecMixed <- gridWRI %>%
              dplyr::group_by(gridlat, gridlon, gridID, ctry_name, ctry_code, region, region_code, param, units)%>%
              dplyr::summarise(gridCellCapacity = sum(gridCellCapacity))%>%
              dplyr::ungroup() %>%
              dplyr::group_by(region,region_code)%>%
              dplyr::mutate(regionCapSum = sum(gridCellCapacity),
                            gridCellPercentage = gridCellCapacity/regionCapSum) %>%
              dplyr::ungroup() %>%
              tibble::rowid_to_column(var = "gridCellIndex") %>%
              dplyr::mutate(gridCellIndex = -gridCellIndex)

           #  # Check GridCell Percentages
           #  (gridWRIallSubsecMixed%>%dplyr::select(region,gridCellPercentage)%>%
           #    dplyr::group_by(region)%>%dplyr::summarize(sumX=sum(gridCellPercentage)))$sumX%>%unique()
           # # Check total cap and generation for a country
           #  gridWRIallSubsecMixed%>%filter(ctry_name=="United States")%>%
           #  dplyr::select(ctry_name,gridCellCapacity)%>%
           #  dplyr::group_by(ctry_name)%>%dplyr::summarize(valSumGW=sum(gridCellCapacity))

            dataBia<- dataFromGCAM %>%
              dplyr::left_join(
                gridWRI%>%dplyr::filter(region %in% regionsSelect)%>%
                  dplyr::select(gridlat, gridlon, gridID, class1, region, region_code, ctry_name, ctry_code, gridCellPercentage),
                by = c("class1", "region"))%>%
              dplyr::mutate(valueDistrib = gridCellPercentage*value, origValueDistrib = gridCellPercentage*origValue)

            # # #Check data to compare with GCAM Model interface
            # # #Will be slightly lower becuase of some regions missing distribution
            # dataBia%>%dplyr::select(scenario,region,param,class1,x,valueDistrib)%>%dplyr::group_by(scenario,param,x)%>%
            #   dplyr::summarize(valSum=sum(valueDistrib,na.rm=T))%>%as.data.frame()%>%dplyr::filter(x==2015)


            #Find the elecricity generation subsectors that are not represented in the powerplant database, but which are predicted by GCAM
            dataBiaNA <- dplyr::filter(dataBia,is.na(gridlat)) %>%
              dplyr::select(-gridlat, -gridlon, -gridID, -region_code, -ctry_name, -ctry_code, -gridCellPercentage)

            distribByTotalCap <- expand.grid(unique(dataBiaNA$class1), (dplyr::filter(gridWRIallSubsecMixed, region %in% regionsSelect))$gridCellIndex) %>%
              tibble::as_tibble() %>%
              dplyr::rename(class1 = Var1, gridCellIndex = Var2) %>%
              dplyr::mutate(class1 = as.character(class1)) %>%
              dplyr::left_join(gridWRIallSubsecMixed, by = "gridCellIndex") %>%
              dplyr::select(-param, -units)

            distribByTotalCap <- dplyr::left_join(dataBiaNA, distribByTotalCap, by = c("class1", "region")) %>%
              dplyr::select(-valueDistrib, -origValueDistrib) %>%
              dplyr::mutate(valueDistrib = gridCellPercentage*value, origValueDistrib = gridCellPercentage*origValue)

            # # Check data
            # distribByTotalCap%>%dplyr::select(scenario,region,param,class1,x,valueDistrib)%>%dplyr::group_by(scenario,param,x)%>%
            #   dplyr::summarize(valSum=sum(valueDistrib,na.rm=T))%>%as.data.frame()%>%dplyr::filter(x==2015)

            dataBia <- dataBia %>%
              dplyr::filter(!(is.na(gridlat))) %>%
              dplyr::bind_rows(dplyr::select(distribByTotalCap, -regionCapSum, -gridCellCapacity, -gridCellIndex))

            # #Check total cap and generation for a country
            # dataBia%>%dplyr::filter(region %in% metis.assumptions()$US52)%>%
            # dplyr::select(scenario,param,x,units,valueDistrib)%>%
            # dplyr::group_by(scenario,param,x,units,)%>%dplyr::summarize(valSum=sum(valueDistrib,na.rm=T))%>%
            #   dplyr::filter(x==2015)
            # # Check data to compare with GCAM Model interface
            # dataFromGCAM%>%dplyr::select(scenario,region,param,class1,x,value)%>%dplyr::group_by(scenario,param,x)%>%
            # dplyr::summarize(valSum=sum(value))%>%as.data.frame()%>%dplyr::filter(x==2015)


            } # Close if subsectorNAdistribute == "totalOther"



          } # Close if bia file exists
        } # close bia file loops

      } # Close bia folder

  dataBia <- dataBia %>%
    dplyr::mutate(
      valueAgg=value,
      origValueAgg=origValue,
      value=valueDistrib,
      valueOrig=origValueDistrib)%>%
    dplyr::select(-valueDistrib,-origValueDistrib)

  print("About to return data for distributed electricity generation data")

  print(paste("Bia output data written to: ",paste(dir,"/biaOutput",nameAppend,".csv",sep=""), sep = ""))


  if (file.exists(paste(dir,"/biaOutput",nameAppend,".csv",sep=""))){
    unlink(paste(dir,"/biaOutput",nameAppend,".csv",sep=""),recursive=T)}

  data.table::fwrite(dataBia,
                     file = paste(dir,"/biaOutput",nameAppend,".csv",sep=""),row.names = F, append=F)


    #----------------
    # Function for comparing electricity generation data
    #---------------

     if(diagnosticsON == T){

       if(is.null(regionsSelectDiagnostic)){
         regionsSelectDiagnostic=regionsSelect
       }

       # Add region "USA" for diagnostics GCAM USA with states is being used
       if(any(regionsSelectDiagnostic %in% metis.assumptions()$US52)){
            regionsSelectDiagnostic <- c(regionsSelectDiagnostic[!regionsSelectDiagnostic %in% metis.assumptions()$US52],
                                         metis.assumptions()$US52,"USA")%>%unique()
            print("Including all US States for Diagnostics...")
       }


       print(paste("Diagnostic regions selected: ",
                   paste(regionsSelectDiagnostic,collapse=", "),sep=""))

       biaInputsFile_i<-biaInputsFiles[1]

       ctr<-data.table::fread(file = paste(biaInputsFolder,"/country_to_region.csv",sep=""), header=T,stringsAsFactors = F,encoding="Latin-1")%>%
         tibble::as_tibble()%>%
         dplyr::mutate(country_long=ctry_name)

       gWRI<-data.table::fread(file = paste(biaInputsFolder,"/",biaInputsFile_i, '.csv',sep=""), header=T,encoding="Latin-1")

       # gWRI$country%>%unique()
       # gWRI%>%filter(country %in% "USA")%>%dplyr::group_by(fuel1)%>%dplyr::summarize(sumVal=sum(capacity_mw)/1000)
       # gWRI%>%filter(country %in% "USA")%>%dplyr::summarize(sumVal=sum(capacity_mw)/1000)

       gWRI[gWRI=="Democratic Republic of the Congo"]<-"Congo DRC"
       gWRI[gWRI=="Congo"]<-"Congo Rep."
       gWRI[gWRI=="Taiwan"]<-"Taiwan China"
       ctr$country_long[ctr$region == "Taiwan"] <- "Taiwan China"
       gWRI[gWRI=="Congo"]<-"Congo Rep."

       for(country_long_i in c("United States","Bosnia","Brunei",
                               "Cote","Gambia","Syria","Taiwan","Trinidad")){
         if(length(unique(ctr$country_long)[grepl(country_long_i,unique(ctr$country_long),ignore.case=T)])==1){
           gWRI <- gWRI %>%
             dplyr::mutate(country_long=dplyr::if_else(grepl(country_long_i,country_long,ignore.case=T),
                                                unique(ctr$country_long)[grepl(country_long_i,unique(ctr$country_long),ignore.case=T)][1],
                                                country_long))}}


       # gWRI$country%>%unique()
       # gWRI%>%filter(country %in% "USA")%>%dplyr::group_by(fuel1)%>%dplyr::summarize(sumVal=sum(capacity_mw)/1000)
       # gWRI%>%filter(country %in% "USA")%>%dplyr::summarize(sumVal=sum(capacity_mw)/1000)

       gWRI<-gWRI%>%tibble::as_tibble()%>%
         dplyr::select(latitude, longitude,fuel1,capacity_mw,estimated_generation_gwh,country_long,
                       generation_gwh_2015,generation_gwh_2016,generation_gwh_2015,generation_gwh_2016)%>%
         dplyr::filter(country_long %in% unique(ctr$country_long))%>%
         dplyr::left_join(ctr%>%dplyr::filter(region_code<33)%>%dplyr::select(region,country_long),by="country_long")%>%
         dplyr::select(-country_long)

      #gWRI%>%filter(region %in% "USA")%>%dplyr::summarize(sumVal=sum(capacity_mw)/1000)


       # to_do:
       # Aggregate data to any US states selected
       # if(any(regionsSelectDiagnostic %in% unique(metis::mapUS52@data$subRegion))){
       #
       #   gridX = gWRI%>%dplyr::select(lat=latitude,lon=longitude,value=capacity_mw,fuel1)%>%unique(); gridX
       #   shapeX = metis::mapUS52[metis::mapUS52@data$subRegion %in% regionsSelectDiagnostic,]
       #   shapeX@data = shapeX@data%>%droplevels()
       #   plot(shapeX)
       #
       #   grid2polyX<-metis.grid2poly(gridFiles= gridX,
       #                                     subRegShape=shapeX,
       #                                     subRegCol="subRegion",
       #                                     saveFiles = F)
       # }


       gWRI<-gWRI%>%dplyr::mutate(lat=latitude,
                                  lon=longitude,
                                  param="biaElecGen",
                                  units= "Capacity (GW)",
                                  aggType="vol",
                                  classPalette="pal_elec_subsec",
                                  class1=fuel1,
                                  value=capacity_mw/1000,
                                  x=NA,
                                  BackCalcCapFactr=estimated_generation_gwh/capacity_mw*(1000/(365*24)),
                                  BCCF_gen2015=(generation_gwh_2015/capacity_mw)*(1000/(365*24)),
                                  BCCF_gen2016=(generation_gwh_2016/capacity_mw)*(1000/(365*24)),
                                  est_gen_gwh=estimated_generation_gwh,
                                  gen_gwh_2013=generation_gwh_2013,
                                  gen_gwh_2014=generation_gwh_2014,
                                  gen_gwh_2015=generation_gwh_2015,
                                  gen_gwh_2016=generation_gwh_2016)%>%
         tibble::as_tibble()%>%dplyr::select(region,class1,value)




       gWRI <- gWRI%>%
         dplyr::group_by(region, class1)%>%
         dplyr::summarise(WRI_total_capacity=sum(value))%>%
         dplyr::filter(region %in% regionsSelectDiagnostic)

       #gWRI %>% dplyr::group_by(region)%>%dplyr::summarize(sumValGW=sum(WRI_total_capacity))


       # Rename to GCAM class names
       if(length(unique(dataFromGCAM$class1)[grepl("chp",unique(dataFromGCAM$class1),ignore.case=T)])==1){
         gWRI <- gWRI %>%
           dplyr::mutate(class1=dplyr::if_else(grepl("cogen",class1,ignore.case=T),
                                        unique(dataFromGCAM$class1)[grepl("chp",unique(dataFromGCAM$class1),ignore.case=T)],
                                        class1))}

       # Rename to GCAM class names
       if(length(unique(dataFromGCAM$class1)[grepl("refined liquids",unique(dataFromGCAM$class1),ignore.case=T)])==1){
         gWRI <- gWRI %>%
           dplyr::mutate(class1=dplyr::if_else(grepl("oil",class1,ignore.case=T),
                                               unique(dataFromGCAM$class1)[grepl("refined liquids",unique(dataFromGCAM$class1),ignore.case=T)],
                                               class1))}

       for(class1_i in c("coal","gas","oil","biomass","nuclear","geothermal","hydro","wind","solar")){
         if(length(unique(dataFromGCAM$class1)[grepl(class1_i,unique(dataFromGCAM$class1),ignore.case=T)])==1){
           gWRI <- gWRI %>%
             dplyr::mutate(class1=dplyr::if_else(grepl(class1_i,class1,ignore.case=T),
                                          unique(dataFromGCAM$class1)[grepl(class1_i,unique(dataFromGCAM$class1),ignore.case=T)],
                                          class1))}}

       readAllGCAMcapDataList<-metis.readgcam(gcamdatabasePath = gcamdatabasePath, gcamdatabaseName = gcamdatabaseName,
                                    queryxml = queryxml, queryPath = queryPath,
                                    scenOrigNames = scenOrigNames, scenNewNames = scenNewNames, reReadData = reReadData,
                                    dataProj = dataProj, dataProjPath = dataProjPath, dirOutputs = dir,
                                    regionsSelect = regionsSelectDiagnostic, nameAppend = paste("Diagnostic",nameAppend,sep=""),
                                    queriesSelect = queriesSelect , paramsSelect = c("elecByTechTWh", "elecCapByFuel"))

       readAllGCAMcapData<-readAllGCAMcapDataList$data%>%
         dplyr::filter(param=="elecCapByFuel")%>%
         dplyr::group_by(scenario, region, param, sources,class1, x, xLabel, vintage, units,
                                                              aggregate, classLabel1, classPalette1,
                                                              origScen, origQuery, origUnits, origX)%>%
         dplyr::summarize_at(dplyr::vars("value","origValue"),list(~sum(.,na.rm = T)))%>%dplyr::ungroup()%>%
         dplyr::filter(!is.na(value))

       # Check total US Cap (GW) in 2015 ~ about 1000 GW
       # readAllGCAMcapData%>%
       #   dplyr::filter(region %in% metis.assumptions()$US52, scenario=="Ref",x==2015)%>%
       #   dplyr::group_by(scenario,param)%>%dplyr::summarize(sumVal=sum(value))

       gGCAMelecCap<-readAllGCAMcapData%>%dplyr::filter(x==2015)%>%
         dplyr::select(scenario,region, class1, value)%>%
         dplyr::group_by(scenario, region, class1)%>%
         dplyr::summarize(GCAM_total_capacity=sum(value))%>%
         dplyr::ungroup();  gGCAMelecCap

       # Combine states for Diagnostics
       # Add a check for total US
       if(any(regionsSelectDiagnostic %in% metis.assumptions()$US52)){
         print("Comparing sum of chosen states against total US")
         gGCAMelecCapUS52 <- gGCAMelecCap %>%
           dplyr::filter(region %in% metis.assumptions()$US52,
                         scenario != "WRI") %>%
           dplyr::group_by(scenario,class1)%>%
           dplyr::summarize(GCAM_total_capacity=sum(GCAM_total_capacity,na.rm=T))%>%
           dplyr::ungroup()%>%
           dplyr::mutate(region="USA")

         gGCAMelecCap <- gGCAMelecCap %>%
           dplyr::bind_rows(gGCAMelecCapUS52)%>%
           dplyr::filter(!region %in% metis.assumptions()$US52)

         regionsSelectDiagnostic <- c(regionsSelectDiagnostic,"USA")
       }

       # Rename to GCAM class names
       if(length(unique(readAllGCAMcapData$class1)[grepl("chp",unique(readAllGCAMcapData$class1),ignore.case=T)])>1){
         gWRI <- gWRI %>%
           dplyr::mutate(class1=dplyr::if_else(grepl("cogen",class1,ignore.case=T),
                                               unique(readAllGCAMcapData$class1)[grepl("chp",unique(readAllGCAMcapData$class1),ignore.case=T)][1],
                                               class1))}

       for(class1_i in c("coal","gas","oil","biomass","nuclear","geothermal","hydrogen","hydro","wind","solar")){
         if(length(unique(readAllGCAMcapData$class1)[grepl(class1_i,unique(readAllGCAMcapData$class1),ignore.case=T)])>1){
           gWRI <- gWRI %>%
             dplyr::mutate(class1=dplyr::if_else(grepl(class1_i,class1,ignore.case=T),
                                                 unique(readAllGCAMcapData$class1)[grepl(class1_i,unique(readAllGCAMcapData$class1),ignore.case=T)][1],
                                                 class1))}}

       gCapComparison<-gGCAMelecCap %>%
         dplyr::full_join(gWRI, by = c("region", "class1"))%>%
         tidyr::gather(key="data_source",value="est_installed_capacity",-c("region","class1","scenario"))%>%
         dplyr::mutate(scenario=dplyr::if_else(data_source=="WRI_total_capacity","WRI",scenario));
       #gCapComparison%>%head();gCapComparison%>%tail();

       # Check total US Cap for WRI and GCAM
       # gGCAMelecCap%>%
       #   dplyr::group_by(scenario)%>%dplyr::summarize(sumVal=sum(GCAM_total_capacity,na.rm=T))
       # gWRI%>%
       #   dplyr::group_by(region)%>%dplyr::summarize(sumVal=sum(WRI_total_capacity,na.rm=T))


       for(regioni in regionsSelectDiagnostic[!regionsSelectDiagnostic %in% metis.assumptions()$US52]){
         gridR<-gCapComparison%>%dplyr::filter(region==regioni)
         fname=paste(unique(gridR$region),"_est_installed_capacity",nameAppend,sep="")
         figX = ggplot2::ggplot(data = gridR, ggplot2::aes(fill = data_source, x = class1, y = est_installed_capacity))+
                ggplot2::geom_bar(position = "dodge", stat="identity")+
           ggplot2::theme(axis.text.x=ggplot2::element_text(angle=90,hjust=1))
         metis.printPdfPng(figure=figX,
                           dir=dirDiag,
                           filename=fname,
                           figWidth=9,figHeight=7,pdfpng="png")

       }     #close for loop
     } # Close if FALSE

  }else{
    print(paste("Skipping Bia analysis..."))
  }

  return(dataBia)

} # Close Function
