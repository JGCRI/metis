#' metis.bia
#'
#' This function downscales GCAM electricity generation and installed capacity onto a grid, based on WRI PowerWatch dataset of present capacity
#' @param biaInputsFolder andym Bia Inputs Folder Path
#' @param biaInputsFiles andym Bia Files to Read
#' @param biaScenarioAssign andym Default "NA". Scenario name if testing a single scenario.
#' @param zelusFolder andym Full path to zelus outputs
#' @param zelusFiles andym Default =c(?_?'edtrnsp','edbld','edindus'?_?)
#' @param zelusScenario andym Scenario name for zelus run
#' @param zelusUnits andym No Default
#' @param popFolder Default = <-paste(getwd(),"/dataFiles/grids/griddedIDsPop/",sep="")
#' @param popFiles Default = <-"grid_pop_map"
#' @param popUnits Default = <-"person"
#' @param biaOutputsFolder Default =paste(getwd(),"/dataFiles/grids/bia/biaOutputs",sep=""),
#' @param reReadData Default =1,
#' @param sqliteUSE Default = T,
#' @param sqliteDBNamePath Default = paste(getwd(),"/outputs/Grids/gridMetis.sqlite", sep = "")
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
#' @param reReadData If TRUE will read the GCAM data base and create a queryData.proj file
#' in the same folder as the GCAM database. If FALSE will load a '.proj' file if a file
#' with full path is provided otherwise it will search for a dataProj.proj file in the existing
#' folder which may have been created from an old run.
#' @param dataProj Optional. A default 'dataProj.proj' is produced if no .Proj file is specified.
#' @param dataProjPath Folder that contains the dataProj or where it will be produced.
#' @param regionsSelect The regions to analyze in a vector. Example c('Colombia','Argentina')
#' @param paramsSelect Default = c("elecByTech", "elecCapBySubsector") . Vector of parameters to be read from the GCAM database
#' @param gridChoice Default = "grid_050" . Choice of whether to use 50 km x 50 km grid cells ("grid_050") or 25 km x 25 km ("grid_025").
#' @return #andym a tibble with GCAM electricity generation distributed on a grid for a selected region
#' @keywords electricity, generation, gcam, gridded, downscale, downscaling, downscaled
#' @export

metis.bia<- function(biaInputsFolder = "NA",
                     biaInputsFiles = "NA",
                     biaScenarioAssign = "NA",
                     zelusFolder = "NA",
                     zelusScenario = "NA",
                     zelusUnits = "NA",
                     zelusFiles = "NA",
                     popFolder = "NA",
                     popFiles = "NA",
                     popUnits = "NA",
                     biaOutputsFolder = paste(getwd(),"/dataFiles/grids/bia/biaOutputs",sep=""),
                     reReadData = 1,
                     sqliteUSE = F,
                     sqliteDBNamePath = paste(getwd(),"/outputs/Grids/gridMetis.sqlite", sep = ""),
                     regionsSelect = NULL,
                     dataProj = "dataProj.proj",
                     dataProjPath = gcamdatabasePath,
                     scenOrigNames = c("GCAMOrig","GCAMModified"),
                     scenNewNames = c("GCAMOrig","GCAMModified"),
                     gcamdatabasePath = "NA",
                     gcamdatabaseName = "NA",
                     queryxml = "metisQueries.xml",
                     queryPath = gcamdatabasePath,
                     queriesSelect = "All",
                     paramsSelect = c("elecByTech", "elecCapBySubsector"),
                     gridChoice = "grid_050"
){



  # biaInputsFolder="NA"
  # biaInputsFiles="NA"
  # biaScenarioAssign="NA"
  # zelusFolder="NA"
  # zelusScenario="NA"
  # zelusUnits="NA"
  # zelusFiles="NA
  # popFolder="NA"
  # popFiles="NA"
  # popUnits="NA"
  # biaOutputsFolder=paste(getwd(),"/dataFiles/grids/bia/biaOutputs",sep="")
  # reReadData=1
  # gridMetisData=paste(getwd(),"/outputs/Grids/gridMetis.RData", sep = "")
  # sqliteUSE = F
  # sqliteDBNamePath = paste(getwd(),"/outputs/Grids/gridMetis.sqlite", sep = "")
  # regionsSelect = NULL
  # queriesSelect="All"

  #----------------
  # Initialize variables by setting to NULL
  #----------------

  NULL -> lat -> lon -> latitude -> longitude -> aez_id -> region_id ->X..ID->
    ilon->ilat->param->V2->V3->scenario->classPalette->x->value->id->
    biaScenarios->biaYears->zelusScenarios->zelusYears->
    commonYears->commonScenarios->V1->Area_hec->Area_km2->valueBia->valueZelus->commonYears_i->
    scenarioSSP->scenarioPolicy->scenarioGCM->scenarioRCP->
    country->name->country_long_gppd_idnr->fuel1->fuel2->fuel3->fuel4->owner->geolocation_source->
    GCMRCP->capacity_gw->capacity_mw->cf1971to2100->class1->data_source->dataBia->est_installed_capacity->
    estimated_generation_gwh->gcamCapacityFactor->generation_gwh_2013->generation_gwh_2014->
    generation_gwh_2015->generation_gwh_2016->
    owner->region->regionsSelectAll->rowid->country_long->gppd_idnr->regionsSelectAll->
    vintage -> year -> xLabel -> x -> value -> sector -> scenario -> param -> origX -> origValue ->
    origUnits -> origScen -> origQuery -> classPalette2 -> classPalette1 -> classLabel2 -> classLabel1 -> class2 ->
    class1 -> connx -> aggregate -> Units -> sources -> paramx -> technology -> input -> output -> gcamCapacityFactor ->
    gridlat -> gridlon -> gridID -> region_32_code -> ctry_name -> ctry_code -> gridCellPercentage -> aggregate ->
    valueDistrib -> origValueDistrib





  #------------------
  # If reread data
  #------------------

  reReadBiaData<-1
  if(reReadBiaData==1){

    #andym   not sure about whether we should even have this SQLite stuff at all
    if(sqliteUSE==T){
      if(file.exists(sqliteDBNamePath)){file.remove(sqliteDBNamePath)}
      dbConn<-DBI::dbConnect(RSQLite::SQLite(), sqliteDBNamePath)
      DBI::dbDisconnect(dbConn);dbConn
    }

    #library(RMySQL)
    #dbListConnections( dbDriver( drv = "MySQL"))





  #------------------
  # Create folders if needed, read in GCAM data, load grid cell file
  #------------------
  if (!dir.exists(biaOutputsFolder)){
    dir.create(biaOutputsFolder)}

  if (!dir.exists(paste(biaOutputsFolder, "/biadiagnostics",sep=""))){
    dir.create(paste(biaOutputsFolder, "/biadiagnostics",sep=""))}


  readgcamdata<-vector("list", length = 4)

  readgcamdata<-metis.readgcam(gcamdatabasePath = gcamdatabasePath, gcamdatabaseName = gcamdatabaseName,
                               queryxml = queryxml, queryPath = queryPath,
                               scenOrigNames = scenOrigNames, scenNewNames = scenNewNames, reReadData = reReadData,
                               dataProj = gcamdataProjFile, dataProjPath = dataProjPath, dirOutputs = biaOutputsFolder,
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
  if(any(regionsSelect=="All" | regionsSelect=="all" )){regionsSelect<-regionsAll; regionsSelectAll=T}else{
    regionsSelectAll=F
  }

  # Loading a list that gives which of the 32 regions each country is in
  ctor<-data.table::fread(file=paste(biaInputsFolder,"/country_to_region.csv",sep=""), header=T,stringsAsFactors = F)%>%
    tibble::as_tibble()%>%
    dplyr::mutate(country_long=ctry_name)



  listOfGridCells<-data.table::fread(file=paste(getwd(),"/dataFiles/grids/emptyGrids/",gridChoice,".csv",sep=""), header=T,stringsAsFactors = F)%>%
    tibble::as_tibble()%>%
    rename(gridlat = lat,
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

  gridDimlat<-min(abs(latranked[2:length(latranked)]-latranked[1:length(latranked)-1]))
  gridDimlon<-min(abs(lonranked[2:length(lonranked)]-lonranked[1:length(lonranked)-1]))

  gridShiftlat<-latranked[sort.list(abs(latranked))][1]  # The latitude of the center of the grid cells closest to the equator
  gridShiftlon<-lonranked[sort.list(abs(lonranked))][1]  # The longitude of the center of the grid cells closest to prime meridian, Greenwich Meridian


  listOfGridCells$gridlat<-round(listOfGridCells$gridlat, digits = 10)
  listOfGridCells$gridlon<-round(listOfGridCells$gridlon, digits = 10)


  if(!(sum(round(latranked, digits = 4) %in% round(seq(latmin,latmax,length.out = (round((latmax-latmin)/gridDimlat)+1)),digits = 4))==length(latranked))){
    stop(paste("grid file ", getwd(),"/dataFiles/grids/emptyGrids/",gridChoice,".csv"," does not appear to contain the centers of regurlarly-spaced lat lon grid cells.",sep=""))}

#
#   if(gridChoice=="grid_050"){gridDim<-0.5
#   }else{
#     if(gridChoice=="grid_025"){gridDim<-0.25}
#   }




    #----------------
    # Create a distribution grid for each of the 32 regions
    #---------------


    if(!dir.exists(biaInputsFolder)){
      print(paste("bia input folder: ", biaInputsFolder ," is incorrect or doesn't exist.",sep=""))
      print(paste("Skipping electricity generation distribution with bia",sep=""))}else {

        if(sqliteUSE==T){dbConn <- DBI::dbConnect(RSQLite::SQLite(), sqliteDBNamePath)}

        biaScenarios<-character()
        biaYears<-numeric()

        for(biaInputsFile_i in biaInputsFiles){

          if(!grepl(".csv",biaInputsFile_i)){biaInputsFile_i=paste(biaInputsFile_i,".csv",sep="")}

          if(!file.exists(paste(biaInputsFolder,"/",biaInputsFile_i,sep=""))){
            print(paste("bia input file: ", biaInputsFolder,"/",biaInputsFile_i," is incorrect or doesn't exist.",sep=""))
            print(paste("Skipping file: ",biaInputsFolder,"/",biaInputsFile_i,sep=""))
          }else{

            print(paste("Reading bia input file: ",biaInputsFile_i,"...",sep=""))
            gridWRI<-data.table::fread(paste(biaInputsFolder,"/",biaInputsFile_i,sep=""), header=T,stringsAsFactors = F)
            gridWRI[gridWRI=="United States of America"]<-"United States"
            gridWRI<-gridWRI%>%tibble::as_tibble()%>%dplyr::select(-year_of_capacity_data,-commissioning_year,-name,-country,-gppd_idnr,-fuel2,-fuel3,-fuel4,-owner,-source,-url,-geolocation_source)%>%
              dplyr::left_join(ctor,by="country_long")



            biaScenario<-biaScenarioAssign       #andym not sure exactly the purpose of these
            biaGCM = NA;biaRCP = NA


            aggType="vol"


            gridWRI<-gridWRI%>%dplyr::mutate(lat=latitude,
                                             lon=longitude,
                                             scenario=biaScenario,
                                             scenarioGCM=biaGCM,
                                             scenarioRCP=biaRCP,
                                             scenarioSSP=NA,
                                             scenarioPolicy=NA,
                                             param="biaElecGen",
                                             units= "Capacity (GW)",
                                             aggType=aggType,
                                             classPalette="pal_elec_subsec",
                                             class1=fuel1,
                                             value=capacity_mw/1000,
                                             x=NA,
                                             #gridlat = 1/2*round(latitude*2+0.5)-0.25,
                                             #gridlon = 1/2*round(longitude*2+0.5)-0.25
                                             #gridlat = gridDim*round(latitude*(1/gridDim)+0.5)-(1/2*gridDim),
                                             #gridlon = gridDim*round(longitude*(1/gridDim)+0.5)-(1/2*gridDim),
                                             #gridlat = gridDimlat*round(latitude*(1/gridDimlat)-(gridShiftlat/gridDimlat))+gridShiftlat,
                                             #gridlon = gridDimlon*round(longitude*(1/gridDimlon)-(gridShiftlon/gridDimlon))+gridShiftlon)%>%
                                             gridlat = round(gridDimlat*round(latitude*(1/gridDimlat)-(gridShiftlat/gridDimlat))+gridShiftlat, digits = 10),
                                             gridlon = round(gridDimlon*round(longitude*(1/gridDimlon)-(gridShiftlon/gridDimlon))+gridShiftlon, digits = 10))%>%
                                             #gridlat = round(gridDimlat*round(latitude*(1/gridDimlat)-(gridShiftlat/gridDimlat))+gridShiftlat, digits = 3),
                                             #gridlon = round(gridDimlon*round(longitude*(1/gridDimlon)-(gridShiftlon/gridDimlon))+gridShiftlon, digits = 3))%>%
              tibble::as_tibble()%>%
              dplyr::select(-latitude,-longitude,-fuel1,-capacity_mw,-generation_gwh_2013,-generation_gwh_2014,-generation_gwh_2015,-generation_gwh_2016,-estimated_generation_gwh,-country_long)%>%
              dplyr::left_join(listOfGridCells,by = c("gridlat","gridlon"))%>%
              dplyr::group_by(gridlat, gridlon, class1, gridID, ctry_name, ctry_code, region, region_32_code, param, units)%>%    #andym This group_by, and the following summarise get rid of a few columns
              dplyr::summarise(gridCellCapacity = sum(value))%>%
              dplyr::group_by(class1,region,region_32_code)%>%
              dplyr::mutate(regionCapSum = sum(gridCellCapacity),
                            gridCellPercentage = gridCellCapacity/regionCapSum)



            gridWRI[gridWRI=="Coal"]<-"a Coal"
            gridWRI[gridWRI=="Gas"]<-"c Gas"
            gridWRI[gridWRI=="Oil"]<-"e Oil"
            gridWRI[gridWRI=="Biomass"]<-"g Biomass"
            gridWRI[gridWRI=="Nuclear"]<-"i Nuclear"
            gridWRI[gridWRI=="Geothermal"]<-"j Geothermal"
            gridWRI[gridWRI=="Hydro"]<-"k Hydro"
            gridWRI[gridWRI=="Wind"]<-"l Wind"
            gridWRI[gridWRI=="Solar"]<-"m Solar"


###Apr 1: In order to find a specific grid cell within gridWRI I have to do the round (___,digits = 10) - necessary for numbers with repeating digits like 1/3.


            dataBia<-gridWRI%>%dplyr::filter(region %in% regionsSelect)%>%
              dplyr::select(gridlat, gridlon, gridID, class1, region, region_32_code, ctry_name, ctry_code, gridCellPercentage)%>%
              left_join(dataFromGCAM, by = c("class1", "region"))%>%
              dplyr::mutate(valueDistrib = gridCellPercentage*value, origValueDistrib = gridCellPercentage*origValue)









#             print(paste("Data for bia file gathered into columns.", sep=""))
#
#             gridx$x<-as.numeric(gridx$x)
#
#             biaScenarios<-c(biaScenarios,biaScenario)
#             biaYears<-unique(gridx$x)
#
#
#             if(sqliteUSE==T){
#               DBI::dbWriteTable(dbConn, "gridMetis", gridx, append=T)
#               print(paste("Saving data to sqlite as sqlitUSE = ",sqliteUSE,sep=""))
#             }else{
#               print(paste("Using .Rdata format to save data.",sep=""))
#               gridMetis<-dplyr::bind_rows(gridMetis,gridx)
#             }
#
#
#
#             rm(gridx)
#


          } # Close if bia file exists
        } # close bia file loops

        if(sqliteUSE==T){DBI::dbDisconnect(dbConn)}

      } # Close bia folder




    #----------------
    # Prepare Zelus Files
    #---------------

    # if(!dir.exists(zelusFolder)){
    #   print(paste("zelus folder: ", zelusFolder ," is incorrect or doesn't exist.",sep=""))
    #   print(paste("Skipping zelus runs",sep=""))}else {
    #
    #     if(sqliteUSE==T){dbConn <- DBI::dbConnect(RSQLite::SQLite(), sqliteDBNamePath)}
    #
    #     zelusScenarios<-character()
    #     zelusYears<-numeric()
    #
    #     for(zelusFile_i in zelusFiles){
    #
    #       class_i=gsub(".csv","",zelusFile_i)
    #       if(!grepl(".csv",zelusFile_i)){zelusFile_i=paste(zelusFile_i,".csv",sep="")}
    #
    #       if(!file.exists(paste(zelusFolder,"/",zelusFile_i,sep=""))){
    #         print(paste("zelus file: ", zelusFolder,"/",zelusFile_i," is incorrect or doesn't exist.",sep=""))
    #         print(paste("Skipping file: ",zelusFolder,"/",zelusFile_i,sep=""))
    #       }else{
    #         print(paste("Reading zelus data file: ",zelusFile_i,"...",sep=""))
    #         gridx<-data.table::fread(paste(zelusFolder,"/",zelusFile_i,sep=""),fill=T)%>%
    #           tibble::as_tibble()%>%dplyr::select(-'# ID',-ilon,-ilat)                      andym this is where I stopped replacing 'tethys' with 'zelus'
    #         print("File read.")
    #         names(gridx)<-gsub("X","",names(gridx))
    #         if(grepl("mm",tethysUnits)){aggType="depth"}else{aggType="vol"}
    #         gridx<-gridx%>%dplyr::select(-dplyr::contains("Unit"))
    #         gridx<-gridx%>%
    #           dplyr::mutate(lat=lat,lon=lon,
    #                         scenarioGCM=NA,
    #                         scenarioRCP=NA,
    #                         scenarioSSP=NA,
    #                         scenarioPolicy=NA,
    #                         scenario=tethysScenario,
    #                         param="tethysWatWithdraw",
    #                         units=tethysUnits,
    #                         aggType=aggType,
    #                         classPalette="pal_wet",
    #                         class=class_i)%>%
    #           tidyr::gather(key="x",value="value",-c("lat","lon","scenario","scenarioPolicy","scenarioGCM","scenarioRCP","scenarioSSP","aggType","param","units","classPalette","class"))
    #
    #         gridx$x<-as.numeric(gridx$x)
    #
    #         gridx<-gridx%>%
    #           dplyr::mutate(class=dplyr::case_when(grepl("wddom",class)~"Domestic",
    #                                                grepl("elec",class)~"Electric",
    #                                                grepl("irr",class)~"Irrigation",
    #                                                grepl("liv",class)~"Livestock",
    #                                                grepl("mfg",class)~"Manufacturing",
    #                                                grepl("min",class)~"Mining",
    #                                                grepl("nonag",class)~"Non Agriculture",
    #                                                grepl("total",class)~"Total",
    #                                                TRUE~class))
    #
    #         tethysScenarios<-c(tethysScenarios,tethysScenario)
    #         tethysYears<-unique(gridx$x)
    #
    #         if(sqliteUSE==T){
    #           DBI::dbWriteTable(dbConn, "gridMetis", gridx, append=T)
    #           print(paste("Saving data to sqlite as sqlitUSE = ",sqliteUSE,sep=""))
    #         }else{
    #           print(paste("Using .Rdata format to save data.",sep=""))
    #           gridMetis<-dplyr::bind_rows(gridMetis,gridx)
    #         }
    #
    #         rm(gridx)
    #
    #       } # Close if tethys file exists
    #     } # close tethys file loops
    #
    #     if(sqliteUSE==T){DBI::dbDisconnect(dbConn)}
    #
    #   } # Close tethys folder

    #









    #----------------
    # Function for comparing electricity generation data
    #---------------

     if(F){

       ## andym need to fix the readgcam

       regionsSelectCompareCap<-unique(gridWRI$region)

       biaInputsFile_i<-biaInputsFiles[1]

       ctr<-data.table::fread(file = paste(biaInputsFolder,"/country_to_region.csv",sep=""), header=T,stringsAsFactors = F)%>%
         tibble::as_tibble()%>%
         dplyr::mutate(country_long=ctry_name)

       gWRI<-data.table::fread(file = paste(biaInputsFolder,"/",biaInputsFile_i, '.csv',sep=""), header=T)

       gWRI[gWRI=="United States of America"]<-"United States"

       gWRI<-gWRI%>%tibble::as_tibble()%>%dplyr::select(-name,-country,-gppd_idnr,-fuel2,-fuel3,-fuel4,-owner,-source,-url,-geolocation_source)%>%
         dplyr::left_join(ctr,by="country_long")



       biaScenario<-biaScenarioAssign
       biaGCM=NA;biaRCP=NA

       aggType="vol"

       gWRI<-gWRI%>%dplyr::mutate(lat=latitude,
                                  lon=longitude,
                                  scenario=biaScenario,
                                  scenarioGCM=biaGCM,
                                  scenarioRCP=biaRCP,
                                  scenarioSSP=NA,
                                  scenarioPolicy=NA,
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



       #andym in the future can figure out which countries are grouped into the non-nation regions

       gWRI <- gWRI%>%
         dplyr::group_by(region, class1)%>%
         dplyr::summarise(WRI_total_capacity=sum(value))%>%
         dplyr::filter(region %in% regionsSelectCompareCap)
       gWRI[gWRI=="Coal"]<-"a Coal"
       gWRI[gWRI=="Gas"]<-"c Gas"
       gWRI[gWRI=="Oil"]<-"e Oil"
       gWRI[gWRI=="Biomass"]<-"g Biomass"
       gWRI[gWRI=="Nuclear"]<-"i Nuclear"
       gWRI[gWRI=="Geothermal"]<-"j Geothermal"
       gWRI[gWRI=="Hydro"]<-"k Hydro"
       gWRI[gWRI=="Wind"]<-"l Wind"
       gWRI[gWRI=="Solar"]<-"m Solar"


       # andym Should I do scenOrigNames = scenOrigNames[1] in the readgcam function below, since what if there are multiple different scenarios that show non-reference installed capacity predictions
       # andym Maybe we should just set this up, so that it points to a place where the reference GCAM scenario will always be located


       readAllGCAMcapDataList<-metis.readgcam(gcamdatabasePath = gcamdatabasePath, gcamdatabaseName = gcamdatabaseName,
                                    queryxml = queryxml, queryPath = queryPath,
                                    scenOrigNames = scenOrigNames, scenNewNames = scenNewNames, reReadData = reReadData,
                                    dataProj = gcamdataProjFile, dataProjPath = dataProjPath, dirOutputs = biaOutputsFolder,
                                    regionsSelect = "All", queriesSelect = queriesSelect , paramsSelect = c("elecByTech", "elecCapBySubsector"))

       readAllGCAMcapData<-readAllGCAMcapDataList$data%>%
         dplyr::filter(param=="elecCapBySubsector")



       gGCAMelecCap<-readAllGCAMcapData%>%dplyr::filter(x==2015)%>%
         dplyr::mutate(GCAM_total_capacity=value)%>%
         dplyr::select(-c(value))


       gCapComparison<-dplyr::full_join(gGCAMelecCap,gWRI, by = c("region", "class1"))%>%
         tidyr::gather(key="data_source",value="est_installed_capacity",-c("region","class1","aggregate","units","vintage","x","xLabel","class2","sources","param","scenario","origValue","origX","origUnits","origQuery","origScen","classPalette1","classLabel1","classPalette2","classLabel2"))


       #gGSlim<-gGSlim%>%dplyr::mutate(GCAMestCapVals=Elec_Gen_GCAM_2015/gcamCapFactrAv*(10^12)/(365*24*3600))



       # gCapComparisonARG<-gCapComparison%>%dplyr::filter(region %in% c("Argentina"))%>%
       #   dplyr::select(c("region","est_installed_capacity","data_source","class1","origScen"))
       #
       #
       # gCapComparisonCol<-gCapComparison%>%dplyr::filter(region %in% c("Colombia"))%>%
       #   dplyr::select(c("region","est_installed_capacity","data_source","class1","origScen"))


       # chrt3<-ggplot(data = gCapComparisonARG, aes(fill = data_source, x = class1, y = est_installed_capacity))+geom_bar(position = "dodge", stat="identity")
       # chrt3
       #
       # chrt4<-ggplot(data = gCapComparisonCol, aes(fill = data_source, x = class1, y = est_installed_capacity))+geom_bar(position = "dodge", stat="identity")
       # chrt4

       #andym for the next part, if it is important, I can nest this within another for loop, which does through the different scenarios
       #andym put some line so that it doesn't re-make graphs that it already made

       #andym so it doesn't look like China got doubled



       for(regioni in regionsSelectCompareCap){
         gridR<-gCapComparison%>%dplyr::filter(region==regioni)
         fname=paste(unique(gridR$region),"_est_installed_capacity")
         metis.printPdfPng(figure=ggplot(data = gridR, aes(fill = data_source, x = class1, y = est_installed_capacity))+geom_bar(position = "dodge", stat="identity"),
                           dir=paste(biaOutputsFolder, "/biadiagnostics",sep=""),filename=fname,figWidth=9,figHeight=7,pdfpng="png")

       }     #close metis.printPdfPng
     } # Close if FALSE




    #----------------
    # Prepare gridded Population
    #---------------

    # if(!dir.exists(popFolder)){
    #
    #   print(paste("pop folder: ", popFolder ," is incorrect or doesn't exist.",sep=""))
    #   print(paste("Skipping pop runs",sep=""))}else {
    #
    #     if(sqliteUSE==T){dbConn <- DBI::dbConnect(RSQLite::SQLite(), sqliteDBNamePath)}
    #
    #     for(popFile_i in popFiles){
    #
    #       popFile_i=gsub(".csv","",popFile_i)
    #       if(!grepl(".csv",popFile_i)){popFile_i=paste(popFile_i,".csv",sep="")}
    #
    #       if(!file.exists(paste(popFolder,"/",popFile_i,sep=""))){
    #         print(paste("pop file: ", popFolder,"/",popFile_i," is incorrect or doesn't exist.",sep=""))
    #         print(paste("Skipping file: ",popFolder,"/",popFile_i,sep=""))
    #       }else{
    #         print(paste("Reading population data file: ",popFile_i,"...",sep=""))
    #
    #         gridx<-data.table::fread(paste(popFolder,"/",popFile_i,sep=""))%>%
    #           tibble::as_tibble()%>%dplyr::select(lon,lat,dplyr::contains("popGWP"))%>%
    #           tidyr::gather(key="key",value="value",-c("lat","lon"))%>%
    #           tidyr::separate(col="key",into=c("scenario","x"),sep="_")%>%
    #           dplyr::mutate(param="population",
    #                         units=popUnits,
    #                         aggType="vol",
    #                         classPalette="pal_hot",
    #                         class="class")
    #         gridx$x<-as.numeric(gridx$x)
    #
    #         print("File read.")
    #
    #         if(sqliteUSE==T){
    #           DBI::dbWriteTable(dbConn, "gridMetis", gridx, append=T)
    #           print(paste("Saving data to sqlite as sqlitUSE = ",sqliteUSE,sep=""))
    #         }else{
    #           print(paste("Using .Rdata format to save data.",sep=""))
    #           gridMetis<-dplyr::bind_rows(gridMetis,gridx)
    #         }
    #
    #         rm(gridx)
    #
    #       } # Close if pop file exists
    #     } # close pop file loops
    #
    #     if(sqliteUSE==T){DBI::dbDisconnect(dbConn)}
    #
    #   } # Close pop folder



    #----------------
    # Prepare gridded Electricity Demands
    #---------------
    # WRI database + GCAM Elec demands
    # Distribute electric demands by population percentage


    #----------------
    # Prepare gridded Power Generation (Supply)
    #---------------
    # WRI database + GCAM Power Generated
    # Calculate total power by fuel by region
    # calculate relative prcnt by fuel by region
    # Distirbute capacity by prnct distirbution

    #----------------
    # Prepare gridded Power Capacity (Supply)
    #---------------
    # WRI database power capacity + GCAM Cum Power Capacity


    #----------------------

    # Test Unique Values
    #a<-gridMetis%>%tidyr::unite(col="key",names(gridMetis)[!names(gridMetis) %in% c("lat","lon","value")],sep="_",remove=T)
    #a<-a%>%tidyr::spread(key=key,value=value)

    #--------------
    # Save RData and csv.
    #----------------

    # if(sqliteUSE==F){
    #   if(nrow(gridMetis)>0){
    #     save(gridMetis,file=gridMetisData)}
    #   #data.table::fwrite(gridMetis,file = paste(dirOutputs, "/Grids/gridMetis.csv", sep = ""),row.names = F)
    #   print(paste("gridMetis params: ", paste(unique(gridMetis$param),collapse=", "),sep=""))
    #   #print(paste("gridMetis.csv saved in: ", paste(dirOutputs, "/Grids/gridMetis.csv", sep = ""),sep=""))
    # }else{
    #   if(file.exists(sqliteDBNamePath)){paste("Gridded data saved in SQLite database : ",sqliteDBNamePath, sep="")}else{
    #     print("No data added to gridMetis. Check datafiles folders to see if data is available.")}
    # }


  }else{ # Close if reReadBiaData==1

    if(sqliteUSE==T){
      # if(file.exists(sqliteDBNamePath)){paste("Re-read set to 0. Use data saved in SQLite database : ",sqliteDBNamePath, sep="")}}else{
      #   if(!file.exists(gridMetisData)){stop(paste("File gridMetisData not found: ",gridMetisData,sep=""))}else{
      #     load(gridMetisData)
      #     paste("Re-read set to 0. Usig saved data from .R data : ",gridMetisData, sep="")}
      }}


  print("About to return data for distributed electricity generation data")

  return(dataBia)


} # Close Function
