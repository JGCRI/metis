#' metis.grid2poly
#'
#' This function takes a .csv file with gridded lat, long data and aggregates
#' the data by spatial boundaries given different shapefiles.
#' @return A table with data by polygon ID for each shapefile provided
#' @keywords gcam, gcam database, query
#' @param gridFiles  Default=NULL. Grid file in .csv format or a R table, data frame or tibble with as a minimum columns with "lat","lon" and "value",
#' @param subRegShape  Default=NULL. shapefile over which grid data is to be aggregated.
#' @param subRegShpFolder  Default=NULL. Folder containing boundary region shapefile. Suggested paste(getwd(),"/dataFiles/gis/naturalEarth",sep  Default=""),
#' @param subRegShpFile  Default=NULL. Name of sub-region shapefile. Suggested paste("ne_10m_admin_1_states_provinces",sep  Default=""),
#' @param subRegCol  Default= NULL. Suggested for states "name",
#' @param subRegType  Default="subRegType". Eg. "states", "basins" etc.
#' @param aggType  Default=NULL. Aggregation method to be used. Either "vol" or "depth" dependening on the type of data provided.
#' @param dirOutputs  Default=paste(getwd(),"/outputs",sep  Default=""),
#' @param folderName Default = NULL
#' @param regionName Default = "region"
#' @param nameAppend  Default="",
#' @param labelsSize Default =1.2. Label size for the region names for the gridoverlay plot.
#' @param paramsSelect Default ="All"
#' @param scenariosSelect Default ="All"
#' @param paramScenariosFixed Default=NULL
#' @param tethysFilesScarcity Default =NULL,
#' @param xanthosFilesScarcity Default =NULL
#' @param calculatePolyScarcity Default = F
#' @param calculatePolyScarcityOnly Default = F
#' @param saveFiles Default =T
#' @param printGridOverlay Default = F
#' @export

metis.grid2poly<- function(gridFiles=NULL,
                           regionName ="region",
                           subRegShape=NULL,
                           subRegShpFolder=NULL,
                           subRegShpFile=NULL,
                           subRegCol=NULL,
                           subRegType="subRegType",
                           aggType=NULL,
                           dirOutputs=paste(getwd(),"/outputs",sep=""),
                           folderName = NULL,
                           nameAppend="",
                           labelsSize=1.2,
                           paramsSelect="All",
                           scenariosSelect="All",
                           paramScenariosFixed=NULL,
                           tethysFilesScarcity=NULL,
                           xanthosFilesScarcity=NULL,
                           calculatePolyScarcity=F,
                           calculatePolyScarcityOnly=F,
                           printGridOverlay =F,
                           saveFiles=T) {

  # grid=NULL
  # regionName ="region"
  # subRegShape=NULL
  # subRegShpFolder=NULL
  # subRegShpFile=NULL
  # subRegCol=NULL
  # subRegType="subRegType"
  # aggType=NULL
  # dirOutputs=paste(getwd(),"/outputs",sep="")
  # folderName = NULL
  # nameAppend=""
  # labelsSize=1.2
  # paramsSelect="All"
  # scenariosSelect="All"
  # paramScenariosFixed=NULL
  # tethysFilesScarcity=NULL
  # xanthosFilesScarcity=NULL
  # calculatePolyScarcity=F

  #----------------
  # Initialize variables by setting to NULL
  #----------------

  NULL->subRegAreaSum->areaPrcnt->weight->ID->subRegion->region->scenario->
    param->shpRegCol->subReg->griddataTables->tbl->key->value->.->classPalette->lat->lon->overlapShape->
    gridPolyLoop->dbHead->paramsSub->sqlGrid->gridMetis -> template_subRegional_mapping -> scenarioGCM ->
    scenarioRCP -> class2 -> scenarioPolicy -> valueTethys -> valueXanthos -> scenarioSSP -> gridCellArea ->
    gridCellAreaRatio -> area -> areaPrcnt -> scenarioMultiA -> scenarioMultiB->nrowOrig->nrowNew

  #------------------
  # Function for adding any missing columns if needed
  # -----------------

  addMissing<-function(data){
    if(!"scenario"%in%names(data)){data<-data%>%dplyr::mutate(scenario="scenario")}
    if(!"param"%in%names(data)){data<-data%>%dplyr::mutate(param="param")}
    if(!"x"%in%names(data)){data<-data%>%dplyr::mutate(x="x")}
    return(data)
  }

  #----------------
  # Check input data format
  #---------------


  #----------------
  # Load Shapefile and save boundary maps
  #---------------

  if(is.null(subRegShape)){
    if(!is.null(subRegShpFolder) & !is.null(subRegShpFile)){
      if(!dir.exists(subRegShpFolder)){
        stop("Shapefile folder: ", subRegShpFolder ," is incorrect or doesn't exist.",sep="")}
      if(!file.exists(paste(subRegShpFolder,"/",subRegShpFile,".shp",sep=""))){
        stop("Shape file: ", paste(subRegShpFolder,"/",subRegShpFile,".shp",sep="")," is incorrect or doesn't exist.",sep="")}
      shape=rgdal::readOGR(dsn=subRegShpFolder,layer=subRegShpFile,use_iconv=T,encoding='UTF-8')
      print(paste("Sub Reg Shape : ",subRegShpFolder,"/",subRegShpFile,".shp",sep=""))
      print(raster::head(shape))
    }else {
      stop("No valid boundary or subregional shape file available")
    }# if(!is.null(subRegShpFolder) & !is.null(subRegShpFile)){
  }else{shape=subRegShape}


   #----------------
  # Create Folders
  #---------------

  if (!dir.exists(dirOutputs)){dir.create(dirOutputs)}
  if (!dir.exists(paste(dirOutputs,"/",folderName, sep = ""))){dir.create(paste(dirOutputs, "/",folderName,sep = ""))}
  if (!dir.exists(paste(dirOutputs,"/",folderName, "/Grid2Poly/", sep = ""))){dir.create(paste(dirOutputs, "/",folderName, "/Grid2Poly/", sep = ""))}
  dirX=paste(dirOutputs,"/",folderName, "/Grid2Poly/",sep = "")

  if(!calculatePolyScarcityOnly){

  #----------------
  # Cropped and Process Grid to Polygons
  #---------------

 poly<-tibble::tibble()
 gridCropped <-tibble::tibble()
 template_subRegional_mapping <- tibble::tibble()
 count=0;
 gridCount=0;

for(grid_i in gridFiles){
  if(gridCount==0){ # In case a R tbl is provided directly

  if(any(class(gridFiles)=="character")){
     print(paste("Reading grid file: ", grid_i,sep=""))
      if(grepl(".csv",grid_i)){grid<-data.table::fread(grid_i,encoding="Latin-1")}
      if(grepl(".rds",grid_i)){grid<-readRDS(grid_i)}}else{
        if(any(class(gridFiles) %in% c("tbl_df","tbl","data.frame"))){
         grid = gridFiles
         gridCount=1;
        }
      }

    if(nrow(grid)>0){

      grid<-grid%>%addMissing()%>%tibble::as_tibble()%>%unique(); grid

      paramScenarios <- grid%>%dplyr::select(param,scenario)%>%unique(); paramScenarios

      if(!is.null(paramScenariosFixed)){
        for(row_i in 1:nrow(paramScenariosFixed)){
          paramScenarios <- paramScenarios%>%
            dplyr::filter(param %in% unique(paramScenarios$param)[unique(paramScenarios$param) %in% unique(paramScenariosFixed[row_i,]$param)],
                          scenario %in% unique(paramScenarios$scenario)[unique(paramScenarios$scenario) %in% unique(paramScenariosFixed[row_i,]$scenario)])

        }
        }

      print("paramScenarios found: ")
      print(paramScenarios)
      scenarios<-unique(paramScenarios$scenario)
      params <-unique(paramScenarios$param)

      print("Subsetting params and scenarios...")
      if(!any(grepl("all",paramsSelect,ignore.case = T))){params=params[params %in% paramsSelect]}
      if(!any(grepl("all",scenariosSelect,ignore.case = T))){scenarios=scenarios[scenarios %in% scenariosSelect]}


      # Check Scenarios
      if(!is.null(paramScenarios)){
        if(nrow(paramScenarios)>0){
          if(any(is.na(unique(paramScenarios$scenario)))){
            print("Removing NA scenarios. Remaining Scenarios:")
            paramScenarios <- paramScenarios %>% dplyr::filter(!is.na(scenario))
            print(paramScenarios)
          }
        }}

    }else{
      stop(paste("Grid file ",grid," does not exist",sep=""))
    }

  count=count+1

  if(!is.null(grid)){

    if(!is.null(paramScenarios)){
      if(all(c("param","scenario") %in% names(grid))){
        paramScenarios <- tibble::tibble()
        for(param_i in unique(grid$param)){
          paramScenarios <- paramScenarios %>%
            dplyr::bind_rows(grid %>% dplyr::filter(param==param_i)%>%
                               dplyr::select(param,scenario)%>%
                               unique())
        }
      }
    }else{paramScenarios <- tibble::tibble(param="param", scenario="scenario")}; paramScenarios


    for(row_i in 1:nrow(paramScenarios)){

      param_i <- paramScenarios[row_i,]$param; param_i
      scenario_i <- paramScenarios[row_i,]$scenario; scenario_i
      gridx<-grid%>%dplyr::filter(param==param_i,scenario==scenario_i);

      if(nrow(gridx)>0){

        gridx <- gridx%>%dplyr::filter(!is.na(x))

        print(paste("Starting aggregation for grid: ", grid_i," and param: ",param_i," and scenario: ",scenario_i,"...",sep=""))

        # Subset to keep only required columns
        cols2Remove <-names(gridx)[names(gridx) %in% c("subRegion",subRegCol,"gridCellArea","subRegAreaSum","gridCellAreaRatio")]
        gridx <- gridx %>% dplyr::select(-cols2Remove)

        if(!"aggType" %in% names(gridx)){
          if(is.null(aggType)){
            print("Column aggType is missing from grid data. Assigning aggType='vol'")
            gridx<-gridx%>%dplyr::mutate(aggType="vol")}else{
              gridx<-gridx%>%dplyr::mutate(aggType=aggType)
            }}

        namesGrid<-names(gridx)


        # Temporary column names merge in order to aggregate params scenarios across sub-regions
        print("setting grid columns ...")
        for(colx in names(gridx)){
          if(is.character(gridx[[colx]])){

            gridx <- gridx %>% dplyr::mutate(
              !!colx := gsub(" ","XSPACEX",!!as.name(colx),perl = TRUE ),
              !!colx:= gsub("\\.","XPERIODX",!!as.name(colx),perl = TRUE ),
              !!colx:= gsub("\\-","XDASHX",!!as.name(colx),perl = TRUE ),
              !!colx:= gsub("\\(","XLPARENTHX",!!as.name(colx),perl = TRUE ),
              !!colx:= gsub("\\)","XRLPARENTHX",!!as.name(colx),perl = TRUE ),
              !!colx:= gsub("\\_","XUNDERX",!!as.name(colx),perl = TRUE ))

          }
        }
        print("Grid Columns set.")



        for (aggType_i in unique(gridx$aggType)){

          if(!unique(gridx$aggType) %in% c("depth","vol")){stop("Incorrect aggType in grid file")}

          print("Uniting columns...")
          gridx<-gridx%>%dplyr::filter(aggType==aggType_i)%>%
            tidyr::unite(col="key",names(gridx)[!names(gridx) %in% c("lat","lon","value")],sep="_",remove=T)
          nrowOrig = nrow(gridx)
          if(aggType_i=="depth"){gridx <- gridx %>% dplyr::group_by(lat,lon,key)%>%dplyr::summarize(value=mean(value))%>%dplyr::ungroup()}
          if(aggType_i=="vol"){gridx <- gridx %>% dplyr::group_by(lat,lon,key)%>%dplyr::summarize(value=sum(value))%>%dplyr::ungroup()}
          nrowNew =nrow(gridx)
          if(nrowOrig!=nrowNew){print("WARNING: Multiple lat/lon data had same attributes and were combined.")}
          print("Columns united.")

          gridx<-gridx%>%unique()%>%tidyr::spread(key=key,value=value)

          print(paste("Cropping grid to shape file for parameter ", param_i," and scenario: ",scenario_i,"...",sep=""))

         gridCropped<-tibble::as_tibble(metis.gridByPoly(gridDataTables=gridx%>%dplyr::ungroup(),
                                                                          shape=shape,colName=subRegCol))
         # gridDataTables=gridx%>%dplyr::ungroup();shape=shape;colName=subRegCol

         print(paste("Grid cropped.",sep=""))

          if(nrow(gridCropped)>0){

            gridCroppedX<-tidyr::gather(gridCropped,key=key,value=value,-c(subRegCol,gridCellArea,subRegAreaSum,gridCellAreaRatio,lat,lon))%>%
              tidyr::separate(col="key",into=namesGrid[!namesGrid %in% c("lat","lon","value")],sep="_")%>%
              unique()%>%dplyr::ungroup()%>%
              dplyr::rename(subRegion=subRegCol)%>%
              dplyr::filter(!is.na(value))

            for(colx in names(gridCroppedX)){
              if(is.character(gridCroppedX[[colx]])){

                gridCroppedX <- gridCroppedX %>% dplyr::mutate(
                  !!colx := gsub("XSPACEX","",!!as.name(colx),perl = TRUE ),
                  !!colx:= gsub("XPERIODX","\\.",!!as.name(colx),perl = TRUE ),
                  !!colx:= gsub("XDASHX","\\-",!!as.name(colx),perl = TRUE ),
                  !!colx:= gsub("XLPARENTHX","\\(",!!as.name(colx),perl = TRUE ),
                  !!colx:= gsub("XRLPARENTHX","\\)",!!as.name(colx),perl = TRUE ),
                  !!colx:= gsub("XUNDERX","\\_",!!as.name(colx),perl = TRUE ))
              }
            }

            polyType=subRegType
            grid_fname<-paste(dirX, "/gridCropped_",scenario_i,"_",polyType,"_",param_i,nameAppend,".csv", sep = "")
            if(saveFiles){
            data.table::fwrite(gridCroppedX%>%dplyr::mutate(polyType=polyType, region=regionName),
                               file = grid_fname,row.names = F, append = T)
            print(paste("Subregional grid data files written to: ",grid_fname, sep = ""))}

          } # If nrow(gridCropped)

          if(is.null(gridPolyLoop)){

            if(printGridOverlay){
            if(!file.exists(paste(dirX,"/subBasin_map_GridSize.png",sep=""))){
            print("Printing Grid overlay...")
            spdf = sp::SpatialPointsDataFrame(sp::SpatialPoints(coords=(cbind(gridx$lon,gridx$lat))),data=gridx)
            sp::gridded(spdf)<-TRUE
            r<-raster::stack(spdf)
            raster::projection(r)<-sp::proj4string(shape)
            shape_ras <- raster::rasterize(shape, r[[1]], getCover=TRUE)
            shape_ras[shape_ras==0] <- NA
            r<-raster::mask(r,shape_ras)
            rCrop <- r
            rCropP<-raster::rasterToPolygons(rCrop)
            sp::proj4string(rCropP)<-sp::proj4string(shape)
            rcropPx<-raster::intersect(shape,rCropP)

            metis.map(labelsSize=labelsSize, dataPolygon=rcropPx,fileName = paste(subRegType,"_map_GridSize_Labels",nameAppend,sep=""),
                      dirOutputs = dirX,
                      overLayer = metis.map(labelsSize=labelsSize, dataPolygon=shape,fillColumn = subRegCol,
                                            fillPalette = "white",alpha=0,facetsOn=F,
                                            labels=T,printFig=F),facetsOn=F)

            print("Printing Grid overlay with Labels...")
            metis.map(labelsSize=labelsSize, dataPolygon=rcropPx,fileName = paste(subRegType,"_map_GridSize",nameAppend,sep=""),
                      dirOutputs = dirX,
                      overLayer = metis.map(labelsSize=labelsSize, dataPolygon=shape,fillColumn = subRegCol,
                                            fillPalette = "white",alpha=0,facetsOn=F,
                                            labels=F,printFig = F),facetsOn=F)
            }
            } # Close if printGridOverlay
          }
          gridPolyLoop=1; # To prevent gridded map being produced multiple times


          if(aggType_i=="depth"){
            print(paste("Aggregating depth for parameter ", param_i," and scenario: ",scenario_i,"...",sep=""))
            x<-data.frame(mapply(`*`,gridCropped%>%
                                   dplyr::select(names(gridCropped)[!names(gridCropped) %in% c(
                                     names(shape),"lat","lon","gridCellArea","subRegAreaSum","gridCellAreaRatio")]),
                                  gridCropped%>%dplyr::select(gridCellAreaRatio),SIMPLIFY=FALSE))%>%
              dplyr::bind_cols(gridCropped%>%dplyr::select( subRegCol))%>%tibble::as_tibble();

           polyDatax<-x%>%dplyr::group_by(.dots = list(subRegCol))%>% dplyr::summarise_all(list(~sum(.,na.rm=T)))%>%dplyr::ungroup()
            print("Aggregation complete.")
          }

          if(aggType_i=="vol"){
            print(paste("Aggregating volume for parameter ", param_i," and scenario: ",scenario_i,"...",sep=""))
            # Convert to Spatial Point Data Frames
            spdf = sp::SpatialPointsDataFrame(sp::SpatialPoints(coords=(cbind(gridx$lon,gridx$lat))),data=gridx)
            sp::gridded(spdf)<-TRUE
            rGrid<-raster::stack(spdf)
            raster::projection(rGrid)<-sp::proj4string(shape)

            w <- raster::extract(rGrid,shape, method="simple",weights=T, normalizeWeights=F);
            dfx<-data.frame()

            for (i in seq(w)){
              if(!is.null(w[[i]])) {
              x<-as.data.frame(w[[i]]) %>% dplyr::mutate(weight=weight*1)
              x$ID<-shape@data[[ subRegCol]][[i]]


              x1<-data.frame(mapply(`*`,x%>%
                                      dplyr::select(names(rGrid)[!names(rGrid) %in% c("lat","lon")]),x%>%
                                      dplyr::select(weight),SIMPLIFY=FALSE))%>%
                dplyr::bind_cols(x%>%dplyr::select(ID));
              #assign(paste0("df", i), x)
              dfx<-rbind.data.frame(dfx,x1)
              }
            }
            names(dfx)[names(dfx)=="ID"]<- subRegCol;
            polyDatax<-dfx%>%dplyr::group_by(.dots = list( subRegCol))%>% dplyr::summarise_all(list(~sum(.,na.rm=T)))%>%
              tibble::as_tibble()%>%dplyr::ungroup()
            print("Aggregation complete.")
          }

          polyData<-tidyr::gather(polyDatax,key=key,value=value,-(subRegCol))%>%
            tidyr::separate(col="key",into=namesGrid[!namesGrid %in% c("lat","lon","value")],sep="_")%>%dplyr::ungroup()

          for(colx in names(polyData)){
            if(is.character(polyData[[colx]])){

              polyData <- polyData %>% dplyr::mutate(
                !!colx := gsub("XSPACEX","",!!as.name(colx),perl = TRUE ),
                !!colx:= gsub("XPERIODX","\\.",!!as.name(colx),perl = TRUE ),
                !!colx:= gsub("XDASHX","\\-",!!as.name(colx),perl = TRUE ),
                !!colx:= gsub("XLPARENTHX","\\(",!!as.name(colx),perl = TRUE ),
                !!colx:= gsub("XRLPARENTHX","\\)",!!as.name(colx),perl = TRUE ),
                !!colx:= gsub("XUNDERX","\\_",!!as.name(colx),perl = TRUE ))

            }
          }

          polyData<-polyData%>%
            dplyr::mutate(subRegType=subRegType)%>%
            dplyr::rename(subRegion:= !!paste(subRegCol))


          if("x" %in% names(polyData)){
            polyData <- polyData%>%
            dplyr::filter(!is.na(x))}

          poly<-polyData%>%dplyr::ungroup()%>%
            dplyr::mutate(subRegion = as.character(subRegion))

          if("scenarioMultiA" %in% names(poly)){
            poly <- poly %>%
            dplyr::mutate(scenarioMultiA = as.character(scenarioMultiA))}
          if("scenarioMultiB" %in% names(poly)){
            poly <- poly %>%
              dplyr::mutate(scenarioMultiB = as.character(scenarioMultiB))}

          polyType=subRegType
          poly_fname<-paste(dirX, "/poly_",scenario_i,"_",polyType,"_",param_i,nameAppend,".csv", sep = "")
          if(saveFiles){
            data.table::fwrite(poly,
                             file = poly_fname,row.names = F, append=T)
          print(paste("Subregional polygon data files written to: ",poly_fname, sep = ""))}

          template_subRegional_mapping <- template_subRegional_mapping %>%
            dplyr::bind_rows(poly %>%
            dplyr::select(c("param","units","class","classPalette")[c("param","units","class","classPalette") %in% names(poly)])%>%
              dplyr::ungroup()%>%unique())%>%unique()
          poly_fname<-paste(dirX, "/poly_subregionalTemplate.csv", sep = "")
          if(saveFiles){
            data.table::fwrite(template_subRegional_mapping,
                             file = poly_fname,row.names = F, append=T)
          print(paste("Subregional polygon template files written to: ",poly_fname, sep = ""))}
        } # Close loop for aggType
      }

      NULL -> gridx -> spdf -> r -> rcrop -> rcropP -> rcropPx -> w -> gridCroppedX->
        x1  -> polyData -> polyx -> dfx
      rm(gridx,spdf,r,rcrop,rcropP,rcropPx,w,gridCroppedX, x1, polyData, polyx, dfx); gc()

      } # Close loop for param_i and scenario_i

    print(paste("Aggregation for all scenarios and params complete."))



  }else{print("No grid provided.")}
}}

  } # Close if(!calculatePolyScarcityOnly)

#----------------
# Calculating polygon scarcity
#---------------

 if(calculatePolyScarcity==T){

   xanthosData<-tibble::tibble()
 # List files in dirX
 if(is.null(tethysFilesScarcity)){tethysFilesx<-list.files(dirX)[grepl("poly_",list.files(dirX)) &
                                                                  grepl("tethys",list.files(dirX)) &
                                                                  grepl("total",list.files(dirX))]}else{
   tethysFilesx<-tethysFilesScarcity}; tethysFilesx
 if(is.null(xanthosFilesScarcity)){xanthosFilesx<-list.files(dirX)[grepl("poly_",list.files(dirX)) &
                                                                    grepl("xanthos",list.files(dirX))]}else{
   xanthosFilesx<-xanthosFilesScarcity}; xanthosFilesx


 print(paste("Tethys files include: ",paste(tethysFilesx,collapse=", "),sep=""))
 print(paste("Xanthos files include: ",paste(xanthosFilesx,collapse=", "),sep=""))
 print(paste("Total combinations are: ", length(tethysFilesx)*length(xanthosFilesx)))


 xanthosTemp <- tibble::tibble()
 for(xanthosFile_i in xanthosFilesx){
   x <- data.table::fread(paste(dirX,"/",xanthosFile_i,sep="")) %>% dplyr::filter(grepl("xanthos",param));
   xanthosTemp <- xanthosTemp %>%
     dplyr::bind_rows(x%>%dplyr::mutate(subRegion=as.character(subRegion)))
 }

 # Create Mean historical xanthos
 colsX<-names(xanthosTemp)[!names(xanthosTemp) %in% c("x","value")]; colsX
 xanthosHist <- xanthosTemp %>%
   dplyr::filter(scenario==unique(xanthosTemp$scenario)[1],x < 2010) %>%
   dplyr::group_by_at(vars(dplyr::one_of(colsX))) %>%
   dplyr::select(-x) %>%
   dplyr::summarize(value=mean(value))%>%
   dplyr::ungroup()%>%
   dplyr::mutate(scenario=paste("xanthosHist",min(unique(xanthosTemp$x)),"to",min(max(unique(xanthosTemp$x)),2010),sep=""));
 xanthosHist;

 xanthosHistx <- tibble::tibble()
 for(i in unique(xanthosTemp$x)){xanthosHistx <- xanthosHistx %>% dplyr::bind_rows(xanthosHist%>%dplyr::mutate(x=i))};
 xanthosHistx<-xanthosHistx%>%unique(); unique(xanthosHistx$scenario)

 if(nrow(xanthosHistx)>0){
 poly_fname<-paste(dirX, "/poly_",unique(xanthosHistx$scenario),"_",unique(xanthosHistx$subRegType),"_",
                   unique(xanthosHistx$param),nameAppend,".csv", sep = "")
 if(saveFiles){
   data.table::fwrite(xanthosHistx,
                    file = poly_fname,row.names = F, append=T)
 print(paste("Subregional polygon data files written to: ",poly_fname, sep = ""))}}


 xanthosData <- xanthosTemp%>%dplyr::bind_rows(xanthosHistx);

 xanthosData<-xanthosData%>%dplyr::mutate(value=signif(value,10))%>%unique();

 if(nrow(xanthosData>0)){
 for(xanthosFile_i in unique(xanthosData$scenario)){
   for(tethysFile_i in tethysFilesx){

     print(paste("polygonScarcity for Xanthos file: ",xanthosFile_i," and tethys file: ",tethysFile_i,sep=""))
     x <- xanthosData%>%dplyr::filter(scenario==xanthosFile_i)
     t <- data.table::fread(paste(dirX,"/",tethysFile_i,sep="")) %>% dplyr::filter(grepl("tethys",param));
     xGCM<-paste(unique(x$scenarioMultiA),sep="");xRCP<-paste(unique(x$scenarioMultiB),sep="")
     t1 <- t %>% tibble::as_tibble() %>%
       dplyr::mutate(scenarioMultiA=as.character(scenarioMultiA),scenarioMultiB=as.character(scenarioMultiB),
                     scenarioMultiA=dplyr::case_when(is.na(scenarioMultiA)~xGCM,
                                              TRUE~scenarioMultiA),
                     scenarioMultiB=dplyr::case_when(is.na(scenarioMultiB)~xRCP,
                                              TRUE~scenarioMultiB))
     for(col_i in names(x)){class(t1[[col_i]])<-class(x[[col_i]])}
     if(unique(x$scenarioMultiA)==unique(t1$scenarioMultiA) & unique(x$scenarioMultiB)==unique(t1$scenarioMultiB)){
       commonyears <- unique(x$x)[unique(x$x) %in% unique(t$x)]
       s <- x %>% dplyr::filter(x %in% commonyears) %>% dplyr::bind_rows(t1 %>% dplyr::filter(x %in% commonyears)) %>% tibble::as_tibble();s
       s1 <- s %>% dplyr::select(subRegion,scenario,scenarioMultiA,scenarioMultiB,param,units,aggType,classPalette,class,x,value,region,class2)%>%
         dplyr::mutate(scenario=paste(scenario,"_",param,sep=""))%>%
         dplyr::select(-param,-units,-class,-class2,-classPalette)%>%dplyr::filter(!is.na(x)) %>% unique();s1
       s2 <- s1 %>% tidyr::spread(key="scenario",value="value");s2 %>% as.data.frame() %>% utils::head()
       scarcityScen <- paste("X",
                             gsub("_xanthosRunoff","",paste(unique(s1$scenario)[grepl("xanthos",unique(s1$scenario))],sep="")),
                             "T",
                             gsub("_tethysWatWithdraw_total","",paste(unique(s1$scenario)[grepl("tethys",unique(s1$scenario))],sep="")),
                             sep=""); scarcityScen
       s3 <- s2 %>%
         dplyr::mutate(!!(rlang::sym("value")):=!!(rlang::sym(unique(s1$scenario)[grepl("tethys",unique(s1$scenario))]))/
                         !!(rlang::sym(unique(s1$scenario)[grepl("xanthos",unique(s1$scenario))])),
                       scenario=scarcityScen)%>%
         dplyr::filter(!is.na(value))%>%
         dplyr::select(-!!(rlang::sym(unique(s1$scenario)[grepl("xanthos",unique(s1$scenario))])),
                       -!!(rlang::sym(unique(s1$scenario)[grepl("tethys",unique(s1$scenario))])))%>%
         dplyr::mutate(param="polygonScarcity",
                       units="Polygon Scarcity (Ratio)",
                       class="class",
                       class2="class2",
                       classPalette="pal_ScarcityCat",
                       subRegType=unique(s$subRegType));

       if(saveFiles){
       data.table::fwrite(s3,paste(dirX,"/poly_Scarcity_",scarcityScen,nameAppend,".csv",sep=""),append=T)
       print(paste("Saving file as: ",dirX,"/polyScarcity_",scarcityScen,".csv",sep=""))}

     }else{print("Xanthos/Tethys GCM RCP's not the same so skipping...")}
    }
 }}
}

  if(nrowOrig!=nrowNew){print("WARNING: Multiple lat/lon data had same attributes and were combined.")}

  return(poly)

} # Close Function
