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
  #  printGridOverlay =F
  # saveFiles=T

  #----------------
  # Initialize variables by setting to NULL
  #----------------

  NULL->subRegAreaSum->areaPrcnt->weight->ID->subRegion->region->scenario->
    param->shpRegCol->subReg->gridTable->tbl->key->value->.->classPalette->lat->lon->overlapShape->
    gridPolyLoop->dbHead->paramsSub->sqlGrid->gridMetis -> template_subRegional_mapping -> scenarioGCM ->
    scenarioRCP -> class2 -> scenarioPolicy -> valueTethys -> valueXanthos -> scenarioSSP -> gridCellArea ->
    gridCellAreaRatio -> area -> areaPrcnt -> scenarioMultiA -> scenarioMultiB->nrowOrig->nrowNew -> year

  #------------------
  # Function for adding any missing columns if needed
  # -----------------

  addMissing<-function(data){
    if(!any(grepl("\\<region\\>",names(data),ignore.case = T))){data<-data%>%dplyr::mutate(region="region")}else{
      data <- data %>% dplyr::rename(!!"region" := (names(data)[grepl("\\<region\\>",names(data),ignore.case = T)])[1])
      data<-data%>%dplyr::mutate(region=as.character(region),region=dplyr::case_when(is.na(region)~"region",TRUE~region))}
    if(!any(grepl("\\<regions\\>",names(data),ignore.case = T))){}else{
      data <- data %>% dplyr::rename(!!"region" := (names(data)[grepl("\\<regions\\>",names(data),ignore.case = T)])[1])
      data<-data%>%dplyr::mutate(region=as.character(region),region=dplyr::case_when(is.na(region)~"region",TRUE~region))}
    if(!any(grepl("\\<scenario\\>",names(data),ignore.case = T))){data<-data%>%dplyr::mutate(scenario="scenario")}else{
      data <- data %>% dplyr::rename(!!"scenario" := (names(data)[grepl("\\<scenario\\>",names(data),ignore.case = T)])[1])
      data<-data%>%dplyr::mutate(scenario=as.character(scenario),scenario=dplyr::case_when(is.na(scenario)~"scenario",TRUE~scenario))}
    if(!any(grepl("\\<scenarios\\>",names(data),ignore.case = T))){}else{
      data <- data %>% dplyr::rename(!!"scenario" := (names(data)[grepl("\\<scenarios\\>",names(data),ignore.case = T)])[1])
      data<-data%>%dplyr::mutate(scenario=as.character(scenario),scenario=dplyr::case_when(is.na(scenario)~"scenario",TRUE~scenario))}
    if(!"x"%in%names(data)){if("year"%in%names(data)){
      data<-data%>%dplyr::mutate(x=year)}else{data<-data%>%dplyr::mutate(x="x")}}
    if(!any(grepl("\\<class\\>",names(data),ignore.case = T))){data<-data%>%dplyr::mutate(class="class")}else{
      data <- data %>% dplyr::rename(!!"class" := (names(data)[grepl("\\<class\\>",names(data),ignore.case = T)])[1])
      data<-data%>%dplyr::mutate(class=as.character(class),class=dplyr::case_when(is.na(class)~"class",TRUE~class))}
    if(!any(grepl("\\<param\\>",names(data),ignore.case = T))){data<-data%>%dplyr::mutate(param="param")}else{
      data <- data %>% dplyr::rename(!!"param" := (names(data)[grepl("\\<param\\>",names(data),ignore.case = T)])[1])
      data<-data%>%dplyr::mutate(param=as.character(param),param=dplyr::case_when(is.na(param)~"param",TRUE~param))}
    if(!any(grepl("\\<params\\>",names(data),ignore.case = T))){}else{
      data <- data %>% dplyr::rename(!!"param" := (names(data)[grepl("\\<params\\>",names(data),ignore.case = T)])[1])
      data<-data%>%dplyr::mutate(param=as.character(param),param=dplyr::case_when(is.na(param)~"params",TRUE~param))}
    return(data)
  }


  #----------------
  # Load Shapefile and save boundary maps
  #---------------

  if(T){
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
  }

   #----------------
  # Create Folders
  #---------------

  if(T){
  if (!dir.exists(dirOutputs)){dir.create(dirOutputs)}
  if (!dir.exists(paste(dirOutputs,"/",folderName, sep = ""))){dir.create(paste(dirOutputs, "/",folderName,sep = ""))}
  if (!dir.exists(paste(dirOutputs,"/",folderName, "/Grid2Poly/", sep = ""))){dir.create(paste(dirOutputs, "/",folderName, "/Grid2Poly/", sep = ""))}
  dirX=paste(dirOutputs,"/",folderName, "/Grid2Poly/",sep = "")
  }


  #----------------
  # Calculate Grid 2 Poly
  #---------------

  if(T){

  #----------------
  # Cropped and Process Grid to Polygons
  #---------------

 poly<-tibble::tibble()
 gridCropped <-tibble::tibble()
 template_subRegional_mapping <- tibble::tibble()
 count=0;
 gridCount=0;

  for(grid_i in gridFiles){
    if(gridCount==0){ # In case a R tbl is provided directly then no need to loop through files
    if(any(class(gridFiles)=="character")){
       print(paste("Reading grid file: ", grid_i,sep=""))
        if(grepl(".csv",grid_i)){
          grid<-data.table::fread(grid_i,encoding="Latin-1")
          if(!all(c("lat","lon","value") %in% names(grid))){
            missingCols = c("lat","lon","value")[!c("lat","lon","value") %in% names(grid)]
            print(paste("Required Columns: '", missingCols, "' missing from grid file provided: ", grid_i,sep=""))
            stop("Please make sure grid files provided have colums 'lat', 'lon' and 'value'")
          }
        }
        if(grepl(".rds",grid_i)){
          grid<-readRDS(grid_i)
          if(!all(c("lat","lon","value") %in% names(grid))){
            missingCols = c("lat","lon","value")[!c("lat","lon","value") %in% names(grid)]
            print(paste("Required Columns: '", missingCols, "' missing from grid file provided: ", grid_i,sep=""))
            stop("Please make sure grid files provided have colums 'lat', 'lon' and 'value'")
          }
          }}else{
          if(any(class(gridFiles) %in% c("tbl_df","tbl","data.frame"))){
           grid = gridFiles
           gridCount=1;
           if(!all(c("lat","lon","value") %in% names(grid))){
             missingCols = c("lat","lon","value")[!c("lat","lon","value") %in% names(grid)]
             print(paste("Required Columns: '", missingCols, "' missing from grid file provided."))
             stop("Please make sure grid files provided have colums 'lat', 'lon' and 'value'")
           }
          }
        }


      if(nrow(grid)>0){

        grid<-grid%>%addMissing()%>%tibble::as_tibble()%>%dplyr::distinct(); grid

        paramScenarios <- grid%>%dplyr::select("param","scenario")%>%dplyr::distinct(); paramScenarios

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
        paramScenarios = paramScenarios %>%
          dplyr::filter(scenario %in% scenarios,
                        param %in% params)
        print("SubSet paramScenarios: ")
        print(paramScenarios)
        scenarios; params; paramScenarios

        # Check Scenarios
        if(!is.null(paramScenarios)){
          if(nrow(paramScenarios)>0){
            if(any(is.na(unique(paramScenarios$scenario)))){
              print("Removing NA scenarios. Remaining Scenarios:")
              paramScenarios <- paramScenarios %>% dplyr::filter(!is.na(scenario))
              print(paramScenarios)
            }
          }
          }

      }else{
        stop(paste("Grid file ",grid," does not exist",sep=""))
      }

    count=count+1

    if(!is.null(grid)){

      if(is.null(paramScenarios)){
        if(all(c("param","scenario") %in% names(grid))){
          paramScenarios <- tibble::tibble()
          for(param_i in unique(grid$param)){
            paramScenarios <- paramScenarios %>%
              dplyr::bind_rows(grid %>% dplyr::filter(param==param_i)%>%
                                 dplyr::select("param","scenario")%>%
                                 dplyr::distinct())
          }
        }
      }; paramScenarios


      if(nrow(paramScenarios)>0){
      for(row_i in 1:nrow(paramScenarios)){

        param_i <- paramScenarios[row_i,]$param; param_i
        scenario_i <- paramScenarios[row_i,]$scenario; scenario_i
        gridx<-grid%>%dplyr::filter(param==param_i,scenario==scenario_i);

        if(nrow(gridx)>0){

          gridx <- gridx%>%dplyr::filter(!is.na(x))

          print(paste("Starting aggregation for grid: ", grid_i," and param: ",param_i," and scenario: ",scenario_i,"...",sep=""))

          # Subset to keep only required columns
          cols2Remove <-names(gridx)[names(gridx) %in% c("subRegion","region",subRegCol,"gridCellArea","subRegAreaSum","gridCellAreaRatio")]
          gridx <- gridx %>% dplyr::select(-tidyselect::all_of(cols2Remove))

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

            gridx<-gridx%>%dplyr::distinct()%>%tidyr::spread(key=key,value=value)

            print(paste("Cropping grid to shape file for parameter ", param_i," and scenario: ",scenario_i,"...",sep=""))

           gridCropped<-tibble::as_tibble(metis.gridByPoly(gridTable=gridx%>%dplyr::ungroup(),
                                                                            shape=shape,colName=subRegCol))
           # gridTable=gridx%>%dplyr::ungroup();shape=shape;colName=subRegCol

           print(paste("Grid cropped.",sep=""))

            if(nrow(gridCropped)>0){

              gridCroppedX1<-tidyr::gather(gridCropped,key=key,value=value,-c(!!subRegCol,"gridCellArea","subRegAreaSum",
                                                                             "gridCellAreaRatio","lat","lon","GridByPolyID"))%>%
                dplyr::filter(!is.na(value),value!=0)
              print("Splitting original column names for each grid cell...")
              newCols <- stringi::stri_split_regex(gridCroppedX1$key,"_",simplify=TRUE)%>%as.data.frame()
              names(newCols)<-namesGrid[!namesGrid %in% c("lat","lon","value")]
              gridCroppedX <- dplyr::bind_cols(gridCroppedX1%>%dplyr::select(-key), newCols)%>%
                dplyr::ungroup()%>%
                dplyr::distinct()%>%
                dplyr::rename(subRegion=!!subRegCol)%>%
                dplyr::filter(!is.na(value),value!=0)
              print("Original columns split.")

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
              r<-(raster::stack(spdf))[[1]]
              raster::projection(r)<-sp::proj4string(shape)
              r1 <- raster::crop(r,shape)
              r1p <-raster::rasterToPolygons(r1)
              grDevices::png(paste(dirX,"/subBasin_map_GridSize.png",sep=""),width=13,height=9, units="in",res=300)
              plot(shape, border="black", col=RColorBrewer::brewer.pal(11,"Set3"),alpha=0.5,lwd=0.1)
              plot(r1p,add=TRUE, border='black', lwd=1)
              grDevices::dev.off()
              print(paste("Grid overlay map saved to: ", dirX,"/subBasin_map_GridSize.png",sep=""))
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
                                        dplyr::select("weight"),SIMPLIFY=FALSE))%>%
                  dplyr::bind_cols(x%>%dplyr::select("ID"));
                #assign(paste0("df", i), x)
                dfx<-rbind.data.frame(dfx,x1)
                }
              }
              names(dfx)[names(dfx)=="ID"]<- subRegCol;
              polyDatax<-dfx%>%dplyr::group_by(.dots = list( subRegCol))%>% dplyr::summarise_all(list(~sum(.,na.rm=T)))%>%
                tibble::as_tibble()%>%dplyr::ungroup()
              print("Aggregation complete.")
            }

            polyDataX1<-tidyr::gather(polyDatax,key=key,value=value,-(subRegCol))%>%
              dplyr::filter(value!=0,!is.na(value))
            print("Splitting original column names...")
            newCols <- stringi::stri_split_regex(polyDataX1$key,"_",simplify=TRUE)%>%as.data.frame()
            names(newCols)<-namesGrid[!namesGrid %in% c("lat","lon","value")]
            polyData <- dplyr::bind_cols(polyDataX1%>%dplyr::select(-key), newCols)%>%
              dplyr::ungroup()%>%
              dplyr::distinct()%>%
              dplyr::filter(!is.na(value),value!=0)
            print("Original columns split.")

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

          #..................
          # Modifying final poly file
          #...................

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
                dplyr::ungroup()%>%dplyr::distinct())%>%dplyr::distinct()
            poly_fname<-paste(dirX, "/poly_subregionalTemplate.csv", sep = "")
            if(saveFiles){
              data.table::fwrite(template_subRegional_mapping,
                               file = poly_fname,row.names = F, append=T)
            print(paste("Subregional polygon template files written to: ",poly_fname, sep = ""))}
          } # Close loop for aggType
        }

        NULL -> gridx -> spdf -> r -> rcrop -> rcropP -> rcropPx -> w -> gridCroppedX->
          x1  -> polyData -> polyx -> dfx
        rm(gridx,spdf,r,rcrop,rcropP,rcropPx,w,gridCroppedX, x1, polyData, polyx, dfx);gc()

        } # Close loop for param_i and scenario_i

      print(paste("Aggregation for all scenarios and params complete."))
      }else{print("None of the selected params and scenarios were found in the grid file. Skipping...")}



    }else{print("No grid provided.")}
  }}

    } # Close grid2poly


  if(nrowOrig!=nrowNew){print("WARNING: Multiple lat/lon data had same attributes and were combined.")}

  return(poly)

} # Close Function
