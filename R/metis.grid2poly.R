#' metis.grid2poly
#'
#' This function takes a .csv file with gridded lat, long data and aggregates
#' the data by spatial boundaries given different shapefiles.
#' @return A table with data by polygon ID for each shapefile provided
#' @keywords gcam, gcam database, query
#' @param grid  Default=NULL. Grid file in .csv format or a R table, data frame or tibble with as a minimum columns with "lat","lon" and "value",
#' @param subRegShape  Default=NULL. shapefile over which grid data is to be aggregated.
#' @param boundaryRegionsSelect  Default=NULL. Larger region name which will be used as the folder name for outputs.
#' @param subRegShpFolder  Default=NULL. Folder containing boundary region shapefile. Suggested paste(getwd(),"/dataFiles/gis/naturalEarth",sep  Default=""),
#' @param subRegShpFile  Default=NULL. Name of sub-region shapefile. Suggested paste("ne_10m_admin_1_states_provinces",sep  Default=""),
#' @param subRegCol  Default= NULL. Suggested for states "name",
#' @param subRegType  Default="subRegType". Eg. "states", "basins" etc.
#' @param aggType  Default=NULL. Aggregation method to be used. Either "vol" or "depth" dependening on the type of data provided.
#' @param dirOutputs  Default=paste(getwd(),"/outputs",sep  Default=""),
#' @param nameAppend  Default="",
#' @param labelsSize Default =1.2. Label size for the region names for the gridoverlay plot.
#' @param paramsSelect Default ="All"
#' @param sqliteUSE Default = T,
#' @param sqliteDBNamePath Default = paste(getwd(),"/outputs/Grids/gridMetis.sqlite", sep = "")
#' @export

metis.grid2poly<- function(grid=NULL,
                           boundaryRegionsSelect=NULL,
                           subRegShape=NULL,
                           subRegShpFolder=NULL,
                           subRegShpFile=NULL,
                           subRegCol=NULL,
                           subRegType="subRegType",
                           aggType=NULL,
                           dirOutputs=paste(getwd(),"/outputs",sep=""),
                           nameAppend="",
                           labelsSize=1.2,
                           paramsSelect="All",
                           sqliteUSE = F,
                           sqliteDBNamePath = paste(getwd(),"/outputs/Grids/gridMetis.sqlite", sep = "")) {

  # grid=NULL
  # boundaryRegionsSelect=NULL
  # subRegShape=NULL
  # subRegShpFolder=NULL
  # subRegShpFile=NULL
  # subRegCol=NULL
  # subRegType="subRegType"
  # aggType=NULL
  # dirOutputs=paste(getwd(),"/outputs",sep="")
  # nameAppend=""
  # labelsSize=1.2

  #----------------
  # Initialize variables by setting to NULL
  #----------------

  NULL->subRegAreaSum->areaPrcnt->weight->ID->subRegion->region->scenario->
    param->shpRegCol->subReg->griddataTables->tbl->key->value->.->classPalette->lat->lon->overlapShape->
    gridPolyLoop->dbHead->paramsSub->sqlGrid->gridMetis

  #----------------
  # Check input data format
  #---------------

  paramScenarios<-tibble::tibble()

  if(sqliteUSE==T){
    paste("Using SQLite database...",sep="")
    if(!file.exists(sqliteDBNamePath)){stop("SQLite file path provided does not exist: ", sqliteDBNamePath, sep="")}
    dbConn <- DBI::dbConnect(RSQLite::SQLite(), sqliteDBNamePath)
    #src_dbi(dbConn)
    sqlGrid<-tbl(dbConn,"gridMetis"); utils::head(sqlGrid)
    dbHead<-utils::head(sqlGrid,1)%>%dplyr::collect();dbHead
    names(dbHead)
    if(!all(c("param","scenario","lon","lat","value") %in% names(dbHead))){
      stop("SQLite database must have columns for lon, lat, value, param and scenario. Sql Database cols are : ",
           paste(names(dbHead),collapse=", "), sep="")}
    print(paste("Finding unique params in sql database...",sep=""))
    params<-sqlGrid%>%dplyr::distinct(param)%>%dplyr::collect();
    params=params$param
    print(paste("Unique params found : ", paste(params,collapse=", "),sep=""))
    paramScenarios<-tibble::tibble()
    for(param_i in params){
      print(paste("Finding unique scenarios in sql database for param: ",param_i,"...",sep=""))
      scenarios<-sqlGrid%>%dplyr::filter(param==param_i)%>%dplyr::distinct(scenario)%>%dplyr::collect();
      scenarios=scenarios$scenario
      paramScenarios<-dplyr::bind_rows(paramScenarios,data.frame(param=rep(param_i,length(scenarios)),scenario=scenarios))
      print(paste("Unique scenarios found : ", paste(scenarios,collapse=", "),sep=""))
    }
    paramScenarios<-paramScenarios%>%unique()
    print("paramScenarios found: ")
    print(paramScenarios)
    scenarios<-unique(paramScenarios$scenario)
  } else {

    # If not using SQL database

    if(!is.null(grid)){
      if(all(!class(grid) %in% c("tbl_df","tbl","data.frame"))){
        if(any(grepl(".csv",paste(grid)))){
          print(paste("Attempting to read grid csv file ",grid,sep=""))
          if(file.exists(grid)){
            grid<-data.table::fread(grid)
            grid<-grid%>%unique()
            }else{
              stop(paste("Grid file ",grid," does not exist",sep=""))
            }
        }else{
          if(any(grepl(".RData",paste(grid)))){
            print(paste("Attempting to read grid Rdata file... ",grid,sep=""))
            if(file.exists(grid)){
              load(grid)
              grid<-gridMetis
              print("Grid data loaded.")}else{
                stop(paste("Grid file ",grid," does not exist",sep=""))
              }
          }
        }
      }else{
        if(is.na(nrow(grid)) | nrow(grid)<1){
          stop(paste("Grid file ",grid," does not exist or has 0 rows.",sep=""))
        }
      }

      if(any(!c("lat","lon","value","param","scenario") %in% names(grid))){
        stop(paste(grid," should have columns lon, lat, value, param and scenario. Missing columns: ",
                   names(grid)[!names(grid) %in% c("lat","lon","value")]))
      } else {

        params=unique(grid$param)

      paramScenarios<-tibble::tibble()
      for(param_i in params){
        print(paste("Finding unique scenarios in grid for param: ",param_i,"...",sep=""))
        scenarios<-grid%>%dplyr::filter(param==param_i)%>%dplyr::distinct(scenario);
        scenarios=scenarios$scenario
        paramScenarios<-dplyr::bind_rows(paramScenarios,data.frame(param=rep(param_i,length(scenarios)),scenario=scenarios))
        print(paste("Unique scenarios found : ", paste(scenarios,collapse=", "),sep=""))
      }
      paramScenarios<-paramScenarios%>%unique()
      print("paramScenarios found: ")
      print(paramScenarios)
      scenarios<-unique(paramScenarios$scenario)

      }

    } # If !is.null(grid)

  } # if not using sqlLite

  # Check Scenarios
  if(nrow(paramScenarios)>0){
    if(any(is.na(unique(paramScenarios$scenario)))){
      print("Removing NA scenarios. Remaining Scenarios:")
      paramScenarios <- paramScenarios %>% dplyr::filter(!is.na(scenario))
      print(paramScenarios)
    }
  }

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


  poly<-tibble::tibble()

  if(is.null(boundaryRegionsSelect)){boundaryRegionsSelect="Region"}


  #----------------
  # Create Folders
  #---------------

  if (!dir.exists(dirOutputs)){dir.create(dirOutputs)}
  if (!dir.exists(paste(dirOutputs, "/Maps", sep = ""))){dir.create(paste(dirOutputs, "/Maps", sep = ""))}
  if (!dir.exists(paste(dirOutputs, "/Maps/Boundaries", sep = ""))){dir.create(paste(dirOutputs, "/Maps/Boundaries", sep = ""))}
  if (!dir.exists(paste(dirOutputs, "/Maps/Boundaries/",boundaryRegionsSelect, sep = ""))){dir.create(paste(dirOutputs, "/Maps/Boundaries/",boundaryRegionsSelect,sep = ""))}
  if (!dir.exists(paste(dirOutputs, "/Maps/Tables", sep = ""))){dir.create(paste(dirOutputs, "/Maps/Tables", sep = ""))}
  dir=paste(dirOutputs, "/Maps/Boundaries/",boundaryRegionsSelect,sep = "")

  # Delete temporary grid folder
  if (dir.exists(paste(dirOutputs, "/Grids/temp", sep = ""))){unlink(paste(dirOutputs, "/Grids/temp", sep = ""),recursive = T)}

  #----------------
  # Read in shapefiles and check format
  #---------------

  if(!is.null(sqlGrid)){
    if(!any(paramsSelect == "All")){
      if(all(paramsSelect %in% params)){
        sqlGrid<-sqlGrid%>%dplyr::filter(param %in% paramsSelect)
        paramsSub<-params[params %in% paramsSelect]
        print(paste("Filtering sqlGrid to selected paramsSelect ",paste(paramsSelect,collapse=", "),sep=""))
      }else{
        if(any(paramsSelect %in% unique(grid$param))){
          grid<-grid%>%dplyr::filter(param %in% paramsSelect)
          paramsSub<-params[params %in% paramsSelect]
          print(paste("Only analyzing params ",paste((paramsSelect %in% unique(grid$param)),collapse=", "),
                      " which are present in sqlGrid of all paramsSelect ",paste(paramsSelect,collapse=", "),sep=""))
        }else{
          print(paste("paramsSelect ",paste(paramsSelect,collapse=", "),
                      " not present in unique(grid$param) ",
                      unique(grid$param),". Using all params in sqlGrid.",sep=""))
        }
      }

    }else {paramsSub<-params}

  }

  if(!is.null(grid)){

    if(!any(paramsSelect == "All")){
      if(all(paramsSelect %in% params)){
        grid<-grid%>%dplyr::filter(param %in% paramsSelect)
        paramsSub<-params[params %in% paramsSelect]
        print(paste("Filtering grid to selected paramsSelect ",paste(paramsSelect,collapse=", "),sep=""))
      }else{
        if(any(paramsSelect %in% params)){
          grid<-grid%>%dplyr::filter(param %in% paramsSelect)
          paramsSub<-params[params %in% paramsSelect]
          print(paste("Only analyzing params ",paste((paramsSelect %in% params),collapse=", "),
                      " which are present in grid of all paramsSelect ",paste(paramsSelect,collapse=", "),sep=""))
        }else{
          print(paste("paramsSelect ",paste(paramsSelect,collapse=", "),
                      " not present in unique(grid$param) ",
                      params,". Using all params in grid.",sep=""))
        }
      }

    } else {paramsSub<-params}

  }# closing if !is.null(grid)

 gridCropped <-tibble::tibble()


  if(!is.null(sqlGrid) | !is.null(grid)){

    for(row_i in 1:nrow(paramScenarios)){

      param_i <- paramScenarios[row_i,]$param
      scenario_i <- paramScenarios[row_i,]$scenario

      if(!is.null(sqlGrid)){
        print(paste("Reading data for param: ",param_i," and scenario: ",scenario_i," from sqlGrid...",sep=""))
        gridx<-sqlGrid%>%dplyr::filter(param==param_i, scenario==scenario_i)%>%dplyr::collect()
        print(paste("Data read.",sep=""))
      }else{
        gridx<-grid%>%dplyr::filter(param==param_i,scenario==scenario_i)
      }

      if(nrow(gridx)>0){

        # Remove column for region from gridx if exists
        if("region" %in% names(gridx)){gridx <- gridx %>% dplyr::select(-region)}


        print(paste("Starting aggregation for param: ",param_i," and scenario: ",scenario_i,"...",sep=""))


        if(!"aggType" %in% names(gridx)){
          if(is.null(aggType)){
            print("Column aggType is missing from grid data. Assigning aggType='depth'")
            gridx<-gridx%>%dplyr::mutate(aggType="depth")}else{
              gridx<-gridx%>%dplyr::mutate(aggType=aggType)
            }}

        namesGrid<-names(gridx)

        # Temporary column names merge in order to aggregate params scenarios across sub-regions
        print("setting grid columns ...")
        for(colx in names(gridx)){
          if(is.character(gridx[[colx]])){
            gridx[[colx]]<-gsub(" ","XSPACEX",gridx[[colx]],perl = TRUE )
            gridx[[colx]]<-gsub("\\.","XPERIODX",gridx[[colx]],perl = TRUE )
            gridx[[colx]]<-gsub("\\-","XDASHX",gridx[[colx]],perl = TRUE )
            gridx[[colx]]<-gsub("\\(","XLPARENTHX",gridx[[colx]],perl = TRUE)
            gridx[[colx]]<-gsub("\\)","XRLPARENTHX",gridx[[colx]],perl = TRUE)
            gridx[[colx]]<-gsub("\\_","XUNDERX",gridx[[colx]],perl = TRUE)
          }
        }
        print("Grid Columns set.")



        for (aggType_i in unique(gridx$aggType)){

          if(!unique(gridx$aggType) %in% c("depth","vol")){stop("Incorrect aggType in grid file")}

          print("Uniting columns...")
          gridx<-gridx%>%dplyr::filter(aggType==aggType_i)%>%
            tidyr::unite(col="key",names(gridx)[!names(gridx) %in% c("lat","lon","value")],sep="_",remove=T)
          print("Columns united.")

          gridx<-gridx%>%tidyr::spread(key=key,value=value)

          # Convert to Spatial Point Data Frames
          spdf = sp::SpatialPointsDataFrame(sp::SpatialPoints(coords=(cbind(gridx$lon,gridx$lat))),data=gridx)
          sp::gridded(spdf)<-TRUE

          r<-raster::stack(spdf)
          raster::projection(r)<-sp::proj4string(shape)

          shapeExpandExtent<-as.data.frame(sp::bbox(shape))   # Get Bounding box
          expandPercent<-3; shapeExpandExtent$min;shapeExpandExtent$max
          rangeX<-abs(range(shapeExpandExtent$min[1],shapeExpandExtent$max[1])[2]-range(shapeExpandExtent$min[1],shapeExpandExtent$max[1])[1])
          rangeY<-abs(range(shapeExpandExtent$min[2],shapeExpandExtent$max[2])[2]-range(shapeExpandExtent$min[2],shapeExpandExtent$max[2])[1])
          shapeExpandExtent$min[1]<-(-rangeX*expandPercent/100)+shapeExpandExtent$min[1];
          shapeExpandExtent$min[2]<-(-rangeY*expandPercent/100)+shapeExpandExtent$min[2];
          shapeExpandExtent$max[1]<-(rangeX*expandPercent/100)+shapeExpandExtent$max[1];
          shapeExpandExtent$max[2]<-(rangeY*expandPercent/100)+shapeExpandExtent$max[2];
          shapeExpandExtent<-methods::as(raster::extent(as.vector(t(shapeExpandExtent))), "SpatialPolygons")
          sp::proj4string(shapeExpandExtent)<-sp::CRS(sp::proj4string(shape)) # ASSIGN COORDINATE SYSTEM

          print(paste("Cropping grid to shape file for parameter ", param_i," and scenario: ",scenario_i,"...",sep=""))

         if(!is.null(raster::intersect(raster::extent(r), raster::extent(shapeExpandExtent)))){

          rcrop<-raster::crop(r,shapeExpandExtent)
          rcropP<-raster::rasterToPolygons(rcrop)

          sp::proj4string(rcropP)<-sp::proj4string(shape)
          rcropPx<-raster::intersect(shape,rcropP)

          gridCropped<-dplyr::bind_rows(gridCropped,tibble::as_tibble(rcropP@data))

          # Save gridCropped to csv

          if(nrow(gridCropped)>0){

            gridCroppedX<-tidyr::gather(gridCropped,key=key,value=value,-c(lat,lon))%>%
              tidyr::separate(col="key",into=namesGrid[!namesGrid %in% c("lat","lon","value")],sep="_")%>%
              unique()%>%
              dplyr::filter(!is.na(value))

            for(colx in names(gridCroppedX)){
              if(is.character(gridCroppedX[[colx]])){
                gridCroppedX[[colx]]<-gsub("XSPACEX"," ",gridCroppedX[[colx]])
                gridCroppedX[[colx]]<-gsub("XPERIODX","\\.",gridCroppedX[[colx]])
                gridCroppedX[[colx]]<-gsub("XDASHX","\\-",gridCroppedX[[colx]])
                gridCroppedX[[colx]]<-gsub("XLPARENTHX","\\(",gridCroppedX[[colx]])
                gridCroppedX[[colx]]<-gsub("XRLPARENTHX","\\)",gridCroppedX[[colx]])
                gridCroppedX[[colx]]<-gsub("XUNDERX","\\_",gridCroppedX[[colx]])
              }
            }

            polyType=subRegType
            if (!dir.exists(paste(dirOutputs, "/Grids", sep = ""))){dir.create(paste(dirOutputs, "/Grids", sep = ""))}
            if (!dir.exists(paste(dirOutputs, "/Grids/temp", sep = ""))){dir.create(paste(dirOutputs, "/Grids/temp", sep = ""))}

            grid_fname<-paste(dirOutputs, "/Grids/temp/gridCropped_",boundaryRegionsSelect,"_",polyType,"_",param_i,"_",scenario_i,nameAppend,".csv", sep = "")
            data.table::fwrite(gridCroppedX%>%dplyr::mutate(region=boundaryRegionsSelect,polyType=polyType),
                               file = grid_fname,row.names = F)
            print(paste("Subregional grid data files written to: ",grid_fname, sep = ""))

          } # If nrow(gridCropped)



          if(is.null(gridPolyLoop)){
            print("Printing Grid overlay...")
            metis.map(labelsSize=labelsSize, dataPolygon=rcropPx,fileName = paste(boundaryRegionsSelect,"_",subRegType,"_map_GridSize_Labels",nameAppend,sep=""),
                      dirOutputs = dir,
                      overLayer = metis.map(labelsSize=labelsSize, dataPolygon=shape,fillColumn = subRegCol,
                                            fillPalette = "white",alpha=0,facetsON=F,
                                            labels=T,printFig=F),facetsON=F)

            print("Printing Grid overlay with Labels...")
            metis.map(labelsSize=labelsSize, dataPolygon=rcropPx,fileName = paste(boundaryRegionsSelect,"_",subRegType,"_map_GridSize",nameAppend,sep=""),
                      dirOutputs = dir,
                      overLayer = metis.map(labelsSize=labelsSize, dataPolygon=shape,fillColumn = subRegCol,
                                            fillPalette = "white",alpha=0,facetsON=F,
                                            labels=F,printFig = F),facetsON=F)
          }
          gridPolyLoop=1; # To prevent gridded map being produced multiple times


          if(aggType_i=="depth"){
            print(paste("Aggregating depth for parameter ", param_i," and scenario: ",scenario_i,"...",sep=""))
            rcropPx@data$area<-raster::area(rcropPx)
            s1<-shape
            s1$subRegAreaSum<-raster::area(shape);
            s1<-s1@data%>%dplyr::select( subRegCol,subRegAreaSum);
            rcropPx@data<-dplyr::left_join(rcropPx@data,s1,by= subRegCol)
            rcropPx@data$areaPrcnt<-rcropPx@data$area/rcropPx@data$subRegAreaSum;

            x<-data.frame(mapply(`*`,rcropPx@data%>%
                                   dplyr::select(names(rcropPx@data)[!names(rcropPx@data) %in% c(
                                     names(shape),"lat","lon","area","subRegAreaSum","areaPrcnt")]),
                                 rcropPx@data%>%dplyr::select(areaPrcnt),SIMPLIFY=FALSE))%>%
              dplyr::bind_cols(rcropPx@data%>%dplyr::select( subRegCol))%>%tibble::as_tibble();
            polyDatax<-x%>%dplyr::group_by(.dots = list( subRegCol))%>% dplyr::summarise_all(dplyr::funs(round(mean(.,na.rm=T),2)))
          }

          if(aggType_i=="vol"){
            print(paste("Aggregating volume for parameter ", param_i," and scenario: ",scenario_i,"...",sep=""))
            w <- raster::extract(r,shape, method="simple",weights=T, normalizeWeights=F);
            dfx<-data.frame()

            for (i in seq(w)){
              if(!is.null(w[[i]])) {
                x<-as.data.frame(w[[i]]) %>% dplyr::mutate(weight=weight*100)
              x$ID<-shape@data[[ subRegCol]][[i]]


              x1<-data.frame(mapply(`*`,x%>%
                                      dplyr::select(names(r)[!names(r) %in% c("lat","lon")]),x%>%
                                      dplyr::select(weight),SIMPLIFY=FALSE))%>%
                dplyr::bind_cols(x%>%dplyr::select(ID));
              #assign(paste0("df", i), x)
              dfx<-rbind.data.frame(dfx,x1)
              }
            }
            names(dfx)[names(dfx)=="ID"]<- subRegCol;
            polyDatax<-dfx%>%dplyr::group_by(.dots = list( subRegCol))%>% dplyr::summarise_all(dplyr::funs(round(sum(.,na.rm=T),2)))%>%tibble::as_tibble()
          }

          polyData<-tidyr::gather(polyDatax,key=key,value=value,-(subRegCol))%>%
            tidyr::separate(col="key",into=namesGrid[!namesGrid %in% c("lat","lon","value")],sep="_")

          for(colx in names(polyData)){
            if(is.character(polyData[[colx]])){
              polyData[[colx]]<-gsub("XSPACEX"," ",polyData[[colx]])
              polyData[[colx]]<-gsub("XPERIODX","\\.",polyData[[colx]])
              polyData[[colx]]<-gsub("XDASHX","\\-",polyData[[colx]])
              polyData[[colx]]<-gsub("XLPARENTHX","\\(",polyData[[colx]])
              polyData[[colx]]<-gsub("XRLPARENTHX","\\)",polyData[[colx]])
              polyData[[colx]]<-gsub("XUNDERX","\\_",polyData[[colx]])
            }
          }

          polyData<-polyData%>%dplyr::mutate(subRegType=subRegType)

          polyx<-shape
          polyx@data<-dplyr::left_join(polyx@data,polyData)
          polyx@data<-polyx@data%>%
            dplyr::rename(subRegion:= !!paste(subRegCol))%>%
            dplyr::mutate(region=boundaryRegionsSelect)%>%
            dplyr::filter(!is.na(x))

          poly<-dplyr::bind_rows(poly,polyx@data)


          #rm(r,spdf,gridx,rcropPx,rcropP,polyx,rcrop,polyDatax)

         } else { # Close loop for if extents overlap
           print(paste("Gridded data provided for param: ",param_i," and scenario: ",scenario_i," did not overlap with shape boundary.",sep=""))
         }
        } # Close loop for aggType
      } else {print(paste("No data for param: ",param_i," and scenario: ",scenario_i,".",sep=""))}# Close loop for nrow>0
    } # Close loop for param_i and scenario_i

    print(paste("Aggregation for all scenarios and params complete."))

  }else{print("No grid provided.")}

  #----------------
  # Save template, csv and .RDATA
  #---------------

  if(nrow(poly)>0){

    if (!dir.exists(paste(getwd(),"/dataFiles", sep = ""))){
      dir.create(paste(getwd(),"/dataFiles", sep = ""))}  # dataFiles directory (should already exist)
    if (!dir.exists(paste(getwd(),"/dataFiles/mapping", sep = ""))){
      dir.create(paste(getwd(),"/dataFiles/mapping", sep = ""))}  # mapping directory
    data.table::fwrite(poly %>% dplyr::filter(region == boundaryRegionsSelect) %>%
                         dplyr::select(param,units,class,classPalette),
                       file = paste(getwd(),"/dataFiles/mapping/template_subRegional_mapping.csv", sep = ""),row.names = F)


    for (boundaryRegionsSelect in boundaryRegionsSelect[(boundaryRegionsSelect %in% unique(poly$region))]) {
      data.table::fwrite(poly %>% dplyr::filter(region == boundaryRegionsSelect) %>%
                           dplyr::select(scenario,param,units,class,value,x,subRegion,subRegType,region)%>%
                           dplyr::mutate(value=0,x=2015)%>%unique,
                         file = paste(dirOutputs, "/Maps/Tables/subReg_",boundaryRegionsSelect,"_",subRegType,"_template",nameAppend,".csv", sep = ""),row.names = F)
      data.table::fwrite(poly %>% dplyr::filter(region == boundaryRegionsSelect) %>%
                           dplyr::select(dplyr::contains("scenario"),param,units,class,value,x,subRegion,subRegType,region,classPalette),
                         file = paste(dirOutputs, "/Maps/Tables/subReg_origData_byClass_",boundaryRegionsSelect,"_",subRegType,"_origDownscaled",nameAppend,".csv", sep = ""),row.names = F)
    }

    print(paste("Subregional Polygon template .csv files written to: ",dirOutputs, "/Maps/Tables/subReg_",boundaryRegionsSelect,"_template",nameAppend,".csv", sep = ""))
    print(paste("Subregional Polygon data .csv files written to: ",dirOutputs, "/Maps/Tables/subReg_origData_byClass_",boundaryRegionsSelect,"_",subRegType,"_origDownscaled",nameAppend,".csv", sep = ""))

  }else{print("Polygon data has 0 rows")}


  # Save Cropped Grid

  if(length(list.files(paste(dirOutputs, "/Grids/temp", sep = "")))>0){

    #gridCroppedCompiled <- tibble::tibble()
    grid_fnameComp<-paste(dirOutputs, "/Grids/gridCropped_",boundaryRegionsSelect,"_",subRegType,nameAppend,".csv", sep = "")

    gridTempX <- tibble::tibble()
    for (file_i in list.files(paste(dirOutputs, "/Grids/temp", sep = ""))){
      print(paste("Compiling grid file",file_i,"...",sep=""))
      gridTemp <- data.table::fread(paste(dirOutputs, "/Grids/temp/",file_i, sep = ""))
      gridTempX <- gridTempX %>%
        dplyr::bind_rows(gridTemp)
    }

    gridTempAll <- gridTempX%>%unique()

    if (file.exists(grid_fnameComp)){unlink(grid_fnameComp,recursive = T)}

    data.table::fwrite(gridTempAll,
                       file = grid_fnameComp,row.names = F, append=F)

    print(paste("Subregional grid data files written to: ",grid_fnameComp, sep = ""))

    # Delete temporary grid folder
   if (dir.exists(paste(dirOutputs, "/Grids/temp", sep = ""))){unlink(paste(dirOutputs, "/Grids/temp", sep = ""),recursive = T)}

  }

  return(poly)

} # Close Function
