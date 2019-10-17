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
                           calculatePolyScarcity=F) {

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

  #----------------
  # Initialize variables by setting to NULL
  #----------------

  NULL->subRegAreaSum->areaPrcnt->weight->ID->subRegion->region->scenario->
    param->shpRegCol->subReg->griddataTables->tbl->key->value->.->classPalette->lat->lon->overlapShape->
    gridPolyLoop->dbHead->paramsSub->sqlGrid->gridMetis -> template_subRegional_mapping -> scenarioGCM ->
    scenarioRCP -> class2 -> scenarioPolicy -> valueTethys -> valueXanthos -> scenarioSSP -> gridCellArea ->
    gridCellAreaRatio -> area -> areaPrcnt

  #------------------
  # Function for adding any missing columns if needed
  # -----------------

  addMissing<-function(data){
    if(!"scenario"%in%names(data)){data<-data%>%dplyr::mutate(scenario="scenario")}
    if(!"param"%in%names(data)){data<-data%>%dplyr::mutate(param="param")}

    return(data)
  }

  #----------------
  # Check input data format
  #---------------


    paramScenarios=paramScenariosFixed

      print("paramScenarios found: ")
      print(paramScenarios)
      scenarios<-unique(paramScenarios$scenario)
      params <-unique(paramScenarios$param)

      print("Subsetting params and scenarios...")
      if(!any(grepl("all",paramsSelect,ignore.case = T))){params=params[params %in% paramsSelect]}
      if(!any(grepl("all",scenariosSelect,ignore.case = T))){scenarios=scenarios[scenarios %in% scenariosSelect]}


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


   #----------------
  # Create Folders
  #---------------

  if (!dir.exists(dirOutputs)){dir.create(dirOutputs)}
  if (!dir.exists(paste(dirOutputs, "/Grid2Poly", sep = ""))){dir.create(paste(dirOutputs, "/Grid2Poly", sep = ""))}
  if(!is.null(folderName)){
  if (!dir.exists(paste(dirOutputs, "/Grid2Poly/",folderName, sep = ""))){dir.create(paste(dirOutputs, "/Grid2Poly/",folderName, sep = ""))}
  dir=paste(dirOutputs, "/Grid2Poly/",folderName,sep = "")} else {
    if (!dir.exists(paste(dirOutputs, "/Grid2Poly", sep = ""))){dir.create(paste(dirOutputs, "/Grid2Poly", sep = ""))}
    dir=paste(dirOutputs, "/Grid2Poly",sep = "")
  }

  #----------------
  # Cropped and Process Grid to Polygons
  #---------------

 poly<-tibble::tibble()
 gridCropped <-tibble::tibble()
 template_subRegional_mapping <- tibble::tibble()
 count=0;

for(grid_i in gridFiles){

    if(file.exists(grid_i)){
      print(paste("Reading grid file: ", grid_i,sep=""))
      if(grepl(".csv",grid_i)){grid<-data.table::fread(grid_i,encoding="Latin-1")}
      if(grepl(".rds",grid_i)){grid<-readRDS(grid_i)}
      grid<-grid%>%unique()
    }else{
      stop(paste("Grid file ",grid," does not exist",sep=""))
    }

  count=count+1

  if(!is.null(grid)){


    for(row_i in 1:nrow(paramScenarios)){

      param_i <- paramScenarios[row_i,]$param; param_i
      scenario_i <- paramScenarios[row_i,]$scenario; scenario_i
      gridx<-grid%>%dplyr::filter(param==param_i,scenario==scenario_i); head(gridx)

      if(nrow(gridx)>0){

        gridx <- gridx%>%filter(!is.na(x))

        print(paste("Starting aggregation for grid: ", grid_i," and param: ",param_i," and scenario: ",scenario_i,"...",sep=""))

        # Subset to keep only required columns
        cols2Remove <-names(gridx)[names(gridx) %in% c("subRegion",subRegCol,"gridCellArea","subRegAreaSum","gridCellAreaRatio")]
        gridx <- gridx %>% dplyr::select(-cols2Remove)

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
          print("Columns united.")

          gridx<-gridx%>%unique()%>%tidyr::spread(key=key,value=value)

          print(paste("Cropping grid to shape file for parameter ", param_i," and scenario: ",scenario_i,"...",sep=""))

         gridCropped<-tibble::as_tibble(metis.gridByPoly(gridDataTables=gridx%>%dplyr::ungroup(),
                                                                          shape=shape,colName=subRegCol))

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
            grid_fname<-paste(dir, "/gridCropped_",scenario_i,"_",polyType,"_",param_i,nameAppend,".csv", sep = "")
            data.table::fwrite(gridCroppedX%>%dplyr::mutate(polyType=polyType, region=regionName),
                               file = grid_fname,row.names = F, append = T)
            print(paste("Subregional grid data files written to: ",grid_fname, sep = ""))

          } # If nrow(gridCropped)

          if(is.null(gridPolyLoop)){

            if(!file.exists(paste(dir,"/subBasin_map_GridSize.png",sep=""))){
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
                      dirOutputs = dir,
                      overLayer = metis.map(labelsSize=labelsSize, dataPolygon=shape,fillColumn = subRegCol,
                                            fillPalette = "white",alpha=0,facetsON=F,
                                            labels=T,printFig=F),facetsON=F)

            print("Printing Grid overlay with Labels...")
            metis.map(labelsSize=labelsSize, dataPolygon=rcropPx,fileName = paste(subRegType,"_map_GridSize",nameAppend,sep=""),
                      dirOutputs = dir,
                      overLayer = metis.map(labelsSize=labelsSize, dataPolygon=shape,fillColumn = subRegCol,
                                            fillPalette = "white",alpha=0,facetsON=F,
                                            labels=F,printFig = F),facetsON=F)
            }
          }
          gridPolyLoop=1; # To prevent gridded map being produced multiple times


          if(aggType_i=="depth"){
            print(paste("Aggregating depth for parameter ", param_i," and scenario: ",scenario_i,"...",sep=""))
            x<-data.frame(mapply(`*`,gridCropped%>%
                                   dplyr::select(names(gridCropped)[!names(gridCropped) %in% c(
                                     names(shape),"lat","lon","gridCellArea","subRegAreaSum","gridCellAreaRatio")]),
                                  gridCropped%>%dplyr::select(gridCellAreaRatio),SIMPLIFY=FALSE))%>%
              dplyr::bind_cols(gridCropped%>%dplyr::select( subRegCol))%>%tibble::as_tibble();

           polyDatax<-x%>%dplyr::group_by(.dots = list( subRegCol))%>% dplyr::summarise_all(list(~mean(.,na.rm=T)))%>%dplyr::ungroup()
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
            dplyr::mutate(subRegion = as.character(subRegion),
                          scenarioMultiA = as.character(scenarioMultiA),
                          scenarioMultiB = as.character(scenarioMultiB))

          polyType=subRegType
          poly_fname<-paste(dir, "/poly_",scenario_i,"_",polyType,"_",param_i,nameAppend,".csv", sep = "")
          data.table::fwrite(poly,
                             file = poly_fname,row.names = F, append=T)
          print(paste("Subregional polygon data files written to: ",poly_fname, sep = ""))

          template_subRegional_mapping <- template_subRegional_mapping %>%
            bind_rows(poly %>%
            dplyr::select(c("param","units","class","classPalette")[c("param","units","class","classPalette") %in% names(poly)])%>%
              dplyr::ungroup()%>%unique())%>%unique()
          poly_fname<-paste(dir, "/poly_subregionalTemplate.csv", sep = "")
          data.table::fwrite(template_subRegional_mapping,
                             file = poly_fname,row.names = F, append=T)
          print(paste("Subregional polygon template files written to: ",poly_fname, sep = ""))
        } # Close loop for aggType
      }

      NULL -> gridx -> spdf -> r -> rcrop -> rcropP -> rcropPx -> w -> gridCroppedX->
        x1  -> polyData -> polyx -> dfx
      rm(gridx,spdf,r,rcrop,rcropP,rcropPx,w,gridCroppedX, x1, polyData, polyx, dfx); gc()

      } # Close loop for param_i and scenario_i

    print(paste("Aggregation for all scenarios and params complete."))



  }else{print("No grid provided.")}
}

#----------------
# Calculating polygon scarcity
#---------------

 if(calculatePolyScarcity==T){
 # List files in dir
 if(is.null(tethysFilesScarcity)){tethysFilesx<-list.files(dir)[grepl("poly_",list.files(dir)) &
                                                                  grepl("tethys",list.files(dir)) &
                                                                  grepl("total",list.files(dir))]}else{
   tethysFilesx<-tethysFilesScarcity}; tethysFilesx
 if(is.null(xanthosFilesScarcity)){xanthosFilesx<-list.files(dir)[grepl("poly_",list.files(dir)) &
                                                                    grepl("xanthos",list.files(dir))]}else{
   xanthosFilesx<-xanthosFilesScarcity}; xanthosFilesx


 print(paste("Tethys files include: ",paste(tethysFilesx,collapse=", "),sep=""))
 print(paste("Xanthos files include: ",paste(xanthosFilesx,collapse=", "),sep=""))
 print(paste("Total combinations are: ", length(tethysFilesx)*length(xanthosFilesx)))

 for(xanthosFile_i in xanthosFilesx){
   for(tethysFile_i in tethysFilesx){

     print(paste("polygonScarcity for Xanthos file: ",xanthosFile_i," and tethys file: ",tethysFile_i,sep=""))
     x <- data.table::fread(paste(dir,"/",xanthosFile_i,sep="")) %>% dplyr::filter(grepl("xanthos",param));head(x)
     t <- data.table::fread(paste(dir,"/",tethysFile_i,sep="")) %>% dplyr::filter(grepl("tethys",param));head(t)
     xGCM<-paste(unique(x$scenarioMultiA),sep="");xRCP<-paste(unique(x$scenarioMultiB),sep="")
     t1 <- t %>% tibble::as_tibble() %>%
       dplyr::mutate(scenarioMultiA=as.character(scenarioMultiA),scenarioMultiB=as.character(scenarioMultiB),
                     scenarioMultiA=case_when(is.na(scenarioMultiA)~xGCM,
                                              TRUE~scenarioMultiA),
                     scenarioMultiB=case_when(is.na(scenarioMultiB)~xRCP,
                                              TRUE~scenarioMultiB))
     for(col_i in names(x)){class(t1[[col_i]])<-class(x[[col_i]])}
     if(unique(x$scenarioMultiA)==unique(t1$scenarioMultiA) & unique(x$scenarioMultiB)==unique(t1$scenarioMultiB)){
       commonyears <- unique(x$x)[unique(x$x) %in% unique(t$x)]
       s <- x %>% dplyr::filter(x %in% commonyears) %>% dplyr::bind_rows(t1 %>% dplyr::filter(x %in% commonyears)) %>% tibble::as_tibble();s
       s1 <- s %>% dplyr::select(subRegion,scenario,scenarioMultiA,scenarioMultiB,param,units,aggType,classPalette,class,x,value,region,class2)%>%
         dplyr::mutate(scenario=paste(scenario,"_",param,sep=""))%>%
         dplyr::select(-param,-units,-class,-class2,-classPalette)%>%dplyr::filter(!is.na(x));s1
       s2 <- s1 %>% tidyr::spread(key="scenario",value="value");s2 %>% as.data.frame() %>% head()
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
                       subRegType=subRegType);head(s3)

       data.table::fwrite(s3,paste(dir,"/poly_Scarcity_",scarcityScen,nameAppend,".csv",sep=""),append=T)
       print(paste("Saving file as: ",dir,"/polyScarcity_",scarcityScen,".csv",sep=""))

     }else{print("Xanthos/Tethys GCM RCP's not the same so skipping...")}
    }
 }
}


  return(poly)

} # Close Function
