#' metis.grid2poly
#'
#' This function takes a .csv file with gridded lat, long data and aggregates
#' the data by spatial boundaries given different shapefiles.
#' @return A table with data by polygon ID for each shapefile provided
#' @keywords gcam, gcam database, query
#' @param grid  Default=NULL,
#' @param subRegShape  Default=NULL,
#' @param boundaryRegionsSelect  Default=NULL,
#' @param subRegShpFolder  Default = NULL, Suggested paste(getwd(),"/dataFiles/gis/naturalEarth",sep  Default="")
#' @param subRegShpFile  Default = NULL, paste("ne_10m_admin_1_states_provinces",sep  Default=""),
#' @param subRegCol  Default = NULL, "NAME_1",
#' @param subRegType  Default="subRegType",
#' @param aggType  Default=NULL,
#' @param dirOutputs  Default=paste(getwd(),"/outputs",sep  Default=""),
#' @param nameAppend  Default="",
#' @param labelsSize Default =1.2
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
                         labelsSize=1.2) {
#
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

#------------------
# Load required Libraries
# -----------------
requireNamespace("raster",quietly = T)
requireNamespace("rgdal",quietly = T)
requireNamespace("tibble",quietly = T)
requireNamespace("dplyr",quietly = T)
requireNamespace("tidyr",quietly = T)
requireNamespace("tmap",quietly = T)


#----------------
# Initialize variables by setting to NULL
#----------------

NULL->subRegAreaSum->areaPrcnt->weight->ID->subRegion->region->scenario->
  param->shpRegCol->subReg->griddataTables->tbl->key->value->.->classPalette->lat->lon->overlapShape->gridPolyLoop

#----------------
# Check input data format
#---------------

  if(!is.null(grid)){
  if(all(!class(grid) %in% c("tbl_df","tbl","data.frame"))){
    if(any(grepl(".csv",paste(grid)))){
    print(paste("Attempting to read grid csv file ",grid,sep=""))
      if(file.exists(grid)){
    grid<-utils::read.csv(grid, stringsAsFactors = F)
    grid<-grid%>%unique()}else{
      stop(paste("Grid file ",grid," does not exist",sep=""))
    }
    }
  }


  if(any(!c("lat","lon","value") %in% names(grid))){
    stop(paste(grid," should have columns lon, lat and value. Missing columns: ",
               names(grid)[!names(grid) %in% c("lat","lon","value")]))}
  } # If !is.null(grid)


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


#----------------
# Read in shapefiles and check format
#---------------

if(!is.null(grid)){

  gridCropped<-tibble::tibble()

  if(!"aggType" %in% names(grid)){
    if(is.null(aggType)){
    print("Column aggType is missing from grid data. Assigning aggType='depth'")
    grid<-grid%>%dplyr::mutate(aggType="depth")}else{
      grid<-grid%>%dplyr::mutate(aggType=aggType)
    }}

  print("setting grid columns ...")
  for(colx in names(grid)){
    if(is.character(grid[[colx]])){
      grid[[colx]]<-gsub(" ","XSPACEX",grid[[colx]])
      grid[[colx]]<-gsub("\\(","XLPARENTHX",grid[[colx]])
      grid[[colx]]<-gsub("\\)","XRLPARENTHX",grid[[colx]])
      grid[[colx]]<-gsub("pal\\_","XPALUNDERX",grid[[colx]])
    }
  }

  for (param_i in unique(grid$param)){

    gridx<-grid%>%dplyr::filter(param==param_i)

  for (aggType_i in unique(grid$aggType)){

  if(!unique(gridx$aggType) %in% c("depth","vol")){stop("Incorrect aggType in grid file")}

  gridx<-gridx%>%dplyr::filter(aggType==aggType_i)%>%
    tidyr::unite(col="key",names(grid)[!names(grid) %in% c("lat","lon","value")],sep="_",remove=T)

  gridx<-gridx%>%tidyr::spread(key=key,value=value)

  # Convert to Spatial Point Data Frames
  spdf = sp::SpatialPointsDataFrame(sp::SpatialPoints(coords=(cbind(gridx$lon,gridx$lat))),data=gridx)
  sp::gridded(spdf)<-TRUE

    r<-raster::stack(spdf)
    raster::projection(r)<-sp::proj4string(shape)

    shapeExpandEtxent<-as.data.frame(sp::bbox(shape))   # Get Bounding box
    expandbboxPercent<-0.5; shapeExpandEtxent$min;shapeExpandEtxent$max
    shapeExpandEtxent$min[1]<-if(shapeExpandEtxent$min[1]<0){(1+expandbboxPercent/100)*shapeExpandEtxent$min[1]}else{(1-expandbboxPercent/100)*shapeExpandEtxent$min[1]};
    shapeExpandEtxent$min[2]<-if(shapeExpandEtxent$min[2]<0){(1+expandbboxPercent/100)*shapeExpandEtxent$min[2]}else{(1-expandbboxPercent/100)*shapeExpandEtxent$min[2]};
    shapeExpandEtxent$max[1]<-if(shapeExpandEtxent$max[1]<0){(1-expandbboxPercent/100)*shapeExpandEtxent$max[1]}else{(1+expandbboxPercent/100)*shapeExpandEtxent$max[1]};
    shapeExpandEtxent$max[2]<-if(shapeExpandEtxent$max[2]<0){(1-expandbboxPercent/100)*shapeExpandEtxent$max[2]}else{(1+expandbboxPercent/100)*shapeExpandEtxent$max[2]};
    shapeExpandEtxent$min;shapeExpandEtxent$max;
    shapeExpandEtxent<-methods::as(raster::extent(as.vector(t(shapeExpandEtxent))), "SpatialPolygons")
    sp::proj4string(shapeExpandEtxent)<-sp::CRS(sp::proj4string(shape)) # ASSIGN COORDINATE SYSTEM

    rcrop<-raster::crop(r,shapeExpandEtxent)
    rcropP<-raster::rasterToPolygons(rcrop)

    gridCropped<-dplyr::bind_rows(gridCropped,tibble::as_tibble(rcropP@data))

    sp::proj4string(rcropP)<-sp::proj4string(shape)
    rcropPx<-raster::crop(shape,rcropP)

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
      print("Aggregating depth ...")
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
      polyDatax<-x%>%dplyr::group_by(.dots = list( subRegCol))%>% dplyr::summarise_all(dplyr::funs(round(sum(.,na.rm=T),2)))
    }
    if(aggType_i=="vol"){
      print("Aggregating vol ...")
      w <- raster::extract(r,shape, method="simple",weights=T, normalizeWeights=F);
      dfx<-data.frame()
      for (i in seq(w)){
        if(!is.null(w[[i]]))
          x<-as.data.frame(w[[i]])
        x$ID<-shape@data[[ subRegCol]][[i]]
        x1<-data.frame(mapply(`*`,x%>%
                                dplyr::select(names(r)[!names(r) %in% c("lat","lon")]),x%>%
                                dplyr::select(weight),SIMPLIFY=FALSE))%>%
          dplyr::bind_cols(x%>%dplyr::select(ID));
        #assign(paste0("df", i), x)
        dfx<-rbind.data.frame(dfx,x1)
      }
      names(dfx)[names(dfx)=="ID"]<- subRegCol;
      polyDatax<-dfx%>%dplyr::group_by(.dots = list( subRegCol))%>% dplyr::summarise_all(dplyr::funs(round(sum(.,na.rm=T),2)))%>%tibble::as_tibble()
    }

    polyData<-tidyr::gather(polyDatax,key=key,value=value,-( subRegCol))%>%
      tidyr::separate(col="key",into=names(grid)[!names(grid) %in% c("lat","lon","value")],sep="_")

    for(colx in names(polyData)){
      if(is.character(polyData[[colx]])){
        polyData[[colx]]<-gsub("XSPACEX"," ",polyData[[colx]])
        polyData[[colx]]<-gsub("XLPARENTHX","\\(",polyData[[colx]])
        polyData[[colx]]<-gsub("XRLPARENTHX","\\)",polyData[[colx]])
        polyData[[colx]]<-gsub("XPALUNDERX","pal\\_",polyData[[colx]])
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

  } # Close loop for aggType
} # Close loop for param_i

}else{print("No grid provided.")}

# Save Cropped Grid
if(!is.null(grid)){
if(nrow(gridCropped)>0){
gridCropped<-tidyr::gather(gridCropped,key=key,value=value,-c(lat,lon))%>%
  tidyr::separate(col="key",into=names(grid)[!names(grid) %in% c("lat","lon","value")],sep="_")%>%
  unique()

for(colx in names(gridCropped)){
  if(is.character(gridCropped[[colx]])){
    gridCropped[[colx]]<-gsub("XSPACEX"," ",gridCropped[[colx]])
    gridCropped[[colx]]<-gsub("XLPARENTHX","\\(",gridCropped[[colx]])
    gridCropped[[colx]]<-gsub("XRLPARENTHX","\\)",gridCropped[[colx]])
    gridCropped[[colx]]<-gsub("XPALUNDERX","pal\\_",gridCropped[[colx]])
  }
}

polyType=subRegType
if (!dir.exists(paste(dirOutputs, "/Grids", sep = ""))){dir.create(paste(dirOutputs, "/Grids", sep = ""))}
utils::write.csv(gridCropped%>%dplyr::mutate(region=boundaryRegionsSelect,polyType=polyType),
                file = paste(dirOutputs, "/Grids/gridCropped_",boundaryRegionsSelect,"_",polyType,nameAppend,".csv", sep = ""),row.names = F)
}} # If null grid


#----------------
# Save template, csv and .RDATA
#---------------

  if(nrow(poly)>0){


    if (!dir.exists(paste(getwd(),"/dataFiles", sep = ""))){
      dir.create(paste(getwd(),"/dataFiles", sep = ""))}  # dataFiles directory (should already exist)
    if (!dir.exists(paste(getwd(),"/dataFiles/mapping", sep = ""))){
      dir.create(paste(getwd(),"/dataFiles/mapping", sep = ""))}  # mapping directory
    utils::write.csv(poly %>% dplyr::filter(region == boundaryRegionsSelect) %>%
                       dplyr::select(param,units,class,classPalette),
                     file = paste(getwd(),"/dataFiles/mapping/template_subRegional_mapping.csv", sep = ""),row.names = F)


  for (boundaryRegionsSelect in boundaryRegionsSelect[(boundaryRegionsSelect %in% unique(poly$region))]) {
    utils::write.csv(poly %>% dplyr::filter(region == boundaryRegionsSelect) %>%
                       dplyr::select(scenario,param,units,class,value,x,subRegion,subRegType,region)%>%
                       dplyr::mutate(value=0,x=2015)%>%unique,
                     file = paste(dirOutputs, "/Maps/Tables/subReg_",boundaryRegionsSelect,"_",subRegType,"_template",nameAppend,".csv", sep = ""),row.names = F)
    utils::write.csv(poly %>% dplyr::filter(region == boundaryRegionsSelect) %>%
                       dplyr::select(scenario,param,units,class,value,x,subRegion,subRegType,region,classPalette),
                     file = paste(dirOutputs, "/Maps/Tables/subReg_origData_byClass_",boundaryRegionsSelect,"_",subRegType,"_origDownscaled",nameAppend,".csv", sep = ""),row.names = F)
    }

    print(paste("Subregional Polygon template .csv files written to: ",dirOutputs, "/Maps/Tables/subReg_",boundaryRegionsSelect,"_template",nameAppend,".csv", sep = ""))
    print(paste("Subregional Polygon data .csv files written to: ",dirOutputs, "/Maps/Tables/subReg_origData_byClass_",boundaryRegionsSelect,"_",subRegType,"_origDownscaled",nameAppend,".csv", sep = ""))

  }else{print("Polygon data has 0 rows")}

    return(poly)

} # Close Function
