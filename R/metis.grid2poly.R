#' metis.grid2poly
#'
#' This function takes a .csv file with gridded lat, long data and aggregates
#' the data by spatial boundaries given different shapefiles.
#' @return A table with data by polygon ID for each shapefile provided
#' @keywords gcam, gcam database, query
#' @param grid  Default=NULL,
#' @param boundaryRegShape  Default=NULL,
#' @param subRegShape  Default=NULL,
#' @param boundaryRegShpFolder  Default=paste(getwd(),"/dataFiles/gis/admin_gadm36",sep  Default=""),
#' @param boundaryRegShpFile  Default=paste("gadm36_0",sep  Default=""),
#' @param boundaryRegCol  Default="NAME_0",
#' @param boundaryRegionsSelect  Default=NULL,
#' @param subRegShpFolder  Default=paste(getwd(),"/dataFiles/gis/admin_gadm36",sep  Default=""),
#' @param subRegShpFile  Default=paste("gadm36_1",sep  Default=""),
#' @param subRegCol  Default="NAME_1",
#' @param subRegionsSelect  Default=NULL,
#' @param subRegType  Default="subRegType",
#' @param aggType  Default=NULL,
#' @param dirOutputs  Default=paste(getwd(),"/outputs",sep  Default=""),
#' @param nameAppend  Default="",
#' @param expandbboxPercent  Default=2,
#' @param extension  Default=T,
#' @export

metis.grid2poly<- function(grid=NULL,
                         boundaryRegShape=NULL,
                         boundaryRegShpFolder=paste(getwd(),"/dataFiles/gis/admin_gadm36",sep=""),
                         boundaryRegShpFile=paste("gadm36_0",sep=""),
                         boundaryRegCol="NAME_0",
                         boundaryRegionsSelect=NULL,
                         subRegShape=NULL,
                         subRegShpFolder=paste(getwd(),"/dataFiles/gis/admin_gadm36",sep=""),
                         subRegShpFile=paste("gadm36_1",sep=""),
                         subRegCol="NAME_1",
                         subRegionsSelect=NULL,
                         subRegType="subRegType",
                         aggType=NULL,
                         dirOutputs=paste(getwd(),"/outputs",sep=""),
                         nameAppend="",
                         expandbboxPercent=2,
                         extension=T) {

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
  param->shpRegCol->subReg->griddataTables->tbl->key->value->.->classPalette->lat->lon

#----------------
# Check input data format
#---------------

  if(!is.null(grid)){
  if(all(!class(grid) %in% c("tbl_df","tbl","data.frame"))){
    if(any(grepl(".csv",paste(class(grid))))){
    print("Attempting to read grid csv file ",grid)
      if(file.exists(grid)){
    grid<-utils::read.csv(grid, stringsAsFactors = F)}else{
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

# GCAM Basin
gcamBasinShpFolder <-paste(getwd(),"/dataFiles/gis/basin_gcam",sep="")
gcamBasinShpFile <-"Global235_CLM_final_5arcmin_multipart"
  if(!is.null(gcamBasinShpFolder) & !is.null(gcamBasinShpFile)){
    if(!dir.exists(gcamBasinShpFolder)){
      stop("Shapefile folder: ", gcamBasinShpFolder ," is incorrect or doesn't exist.",sep="")}
    if(!file.exists(paste(gcamBasinShpFolder,"/",gcamBasinShpFile,".shp",sep=""))){
      stop("Shape file: ", paste(gcamBasinShpFolder,"/",gcamBasinShpFile,".shp",sep="")," is incorrect or doesn't exist.",sep="")}
    gcamBasinShape=rgdal::readOGR(dsn=gcamBasinShpFolder,layer=gcamBasinShpFile,use_iconv=T,encoding='UTF-8')
    print(paste("Sub Reg Shape : ",gcamBasinShpFolder,"/",gcamBasinShpFile,".shp",sep=""))
    print(raster::head(gcamBasinShape))
  } # if(!is.null(gcamBasinShpFolder) & !is.null(gcamBasinShpFile)){

  if(is.null(boundaryRegShape)){
    if(!is.null(boundaryRegShpFolder) & !is.null(boundaryRegShpFile)){
  if(!dir.exists(boundaryRegShpFolder)){
    stop("Shapefile folder: ", boundaryRegShpFolder ," is incorrect or doesn't exist.",sep="")}
  if(!file.exists(paste(boundaryRegShpFolder,"/",boundaryRegShpFile,".shp",sep=""))){
    stop("Shape file: ", paste(boundaryRegShpFolder,"/",boundaryRegShpFile,".shp",sep="")," is incorrect or doesn't exist.",sep="")}
  boundaryRegShape=rgdal::readOGR(dsn=boundaryRegShpFolder,layer=boundaryRegShpFile,use_iconv=T,encoding='UTF-8')
  print(paste("Boundary Shape : ",boundaryRegShpFolder,"/",boundaryRegShpFile,".shp",sep=""))
  print(raster::head(boundaryRegShape))
  } # close if(!is.null(boundaryRegShpFolder) & !is.null(boundaryRegShpFile))
  }

  if(is.null(subRegShape)){
    if(!is.null(subRegShpFolder) & !is.null(subRegShpFile)){
    if(!dir.exists(subRegShpFolder)){
      stop("Shapefile folder: ", subRegShpFolder ," is incorrect or doesn't exist.",sep="")}
    if(!file.exists(paste(subRegShpFolder,"/",subRegShpFile,".shp",sep=""))){
      stop("Shape file: ", paste(subRegShpFolder,"/",subRegShpFile,".shp",sep="")," is incorrect or doesn't exist.",sep="")}
    subRegShape=rgdal::readOGR(dsn=subRegShpFolder,layer=subRegShpFile,use_iconv=T,encoding='UTF-8')
    print(paste("Sub Reg Shape : ",subRegShpFolder,"/",subRegShpFile,".shp",sep=""))
    print(raster::head(subRegShape))
    } # if(!is.null(subRegShpFolder) & !is.null(subRegShpFile)){
  }



  if(is.null(boundaryRegShape) & is.null(subRegShape)){
    stop("No valid boundary or subregional shape file available")}

  if(!is.null(boundaryRegShape) & !is.null(subRegShape)){
    sp::proj4string(boundaryRegShape) <- sp::proj4string(subRegShape)
  }

  poly<-tibble::tibble()

  for (region_i in boundaryRegionsSelect){

#----------------
# Create Folders
#---------------

if (!dir.exists(dirOutputs)){dir.create(dirOutputs)}
if (!dir.exists(paste(dirOutputs, "/Maps", sep = ""))){dir.create(paste(dirOutputs, "/Maps", sep = ""))}
if (!dir.exists(paste(dirOutputs, "/Maps/Boundaries", sep = ""))){dir.create(paste(dirOutputs, "/Maps/Boundaries", sep = ""))}
if (!dir.exists(paste(dirOutputs, "/Maps/Boundaries/",region_i, sep = ""))){dir.create(paste(dirOutputs, "/Maps/Boundaries/",region_i,sep = ""))}
if (!dir.exists(paste(dirOutputs, "/Maps/Tables", sep = ""))){dir.create(paste(dirOutputs, "/Maps/Tables", sep = ""))}
dir=paste(dirOutputs, "/Maps/Boundaries/",region_i,sep = "")

#----------------
# Create Boundary and subRegional shapefiles
#---------------

  if(!is.null(subRegShape) & !is.null(boundaryRegShape)){
    boundaryShape<-boundaryRegShape[which(boundaryRegShape[[boundaryRegCol]] %in% boundaryRegionsSelect),]
    boundaryShape@data<-droplevels(boundaryShape@data)
    shape <- raster::intersect(subRegShape,boundaryShape)
    #shape@data<-shape@data%>%dplyr::select(subRegCol,boundaryRegCol)%>%unique
     if(is.null(shape)){stop("Boundary and subregion files do not intersect.")}
    }

  if(is.null(subRegShape) & !is.null(boundaryRegShape)){
      boundaryShape<-boundaryRegShape[which(boundaryRegShape[[boundaryRegCol]] %in% boundaryRegionsSelect),]
      boundaryShape@data<-droplevels(boundaryShape@data)
      shape<-boundaryRegShape[which(boundaryRegShape[[boundaryRegCol]] %in% boundaryRegionsSelect),]
      #shape@data<-shape@data%>%dplyr::select(subRegCol,boundaryRegCol)%>%unique
      shape@data<-droplevels(shape@data)
      }

  if(!is.null(subRegShape) & is.null(boundaryRegShape)){
    shape<-subRegShape[which(subRegShape[[boundaryRegCol]] %in% boundaryRegionsSelect),]
    shape@data<-droplevels(shape@data)
  }

   shape@data=shape@data%>%dplyr::select(names(shape)[names(shape) %in% c(boundaryRegCol,subRegCol)])

   boundaryShapex<-boundaryShape
   boundaryShapex@data<-boundaryShapex@data%>%dplyr::rename("region"=boundaryRegCol)
   shapex<-shape
   shapex@data<-shapex@data%>%dplyr::rename("subRegion"=subRegCol)%>%dplyr::mutate(region=boundaryRegionsSelect)
   rgdal::writeOGR(obj=boundaryShapex, dsn=dir, layer=paste(boundaryRegionsSelect,"_Boundary",nameAppend,sep=""), driver="ESRI Shapefile", overwrite_layer=TRUE)
   rgdal::writeOGR(obj=shapex, dsn=dir, layer=paste(boundaryRegionsSelect,"_Subregion_",subRegType,nameAppend,sep=""), driver="ESRI Shapefile", overwrite_layer=TRUE)

   # Crop GCAM Basins to shape boundary and set projection
   if(!is.null(gcamBasinShape) & extension==T){
     if(!is.null(boundaryShape)){
     sp::proj4string(gcamBasinShape) <- sp::proj4string(boundaryShape)
     gcamBasinShapeCropped<-raster::intersect(gcamBasinShape,boundaryShape)}else{
       print("BoundaryShape not provided. Not cropping GCAM Basins.")
     }
     }


#----------------
# Create Boundary Extension
#---------------

    if(extension){

    bbox1<-as.data.frame(sp::bbox(boundaryShape))   # Get Bounding box
    expandbboxPercent<-expandbboxPercent; bbox1$min;bbox1$max
    bbox1$min[1]<-if(bbox1$min[1]<0){(1+expandbboxPercent/100)*bbox1$min[1]}else{(1-expandbboxPercent/100)*bbox1$min[1]};
    bbox1$min[2]<-if(bbox1$min[2]<0){(1+expandbboxPercent/100)*bbox1$min[2]}else{(1-expandbboxPercent/100)*bbox1$min[2]};
    bbox1$max[1]<-if(bbox1$max[1]<0){(1-expandbboxPercent/100)*bbox1$max[1]}else{(1+expandbboxPercent/100)*bbox1$max[1]};
    bbox1$max[2]<-if(bbox1$max[2]<0){(1-expandbboxPercent/100)*bbox1$max[2]}else{(1+expandbboxPercent/100)*bbox1$max[2]};
    bbox1$min;bbox1$max;
    bbox1<-methods::as(raster::extent(as.vector(t(bbox1))), "SpatialPolygons")
    sp::proj4string(bbox1)<-sp::CRS(sp::proj4string(shape)) # ASSIGN COORDINATE SYSTEM


    if(!is.null(boundaryRegShape)){
      shapeExtended <- raster::crop(boundaryRegShape,bbox1)
      shapeExtended@data<-droplevels(shapeExtended@data)
    }

    if(!is.null(subRegShape) & is.null(boundaryRegShape)){
      shapeExtended <- raster::crop(subRegShape,bbox1)
      shapeExtended@data<-droplevels(shapeExtended@data)
    }

}

#----------------
# Modify Maps For Particular Regions if Data Available
#---------------

    if(region_i=="Argentina"){  # FOR ARGENTINA MERGE Ciudad de Buenos Aires into Buenos Aires
      if(all(c("Ciudad de Buenos Aires","Buenos Aires") %in% unique(shape@data[[subRegCol]]))){
      shape@data[[subRegCol]][which(shape@data[[subRegCol]]=="Ciudad de Buenos Aires")]<-"Buenos Aires"
      shape<-raster::aggregate(shape, by= subRegCol)
      shape@data<-droplevels(shape@data)}
    }


#----------------
# Save boundary maps
#---------------


# Base Maps
metis.map(dataPolygon=shape,fileName = paste(boundaryRegionsSelect,"_",subRegType,"_map_blank",nameAppend,sep=""),dirOutputs = dir)
metis.map(dataPolygon=shape,fileName = paste(boundaryRegionsSelect,"_",subRegType,"_map_Filled",nameAppend,sep=""),dirOutputs = dir,
           fillColumn = subRegCol)
metis.map(dataPolygon=shape,fileName = paste(boundaryRegionsSelect,"_",subRegType,"_map_blank_Labels",nameAppend,sep=""),dirOutputs = dir,
           fillColumn = subRegCol,fillPalette = "white", labels=T)
metis.map(dataPolygon=shape,fileName = paste(boundaryRegionsSelect,"_",subRegType,"_map_Filled_Labels",nameAppend,sep=""),dirOutputs = dir,
           fillColumn = subRegCol,labels=T)

if(extension){
# Extended Boundaries
underLayer<-metis.map(dataPolygon=shapeExtended,dirOutputs = dir,printFig=F,
                      fillColumn = boundaryRegCol,labels=T, fillPalette = "grey75", bgColor = "lightblue1", frameShow=T, labelsSize=0.7, labelsColor="grey30")
boundaryregion<-metis.map(dataPolygon=boundaryShape,dirOutputs = dir,
        fillColumn = boundaryRegCol, fillPalette = "cornsilk1", labels=T,printFig = F)
#  Extended Map Highlight
metis.map(dataPolygon=boundaryregion,fileName = paste(boundaryRegionsSelect,"_mapRegionHighlight",nameAppend,sep=""),dirOutputs = dir,
        underLayer = underLayer,bgColor="lightblue1",frameShow=T)
metis.map(dataPolygon=shape,fileName = paste(boundaryRegionsSelect,"_",subRegType,"_ExtendedMap_Filled_Labels",nameAppend,sep=""),dirOutputs = dir,
        fillColumn = subRegCol,labels=T,underLayer = underLayer,bgColor="lightblue1",frameShow=T)
metis.map(dataPolygon=shape,fileName = paste(boundaryRegionsSelect,"_",subRegType,"_ExtendedMap_Filled",nameAppend,sep=""),dirOutputs = dir,
        fillColumn = subRegCol,labels=F,underLayer = underLayer,bgColor="lightblue1",frameShow=T)

#  Extended Map Highlight
underLayer<-metis.map(dataPolygon=boundaryregion,fileName = paste(boundaryRegionsSelect,"_mapRegionHighlight",nameAppend,sep=""),dirOutputs = dir,
                    underLayer = underLayer,bgColor="lightblue1",frameShow=T,printFig=F)
metis.map(dataPolygon=gcamBasinShapeCropped,fileName = paste(boundaryRegionsSelect,"_mapRegionHighlightGCAMBASINS",nameAppend,sep=""),dirOutputs = dir,
        underLayer = underLayer,borderColor="red",bgColor="lightblue1",frameShow=T)
}

print(paste("Boundary files saved to: ",dir,sep=""))

#----------------
# Read in shapefiles and check format
#---------------

if(!is.null(grid)){

  gridCropped<-tibble::tibble()

  if(!"aggType" %in% names(grid)){
    print("Column aggType is missing from grid data. Assigning aggType='depth'")
    grid<-grid%>%dplyr::mutate(aggType="depth")}

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

    rcrop<-raster::intersect(r,shapeExpandEtxent)
    rcropP<-raster::rasterToPolygons(rcrop)

    gridCropped<-dplyr::bind_rows(gridCropped,tibble::as_tibble(rcropP@data))

    sp::proj4string(rcropP)<-sp::proj4string(shape)
    rcropPx<-raster::intersect(shape,rcropP)

    metis.map(dataPolygon=rcropPx,fileName = paste(boundaryRegionsSelect,"_",subRegType,"_map_GridSize_Labels",nameAppend,sep=""),
            dirOutputs = dir,
            overLayer = metis.map(dataPolygon=shape,fillColumn = subRegCol,
                                 fillPalette = "white",alpha=0,
                                 labels=T,printFig=F))

    metis.map(dataPolygon=rcropPx,fileName = paste(boundaryRegionsSelect,"_",subRegType,"_map_GridSize",nameAppend,sep=""),
            dirOutputs = dir,
            overLayer = metis.map(dataPolygon=shape,fillColumn = subRegCol,
                                fillPalette = "white",alpha=0,
                                labels=F,printFig = F))

    if(aggType_i=="depth"){
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
      dplyr::rename(subRegion:= subRegCol)%>%
      dplyr::mutate(region=boundaryRegionsSelect)%>%
      dplyr::filter(!is.na(x))

    poly<-dplyr::bind_rows(poly,polyx@data)

  } # Close loop for aggType
} # Close loop for param_i

}else{print("No grid provided.")}

# Save Cropped Grid

gridCropped<-tidyr::gather(gridCropped,key=key,value=value,-c(lat,lon))%>%
  tidyr::separate(col="key",into=names(grid)[!names(grid) %in% c("lat","lon","value")],sep="_")

for(colx in names(gridCropped)){
  if(is.character(gridCropped[[colx]])){
    gridCropped[[colx]]<-gsub("XSPACEX"," ",gridCropped[[colx]])
    gridCropped[[colx]]<-gsub("XLPARENTHX","\\(",gridCropped[[colx]])
    gridCropped[[colx]]<-gsub("XRLPARENTHX","\\)",gridCropped[[colx]])
    gridCropped[[colx]]<-gsub("XPALUNDERX","pal\\_",gridCropped[[colx]])
  }
}

polyType=subRegType
utils::write.csv(gridCropped%>%dplyr::mutate(region=region_i,polyType=polyType),
                file = paste(dirOutputs, "/Grids/gridCropped_",region_i,"_",polyType,nameAppend,".csv", sep = ""),row.names = F)



  }# Close for boundaryRegionsSelect
#----------------
# Save template, csv and .RDATA
#---------------

  if(nrow(poly)>0){


    if (!dir.exists(paste(getwd(),"/dataFiles", sep = ""))){
      dir.create(paste(getwd(),"/dataFiles", sep = ""))}  # dataFiles directory (should already exist)
    if (!dir.exists(paste(getwd(),"/dataFiles/mapping", sep = ""))){
      dir.create(paste(getwd(),"/dataFiles/mapping", sep = ""))}  # mapping directory
    utils::write.csv(poly %>% dplyr::filter(region == region_i) %>%
                       dplyr::select(param,units,class,classPalette),
                     file = paste(getwd(),"/dataFiles/mapping/template_subRegional_mapping.csv", sep = ""),row.names = F)


  for (region_i in boundaryRegionsSelect[(boundaryRegionsSelect %in% unique(poly$region))]) {
    utils::write.csv(poly %>% dplyr::filter(region == region_i) %>%
                       dplyr::select(scenario,param,units,class,value,x,subRegion,subRegType,region)%>%
                       dplyr::mutate(value=0,x=2015)%>%unique,
                     file = paste(dirOutputs, "/Maps/Tables/subReg_",region_i,"_",subRegType,"_template",nameAppend,".csv", sep = ""),row.names = F)
    utils::write.csv(poly %>% dplyr::filter(region == region_i) %>%
                       dplyr::select(scenario,param,units,class,value,x,subRegion,subRegType,region,classPalette),
                     file = paste(dirOutputs, "/Maps/Tables/subReg_origData_byClass_",region_i,"_",subRegType,"_origDownscaled",nameAppend,".csv", sep = ""),row.names = F)
    }

    print(paste("Subregional Polygon template .csv files written to: ",dirOutputs, "/Maps/Tables/subReg_",region_i,"_template",nameAppend,".csv", sep = ""))
    print(paste("Subregional Polygon data .csv files written to: ",dirOutputs, "/Maps/Tables/subReg_origData_byClass_",region_i,"_",subRegType,"_origDownscaled",nameAppend,".csv", sep = ""))

  }else{print("Polygon data has 0 rows")}

    return(poly)

} # Close Function
