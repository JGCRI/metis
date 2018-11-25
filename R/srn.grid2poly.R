#' srn.grid2poly
#'
#' This function takes a .csv file with gridded lat, long data and aggregates
#' the data by spatial boundaries given different shapefiles.
#' @return A table with data by polygon ID for each shapefile provided
#' @keywords gcam, gcam database, query
#' @param grid Grid file with at least lat, lon and value columns.
#' @param shape A shapefile in the "SpatialPolygonsDataFrame" format
#' @param subReg Column from shapefile with subregions
#' @param aggType Either "vol" or "depth" indicating how data will be aggregated.
#' @export

srn.grid2poly<- function(grid,shape,subReg,aggType) {

#------------------
# Load required Libraries
# -----------------
#requireNamespace("raster",quietly = T)

#----------------
# Initialize variables by setting to NULL
#----------------


#----------------
# Check input data format
#---------------

  if(any(!c("lat","lon","value") %in% names(grid))){
    stop(paste(grid," should have columns lon, lat and value. Missing columns: ",
               names(grid)[!names(grid) %in% c("lat","lon","value")]))}

  if(!any(aggType %in% c("vol","depth"))){stop("aggType should be either 'vol' or 'depth'.
                                              aggType entered = ",aggType)}

#----------------
# Read in shapefiles and check format
#---------------


  gridx<-grid%>%
    tidyr::unite(col="key",names(grid)[!names(grid) %in% c("lat","lon","value")],sep="_",remove=T)%>%
    tidyr::spread(key=key,value=value)

  # Convert to Spatial Point Data Frames
  spdf = SpatialPointsDataFrame(SpatialPoints(coords=(cbind(gridx$lon,gridx$lat))),data=gridx)
  gridded(spdf)<-TRUE

    r<-raster::stack(spdf)
    projection(r)<-proj4string(shape)

    if(aggType=="depth"){

      rcrop<-raster::intersect(r,shape)
      rcropP<-rasterToPolygons(rcrop)
      rcropPx<-raster::intersect(shape,rcropP)
      rcropPx@data$area<-area(rcropPx)
      s1<-shape
      s1$subRegAreaSum<-area(shape);head(s1)
      s1<-s1@data%>%dplyr::select(subReg,subRegAreaSum);head(s1)
      rcropPx@data<-dplyr::left_join(rcropPx@data,s1,by=subReg)
      rcropPx@data$areaPrcnt<-rcropPx@data$area/rcropPx@data$subRegAreaSum;head(rcropPx)
      x<-data.frame(mapply(`*`,rcropPx@data%>%
              dplyr::select(names(rcropPx@data)[!names(rcropPx@data) %in% c(subReg,"lat","lon","area","subRegAreaSum","areaPrcnt")]),
              rcropPx@data%>%dplyr::select(areaPrcnt),SIMPLIFY=FALSE))%>%
        bind_cols(rcropPx@data%>%dplyr::select(subReg))%>%as_tibble;head(x)
      dxpX<-x%>%group_by(.dots = list(subReg))%>% summarise_all(funs(round(sum(.,na.rm=T),2)))
      head(dxpX)
    }
    if(aggType=="vol"){
      w <- raster::extract(r,shape, method="simple",weights=T, normalizeWeights=F);head(w)
      dfx<-data.frame()
      for (i in seq(w)){
        if(!is.null(w[[i]]))
          x<-as.data.frame(w[[i]])
        x$ID<-shape@data[[subReg]][[i]]
        x1<-data.frame(mapply(`*`,x%>%
                                dplyr::select(names(r)[!names(r) %in% c("lat","lon")]),x%>%
                                dplyr::select(weight),SIMPLIFY=FALSE))%>%
          bind_cols(x%>%dplyr::select(ID));head(x1)
        #assign(paste0("df", i), x)
        dfx<-rbind.data.frame(dfx,x1)
      }
      names(dfx)[names(dfx)=="ID"]<-subReg;head(dfx)
      dxpX<-dfx%>%group_by(.dots = list(subReg))%>% summarise_all(funs(round(sum(.,na.rm=T),2)))%>%as_tibble
    }

    dxpX1<-gather(dxpX,key=key,value=value,-(subReg))%>%
      separate(col="key",into=names(grid)[!names(grid) %in% c("lat","lon","value")],sep="_")

    return(dxpX)

} # Close Function
