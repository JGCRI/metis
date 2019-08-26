#' metis.gridByPoly
#'
#' This function finds the grids located within a given shapefiles regions
#'
#' @param gridDataTables Default = NULL. Full path to grid file.
#' @param shape Default = NULL,
#' @param shapeFolder Default = NULL,
#' @param shapeFile Default = NULL,
#' @param colName Default = NULL,
#' @param dirOutputs Default = paste(getwd(),"/outputs",sep=""),
#' @param fname Default = "gridByPoly"
#' @param saveFile Default = F. If want csv output then change to T
#' @keywords grid, shape, polygon
#' @return Prints out graphic
#' @export

#-------------
# Print to PDF or PNG
#-------------

metis.gridByPoly <- function(gridDataTables = NULL,
                              shape = NULL,
                              shapeFolder = NULL,
                              shapeFile = NULL,
                              colName = NULL,
                              dirOutputs = paste(getwd(),"/outputs",sep=""),
                              fname = "gridByPoly",
                              saveFile = F){

  # gridDataTables = NULL
  # shape = NULL
  # shapeFolder = NULL
  # shapeFile = NULL
  # colName = NULL
  # dirOutputs = paste(getwd(),"/outputs",sep="")
  # fname = "gridByPoly"
  # saveFile = F

  NULL->lat->lon


#----------------
# Create Folders
#---------------

  if (!dir.exists(dirOutputs)){dir.create(dirOutputs)}
  if (!dir.exists(paste(dirOutputs, "/GridByPoly", sep = ""))){dir.create(paste(dirOutputs, "/GridByPoly", sep = ""))}
  dir = paste(dirOutputs, "/GridByPoly", sep = "")


# Check inputs provided

  if(is.null(shape)){
    if(!is.null(shapeFolder) & !is.null(shapeFile)){
      if(!dir.exists(shapeFolder)){
        stop("Shapefile folder: ", shapeFolder ," is incorrect or doesn't exist.",sep="")}
      if(!file.exists(paste(shapeFolder,"/",shapeFile,".shp",sep=""))){
        stop("Shape file: ", paste(shapeFolder,"/",shapeFile,".shp",sep="")," is incorrect or doesn't exist.",sep="")}
      shape=rgdal::readOGR(dsn=shapeFolder,layer=shapeFile,use_iconv=T,encoding='UTF-8')
      print(paste("Boundary Shape : ",shapeFolder,"/",shapeFile,".shp",sep=""))
      print(raster::head(shape))
    } # close if(!is.null(shapeFolder) & !is.null(shapeFile))
  }else{shape=shape}

# Prepare grid

  if(!is.null(gridDataTables)){

    if(all(!class(gridDataTables) %in% c("tbl_df","tbl","data.frame"))){

      for(grid_i in gridDataTables){
        if(file.exists(grid_i)){
          gridxNew<-data.table::fread(paste(grid_i),encoding="Latin-1")%>%tibble::as_tibble()
          gridx<-dplyr::bind_rows(gridx,gridxNew)
          rm(gridxNew)
        } else {stop(paste(grid_i," does not exist"))}
      }
    }else{gridx<-gridDataTables}

  }else{gridx=gridDataTables}

  if(is.null(colName)){stop("Must provide correct colName from shapeFile data.")} else{
    if(!colName %in% names(shape@data)){stop("Must provide correct colName from shapeFile data.")}}
  if(is.null(gridDataTables)){stop("Must provide gridfile csv with lat and lon.")}else {
    if(!any(c("lat","lon") %in% names(gridDataTables))){stop("Must provide gridfile csv with lat and lon.")}
  }


  # Convert to Spatial Point Data Frames
  spdf = sp::SpatialPointsDataFrame(sp::SpatialPoints(coords=(cbind(gridx$lon,gridx$lat))),data=gridx)
  sp::gridded(spdf)<-TRUE

  r<-raster::stack(spdf)
  raster::projection(r)<-sp::proj4string(shape)
  shape_ras <- raster::rasterize(shape, r[[1]], getCover=TRUE)
  shape_ras[shape_ras==0] <- NA
  r<-raster::mask(r,shape_ras)
  r<-methods::as(r, "SpatialPixelsDataFrame")
  r@data<-Filter(function(x)!all(is.na(x)), r@data)

  rCrop <- r@data%>%dplyr::select(lat,lon)%>%unique()

# Subset gridded data
gridByPoly<-dplyr::left_join(rCrop,gridx, by=c("lat","lon"))

# Save Data
if(saveFile){
fname<-gsub(".csv","",fname)
fname<- paste(dir,"/",fname,".csv",sep="")
data.table::fwrite(gridByPoly, file = fname,row.names = F)
print(paste("File saved as ",fname,sep=""))}

invisible(gridByPoly)

}
