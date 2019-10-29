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
#' @param folderName Default ="folderNameDefault",
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
                              folderName="folderNameDefault",
                              saveFile = F){

  # gridDataTables = NULL
  # shape = NULL
  # colName = NULL
  # shapeFolder = NULL
  # shapeFile = NULL
  # dirOutputs = paste(getwd(),"/outputs",sep="")
  # fname = "gridByPoly"
  # folderName="folderNameDefault"
  # saveFile = F

  print(paste("Starting metis.gridByPoly.R...",sep=""))

  NULL->lat->lon->gridx->area -> areaRatio -> subRegAreaSum


#----------------
# Create Folders
#---------------

  if (!dir.exists(dirOutputs)){dir.create(dirOutputs)}

  if(saveFile){
  if (!dir.exists(paste(dirOutputs, "/GridByPoly", sep = ""))){dir.create(paste(dirOutputs, "/GridByPoly", sep = ""))}
  if (!dir.exists(paste(dirOutputs, "/GridByPoly/",folderName, sep = ""))){dir.create(paste(dirOutputs, "/GridByPoly/",folderName, sep = ""))}
  dir = paste(dirOutputs, "/GridByPoly/",folderName, sep = "")}


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

  }else{stop("Must provide a gridDataTable.")}

  if(!is.null(gridx)){
    names2Remove <- c(colName,"gridCellArea","subRegAreaSum","gridCellAreaRatio")[
      c(colName,"gridCellArea","subRegAreaSum","gridCellAreaRatio") %in% names(gridx)]; names2Remove
    gridx<-gridx%>%dplyr::select(-names2Remove)}


  if(is.null(colName)){stop("Must provide correct colName from shapeFile data.")} else{
    if(!colName %in% names(shape@data)){stop("Must provide correct colName from shapeFile data.")}}

  if(is.null(gridx)){stop("Must provide gridfile csv with lat and lon.")}else {
    if(!any(c("lat","lon") %in% names(gridx))){stop("Must provide gridfile csv with lat and lon.")}
  }


  # Convert to Spatial Point Data Frames
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

  rcropPx@data$area<-raster::area(rcropPx)
  s1<-shape
  s1$subRegAreaSum<-raster::area(shape);
  s1<-s1@data%>%dplyr::select( colName,subRegAreaSum);
  if(c("subRegAreaSum") %in% names(rcropPx@data)){rcropPx@data<-rcropPx@data%>%dplyr::select(-subRegAreaSum)}
  rcropPx@data<-dplyr::left_join(rcropPx@data,s1,by= colName)
  rcropPx@data$areaRatio<-rcropPx@data$area/rcropPx@data$subRegAreaSum;

# Subset gridded data
gridByPoly<-rcropPx@data%>%dplyr::select(lat,lon,colName,gridCellArea=area,subRegAreaSum,gridCellAreaRatio=areaRatio)%>%
  dplyr::left_join(gridx, by=c("lat","lon"))%>%
  unique()

# Save Data
if(saveFile){
fname<-gsub(".csv","",fname)
fname<- paste(dir,"/",fname,".csv",sep="")
data.table::fwrite(gridByPoly, file = fname,row.names = F)
print(paste("File saved as ",fname,sep=""))}

print(paste("metis.gridByPoly.R run complete.",sep=""))

invisible(gridByPoly)

}
