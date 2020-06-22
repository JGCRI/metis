#' metis.gridByPoly
#'
#' This function finds the grids located within a given shapefiles regions
#'
#' @param gridTable Default = NULL. Full path to grid file.
#' @param shape Default = NULL,
#' @param shapeFolder Default = NULL,
#' @param shapeFile Default = NULL,
#' @param colName Default = NULL,
#' @param dirOutputs Default = paste(getwd(),"/outputs",sep=""),
#' @param fname Default = "gridByPoly"
#' @param folderName Default ="folderNameDefault",
#' @param saveFile Default = F. If want csv output then change to T
#' @param printFig Default = F. If grid overlap with shape is wanted
#' @keywords grid, shape, polygon
#' @return Prints out graphic
#' @export

#-------------
# Print to PDF or PNG
#-------------

metis.gridByPoly <- function(gridTable = NULL,
                              shape = NULL,
                              shapeFolder = NULL,
                              shapeFile = NULL,
                              colName = NULL,
                              dirOutputs = paste(getwd(),"/outputs",sep=""),
                              fname = "gridByPoly",
                              folderName=NULL,
                              saveFile = F,
                              printFig = F){
#
# gridTable = NULL
# shape = NULL
# colName = NULL
# shapeFolder = NULL
# shapeFile = NULL
# dirOutputs = paste(getwd(),"/outputs",sep="")
# fname = "gridByPoly"
# folderName="folderNameDefault"
# saveFile = F
# printFig =F

  print(paste("Starting metis.gridByPoly.R...",sep=""))

  NULL->lat->lon->gridx->area -> areaRatio -> subRegAreaSum->GridByPolyID->gridCellArea->maxAreaDuplicates


#----------------
# Create Folders
#---------------

  if(saveFile){
  if (!dir.exists(dirOutputs)){dir.create(dirOutputs)}
  if (!dir.exists(paste(dirOutputs,"/",folderName, sep = ""))){dir.create(paste(dirOutputs, "/",folderName,sep = ""))}
  if (!dir.exists(paste(dirOutputs,"/",folderName, "/GridByPoly/", sep = ""))){dir.create(paste(dirOutputs, "/",folderName, "/GridByPoly/", sep = ""))}
    dir = paste(dirOutputs,"/",folderName, "/GridByPoly/",sep = "")
  }


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

  if(!is.null(gridTable)){

    if(all(!class(gridTable) %in% c("tbl_df","tbl","data.frame"))){

      for(grid_i in gridTable){
        if(file.exists(grid_i)){
          gridxNew<-data.table::fread(paste(grid_i),encoding="Latin-1")%>%tibble::as_tibble()
          gridx<-dplyr::bind_rows(gridx,gridxNew)
          rm(gridxNew)
        } else {stop(paste(grid_i," does not exist"))}
      }
    }else{gridx<-gridTable}

  }else{stop("Must provide a gridDataTable.")}

  if(!is.null(gridx)){
    names2Remove <- c(colName,"gridCellArea","subRegAreaSum","gridCellAreaRatio")[
      c(colName,"gridCellArea","subRegAreaSum","gridCellAreaRatio") %in% names(gridx)]; names2Remove
    gridx<-gridx%>%dplyr::select(-tidyselect::all_of(names2Remove))}


  if(is.null(colName)){stop("Must provide correct colName from shapeFile data.")} else{
    if(!colName %in% names(shape@data)){stop("Must provide correct colName from shapeFile data.")}}

  if(is.null(gridx)){stop("Must provide gridfile csv with lat and lon.")}else {
    if(!any(c("lat","lon") %in% names(gridx))){stop("Must provide gridfile csv with lat and lon or x and y")}
  }


  # Extract the gridcells by polygon
  # Create a raster from the grid file
  spdf = sp::SpatialPointsDataFrame(sp::SpatialPoints(coords=(cbind(gridx$lon,gridx$lat))),data=gridx%>%dplyr::select(lat,lon))
  sp::gridded(spdf)<-TRUE
  r<-raster::stack(spdf); r
  # Crop to the shape boundary
  rCrop <- raster::crop(r,shape); rCrop
  # Create a polygon
  rCropP <- raster::rasterToPolygons(rCrop); rCropP
  sp::proj4string(rCropP)<-sp::proj4string(shape)
  # Intersect with the shape
  print(paste("Intersecting raster with shape...",sep=""))
  rCropPx <- raster::intersect(shape,rCropP); rCropPx
  print(paste("Done.",sep=""))

  rCropPx@data$area<-raster::area(rCropPx)
  s1<-shape
  s1$subRegAreaSum<-raster::area(shape);
  s1<-s1@data%>%dplyr::select(!!colName,"subRegAreaSum");
  if(c("subRegAreaSum") %in% names(rCropPx@data)){rCropPx@data<-rCropPx@data%>%dplyr::select(-subRegAreaSum)}
  rCropPx@data<-dplyr::left_join(rCropPx@data,s1,by= colName)
  rCropPx@data$areaRatio<-rCropPx@data$area/rCropPx@data$subRegAreaSum;

# Subset gridded data
gridByPoly<-rCropPx@data%>%dplyr::select(lat,lon,colName,gridCellArea=area,subRegAreaSum,gridCellAreaRatio=areaRatio)%>%
    dplyr::mutate(lat=round(lat,4),lon=round(lon,4))%>%
  dplyr::left_join(gridx %>% dplyr::mutate(lat=round(lat,4),lon=round(lon,4)), by=c("lat","lon"))%>%
  unique()%>%
  tibble::rowid_to_column(var = "GridByPolyID")


nrowOrig = nrow(gridByPoly)
# If multiple regions cut across same grid cells may get duplicate id's
# In this case choose grid cell with larger area
  gridByPoly <- gridByPoly%>%
    dplyr::group_by(GridByPolyID)%>%
    dplyr::mutate(maxAreaDuplicates=max(gridCellArea))%>%
    dplyr::ungroup()%>%
    dplyr::filter(gridCellArea==maxAreaDuplicates)%>%
    dplyr::ungroup()%>%
    dplyr::select(-maxAreaDuplicates)
  if(nrow(gridByPoly)<nrowOrig){print("Multiple shapes overlapped same grid cell. Grid cell assigned to shape with most area in cell.")}

# Save Data
print(paste("Saving data...",sep=""))
if(saveFile){
fname<-gsub(".csv","",fname)
fname<- paste(dir,"/",fname,".csv",sep="")
data.table::fwrite(gridByPoly, file = fname,row.names = F)
print(paste("File saved as ",fname,sep=""))}
if(printFig){metis::metis.printPdfPng(figure=rCropPx, dir=dir,filename = fname)}
print(paste("metis.gridByPoly.R run complete.",sep=""))

invisible(gridByPoly)

}
