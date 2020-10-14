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

  NULL->lat->lon->gridx->area -> areaRatio -> subRegAreaSum->GridByPolyID->gridCellArea->maxAreaDuplicates->id->
    Var1->Var2


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

  # Check if Irregular grid then shift onto regular grid
  # Assum regular grid is centered at 0 and the lat determines dimension for both lat and lon
  lat_diff <- unique(round(diff(unique(sort(gridx$lat))),4)); lat_diff[!lat_diff %in% 0]
  lon_diff <- unique(round(diff(unique(sort(gridx$lon))),4)); lon_diff[!lon_diff %in% 0]
  if(length(lat_diff)==0 & length(lon_diff)==0){stop("grid cell spacing is 0. Check input grid data")}
  if(length(lat_diff)==0 & length(lon_diff)!=0){print(paste("Lat spacing is 0. Setting to lon spacing: ",lon_diff,sep=""));
                                lat_diff<-lon_diff}
  if(length(lat_diff)!=0 & length(lon_diff)==0){print(paste("Lon spacing is 0. Setting to lat spacing: ",lat_diff,sep=""));
    lon_diff=lat_diff}

  if(length(lat_diff[!lat_diff %in% 0])>1 | length(lon_diff[!lon_diff %in% 0])>1){

    # Grid Shift
    dimGridLat <- round(min(lat_diff[!lat_diff %in% 0]),4); dimGridLat
    lat=c(seq(from=(0+dimGridLat/2),to=85,by=dimGridLat),
          seq(from=(0-dimGridLat/2),to=-56,by=-dimGridLat))
    lon=c(seq(from=(0+dimGridLat/2),to=180,by=dimGridLat),
          seq(from=(0-dimGridLat/2),to=-180,by=-dimGridLat))

    grid00833X <- tibble::as_tibble(expand.grid(lat,lon)) %>%
      dplyr::rename(lat=Var1,lon=Var2) %>%
      dplyr::mutate(id=dplyr::n())

    listOfGridCells <- grid00833X

    listOfGridCells <- listOfGridCells %>%
      dplyr::rename(
        gridlat = lat,
        gridlon = lon,
        gridID = id)

    latmin<-min(listOfGridCells$gridlat); latmin
    latmax<-max(listOfGridCells$gridlat); latmax
    lonmin<-min(listOfGridCells$gridlon); lonmin
    lonmax<-max(listOfGridCells$gridlon); lonmax

    latranked<-listOfGridCells$gridlat[sort.list(listOfGridCells$gridlat)]%>%
      unique()
    lonranked<-listOfGridCells$gridlon[sort.list(listOfGridCells$gridlon)]%>%
      unique()

    # Understand the characteristcs of the chosen grid (Used to certify that grid is equally spaced)
    # This assumes equally spaced grids by degree.
    gridDimlat<-min(abs(latranked[2:length(latranked)]-latranked[1:length(latranked)-1])); gridDimlat
    gridDimlon<-min(abs(lonranked[2:length(lonranked)]-lonranked[1:length(lonranked)-1])); gridDimlon
    gridShiftlat<-latranked[sort.list(abs(latranked))][1]; gridShiftlat  # The latitude of the center of the grid cells closest to the equator
    gridShiftlon<-lonranked[sort.list(abs(lonranked))][1]; gridShiftlon  # The longitude of the center of the grid cells closest to prime meridian, Greenwich Meridian
    listOfGridCells$gridlat<-round(listOfGridCells$gridlat, digits = 10)
    listOfGridCells$gridlon<-round(listOfGridCells$gridlon, digits = 10)

    # Re-organizing dataset and shifting lat/lon locations into equally spaced grids
    gridx<-gridx%>%
      dplyr::mutate(lat = round(gridDimlat*round(lat*(1/gridDimlat)-(gridShiftlat/gridDimlat))+gridShiftlat, digits = 10),
                    lon = round(gridDimlon*round(lon*(1/gridDimlon)-(gridShiftlon/gridDimlon))+gridShiftlon, digits = 10)) %>%
      tidyr::complete(lat=latranked,lon=lonranked,fill=list(value=0));gridx
  }
  lat_diff <- unique(round(diff(unique(sort(gridx$lat))),4)); lat_diff[!lat_diff %in% 0]
  lon_diff <- unique(round(diff(unique(sort(gridx$lon))),4)); lon_diff[!lon_diff %in% 0]

  if(length(lat_diff)==0 & length(lon_diff)==0){stop("grid cell spacing is 0. Check input grid data")}
  if(length(lat_diff)==0 & length(lon_diff)!=0){print(paste("Lat spacing is 0. Setting to lon spacing: ",lon_diff,sep=""));
    lat_diff<-lon_diff}
  if(length(lat_diff)!=0 & length(lon_diff)==0){print(paste("Lon spacing is 0. Setting to lat spacing: ",lat_diff,sep=""));
    lon_diff=lat_diff}


  spdf = sp::SpatialPointsDataFrame(sp::SpatialPoints(coords=(cbind(gridx$lon,gridx$lat))),data=gridx%>%dplyr::select(lat,lon))
  #pointsx <- sp::points2grid(points=spdf, tolerance=0., round); pointsx
  #spdf <- sp::SpatialPixelsDataFrame(points=pointsx,data=gridx%>%dplyr::select(lat,lon))
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
  if(is.null(rCropPx)){stop("No intersection between grid and shape provided.")}
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
  dplyr::distinct()%>%
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
