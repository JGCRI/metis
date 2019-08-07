#' metis.gridByPoly
#'
#' This function finds the grids located within a given shapefiles regions
#'
#' @param grid Default = NULL. Full path to grid file.
#' @param boundaryRegShpFolder Default = NULL,
#' @param boundaryRegShpFile Default = NULL,
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

metis.gridByPoly <- function(grid = NULL,
                              boundaryRegShpFolder = NULL,
                              boundaryRegShpFile = NULL,
                              colName = NULL,
                              dirOutputs = paste(getwd(),"/outputs",sep=""),
                              fname = "gridByPoly",
                              saveFile = F){

  NULL->lat->lon


#----------------
# Create Folders
#---------------

  if (!dir.exists(dirOutputs)){dir.create(dirOutputs)}
  if (!dir.exists(paste(dirOutputs, "/GridByPoly", sep = ""))){dir.create(paste(dirOutputs, "/GridByPoly", sep = ""))}
  dir = paste(dirOutputs, "/GridByPoly", sep = "")


# Check inputs provided
  if(is.null(colName)){stop("Must provide correct colName from shapeFile data.")}
  if(is.null(grid)){stop("Must provide gridfile csv with lat and lon.")}

  if(!is.null(boundaryRegShpFolder) & !is.null(boundaryRegShpFile)){
      if(!dir.exists(boundaryRegShpFolder)){
        stop("Shapefile folder: ", boundaryRegShpFolder ," is incorrect or doesn't exist.",sep="")}
      if(!file.exists(paste(boundaryRegShpFolder,"/",boundaryRegShpFile,".shp",sep=""))){
        stop("Shape file: ", paste(boundaryRegShpFolder,"/",boundaryRegShpFile,".shp",sep="")," is incorrect or doesn't exist.",sep="")}
      boundaryRegShape=rgdal::readOGR(dsn=boundaryRegShpFolder,layer=boundaryRegShpFile,use_iconv=T,encoding='UTF-8')
      print(paste("Boundary Shape : ",boundaryRegShpFolder,"/",boundaryRegShpFile,".shp",sep=""))
      print(raster::head(boundaryRegShape))
    } else {"Must provided boundaryRegShpFolder and boundaryRegShpFile"} # close if(!is.null(boundaryRegShpFolder) & !is.null(boundaryRegShpFile))


# Prepare grid
gridx<-data.table::fread(grid)
gridxspdf = sp::SpatialPointsDataFrame(sp::SpatialPoints(coords=(cbind(gridx$lon,gridx$lat))),data=gridx)
sp::gridded(gridxspdf)<-TRUE
r<-raster::stack(gridxspdf)

#prepare Shape
shape<-rgdal::readOGR(dsn=boundaryRegShpFolder,layer=boundaryRegShpFile,use_iconv=T,encoding='UTF-8')

# Get 1% extended boundaries around shape
shapeExpandEtxent<-as.data.frame(sp::bbox(shape))   # Get Bounding box
expandbboxPercent<-1; shapeExpandEtxent$min;shapeExpandEtxent$max
shapeExpandEtxent$min[1]<-if(shapeExpandEtxent$min[1]<0){(1+expandbboxPercent/100)*shapeExpandEtxent$min[1]}else{(1-expandbboxPercent/100)*shapeExpandEtxent$min[1]};
shapeExpandEtxent$min[2]<-if(shapeExpandEtxent$min[2]<0){(1+expandbboxPercent/100)*shapeExpandEtxent$min[2]}else{(1-expandbboxPercent/100)*shapeExpandEtxent$min[2]};
shapeExpandEtxent$max[1]<-if(shapeExpandEtxent$max[1]<0){(1-expandbboxPercent/100)*shapeExpandEtxent$max[1]}else{(1+expandbboxPercent/100)*shapeExpandEtxent$max[1]};
shapeExpandEtxent$max[2]<-if(shapeExpandEtxent$max[2]<0){(1-expandbboxPercent/100)*shapeExpandEtxent$max[2]}else{(1+expandbboxPercent/100)*shapeExpandEtxent$max[2]};
shapeExpandEtxent$min;shapeExpandEtxent$max;
shapeExpandEtxent<-methods::as(raster::extent(as.vector(t(shapeExpandEtxent))), "SpatialPolygons")
sp::proj4string(shapeExpandEtxent)<-sp::CRS(sp::proj4string(shape)) # ASSIGN COORDINATE SYSTEM
rcrop<-raster::crop(r,shapeExpandEtxent)
rcropP<-raster::rasterToPolygons(rcrop)
sp::proj4string(rcropP)<-sp::proj4string(shape)

# Intersect grid polygons with shape to get IDs
x<-raster::intersect(shape,rcropP)
gridByPoly<-x@data %>%
  dplyr::select(colName,lat,lon)

# Save Data
if(saveFile){
fname<-gsub(".csv","",fname)
fname<- paste(dir,"/",fname,".csv",sep="")
data.table::fwrite(gridByPoly, file = fname,row.names = F)}

return(gridByPoly)

}
