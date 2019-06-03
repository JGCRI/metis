#' metis.boundaries
#'
#' This function takes a .csv file with gridded lat, long data and aggregates
#' the data by spatial boundaries given different shapefiles.
#' @return A table with data by polygon ID for each shapefile provided
#' @keywords gcam, gcam database, query
#' @param boundaryRegShape  Default=NULL. Boundary region shape if already read into R.
#' @param subRegShape  Default=NULL. Sub-region shape if already read into R.
#' @param boundaryRegShpFolder  Default= NULL. Folder containing boundary region shapefile. Suggested: paste(getwd(),"/dataFiles/gis/naturalEarth",sep  Default=""),
#' @param boundaryRegShpFile  Default=NULL. Name of shapefile. Suggested: paste("ne_10m_admin_0_countries",sep  Default=""),
#' @param boundaryRegCol  Default=NULL. Column name with region names. Suggested "NAME_0",
#' @param boundaryRegionsSelect  Default=NULL. The region to choose from the given shapefile.
#' @param subRegShpFolder  Default=NULL. Folder containing boundary region shapefile. Suggested paste(getwd(),"/dataFiles/gis/naturalEarth",sep  Default=""),
#' @param subRegShpFile  Default=NULL. Name of sub-region shapefile. Suggested paste("ne_10m_admin_1_states_provinces",sep  Default=""),
#' @param subRegCol  Default= NULL. Suggested for states "name",
#' @param subRegionsSelect  Default=NULL. The region to choose from the given sub-region shapefile.
#' @param subRegType  Default="subRegType". Eg. "states", "basins" etc.
#' @param dirOutputs  Default=paste(getwd(),"/outputs",sep  Default=""). Location for outputs.
#' @param nameAppend  Default="".
#' @param expandPercent  Default=2. Percentage to expand boundary region beyond chosen region.
#' @param overlapShape Default = NULL. If boundary lines of another shapefile are desired specify the shape here.
#' @param overlapShpFolder Default = NULL. For GCAM basins use paste(getwd(),"/dataFiles/gis/basin_gcam",sep="").
#' @param overlapShpFile Default = NULL. For GCAM basins use ="Global235_CLM_final_5arcmin_multipart"
#' @param fillcolorNA Default =NULL.
#' @param labelsSize Default =1.2.
#' @param projX Default ="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0".
#' @param extendedFillColor Default = "grey75".
#' @param extendedBGColor Default = "lightblue1".
#' @param extendedHighLightColor Default = "cornsilk1".
#' @param extendedLabelsColor Default = "grey30".
#' @param extdendedLabelSize Default =0.7.
#' @param extension Default = T
#' @param fillPalette Default ="Spectral".
#' @param cropSubShape2Bound Default = T. Set to False if subregion shape is larger than boundary, but desired fro extension.
#' @param grids Default = NULL. Suggested is c(paste(getwd(),"/dataFiles/grids/emptyGrids/grid_025.csv",sep=""),
#' @param innerMargins Default =c(0,0.1,0,0.1), # bottom, left, top, right
#' @param outerMargins Default =c(0.01,0.01,0.01,0.01) # bottom, left, top, right
#' paste(getwd(),"/dataFiles/grids/emptyGrids/grid_050.csv",sep=""))
#' This may happen in the case of disputed boundaries.
#' @export

metis.boundaries<- function(boundaryRegShape=NULL,
                         boundaryRegShpFolder=NULL,
                         boundaryRegShpFile=NULL,
                         boundaryRegCol=NULL,
                         boundaryRegionsSelect=NULL,
                         subRegShape=NULL,
                         subRegShpFolder=NULL,
                         subRegShpFile=NULL,
                         subRegCol=NULL,
                         subRegionsSelect=NULL,
                         subRegType="subRegType",
                         dirOutputs=paste(getwd(),"/outputs",sep=""),
                         nameAppend="",
                         expandPercent=2,
                         overlapShape=NULL,
                         overlapShpFolder=NULL,
                         overlapShpFile=NULL,
                         labelsSize=1.2,
                         fillcolorNA=NULL,
                         projX="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0",
                         extendedFillColor="grey75",
                         extendedBGColor="lightblue1",
                         extendedHighLightColor="cornsilk1",
                         extendedLabelsColor="grey30",
                         extdendedLabelSize=0.7,
                         extension=T,
                         fillPalette="Spectral",
                         cropSubShape2Bound=T,
                         grids=NULL,
                         innerMargins=c(0,0.1,0,0.1), # bottom, left, top, right
                         outerMargins=c(0.01,0.01,0.01,0.01) # bottom, left, top, right
                         ) {

  # boundaryRegShape=NULL
  # boundaryRegShpFolder=NULL
  # boundaryRegShpFile=NULL
  # boundaryRegCol=NULL
  # boundaryRegionsSelect=NULL
  # subRegShape=NULL
  # subRegShpFolder=NULL
  # subRegShpFile=NULL
  # subRegCol=NULL
  # subRegionsSelect=NULL
  # subRegType="subRegType"
  # dirOutputs=paste(getwd(),"/outputs",sep="")
  # nameAppend=""
  # expandPercent=2
  # overlapShape=NULL
  # overlapShpFolder=NULL
  # overlapShpFile=NULL
  # labelsSize=1.2
  # fillcolorNA=NULL
  # projX="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=000"
  # extendedFillColor="grey75"
  # extendedBGColor="lightblue1"
  # extendedHighLightColor="cornsilk1"
  # extendedLabelsColor="grey30"
  # extdendedLabelSize=0.7
  # extension=T
  # cropSubShape2Bound=T
  # grids=NULL


#----------------
# Initialize variables by setting to NULL
#----------------

NULL->bbox1->extendedBoundary->extendedSubReg->shape->boundaryHighlight->
  regionHL->subRegHighlight->subRegionHL->extendedShape->underLayer->subRegHighlightLabels->
  add_grid_name->fillPaletteOrig

tmap::tmap_options(max.categories=1000)

#----------------
# Check Input Shape files
#---------------

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

  # OverLap Shape
  if(is.null(overlapShape)){
   if(!is.null(overlapShpFolder) & !is.null(overlapShpFile)){
    if(!dir.exists(overlapShpFolder)){
      stop("Shapefile folder: ", overlapShpFolder ," is incorrect or doesn't exist.",sep="")}
    if(!file.exists(paste(overlapShpFolder,"/",overlapShpFile,".shp",sep=""))){
      stop("Shape file: ", paste(overlapShpFolder,"/",overlapShpFile,".shp",sep="")," is incorrect or doesn't exist.",sep="")}
    overlapShape=rgdal::readOGR(dsn=overlapShpFolder,layer=overlapShpFile,use_iconv=T,encoding='UTF-8')
    print(paste("Overlap Shape : ",overlapShpFolder,"/",overlapShpFile,".shp",sep=""))
    print(raster::head(overlapShape))
  }} # if(!is.null(overlapShpFolder) & !is.null(overlapShpFile)){


# Buffer region boundaries
  if(!is.null(overlapShape)){overlapShape=rgeos::gBuffer(overlapShape, byid=TRUE, width=0)}
  if(!is.null(boundaryRegShape)){boundaryRegShape=rgeos::gBuffer(boundaryRegShape, byid=TRUE, width=0)}
  if(!is.null(subRegShape)){subRegShape=rgeos::gBuffer(subRegShape, byid=TRUE, width=0)}



if(is.null(boundaryRegionsSelect)){
  print("No boundaryRegionsSelect provided, setting region folder as 'Region'")
  boundaryRegionsSelect="Region"}

if(!is.null(overlapShape)){overlapShape<-sp::spTransform(overlapShape,sp::CRS(projX))}
if(!is.null(subRegShape)){subRegShape<-sp::spTransform(subRegShape,sp::CRS(projX))}
if(!is.null(boundaryRegShape)){boundaryRegShape<-sp::spTransform(boundaryRegShape,sp::CRS(projX))}

#----------------
# Create Folders
#---------------

if (!dir.exists(dirOutputs)){dir.create(dirOutputs)}
if (!dir.exists(paste(dirOutputs, "/Maps", sep = ""))){dir.create(paste(dirOutputs, "/Maps", sep = ""))}
if (!dir.exists(paste(dirOutputs, "/Maps/Boundaries", sep = ""))){dir.create(paste(dirOutputs, "/Maps/Boundaries", sep = ""))}
if (!dir.exists(paste(dirOutputs, "/Maps/Boundaries/",boundaryRegionsSelect, sep = ""))){dir.create(paste(dirOutputs, "/Maps/Boundaries/",boundaryRegionsSelect,sep = ""))}
dir=paste(dirOutputs, "/Maps/Boundaries/",boundaryRegionsSelect,sep = "")

#----------------
# Create Boundary and subRegional shapefiles
#---------------

# Subset the boundary Region Shape
if(!is.null(boundaryRegShape)){
  if(!is.null(boundaryRegCol) & !is.null(boundaryRegionsSelect)){
    if(!boundaryRegCol %in% names(boundaryRegShape@data)){
      print(paste("boundaryRegCol provided: ",boundaryRegCol," is not a column in boundaryRegShape.",sep=""))
      print(paste(names(boundaryRegShape@data),sep=""))}else{
    if(!boundaryRegionsSelect %in% unique(boundaryRegShape@data[[boundaryRegCol]])){
          print(paste("boundaryRegionsSelect provided: ",boundaryRegionsSelect," is not a region in boundaryRegShape.",sep=""))
          print(paste(unique(boundaryRegShape@data[[boundaryRegCol]]),sep=""))}else{
    extendedBoundary<-boundaryRegShape
    boundaryRegShape<-boundaryRegShape[which(boundaryRegShape[[boundaryRegCol]] %in% boundaryRegionsSelect),]
    print(paste("boundaryRegShape subset to boundaryRegionSelect: ",boundaryRegionsSelect,sep=""))
        }
      }
  } else{print(paste("boundaryRegCol provided: ",boundaryRegCol," is not a column in boundaryRegShape.",sep=""))
         print(paste("OR boundaryRegionsSelect provided: ",boundaryRegionsSelect," is not a region in boundaryRegShape.",sep=""))
         print(paste("Boundary Shape not subset",sep=""))}
  }

# Subset the subRegion Shape
if(!is.null(subRegShape) & !is.null(boundaryRegShape)){
  if(cropSubShape2Bound==T){
  extendedSubReg<-subRegShape
  print(paste("subsetting subRegShape to boundary region...",sep=""))
  subRegShape<-raster::crop(subRegShape, boundaryRegShape)
  print(paste("subRegShape subset to boundary region",sep=""))}else{
    extendedSubReg<-subRegShape
    subRegShape<-subRegShape
    print(paste("subRegShape not subset",sep=""))
  }
}else{
if(!is.null(subRegShape)){
  if(!is.null(subRegCol) & !is.null(subRegionsSelect)){
    if(!subRegCol %in% names(subRegShape@data)){
      print(paste("subRegCol provided: ",subRegCol," is not a column in subRegShape.",sep=""))
      print(paste(names(subRegShape@data),sep=""))}else{
        if(!subRegionsSelect %in% unique(subRegShape@data[[subRegCol]])){
          print(paste("subRegionsSelect provided: ",subRegionsSelect," is not a region in subRegShape.",sep=""))
          print(paste(unique(subRegShape@data[[subRegCol]]),sep=""))}else{
            extendedSubReg<-subRegShape
            subRegShape<-subRegShape[which(subRegShape[[subRegCol]] %in% subRegionsSelect),]
            print(paste("subRegShape subset to subRegionSelect: ",subRegionsSelect,sep=""))
          }
      }
  }else{extendedSubReg<-subRegShape
        subRegShape<-subRegShape
        print(paste("subRegShape not subset",sep=""))
        }
       }
}


# Create Extended Shape
if(extension==T){
if(!is.null(extendedBoundary)){
  bbox1<-as.data.frame(sp::bbox(boundaryRegShape))
  }else{
    if(!is.null(extendedSubReg)){
      bbox1<-as.data.frame(sp::bbox(subRegShape))
      print(paste("boundaryRegShape is not subSet so extended shape using subRegShape",sep=""))}else{
    print(paste("boundaryRegShape and subRegShape are not subSet so no extended shape",sep=""))
  }}

if(!is.null(bbox1)){
  bbox1$min;bbox1$max
  bbox1$min[1]<-if(bbox1$min[1]<0){(1+expandPercent/100)*bbox1$min[1]}else{(1-expandPercent/100)*bbox1$min[1]};
  bbox1$min[2]<-if(bbox1$min[2]<0){(1+expandPercent/100)*bbox1$min[2]}else{(1-expandPercent/100)*bbox1$min[2]};
  bbox1$max[1]<-if(bbox1$max[1]<0){(1-expandPercent/100)*bbox1$max[1]}else{(1+expandPercent/100)*bbox1$max[1]};
  bbox1$max[2]<-if(bbox1$max[2]<0){(1-expandPercent/100)*bbox1$max[2]}else{(1+expandPercent/100)*bbox1$max[2]};
  bbox1$min;bbox1$max;
  bbox1<-methods::as(raster::extent(as.vector(t(bbox1))), "SpatialPolygons")
  sp::proj4string(bbox1)<-sp::CRS(projX) # ASSIGN COORDINATE SYSTEM
  if(!is.null(extendedBoundary)){
  print("Creating extended boundary using boundaryRegShape...")
  extendedShape<-raster::crop(extendedBoundary, bbox1)
  }else{
    if(!is.null(extendedSubReg)){
      print("Creating extended boundary using subRegShape...")
      extendedShape<-raster::crop(extendedSubReg, bbox1)

    }}
  extendedShape<-sp::SpatialPolygonsDataFrame(extendedShape,data=extendedShape@data)
  sp::proj4string(extendedShape)<-sp::CRS(projX) # ASSIGN COORDINATE SYSTEM
  print(paste("Writing extendedShape: ",paste(boundaryRegionsSelect,"_Extended",nameAppend,sep="")," to: ",dir,sep=""))
  rgdal::writeOGR(obj=extendedShape,
                  dsn=dir,
                  layer=paste(boundaryRegionsSelect,"_Extended",nameAppend,sep=""),
                  driver="ESRI Shapefile", overwrite_layer=TRUE)
}
}else{print("Extension is off.")}

# Crop overlap shape to boundary and subReg
   if(!is.null(overlapShape)){
     if(!is.null(boundaryRegShape)){
       print("Cropping overlapBoundary to boundaryRegShape...")
       overlapBoundary<-raster::crop(overlapShape, boundaryRegShape)
       print("overlapBoundary cropped to boundaryRegShape.")
     }else{
       print("BoundaryShape not provided. Not overlapping over boundary.")
     }}

    if(!is.null(overlapShape)){
      if(!is.null(subRegShape)){
        print("Cropping overlapSubReg to subRegShape...")
          overlapSubReg<-raster::crop(overlapShape, subRegShape)
          print("overlapSubReg cropped to subRegShape.")
      }else{
        print("subRegShape not provided. Not overlapping over subReg.")
      }}



#----------------
# Save boundary maps
#---------------

fillPaletteOrig <- fillPalette

# Extended underLayer and Regional Highlights
if(!is.null(extendedShape)){
  if(!is.null(extendedBoundary)){
  underLayer<- metis.map(legendStyle="cat",fillcolorNA=fillcolorNA, dataPolygon=extendedShape,printFig=F,
                        fillColumn = boundaryRegCol,
                        labels=T, innerMargins = innerMargins,outerMargins = outerMargins,
                        fillPalette = extendedFillColor,
                        bgColor = extendedBGColor, frameShow=T, labelsSize=extdendedLabelSize, labelsColor=extendedLabelsColor,facetsON = F)
  boundaryHighlight<-metis.map(legendStyle="cat",fillcolorNA=fillcolorNA, labelsSize=labelsSize, dataPolygon=boundaryRegShape,
                               fillColumn = boundaryRegCol, fillPalette = extendedHighLightColor, labels=T, innerMargins = innerMargins,outerMargins = outerMargins,printFig = F,facetsON = F)
  if(!is.null(subRegShape)){
    subRegHighlight<-metis.map(legendStyle="cat",fillcolorNA=fillcolorNA, labelsSize=labelsSize, dataPolygon=subRegShape,
                               fillColumn = subRegCol, fillPalette = extendedHighLightColor, labels=T, innerMargins = innerMargins,outerMargins = outerMargins,printFig = F,facetsON = F)
  }

  }else{
    if(!is.null(extendedSubReg)){
    underLayer<- metis.map(legendStyle="cat",fillcolorNA=fillcolorNA, dataPolygon=extendedShape,printFig=F,
                           fillColumn = subRegCol,
                           labels=T, innerMargins = innerMargins,outerMargins = outerMargins,
                           fillPalette = extendedFillColor,
                           bgColor = extendedBGColor, frameShow=T, labelsSize=extdendedLabelSize, labelsColor=extendedLabelsColor,facetsON = F)
    subRegHighlight<-metis.map(legendStyle="cat",fillcolorNA=fillcolorNA, labelsSize=labelsSize, dataPolygon=subRegShape,
                                 fillColumn = subRegCol, fillPalette = extendedHighLightColor, labels=T, innerMargins = innerMargins,outerMargins = outerMargins,printFig = F,facetsON = F)
  }}
} else {
  if(!is.null(boundaryRegShape)){
  boundaryHighlight<-metis.map(legendStyle="cat",fillcolorNA=fillcolorNA, labelsSize=labelsSize, dataPolygon=boundaryRegShape,
                               fillColumn = boundaryRegCol, fillPalette = extendedHighLightColor, labels=T, innerMargins = innerMargins,outerMargins = outerMargins,printFig = F,facetsON = F)
  }
  if(!is.null(subRegShape)){
    subRegHighlightLabels<-metis.map(legendStyle="cat",fillcolorNA=fillcolorNA, labelsSize=labelsSize, dataPolygon=subRegShape,
                               fillColumn = subRegCol, fillPalette = extendedHighLightColor, labels=T, innerMargins = innerMargins,outerMargins = outerMargins,printFig = F,facetsON = F)
    subRegHighlight<-metis.map(legendStyle="cat",fillcolorNA=fillcolorNA, labelsSize=labelsSize, dataPolygon=subRegShape,
                               fillColumn = subRegCol, fillPalette = extendedHighLightColor, labels=F,printFig = F,facetsON = F)
  }
}

# fillcolorNA=fillcolorNA; labelsSize=labelsSize; dataPolygon=boundaryHighlight;
# fileName = paste(boundaryRegionsSelect,"_highlight_region_",subRegType,nameAppend,sep="");dirOutputs = dir;
# underLayer = underLayer;bgColor=extendedBGColor;frameShow=T;facetsON = F;labels=F

# Regional highlights
if(!is.null(boundaryHighlight) & !is.null(underLayer)){
regionHL<-metis.map(legendStyle="cat",fillcolorNA=fillcolorNA, labelsSize=labelsSize, dataPolygon=boundaryHighlight,
          fileName = paste(boundaryRegionsSelect,"_highlight_region_",subRegType,nameAppend,sep=""),dirOutputs = dir,
          underLayer = underLayer,bgColor=extendedBGColor,frameShow=T,facetsON = F,labels=F)
}
if(!is.null(subRegHighlight) & !is.null(underLayer)){
subRegionHL<-metis.map(legendStyle="cat",fillcolorNA=fillcolorNA, labelsSize=labelsSize, dataPolygon=subRegHighlight,
                    fileName = paste(boundaryRegionsSelect,"_highlight_subRegion_",subRegType,nameAppend,sep=""),dirOutputs = dir,
                    underLayer = underLayer,bgColor=extendedBGColor,frameShow=T,facetsON = F)}
if(!is.null(subRegHighlightLabels) & !is.null(underLayer)){
subRegionHL<-metis.map(legendStyle="cat",fillcolorNA=fillcolorNA, labelsSize=labelsSize, dataPolygon=subRegHighlightLabels,
                       fileName = paste(boundaryRegionsSelect,"_highlight_subRegion_Labels_",subRegType,nameAppend,sep=""),dirOutputs = dir,
                       underLayer = underLayer,bgColor=extendedBGColor,frameShow=T,facetsON = F)}

if(!is.null(subRegShape) & !is.null(regionHL)){
metis.map(legendStyle="cat",fillcolorNA=fillcolorNA, labelsSize=labelsSize, dataPolygon=subRegShape,fillColumn = subRegCol,
          fileName = paste(boundaryRegionsSelect,"_highlight_subReg_Region_",subRegType,nameAppend,sep=""),dirOutputs = dir,
          underLayer = regionHL,bgColor=extendedBGColor,frameShow=T,labels=F,facetsON = F,fillPalette=fillPaletteOrig)
metis.map(legendStyle="cat",fillcolorNA=fillcolorNA, labelsSize=labelsSize, dataPolygon=subRegShape,fillColumn = subRegCol,
            fileName = paste(boundaryRegionsSelect,"_highlight_subReg_Region_Labels_",subRegType,nameAppend,sep=""),dirOutputs = dir,
            underLayer = regionHL,bgColor=extendedBGColor,frameShow=T,labels=T, innerMargins = innerMargins,outerMargins = outerMargins,facetsON = F,fillPalette=fillPaletteOrig)
}

if(!is.null(overlapShape)){
  if(!is.null(regionHL)){
  metis.map(legendStyle="cat",fillcolorNA=fillcolorNA, labelsSize=labelsSize, dataPolygon=overlapBoundary,
            fileName = paste(boundaryRegionsSelect,"_highlight_region_OverLap_",subRegType,nameAppend,sep=""),dirOutputs = dir,
            underLayer = regionHL,borderColor="red",bgColor=extendedBGColor,frameShow=T,facetsON=F)}
  if(!is.null(subRegionHL)){
  metis.map(legendStyle="cat",fillcolorNA=fillcolorNA, labelsSize=labelsSize, dataPolygon=overlapSubReg,
          fileName = paste(boundaryRegionsSelect,"_highlight_subRegion_OverLap_",subRegType,nameAppend,sep=""),dirOutputs = dir,
          underLayer = subRegionHL,borderColor="red",bgColor=extendedBGColor,frameShow=T,facetsON=F)}
}


# Detailed Maps

if(!is.null(boundaryRegShape)){

  if(length(unique(boundaryRegShape@data[[boundaryRegCol]]))<2){fillPalette=extendedHighLightColor}else{
    fillPalette=fillPaletteOrig
  }

  boundaryRegShapeBlank<-metis.map(legendStyle="cat",fillcolorNA=fillcolorNA,bgColor="white",frameShow=F,
                              labelsSize=labelsSize, facetsON = F,dataPolygon=boundaryRegShape,fileName = paste(boundaryRegionsSelect,"_detail_regional_map_blank",nameAppend,sep=""),dirOutputs = dir)
  boundaryRegShapeFilled<-metis.map(legendStyle="cat",fillcolorNA=fillcolorNA, labelsSize=labelsSize, facetsON = F, dataPolygon=boundaryRegShape,fileName = paste(boundaryRegionsSelect,"_detail_regional_map_Filled",nameAppend,sep=""),dirOutputs = dir,
                               fillColumn = boundaryRegCol,fillPalette = fillPalette,frameShow=F,bgColor="white")
  boundaryRegShapeBlankLabels<-metis.map(legendStyle="cat",fillcolorNA=fillcolorNA, labelsSize=labelsSize, facetsON = F, dataPolygon=boundaryRegShape,fileName = paste(boundaryRegionsSelect,"_detail_regional_map_blank_Labels",nameAppend,sep=""),dirOutputs = dir,
                                    fillColumn = boundaryRegCol,fillPalette = "white", labels=T, innerMargins = innerMargins,outerMargins = outerMargins,frameShow=F,bgColor="white")
  boundaryRegShapeFilledLabels<-metis.map(legendStyle="cat",fillcolorNA=fillcolorNA, labelsSize=labelsSize, facetsON = F, dataPolygon=boundaryRegShape,fileName = paste(boundaryRegionsSelect,"_detail_regional_map_Filled_Labels",nameAppend,sep=""),dirOutputs = dir,
                                     fillColumn = boundaryRegCol,labels=T, innerMargins = innerMargins,outerMargins = outerMargins,fillPalette = fillPalette,frameShow=F,bgColor="white")

  if(!is.null(overlapShape)){
    if(!is.null(boundaryRegShapeBlank)){
      metis.map(legendStyle="cat",fillcolorNA=fillcolorNA, labelsSize=labelsSize, dataPolygon=overlapBoundary,
                fileName = paste(boundaryRegionsSelect,"_detail_regional_OverLap",nameAppend,sep=""),dirOutputs = dir,
                underLayer = boundaryRegShapeBlank,borderColor="red",frameShow=F,facetsON=F)}
    if(!is.null(boundaryRegShapeBlankLabels)){
      metis.map(legendStyle="cat",fillcolorNA=fillcolorNA, labelsSize=labelsSize, dataPolygon=overlapBoundary,
                fileName = paste(boundaryRegionsSelect,"_detail_regional_OverLap_Labels",nameAppend,sep=""),dirOutputs = dir,
                underLayer = boundaryRegShapeBlankLabels,borderColor="red",frameShow=F,facetsON=F)}
  }

  }


if(!is.null(subRegShape)){

  if(length(unique(subRegShape@data[[subRegCol]]))<2){fillPalette=extendedHighLightColor}else{fillPalette=fillPaletteOrig}

# SubRegion Maps No Extension
subRegShapeBlank<-metis.map(legendStyle="cat",fillcolorNA=fillcolorNA,bgColor="white",frameShow=F,
          labelsSize=labelsSize, facetsON = F,dataPolygon=subRegShape,fileName = paste(boundaryRegionsSelect,"_detail_subregion_",subRegType,"_map_blank",nameAppend,sep=""),dirOutputs = dir)
subRegShapeFilled<-metis.map(legendStyle="cat",fillcolorNA=fillcolorNA, labelsSize=labelsSize, facetsON = F, dataPolygon=subRegShape,fileName = paste(boundaryRegionsSelect,"_detail_subregion_",subRegType,"_map_Filled",nameAppend,sep=""),dirOutputs = dir,
           fillColumn = subRegCol,fillPalette = fillPalette,frameShow=F,bgColor="white")
subRegShapeBlankLabels<-metis.map(legendStyle="cat",fillcolorNA=fillcolorNA, labelsSize=labelsSize, facetsON = F, dataPolygon=subRegShape,fileName = paste(boundaryRegionsSelect,"_detail_subregion_",subRegType,"_map_blank_Labels",nameAppend,sep=""),dirOutputs = dir,
           fillColumn = subRegCol,fillPalette = "white", labels=T, innerMargins = innerMargins,outerMargins = outerMargins,frameShow=F,bgColor="white")
subRegShapeFilledLabels<-metis.map(legendStyle="cat",fillcolorNA=fillcolorNA, labelsSize=labelsSize, facetsON = F, dataPolygon=subRegShape,fileName = paste(boundaryRegionsSelect,"_detail_subregion_",subRegType,"_map_Filled_Labels",nameAppend,sep=""),dirOutputs = dir,
           fillColumn = subRegCol,labels=T, innerMargins = innerMargins,outerMargins = outerMargins,fillPalette = fillPalette,frameShow=F,bgColor="white")

if(!is.null(overlapShape)){
  if(!is.null(subRegShapeBlank)){
    metis.map(legendStyle="cat",fillcolorNA=fillcolorNA, labelsSize=labelsSize, dataPolygon=overlapSubReg,
              fileName = paste(boundaryRegionsSelect,"_detail_subRegion_",subRegType,"_OverLap_",nameAppend,sep=""),dirOutputs = dir,
              underLayer = subRegShapeBlank,borderColor="red",frameShow=F,facetsON=F)}
  if(!is.null(subRegShapeBlankLabels)){
    metis.map(legendStyle="cat",fillcolorNA=fillcolorNA, labelsSize=labelsSize, dataPolygon=overlapSubReg,
              fileName = paste(boundaryRegionsSelect,"_detail_subRegion_",subRegType,"_OverLap_",nameAppend,sep=""),dirOutputs = dir,
              underLayer = subRegShapeBlankLabels,borderColor="red",frameShow=F,facetsON=F)}
}


# Grid Overlay

for(grid_i in grids){
  if(!is.null(grid_i)){
    if(all(!class(grid_i) %in% c("tbl_df","tbl","data.frame"))){
      if(any(grepl(".csv",paste(grid_i)))){
        print(paste("Attempting to read grid csv file ",grid_i,sep=""))
        if(file.exists(grid_i)){
          gridx<-data.table::fread(grid_i)
          gridx<-gridx%>%unique()}}}}else{
            print(paste("Grid file ",grid_i," does not exist. Skipping Grid Overlay",sep=""))
            gridx=NULL
          }

if(!is.null(gridx)){
  names(gridx)=gsub("latitude","lat",names(gridx))
  names(gridx)=gsub("longitude","lon",names(gridx))
gridxspdf = sp::SpatialPointsDataFrame(sp::SpatialPoints(coords=(cbind(gridx$lon,gridx$lat))),data=gridx)
sp::gridded(gridxspdf)<-TRUE

r<-raster::stack(gridxspdf)
raster::projection(r)<-sp::proj4string(subRegShape)
shapeExpandEtxent<-as.data.frame(sp::bbox(subRegShape))   # Get Bounding box
expandbboxPercent<-0.5; shapeExpandEtxent$min;shapeExpandEtxent$max
shapeExpandEtxent$min[1]<-if(shapeExpandEtxent$min[1]<0){(1+expandbboxPercent/100)*shapeExpandEtxent$min[1]}else{(1-expandbboxPercent/100)*shapeExpandEtxent$min[1]};
shapeExpandEtxent$min[2]<-if(shapeExpandEtxent$min[2]<0){(1+expandbboxPercent/100)*shapeExpandEtxent$min[2]}else{(1-expandbboxPercent/100)*shapeExpandEtxent$min[2]};
shapeExpandEtxent$max[1]<-if(shapeExpandEtxent$max[1]<0){(1-expandbboxPercent/100)*shapeExpandEtxent$max[1]}else{(1+expandbboxPercent/100)*shapeExpandEtxent$max[1]};
shapeExpandEtxent$max[2]<-if(shapeExpandEtxent$max[2]<0){(1-expandbboxPercent/100)*shapeExpandEtxent$max[2]}else{(1+expandbboxPercent/100)*shapeExpandEtxent$max[2]};
shapeExpandEtxent$min;shapeExpandEtxent$max;
shapeExpandEtxent<-methods::as(raster::extent(as.vector(t(shapeExpandEtxent))), "SpatialPolygons")
sp::proj4string(shapeExpandEtxent)<-sp::CRS(sp::proj4string(subRegShape)) # ASSIGN COORDINATE SYSTEM
rcrop<-raster::crop(r,shapeExpandEtxent)
rcropP<-raster::rasterToPolygons(rcrop)
sp::proj4string(rcropP)<-sp::proj4string(subRegShape)
print("Intersecting grid with subRegShape...")
rcropPx<-raster::crop(rcropP,subRegShape)

if(grepl("025",grid_i)){add_grid_name="_25Grid"}else{
if(grepl("050",grid_i)){add_grid_name="_50Grid"}else{add_grid_name=""}}

print("Printing Grid overlay...")
metis.map(legendStyle="cat",fillcolorNA=fillcolorNA, labelsSize=labelsSize, dataPolygon=rcropPx,
          fileName = paste(boundaryRegionsSelect,"_detail_subRegion_",subRegType,"_GridSize_Labels",add_grid_name,nameAppend,sep=""),
          dirOutputs = dir,
          overLayer = metis.map(legendStyle="cat",fillcolorNA=fillcolorNA, labelsSize=labelsSize, dataPolygon=subRegShape,fillColumn = subRegCol,
                                fillPalette = "white",alpha=0,facetsON=F,
                                labels=T, innerMargins = innerMargins,outerMargins = outerMargins,printFig=F,borderColor="red",
                                lwd=1),facetsON=F)

print("Printing Grid overlay with Labels...")
metis.map(legendStyle="cat",fillcolorNA=fillcolorNA, labelsSize=labelsSize, dataPolygon=rcropPx,
          fileName = paste(boundaryRegionsSelect,"_detail_subRegion_",subRegType,"_GridSize",add_grid_name,nameAppend,sep=""),
          dirOutputs = dir,
          overLayer = metis.map(legendStyle="cat",fillcolorNA=fillcolorNA, labelsSize=labelsSize, dataPolygon=subRegShape,fillColumn = subRegCol,
                                fillPalette = "white",alpha=0,facetsON=F,
                                labels=F,printFig = F,borderColor="red",
                                lwd=1),facetsON=F)
}

}
}

if(!is.null(subRegShape) & !is.null(boundaryRegShape)){

  if(length(unique(subRegShape@data[[subRegCol]]))<2){fillPalette=extendedHighLightColor}else{fillPalette=fillPaletteOrig}

  if(!is.null(boundaryRegShapeBlank) & !is.null(subRegShapeFilled)){
    metis.map(legendStyle="cat",fillcolorNA=fillcolorNA, labelsSize=labelsSize, dataPolygon=subRegShape,
              fileName = paste(boundaryRegionsSelect,"_detail_regSubReg_",subRegType,"_map_filled",nameAppend,sep=""),dirOutputs = dir,
              underLayer = boundaryRegShapeBlank,fillColumn = subRegCol,fillPalette = fillPalette,bgColor="white",frameShow=F,facetsON=F)
    metis.map(legendStyle="cat",fillcolorNA=fillcolorNA, labelsSize=labelsSize, dataPolygon=subRegShape,
              fileName = paste(boundaryRegionsSelect,"_detail_regSubReg_",subRegType,"_map_filled_Labels",nameAppend,sep=""),dirOutputs = dir,
              underLayer = boundaryRegShapeBlank,labels=T, innerMargins = innerMargins,outerMargins = outerMargins,fillColumn = subRegCol,fillPalette = fillPalette,bgColor="white",frameShow=F,facetsON=F)
    }
}



if(!is.null(extendedShape)){print(paste("Extended shapefile ",paste(boundaryRegionsSelect,"_Extended",nameAppend,sep="")," saved to: ",dir,sep=""))}

return(extendedShape)

} # Close Function
