#' metis.mapProcess
#'
#' This function produce different kinds of maps for the metis package.
#' Each figure is accompanied with a csv table.
#'
#' @keywords charts, diffplots
#' @return Returns the formatted data used to produce chart
#' @param polygonDataTables Default = NULL,
#' @param gridDataTables Default = NULL,
#' @param dirOutputs Default = paste(getwd(),"/outputs",sep=""),
#' @param xRange Default ="All",
#' @param labels Default = F,
#' @param labelsSize Default = 1.2,
#' @param boundaryRegionsSelect Default = NULL,
#' @param subRegShape Default = NULL,
#' @param subRegShpFolder Default = paste(getwd(),"/dataFiles/gis/admin_gadm36",sep=""),
#' @param subRegShpFile Default = paste("gadm36_1",sep=""),
#' @param subRegCol Default ="NAME_1",
#' @param subRegType Default ="subRegType",
#' @param nameAppend Default =""
#' @param legendOutsideSingle Default =F, Single plots by default have legends inside. This can be moved out if wanted.
#' @param legendOutsidePosition Default = NULL, # "right","left","top","bottom", "center"
#' @param legendPosition Default = NULL, # c("RIGHT','top') - RIGHT LEFT TOP BOTTOM
#' @param legendFixedBreaks Default = "5",
#' @param animateOn Default = T,
#' @param delay Default = 100,
#' @param legendTitleSizeO Default = 2,
#' @param legendTextSizeO Default =1,
#' @param legendTitleSizeI Default = 1,
#' @param legendTextSizeI Default =0.5,
#' @param scenRef Default = NULL
#' @param extension Default =F,
#' @param boundaryRegShape Default = NULL,
#' @param boundaryRegShpFolder Default= NULL . Suggested paste(getwd(),"/dataFiles/gis/naturalEarth",sep  Default="")
#' @param boundaryRegShpFile Default=NULL . Suggested paste("ne_10m_admin_0_countries",sep  Default=""),
#' @param boundaryRegCol Default=NULL. Suggested "NAME_0",
#' @param fillcolorNA Default = NULL
#' @param extendedFillColor Default ="grey75",
#' @param extendedBGColor Default ="lightblue1",
#' @param extendedHighLightColor Default ="cornsilk1",
#' @param extendedLabelsColor Default ="grey30",
#' @param extdendedLabelSize Default =0.7,
#' @param extendedShape Default =NULL,
#' @param extendedShapeCol Default =NULL,
#' @param expandPercent Default =2
#' @param projX Default = projX="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
#' @param figWidth Default =9
#' @param figHeight Default =7
#' @param scaleRange Default NULL. Dataframe with columns param, maxScale, minScale to indicate maximum and minumum values for a parameter scale.
#' @param paramsSelect Default ="All"
#' @export

metis.mapProcess<-function(polygonDataTables=NULL,
                         gridDataTables=NULL,
                         dirOutputs=paste(getwd(),"/outputs",sep=""),
                         xRange="All",
                         labels=F,
                         labelsSize=1.2,
                         subRegShape=NULL,
                         subRegShpFolder=NULL,
                         subRegShpFile=NULL,
                         subRegCol=NULL,
                         subRegType="subRegType",
                         nameAppend="",
                         legendOutsideSingle=F,
                         legendOutsidePosition=NULL,
                         legendPosition=NULL,
                         legendFixedBreaks=5,
                         legendTitleSizeO=2,
                         legendTextSizeO=1,
                         legendTitleSizeI=1.5,
                         legendTextSizeI=1,
                         animateOn=T,
                         delay=100,
                         scenRef=NULL,
                         extension=F,
                         boundaryRegShape=NULL,
                         boundaryRegShpFolder=NULL,
                         boundaryRegShpFile=NULL,
                         boundaryRegCol=NULL,
                         boundaryRegionsSelect=NULL,
                         fillcolorNA=NULL,
                         extendedFillColor="grey75",
                         extendedBGColor="lightblue1",
                         extendedHighLightColor="cornsilk1",
                         extendedLabelsColor="grey30",
                         extdendedLabelSize=0.7,
                         extendedShape=NULL,
                         extendedShapeCol=NULL,
                         expandPercent=2,
                         projX="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0",
                         figWidth=9,
                         figHeight=7,
                         scaleRange=NULL,
                         paramsSelect="All"){

  # polygonDataTables=NULL
  # gridDataTables=NULL
  # dirOutputs=paste(getwd(),"/outputs",sep="")
  # xRange="All"
  # labels=F
  # labelsSize=1.2
  # subRegShape=NULL
  # subRegShpFolder=NULL
  # subRegShpFile=NULL
  # subRegCol=NULL
  # subRegType="subRegType"
  # nameAppend=""
  # legendOutsideSingle=F
  # legendOutsidePosition=NULL
  # legendPosition=NULL
  # legendFixedBreaks=5
  # legendTitleSizeO=2
  # legendTextSizeO=1
  # legendTitleSizeI=1.5
  # legendTextSizeI=1
  # animateOn=T
  # delay=100
  # scenRef=NULL
  # extension=F
  # boundaryRegShape=NULL
  # boundaryRegShpFolder=NULL
  # boundaryRegShpFile=NULL
  # boundaryRegCol=NULL
  # boundaryRegionsSelect=NULL
  # fillcolorNA=NULL
  # extendedFillColor="grey75"
  # extendedBGColor="lightblue1"
  # extendedHighLightColor="cornsilk1"
  # extendedLabelsColor="grey30"
  # extdendedLabelSize=0.7
  # extendedShape=NULL
  # extendedShapeCol=NULL
  # expandPercent=2
  # projX="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
  # figWidth=9
  # figHeight=7
  # scaleRange=NULL
  # paramsSelect="All"


#------------------
# Initialize variables to remove binding errors
# -----------------

  NULL->lat->lon->param->region->scenario->subRegion->
  value->x->year->gridID->underLayer->maxScale->minScale

  # Set legend size based on where legend is placed
  if(legendOutsideSingle==T){legendTitleSizeS=legendTitleSizeO;legendTextSizeS=legendTextSizeO;legendPositionS=NULL}
  if(legendOutsideSingle==F){legendTitleSizeS=legendTitleSizeI;legendTextSizeS=legendTextSizeI;legendPositionS=legendPosition}

#------------------
# Function for adding any missing columns if needed
# -----------------

  addMissing<-function(data){
    if(!"scenario"%in%names(data)){data<-data%>%dplyr::mutate(scenario="scenario")}
    if(!"x"%in%names(data)){if("year"%in%names(data)){
      data<-data%>%dplyr::mutate(x=year)}else{data<-data%>%dplyr::mutate(x="x")}}
    if(!"region"%in%names(data)){data<-data%>%dplyr::mutate(region="region")}
    if(!"classPalette"%in%names(data)){data<-data%>%dplyr::mutate(classPalette="pal_hot")}
    if(!"param"%in%names(data)){data<-data%>%dplyr::mutate(param="param")}
    return(data)
  }



  if(is.null(gridDataTables) & is.null(polygonDataTables)){
    stop ("Both gridDataTables and polygonDataTables are Null. Need to provide atleast one of the two.")
  }

#------------------
# Read in grid Tables (Either csv tables or an R Table)
#------------------

  gridTbl<-tibble::tibble()


  if(!is.null(gridDataTables)){

    if(all(!class(gridDataTables) %in% c("tbl_df","tbl","data.frame"))){

    for(grid_i in gridDataTables){
      if(file.exists(grid_i)){
        gridTblNew<-data.table::fread(paste(grid_i))%>%tibble::as.tibble()
        gridTbl<-dplyr::bind_rows(gridTbl,gridTblNew)
        rm(gridTblNew)
      } else {stop(paste(grid_i," does not exist"))}
    }

    # Join relevant colors and classes using the mapping file if it exists
    if(!"classPalette" %in% names(gridTbl)){
    if(file.exists(paste(getwd(),"/dataFiles/mapping/template_subRegional_mapping.csv", sep = ""))){
      map<-data.table::fread(paste(getwd(),"/dataFiles/mapping/template_subRegional_mapping.csv", sep = ""))%>%tibble::as.tibble()
      gridTbl<-gridTbl%>%dplyr::left_join(map,by=c("param","units","class"))
    }}

    # Add missing columns
    gridTbl<-addMissing(gridTbl)
   }else{gridTbl<-gridDataTables}

  if(!"subRegType" %in% names(gridTbl)){
      print(paste("'subRegType' column not present in grid data provided. Creating class column 'subRegType'",sep=""))
      gridTbl<-gridTbl%>%dplyr::mutate(subRegType="raster")
    }
  if(!"class" %in% names(gridTbl)){
    print(paste("'class' column not present in grid data provided. Creating class column 'class'",sep=""))
    gridTbl<-gridTbl%>%dplyr::mutate(class="class")
  }
  if(!"x" %in% names(gridTbl)){
    print(paste("'x' column not present in grid data provided. Creating x column 'x'",sep=""))
    gridTbl<-gridTbl%>%dplyr::mutate(x="x")
  }
  if(!"param" %in% names(gridTbl)){
    print(paste("'param' column not present in grid data provided. Creating param column 'param'",sep=""))
    gridTbl<-gridTbl%>%dplyr::mutate(param="param")
  }
  if(!"classPalette" %in% names(gridTbl)){
    print(paste("'classPalette' column not present in grid data provided. Creating classPalette column 'classPalette'",sep=""))
      gridTbl<-gridTbl%>%dplyr::mutate(classPalette="pal_hot")
    }
  if(!"value" %in% names(gridTbl)){
    stop("'value' column not present in grid data provided. Need to have values. Check data.",sep="")
  }

  if(!"lat" %in% names(gridTbl)){stop("'lat' column not present in grid data provided. Need to have lat. Check data.",sep="")}
  if(!"lon" %in% names(gridTbl)){stop("'lon' column not present in grid data provided. Need to have lat. Check data.",sep="")}

  }else{gridTbl=gridDataTables}

#------------------
# Read in shape Tables (Either csv tables or R table
#------------------

  shapeTbl<-tibble::tibble()

  if(!is.null(polygonDataTables)){

    if(all(!class(polygonDataTables) %in% c("tbl_df","tbl","data.frame"))){
      if(class(polygonDataTables)!="character"){stop("polygonDataTables neither .csv file path nor dataframe or tibble")}
    for(i in polygonDataTables){
      if(file.exists(i)){
        shapeTblNew<-data.table::fread(paste(i))%>%tibble::as.tibble()

        # Join relevant colors and classes using the mapping file if it exists
        if(!"classPalette" %in% names(shapeTblNew)){
        if(file.exists(paste(getwd(),"/dataFiles/mapping/template_subRegional_mapping.csv", sep = ""))){
          map<-data.table::fread(paste(getwd(),"/dataFiles/mapping/template_subRegional_mapping.csv", sep = ""))%>%tibble::as.tibble()
          shapeTblNew<-shapeTblNew%>%dplyr::left_join(map,by=c("param","units","class"))
        }else{"subregional mapping not found. Using defaults."}}

        shapeTbl<-dplyr::bind_rows(shapeTbl,shapeTblNew)

        if(!"subRegion" %in% names(shapeTbl)){stop(paste("SubRegCol: ",subRegCol," not present in polygonDataTables ",i,sep=""))}

      } else {stop(paste(i," does not exist"))}
    }


    # Add missing columns
    shapeTbl<-addMissing(shapeTbl)
    }else{shapeTbl<-polygonDataTables}}

  if(nrow(shapeTbl)>0){
  if(!"class" %in% names(shapeTbl)){
    print(paste("'class' column not present in polygon data provided. Creating class column 'class'",sep=""))
    shapeTbl<-shapeTbl%>%dplyr::mutate(class="class")
  }
  if(!"x" %in% names(shapeTbl)){
    print(paste("'x' column not present in polygon data provided. Creating x column 'x'",sep=""))
    shapeTbl<-shapeTbl%>%dplyr::mutate(x="x")
  }
  if(!"param" %in% names(shapeTbl)){
    print(paste("'param' column not present in polygon data provided. Creating param column 'param'",sep=""))
    shapeTbl<-shapeTbl%>%dplyr::mutate(param="param")
  }
  if(!"classPalette" %in% names(shapeTbl)){
    print(paste("'classPalette' column not present in polygon data provided. Creating classPalette column 'classPalette'",sep=""))
      shapeTbl<-shapeTbl%>%dplyr::mutate(classPalette="pal_hot")
    }
  if(!"value" %in% names(shapeTbl)){
    stop("'value' column not present in polygon data provided. Need to have values. Check data.",sep="")
  }
  }

#------------------
# Read in shape files
#------------------

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


if(!subRegCol %in% names(subRegShape)){stop(paste("SubRegCol: ",subRegCol," not present in subRegShape",sep=""))}

subRegShape@data<-subRegShape@data%>%dplyr::mutate(subRegion=get(subRegCol))


#----------------
# Check scaleRange
#---------------

scaleRange[is.na(scaleRange)]<-NA_real_
scaleRange[scaleRange=="NA"]<-NA_real_
if(!all(c("param","maxScale","minScale") %in% names(scaleRange))){
  paste("Incorrect column names for scaleRange: ",names(scaleRange),". Should include param, maxScale, minscale.")
  paste("Setting scaleRange to NULL.")
  scaleRange=NULL
}

#----------------
# Create Boundary and subRegional shapefiles
#---------------

shape<-subRegShape

if(!subRegCol %in% names(shape)){stop(paste("SubRegCol: ",subRegCol," not present in shape",sep=""))}

shape@data<-shape@data%>%dplyr::mutate(subRegion=get(subRegCol))

#----------------
# Create Boundary Extension
#---------------

bgColorChosen="white"

if(extension==T){

if(is.null(extendedShape)){
  if(!is.null(boundaryRegCol) & !is.null(boundaryRegShape)){
if(boundaryRegCol %in% names(boundaryRegShape)){
extendedBoundary<-boundaryRegShape
boundaryRegShape<-boundaryRegShape[which(boundaryRegShape[[boundaryRegCol]] %in% boundaryRegionsSelect),]
print(paste("boundaryRegShape subset to boundaryRegionSelect: ",boundaryRegionsSelect,sep=""))
bbox1<-as.data.frame(sp::bbox(boundaryRegShape))
}}else{
  print(paste("boundaryRegCol provided: ",boundaryRegCol," is not a column in boundaryRegShape.",sep=""))
  print(paste("OR boundaryRegionsSelect provided: ",boundaryRegionsSelect," is not a region in boundaryRegShape.",sep=""))
  print(paste("Boundary Shape not subset. Skipping Extension.",sep=""))
  bbox1=NULL
}

if(!is.null(bbox1)){
    bbox1$min;bbox1$max
    rangeX<-abs(range(bbox1$min[1],bbox1$max[1])[2]-range(bbox1$min[1],bbox1$max[1])[1])
    rangeY<-abs(range(bbox1$min[2],bbox1$max[2])[2]-range(bbox1$min[2],bbox1$max[2])[1])
    bbox1$min[1]<-(-rangeX*expandPercent/100)+bbox1$min[1];
    bbox1$min[2]<-(-rangeY*expandPercent/100)+bbox1$min[2];
    bbox1$max[1]<-(rangeX*expandPercent/100)+bbox1$max[1];
    bbox1$max[2]<-(rangeY*expandPercent/100)+bbox1$max[2];
    bbox1$min;bbox1$max;
    bbox1<-methods::as(raster::extent(as.vector(t(bbox1))), "SpatialPolygons")
    sp::proj4string(bbox1)<-sp::CRS(projX) # ASSIGN COORDINATE SYSTEM
    print("Creating extended boundary using boundaryRegShape...")
    extendedShape<-raster::crop(extendedBoundary, bbox1)
    extendedShapeCol<-boundaryRegCol
}else{print("No extended boundary.")}
}

if(!is.null(extendedShape)){
  if(extendedShapeCol %in% names(extendedShape)){
underLayer<-metis.map(fillcolorNA=fillcolorNA, dataPolygon=extendedShape, printFig=F,
                      fillColumn = extendedShapeCol,labels=T, fillPalette = extendedFillColor,legendShow=F,
                      bgColor = extendedBGColor, frameShow=T, labelsSize=extdendedLabelSize, labelsColor=extendedLabelsColor,
                      facetsON=F, figWidth=figWidth,figHeight=figHeight)
bgColorChosen= extendedBGColor
  }else{ print(paste("boundaryRegCol provided: ",boundaryRegCol," is not a column in boundaryRegShape.",sep=""))
    print(paste("OR boundaryRegionsSelect provided: ",boundaryRegionsSelect," is not a region in boundaryRegShape.",sep=""))
    print(paste("Boundary Shape not subset. Skipping Extension.",sep=""))
    underLayer=NULL}
}
}


#------------------
# Subset Data
#------------------


if(nrow(shapeTbl)>0){
if(any(!boundaryRegionsSelect %in% unique(shapeTbl$region))){
  stop(paste("boundaryRegionsSelect: ",boundaryRegionsSelect," not in shapeTbl regions"))}}


if(!is.null(shapeTbl)){
  shapeTbl<-shapeTbl%>%
    unique()%>%
    dplyr::filter(region %in% boundaryRegionsSelect)
  if(any(xRange!="All")){shapeTbl<-shapeTbl%>%dplyr::filter(x %in% xRange);
  paste("Subset shapeTbl x to xRange: ",xRange,sep="")}
  if(any(paramsSelect!="All")){
    if(any(paramsSelect %in% unique(shapeTbl$param))){
      shapeTbl<-shapeTbl%>%dplyr::filter(param %in% paramsSelect);
      paste("Subset shapeTbl param to paramsSelect: ",paramsSelect,sep="")}else{
        paste("None of the paramsSelect: ",paste(paramsSelect,collapse=", ")," are present in shapeTbl params. Skipping subset.",sep="")
      }
  }

  if(!is.null(scaleRange) & any(unique(scaleRange$param) %in% unique(shapeTbl$param))){
  shapeTbl<-shapeTbl%>%dplyr::left_join(scaleRange,by="param")%>%
    dplyr::mutate(value=dplyr::case_when((!is.na(maxScale) & value>maxScale)~maxScale,
                                  (!is.na(minScale) & value<minScale)~minScale,
                                  TRUE~value))%>%
    dplyr::select(-maxScale,-minScale)
  paste("Used scaleRange to adjust value for params in shapeTbl: ",
        paste(unique(scaleRange$param)[unique(scaleRange$param) %in% unique(shapeTbl$param)],collaspe=","),sep="")
  print(scaleRange)
}

  shapeTbl<-droplevels(shapeTbl)
  }

if(!is.null(gridTbl)){
  if(any(xRange!="All")){gridTbl<-gridTbl%>%dplyr::filter(x %in% xRange);
  paste("Subset gridTbl x to xRange: ",xRange,sep="")}
  if(any(paramsSelect!="All")){
    if(any(paramsSelect %in% unique(gridTbl$param))){
    gridTbl<-gridTbl%>%dplyr::filter(param %in% paramsSelect);
  paste("Subset gridTbl param to paramsSelect: ",paramsSelect,sep="")}else{
    paste("None of the paramsSelect: ",paste(paramsSelect,collapse=", ")," are present in gridTbl params. Skipping subset.",sep="")
  }
    }


  if(!is.null(scaleRange) & any(unique(scaleRange$param) %in% unique(gridTbl$param))){
    gridTbl<-gridTbl%>%dplyr::left_join(scaleRange,by="param")%>%
      dplyr::mutate(value=dplyr::case_when((!is.na(maxScale) & value>maxScale)~maxScale,
                                    (!is.na(minScale) & value<minScale)~minScale,
                                    TRUE~value))%>%
      dplyr::select(-maxScale,-minScale)
    paste("Used scaleRange to adjust value for params in gridTbl: ",
          paste(unique(scaleRange$param)[unique(scaleRange$param) %in% unique(gridTbl$param)],collaspe=","),sep="")
    print(scaleRange)
  }
  gridTbl<-droplevels(gridTbl)
  }



#--------------------
# Cropped Gridded Data
#---------------------

if(!is.null(gridTbl) & !is.null(shape)){

shapeExpandEtxent<-shape@bbox
shapeExpandEtxent<-methods::as(raster::extent(as.vector(t(shapeExpandEtxent))), "SpatialPolygons")
sp::proj4string(shapeExpandEtxent)<-sp::CRS(sp::proj4string(shape)) # ASSIGN COORDINATE SYSTEM
gridTbl<-gridTbl%>%dplyr::mutate(gridID=seq(1:nrow(gridTbl)))
croppedCoords<-gridTbl%>%dplyr::select(lat,lon,gridID)%>%unique()
croppedCoords<-sp::SpatialPointsDataFrame(sp::SpatialPoints(coords=(cbind(croppedCoords$lon,croppedCoords$lat))),data=croppedCoords)
sp::proj4string(croppedCoords)<-sp::proj4string(shapeExpandEtxent)
croppedCoords<-raster::crop(croppedCoords,shapeExpandEtxent)
sp::gridded(croppedCoords)<-T

gridTbl<-gridTbl%>%dplyr::filter(gridID %in% unique((croppedCoords@data)$gridID))%>%dplyr::select(-gridID)

}

# Remove NA's

  if(!is.null(gridTbl)){gridTbl<-gridTbl%>%dplyr::filter(!is.na(value))}
  if(!is.null(shapeTbl)){shapeTbl<-shapeTbl%>%dplyr::filter(!is.na(value))}


#------------------
# Compare Scenarios
#------------------

# Compare Gridded Data
# Create Example Data For Testing
if(!is.null(gridTbl)){

if(length(unique(gridTbl$scenario))>1){
  # Get Diff Values
  if(is.null(scenRef)){
    print(paste("No reference scenario provided",sep=""))
    print(paste("Using ",unique(gridTbl$scenario)[1]," as reference",sep=""))
    scenRef_i = unique(gridTbl$scenario)[1]}else{
      if(!scenRef %in% unique(gridTbl$scenario)[1]){
        print(paste("scenario ",scenRef," not in scenarios",sep=""))
        print(paste("Using ",unique(gridTbl$scenario)[1]," as reference",sep=""))
        scenRef_i = unique(gridTbl$scenario)[1]}else{
          scenRef_i <- scenRef}
    } # Check if Ref Scenario Chosen

  # Calculate Diff Values
  gridTblDiff<-gridTbl%>%
    tidyr::spread(scenario,value)

  for (scenario_i in unique(gridTbl$scenario)[unique(gridTbl$scenario)!=scenRef_i]){
    tbl_temp1 <-gridTblDiff%>%
      dplyr::mutate(!!paste("Diff_ABS_",scenario_i,"_",scenRef_i,sep=""):=get(scenario_i)-get(scenRef_i),
                    classPalette="pal_div")%>%
      dplyr::select(-dplyr::one_of(c(scenario_i,scenRef_i)))
    tbl_temp1<-tbl_temp1%>%
      tidyr::gather(key=scenario,value=value,
                    -c(names(tbl_temp1)[!names(tbl_temp1) %in% paste("Diff_ABS_",scenario_i,"_",scenRef_i,sep="")]))%>%
      dplyr::filter(!is.na(value))

    tbl_temp2 <-gridTblDiff%>%
      dplyr::mutate(!!paste("Diff_PRCNT_",scenario_i,"_",scenRef_i,sep=""):=((get(scenario_i)-get(scenRef_i))*100/get(scenRef_i)),
                    classPalette="pal_div")%>%
      dplyr::select(-dplyr::one_of(c(scenario_i,scenRef_i)))
    tbl_temp2<-tbl_temp2%>%
      tidyr::gather(key=scenario,value=value,
                    -c(names(tbl_temp2)[!names(tbl_temp2) %in% paste("Diff_PRCNT_",scenario_i,"_",scenRef_i,sep="")]))%>%
      dplyr::filter(!is.na(value))

    gridTbl<-dplyr::bind_rows(gridTbl,tbl_temp1,tbl_temp2)
  }

  gridTbl <-gridTbl %>%
    dplyr::mutate(scenario=factor(scenario,
                                  levels=c(scenRef_i,
                                           unique(gridTbl$scenario)[unique(gridTbl$scenario)!=scenRef_i])))
}

# Check to see if correct columns are present in data
if(!"scenario" %in% names(gridTbl)){
  paste("'scenario' column not present in polygon data provided. Creating scenario 'scenario'",sep="")
  gridTbl<-gridTbl%>%dplyr::mutate(scenario="scenario")
}
if(!"subRegType" %in% names(gridTbl)){
  paste("'subRegType' column not present in polygon data provided. Creating subRegType 'subRegion'",sep="")
  gridTbl<-gridTbl%>%dplyr::mutate(subRegType="subRegion")
}

}

# Compare Shape Data
# Create Example Data for Testing
  if(!is.null(shapeTbl) | nrow(shapeTbl)>0){

if(length(unique(shapeTbl$scenario))>1){
  # Get Diff Values
  if(is.null(scenRef)){
    print(paste("No reference scenario provided",sep=""))
    print(paste("Using ",unique(shapeTbl$scenario)[1]," as reference",sep=""))
    scenRef_i = unique(shapeTbl$scenario)[1]}else{
      if(!scenRef %in% unique(shapeTbl$scenario)){
        print(paste("scenario ",scenRef," not in scenarios",sep=""))
        print(paste("Using ",unique(shapeTbl$scenario)[1]," as reference",sep=""))
        scenRef_i = unique(shapeTbl$scenario)[1]}else{
          scenRef_i <- scenRef}
    } # Check if Ref Scenario Chosen

  # Calculate Diff Values
  shapeTblDiff<-shapeTbl%>%
    tidyr::spread(scenario,value)

  for (scenario_i in unique(shapeTbl$scenario)[unique(shapeTbl$scenario)!=scenRef_i]){
    tbl_temp1 <-shapeTblDiff%>%
      dplyr::mutate(!!paste("Diff_ABS_",scenario_i,"_",scenRef_i,sep=""):=get(scenario_i)-get(scenRef_i),
                    classPalette="pal_div")%>%
      dplyr::select(-dplyr::one_of(c(scenario_i,scenRef_i)))
    tbl_temp1<-tbl_temp1%>%
      tidyr::gather(key=scenario,value=value,
                    -c(names(tbl_temp1)[!names(tbl_temp1) %in% paste("Diff_ABS_",scenario_i,"_",scenRef_i,sep="")]))%>%
      dplyr::filter(!is.na(value))

    tbl_temp2 <-shapeTblDiff%>%
     dplyr::mutate(!!paste("Diff_PRCNT_",scenario_i,"_",scenRef_i,sep=""):=((get(scenario_i)-get(scenRef_i))*100/get(scenRef_i)),
                    classPalette="pal_div")%>%
      dplyr::select(-dplyr::one_of(c(scenario_i,scenRef_i)))
    tbl_temp2<-tbl_temp2%>%
      tidyr::gather(key=scenario,value=value,
                    -c(names(tbl_temp2)[!names(tbl_temp2) %in% paste("Diff_PRCNT_",scenario_i,"_",scenRef_i,sep="")]))%>%
      dplyr::filter(!is.na(value))

    shapeTbl<-dplyr::bind_rows(shapeTbl,tbl_temp1,tbl_temp2)
  }

  shapeTbl <-shapeTbl %>%
    dplyr::mutate(scenario=factor(scenario,
                                  levels=c(scenRef_i,
                                           unique(shapeTbl$scenario)[unique(shapeTbl$scenario)!=scenRef_i])))
}


# Check to see if correct columns are present in data
if(!"scenario" %in% names(shapeTbl)){
  paste("'scenario' column not present in polygon data provided. Creating scenario 'scenario'",sep="")
  shapeTbl<-shapeTbl%>%dplyr::mutate(scenario="scenario")
}
if(!"subRegType" %in% names(shapeTbl)){
  paste("'subRegType' column not present in polygon data provided. Creating subRegType 'subRegion'",sep="")
  shapeTbl<-shapeTbl%>%dplyr::mutate(subRegType="subRegion")
}

  }


#------------------
# Re-Categorize Data
#------------------

# if(!is.null(shapeTbl)){
#
#     if(!is.null(scaleRangeCat) & any(unique(scaleRangeCat$param) %in% unique(shapeTbl$param))){
#
#     scaleRangeCat$newCat<-as.character(scaleRangeCat$newCat)
#     scaleRangeCat$newPal<-as.character(scaleRangeCat$newPal)
#
#     shapeTbl<-shapeTbl%>%dplyr::mutate(valueCat=as.character(value),palCat=as.character(classPalette))
#     for(i in 1:nrow(scaleRangeCat)){
#     shapeTbl<-shapeTbl%>%dplyr::mutate(valueCat=dplyr::case_when((value>=scaleRangeCat[i,]$minRange & value<=scaleRangeCat[i,]$maxRange)~scaleRangeCat[i,]$newCat,
#                                     TRUE~as.character(valueCat)),
#                                     palCat=dplyr::case_when((value>=scaleRangeCat[i,]$minRange & value<=scaleRangeCat[i,]$maxRange)~scaleRangeCat[i,]$newPal,
#                                                        TRUE~as.character(palCat)))
#
#     }
#
#     paste("Used scaleRangeCat to adjust valueCat and palCat for params in shapeTbl: ",
#           paste(unique(scaleRangeCat$param)[unique(scaleRangeCat$param) %in% unique(shapeTbl$param)],collaspe=","),sep="")
#   }
#
#   shapeTbl<-droplevels(shapeTbl)
# }
#
# if(!is.null(gridTbl)){
#
#   if(!is.null(scaleRangeCat) & any(unique(scaleRangeCat$param) %in% unique(gridTbl$param))){
#
#     scaleRangeCat$newCat<-as.character(scaleRangeCat$newCat)
#     scaleRangeCat$newPal<-as.character(scaleRangeCat$newPal)
#
#     gridTbl<-gridTbl%>%dplyr::mutate(valueCat=as.character(value),palCat=as.character(classPalette))
#     for(i in 1:nrow(scaleRangeCat)){
#       gridTbl<-gridTbl%>%dplyr::mutate(valueCat=dplyr::case_when((value>=scaleRangeCat[i,]$minRange & value<=scaleRangeCat[i,]$maxRange)~scaleRangeCat[i,]$newCat,
#                                                             TRUE~as.character(valueCat)),
#                                          palCat=dplyr::case_when((value>=scaleRangeCat[i,]$minRange & value<=scaleRangeCat[i,]$maxRange)~scaleRangeCat[i,]$newPal,
#                                                           TRUE~as.character(palCat)))
#
#     }
#
#     paste("Used scaleRangeCat to adjust valueCat and palCat for params in gridTbl: ",
#           paste(unique(scaleRangeCat$param)[unique(scaleRangeCat$param) %in% unique(gridTbl$param)],collaspe=","),sep="")
#   }
#
#   gridTbl<-droplevels(gridTbl)
# }


#------------------
# Create Folders if needed
#------------------

  if (!dir.exists(dirOutputs)){dir.create(dirOutputs)}
  if (!dir.exists(paste(dirOutputs, "/Maps", sep = ""))){
    dir.create(paste(dirOutputs, "/Maps", sep = ""))}

    if (!dir.exists(paste(dirOutputs, "/Maps/",boundaryRegionsSelect,sep = ""))){
      dir.create(paste(dirOutputs, "/Maps/",boundaryRegionsSelect,sep = ""))}

      if (!dir.exists(paste(dirOutputs, "/Maps/",boundaryRegionsSelect,"/raster",sep = ""))){
        dir.create(paste(dirOutputs, "/Maps/",boundaryRegionsSelect,"/raster",sep = ""))}

      if (!dir.exists(paste(dirOutputs, "/Maps/",boundaryRegionsSelect,"/raster/compareScen",sep = ""))){
          dir.create(paste(dirOutputs, "/Maps/",boundaryRegionsSelect,"/raster/compareScen",sep = ""))}

        for (scenario_i in unique(gridTbl$scenario)) {
          if (!dir.exists(paste(dirOutputs, "/Maps/",boundaryRegionsSelect,"/raster/", scenario_i,sep = ""))){
            dir.create(paste(dirOutputs,  "/Maps/",boundaryRegionsSelect,"/raster/",scenario_i,sep = ""))}

          if (!dir.exists(paste(dirOutputs, "/Maps/",boundaryRegionsSelect,"/raster/", scenario_i,"/byYear",sep = ""))){
            dir.create(paste(dirOutputs,  "/Maps/",boundaryRegionsSelect,"/raster/",scenario_i,"/byYear",sep = ""))}

        } # Close for scenario i

      for (subRegion_i in unique(shapeTbl$subRegType)) {
        if (!dir.exists(paste(dirOutputs, "/Maps/",boundaryRegionsSelect,"/",subRegion_i,sep = ""))){
          dir.create(paste(dirOutputs, "/Maps/",boundaryRegionsSelect,"/",subRegion_i,sep = ""))

          if (!dir.exists(paste(dirOutputs, "/Maps/",boundaryRegionsSelect,"/",subRegion_i,"/compareScen",sep = ""))){
            dir.create(paste(dirOutputs, "/Maps/",boundaryRegionsSelect,"/",subRegion_i,"/compareScen",sep = ""))}


          for (scenario_i in unique(shapeTbl$scenario)) {
            if (!dir.exists(paste(dirOutputs, "/Maps/",boundaryRegionsSelect,"/",subRegion_i,"/", scenario_i,sep = ""))){
              dir.create(paste(dirOutputs,  "/Maps/",boundaryRegionsSelect,"/",subRegion_i,"/", scenario_i,sep = ""))}

            if (!dir.exists(paste(dirOutputs, "/Maps/",boundaryRegionsSelect,"/",subRegion_i,"/",scenario_i,"/byYear",sep = ""))){
              dir.create(paste(dirOutputs,  "/Maps/",boundaryRegionsSelect,"/",subRegion_i,"/",scenario_i,"/byYear",sep = ""))}

          } # Close for scenario i

        } # Close subRegion directory
      } # Close subRegion


# -------------------
# Create Raster Plots
# -------------------

if(!is.null(gridTbl)){


  for (scenario_i in unique(gridTbl$scenario)){
    for (param_i in unique(gridTbl$param)){

      if(nrow(gridTbl%>%dplyr::filter(scenario==scenario_i,param==param_i))>0){
      animScaleGrid<-(gridTbl%>%dplyr::filter(scenario==scenario_i,param==param_i))$value
      animPrettyBreaksGrid<-scales::pretty_breaks(n=legendFixedBreaks)(animScaleGrid)
      animKmeanBreaksGrid<-sort(as.vector((stats::kmeans(animScaleGrid,centers=legendFixedBreaks))$centers[,1]))

      if((max(range(animScaleGrid))-min(range(animScaleGrid)))<1E-10 &
         (max(range(animScaleGrid))-min(range(animScaleGrid)))>-1E-10){animScaleGridRange=min(animScaleGrid)}else{
        animScaleGridRange=range(animScaleGrid)
      }
      if(mean(animScaleGridRange,na.rm = T)<0.01 & mean(animScaleGridRange,na.rm = T)>(-0.01)){animLegendDigits<-4}else{
        if(mean(animScaleGridRange,na.rm = T)<0.1 & mean(animScaleGridRange,na.rm = T)>(-0.1)){animLegendDigits<-3}else{
          if(mean(animScaleGridRange,na.rm = T)<1 & mean(animScaleGridRange,na.rm = T)>(-1)){animLegendDigits<-2}else{
            if(mean(animScaleGridRange,na.rm = T)<10 & mean(animScaleGridRange,na.rm = T)>(-10)){animLegendDigits<-1}else{animLegendDigits<-0}}}}

        # Figure 1 : each param: If class > 1 { (Map x Class) x Selected Years}

      gridTblx<-gridTbl%>%dplyr::filter(scenario==scenario_i,param==param_i)

        for (x_i in unique(gridTblx$x)){

            datax<-gridTblx%>%dplyr::filter(x==x_i)
        if(nrow(datax)>1){
            legendTitle<-unique(datax$units)
            fillPalette<-as.character(unique(datax$classPalette))

            datax<-datax%>%dplyr::select(lat,lon,class,value)%>%
              tidyr::spread(key=class,value=value)

            rasterx<-sp::SpatialPointsDataFrame(sp::SpatialPoints(coords=(cbind(datax$lon,datax$lat))),data=datax)
            sp::proj4string(rasterx)<-sp::proj4string(shape)
            rasterx<-raster::crop(rasterx,shapeExpandEtxent)
            sp::gridded(rasterx)<-T

            scaleData<-datax%>%dplyr::select(-lat,-lon)
            if(mean(range(scaleData,na.rm=T),na.rm = T)<0.01 & mean(range(scaleData,na.rm=T),na.rm = T)>(-0.01)){legendDigits<-4}else{
              if(mean(range(scaleData,na.rm=T),na.rm = T)<0.1 & mean(range(scaleData,na.rm=T),na.rm = T)>(-0.1)){legendDigits<-3}else{
                if(mean(range(scaleData,na.rm=T),na.rm = T)<1 & mean(range(scaleData,na.rm=T),na.rm = T)>(-1)){legendDigits<-2}else{
                  if(mean(range(scaleData,na.rm=T),na.rm = T)<10 & mean(range(scaleData,na.rm=T),na.rm = T)>(-10)){legendDigits<-1}else{legendDigits<-0}}}}

            mapx<-rasterx
            mapx@data<-mapx@data%>%dplyr::select(-lat,-lon)


            if(length(names(mapx@data))==1){
              legendOutsideAnimated=legendOutsideSingle
              legendTitleAnimated=legendTitle
              panelLabelAnimated=paste(x_i)
              legendAnimatedPosition=legendPositionS
              legendTitleSizeAnim = legendTitleSizeS
              legendTextSizeAnim = legendTextSizeS
              legendBreaksAnim = animKmeanBreaksGrid
              legendStyleAnim="fixed"}else{
                legendStyleAnim="fixed"
                legendBreaksAnim = animKmeanBreaksGrid
                legendOutsideAnimated=legendOutsideSingle
                legendTitleAnimated=paste(x_i,"\n",legendTitle,sep="")
                panelLabelAnimated=NULL
                legendAnimatedPosition=legendPositionS
                legendTitleSizeAnim = legendTitleSizeS
                legendTextSizeAnim = legendTextSizeS
              }

            metis.map(underLayer=underLayer,
                      panelLabel=panelLabelAnimated,
                      dataPolygon=shape,
                      dataGrid=mapx,
                      fillColumn = names(mapx@data),
                      legendShow = T,
                      legendOutside = legendOutsideAnimated,
                      facetFreeScale = F,
                      frameShow = T,
                      labels=labels,
                      labelsSize = labelsSize,
                      legendTitle =legendTitleAnimated,
                      legendTitleSize = legendTitleSizeAnim,legendTextSize = legendTextSizeAnim,
                      legendStyle=legendStyleAnim,
                      legendBreaks = legendBreaksAnim,
                      legendFixedBreaks=legendFixedBreaks,
                      legendDigits = animLegendDigits,
                      legendOutsidePosition = legendOutsidePosition,
                      legendPosition = legendAnimatedPosition,
                      fillPalette = fillPalette,
                      bgColor = bgColorChosen,
                      figWidth=figWidth,
                      figHeight=figHeight,
                    fileName = paste("map_",boundaryRegionsSelect,"_raster_",param_i,"_",x_i,"_",scenario_i,nameAppend,"_KMEANS",sep=""),
                    dirOutputs = paste(dirOutputs,"/Maps/",boundaryRegionsSelect,"/raster/", scenario_i,"/byYear",sep = ""))


            if(length(names(mapx@data))==1){
              legendBreaksAnim = animPrettyBreaksGrid
              legendStyleAnim="fixed"}else{
                legendStyleAnim="fixed"
                legendBreaksAnim = animPrettyBreaksGrid
              }

            metis.map(panelLabel=panelLabelAnimated,
                      underLayer=underLayer, dataPolygon=shape,
                      dataGrid=mapx,
                      fillColumn = names(mapx@data),
                      legendShow = T,
                      legendOutside = legendOutsideAnimated,
                      facetFreeScale = F,
                      frameShow = T,
                      labels=labels,
                      labelsSize = labelsSize,
                      legendTitle =legendTitleAnimated,
                      legendTitleSize = legendTitleSizeAnim,legendTextSize = legendTextSizeAnim,
                      legendStyle=legendStyleAnim,
                      legendBreaks = animPrettyBreaksGrid,
                      legendFixedBreaks=legendFixedBreaks,
                      legendDigits = animLegendDigits,
                      legendOutsidePosition = legendOutsidePosition,
                      legendPosition = legendAnimatedPosition,
                      fillPalette = fillPalette,
                      bgColor = bgColorChosen,
                      figWidth=figWidth,
                      figHeight=figHeight,
                    fileName = paste("map_",boundaryRegionsSelect,"_raster_",param_i,"_",x_i,"_",scenario_i,nameAppend,"_PRETTY",sep=""),
                    dirOutputs = paste(dirOutputs,"/Maps/",boundaryRegionsSelect,"/raster/", scenario_i,"/byYear",sep = ""))

            if(length(names(mapx@data))==1){
              legendOutsideAnimated=legendOutsideSingle
              legendTitleSizeAnim = legendTitleSizeS
              legendTextSizeAnim = legendTextSizeS}else{
                legendOutsideAnimated=F
                legendAnimatedPosition=legendPosition
                legendTitleSizeAnim = legendTitleSizeI
                legendTextSizeAnim = legendTextSizeI
              }

            metis.map(panelLabel=panelLabelAnimated,
                      underLayer=underLayer, dataPolygon=shape,
                      dataGrid=mapx,
                      fillColumn = names(mapx@data),
                      legendShow = T,
                      legendOutside = legendOutsideAnimated,
                      facetFreeScale = T,
                      frameShow = T,
                      labels=labels,
                      labelsSize = labelsSize,
                      legendTitleSize = legendTitleSizeAnim,legendTextSize = legendTextSizeAnim,
                      legendTitle = legendTitleAnimated,
                      legendStyle="kmeans",
                      legendDigits = animLegendDigits,
                      legendFixedBreaks=legendFixedBreaks,
                      legendOutsidePosition = legendOutsidePosition,
                      legendPosition = legendAnimatedPosition,
                      fillPalette = fillPalette,
                      bgColor = bgColorChosen,
                      figWidth=figWidth,
                      figHeight=figHeight,
                    fileName = paste("map_",boundaryRegionsSelect,"_raster_",param_i,"_",x_i,"_",scenario_i,nameAppend,"_FREESCALE",sep=""),
                    dirOutputs = paste(dirOutputs,"/Maps/",boundaryRegionsSelect,"/raster/", scenario_i,"/byYear",sep = ""))

            } # if nrow(datax) > 1
          }# Close years loop

          # Animate 1 : each param: If class > 1 { (Map x Class) x Anim Years}

        if(animateOn==T){

          checkIM <- system("cmd.exe",input="magick -version")
          if (checkIM!=0) stop("Could not find ImageMagick. Make sure it is installed and included in the systems PATH")

          animName<-paste("anim_",boundaryRegionsSelect,"_raster_",param_i,"_",scenario_i,nameAppend,"_PRETTY.gif",sep="")
          processed <- system("cmd.exe",input=paste("magick -delay ",
                                                    delay=delay,
                                                    " ", paste(dirOutputs,"/Maps/",boundaryRegionsSelect,"/raster/", scenario_i,"/byYear",
                                                    "/*",param_i,"*PRETTY.png ",sep = ""),
                                                    paste(dirOutputs,"/Maps/",boundaryRegionsSelect,"/raster/", scenario_i,"/",
                                                          animName,sep = ""),
                                                    "\"",sep=""))

          animName<-paste("anim_",boundaryRegionsSelect,"_raster_",param_i,"_",scenario_i,nameAppend,"_KMEANS.gif",sep="")
          processed <- system("cmd.exe",input=paste("magick -delay ",
                                                    delay=delay,
                                                    " ", paste(dirOutputs,"/Maps/",boundaryRegionsSelect,"/raster/", scenario_i,"/byYear",
                                                               "/*",param_i,"*KMEANS.png ",sep = ""),
                                                    paste(dirOutputs,"/Maps/",boundaryRegionsSelect,"/raster/", scenario_i,"/",
                                                          animName,sep = ""),
                                                    "\"",sep=""))

          animName<-paste("anim_",boundaryRegionsSelect,"_raster_",param_i,"_",scenario_i,nameAppend,"_FREESCALE.gif",sep="")
          processed <- system("cmd.exe",input=paste("magick -delay ",
                                                    delay=delay,
                                                    " ", paste(dirOutputs,"/Maps/",boundaryRegionsSelect,"/raster/", scenario_i,"/byYear",
                                                               "/*",param_i,"*FREESCALE.png ",sep = ""),
                                                    paste(dirOutputs,"/Maps/",boundaryRegionsSelect,"/raster/", scenario_i,"/",
                                                          animName,sep = ""),
                                                    "\"",sep=""))

          #unlink(paste(dirOutputs,"/Maps/",boundaryRegionsSelect,"/raster/", scenario_i,"/byYear/animate_",param_i,sep = ""), recursive = TRUE) #-------------- cleaning up plots and temporary variables
          } # If Animate ON==t


        #------------------------------
        # Figure 2 : each param: If class ==1 { Map x years}
        #-----------------------------

      checkTbl<-gridTbl%>%dplyr::filter(scenario==scenario_i,param==param_i)
      checkTbl<-droplevels(checkTbl)

       if(length(unique(checkTbl$class))==1){
         rm(checkTbl)

          datax<-gridTbl%>%dplyr::filter(scenario==scenario_i,param==param_i)
        if(nrow(datax)>1){
          legendTitle<-unique(datax$units)
          fillPalette<-as.character(unique(datax$classPalette))

          datax<-datax%>%dplyr::select(lat,lon,x,value)%>%
            tidyr::spread(key=x,value=value)

          rasterx<-sp::SpatialPointsDataFrame(sp::SpatialPoints(coords=(cbind(datax$lon,datax$lat))),data=datax)
          sp::proj4string(rasterx)<-sp::proj4string(shape)
          rasterx<-raster::crop(rasterx,shapeExpandEtxent)
          sp::gridded(rasterx)<-T

          scaleData<-datax%>%dplyr::select(-lat,-lon)
          if(mean(range(scaleData,na.rm=T),na.rm = T)<0.01 & mean(range(scaleData,na.rm=T),na.rm = T)>(-0.01)){legendDigits<-4}else{
            if(mean(range(scaleData,na.rm=T),na.rm = T)<0.1 & mean(range(scaleData,na.rm=T),na.rm = T)>(-0.1)){legendDigits<-3}else{
              if(mean(range(scaleData,na.rm=T),na.rm = T)<1 & mean(range(scaleData,na.rm=T),na.rm = T)>(-1)){legendDigits<-2}else{
                if(mean(range(scaleData,na.rm=T),na.rm = T)<10 & mean(range(scaleData,na.rm=T),na.rm = T)>(-10)){legendDigits<-1}else{legendDigits<-0}}}}

          mapx<-rasterx
          mapx@data<-mapx@data%>%dplyr::select(-lat,-lon)
          names(mapx@data)<-paste("X",names(mapx@data),sep="")

          metis.map(underLayer=underLayer, dataPolygon=shape,
                  dataGrid=mapx,
                  fillColumn = names(mapx@data),
                  legendShow = T,
                  legendOutside = legendOutsideSingle,
                  facetFreeScale = F,
                  frameShow = T,
                  labels=labels,
                  labelsSize = labelsSize,
                  legendTitle =legendTitle,legendTitleSize = legendTitleSizeS,legendTextSize = legendTextSizeS,
                  legendStyle="kmeans",
                  legendFixedBreaks=legendFixedBreaks,
                  legendDigits = legendDigits,
                  legendOutsidePosition = legendOutsidePosition,
                  legendPosition = legendPositionS,
                  fillPalette = fillPalette,
                  bgColor = bgColorChosen,
                  figWidth=figWidth,figHeight=figHeight,
                  fileName = paste("map_",boundaryRegionsSelect,"_raster_",param_i,"_",scenario_i,nameAppend,"_KMEANS",sep=""),
                  dirOutputs = paste(dirOutputs,"/Maps/",boundaryRegionsSelect,"/raster/", scenario_i,sep = ""))

          metis.map(underLayer=underLayer, dataPolygon=shape,
                  dataGrid=mapx,
                  fillColumn = names(mapx@data),
                  legendShow = T,
                  legendOutside = legendOutsideSingle,
                  facetFreeScale = F,
                  frameShow = T,
                  labels=labels,
                  labelsSize = labelsSize,
                  legendTitle =legendTitle,legendTitleSize = legendTitleSizeS,legendTextSize = legendTextSizeS,
                  legendStyle="pretty",
                  legendFixedBreaks=legendFixedBreaks,
                  legendDigits = legendDigits,
                  legendOutsidePosition = legendOutsidePosition,
                  legendPosition = legendPositionS,
                  fillPalette = fillPalette,
                  bgColor = bgColorChosen,
                  figWidth=figWidth,figHeight=figHeight,
                  fileName = paste("map_",boundaryRegionsSelect,"_raster_",param_i,"_",scenario_i,nameAppend,"_PRETTY",sep=""),
                  dirOutputs = paste(dirOutputs,"/Maps/",boundaryRegionsSelect,"/raster/", scenario_i,sep = ""))


          if(length(names(mapx@data))==1){
            legendOutsideAnimated=legendOutsideSingle
            legendTitleSizeAnim = legendTitleSizeS
            legendTextSizeAnim = legendTextSizeS}else{
              legendOutsideAnimated=F
              legendTitleSizeAnim = legendTitleSizeI
              legendTextSizeAnim = legendTextSizeI
            }

          metis.map(underLayer=underLayer, dataPolygon=shape,
                  dataGrid=mapx,
                  fillColumn = names(mapx@data),
                  legendShow = T,
                  legendOutside = legendOutsideAnimated,
                  facetFreeScale = T,
                  frameShow = T,
                  labels=labels,
                  labelsSize = labelsSize,
                  legendTitle =legendTitle,legendTitleSize = legendTitleSizeAnim,legendTextSize = legendTextSizeAnim,
                  legendStyle="kmeans",
                  legendFixedBreaks=legendFixedBreaks,
                  legendDigits = legendDigits,
                  legendOutsidePosition = legendOutsidePosition,
                  legendPosition = legendPosition,
                  fillPalette = fillPalette,
                  bgColor = bgColorChosen,
                  figWidth=figWidth,figHeight=figHeight,
                  fileName = paste("map_",boundaryRegionsSelect,"_raster_",param_i,"_",scenario_i,nameAppend,"_FREESCALE",sep=""),
                  dirOutputs = paste(dirOutputs,"/Maps/",boundaryRegionsSelect,"/raster/", scenario_i,sep = ""))

        } # if(nrow(datax)>1){

          # Mean for all years provided

          datax<-gridTbl%>%dplyr::filter(scenario==scenario_i,param==param_i)
          if(nrow(datax)>1){
            legendTitle<-unique(datax$units)
            fillPalette<-as.character(unique(datax$classPalette))

            datax<-datax%>%dplyr::select(lat,lon,x,value)%>%
              dplyr::group_by(lat,lon)%>%
              dplyr::summarize(!!paste("Mean_",min(xRange),"to",max(xRange),sep=""):=mean(value))%>%
              dplyr::ungroup()

            rasterx<-sp::SpatialPointsDataFrame(sp::SpatialPoints(coords=(cbind(datax$lon,datax$lat))),data=datax)
            sp::proj4string(rasterx)<-sp::proj4string(shape)
            rasterx<-raster::crop(rasterx,shapeExpandEtxent)
            sp::gridded(rasterx)<-T

            scaleData<-datax%>%dplyr::select(-lat,-lon)
            if(mean(range(scaleData,na.rm=T),na.rm = T)<0.01 & mean(range(scaleData,na.rm=T),na.rm = T)>(-0.01)){legendDigits<-4}else{
              if(mean(range(scaleData,na.rm=T),na.rm = T)<0.1 & mean(range(scaleData,na.rm=T),na.rm = T)>(-0.1)){legendDigits<-3}else{
                if(mean(range(scaleData,na.rm=T),na.rm = T)<1 & mean(range(scaleData,na.rm=T),na.rm = T)>(-1)){legendDigits<-2}else{
                  if(mean(range(scaleData,na.rm=T),na.rm = T)<10 & mean(range(scaleData,na.rm=T),na.rm = T)>(-10)){legendDigits<-1}else{legendDigits<-0}}}}

            mapx<-rasterx
            mapx@data<-mapx@data%>%dplyr::select(-lat,-lon)
            names(mapx@data)<-paste("X",names(mapx@data),sep="")

            metis.map(underLayer=underLayer, dataPolygon=shape,
                      dataGrid=mapx,
                      fillColumn = names(mapx@data),
                      legendShow = T,
                      legendOutside = legendOutsideSingle,
                      facetFreeScale = F,
                      frameShow = T,
                      labels=labels,
                      labelsSize = labelsSize,
                      panelLabel = paste(names(datax)[!names(datax) %in% c("lat","lon")],sep=""),
                      legendTitle =paste(legendTitle,sep=""),
                      legendTitleSize = legendTitleSizeS,legendTextSize = legendTextSizeS,
                      legendStyle="kmeans",
                      legendFixedBreaks=legendFixedBreaks,
                      legendDigits = legendDigits,
                      legendOutsidePosition = legendOutsidePosition,
                      legendPosition = legendPositionS,
                      fillPalette = fillPalette,
                      bgColor = bgColorChosen,
                      figWidth=figWidth,figHeight=figHeight,
                      fileName = paste("map_",boundaryRegionsSelect,"_raster_",param_i,"_",scenario_i,nameAppend,"_MEAN_KMEANS",sep=""),
                      dirOutputs = paste(dirOutputs,"/Maps/",boundaryRegionsSelect,"/raster/", scenario_i,sep = ""))

            metis.map(underLayer=underLayer, dataPolygon=shape,
                      dataGrid=mapx,
                      fillColumn = names(mapx@data),
                      legendShow = T,
                      legendOutside = legendOutsideSingle,
                      facetFreeScale = F,
                      frameShow = T,
                      labels=labels,
                      labelsSize = labelsSize,
                      panelLabel = paste(names(datax)[!names(datax) %in% c("lat","lon")],sep=""),
                      legendTitle =paste(legendTitle,sep=""),legendTitleSize = legendTitleSizeS,legendTextSize = legendTextSizeS,
                      legendStyle="pretty",
                      legendFixedBreaks=legendFixedBreaks,
                      legendDigits = legendDigits,
                      legendOutsidePosition = legendOutsidePosition,
                      legendPosition = legendPositionS,
                      fillPalette = fillPalette,
                      bgColor = bgColorChosen,
                      figWidth=figWidth,figHeight=figHeight,
                      fileName = paste("map_",boundaryRegionsSelect,"_raster_",param_i,"_",scenario_i,nameAppend,"_MEAN_PRETTY",sep=""),
                      dirOutputs = paste(dirOutputs,"/Maps/",boundaryRegionsSelect,"/raster/", scenario_i,sep = ""))


            if(length(names(mapx@data))==1){
              legendOutsideAnimated=legendOutsideSingle
              legendTitleSizeAnim = legendTitleSizeS
              legendTextSizeAnim = legendTextSizeS}else{
                legendOutsideAnimated=F
                legendTitleSizeAnim = legendTitleSizeI
                legendTextSizeAnim = legendTextSizeI
              }

            metis.map(underLayer=underLayer, dataPolygon=shape,
                      dataGrid=mapx,
                      fillColumn = names(mapx@data),
                      legendShow = T,
                      legendOutside = legendOutsideAnimated,
                      facetFreeScale = T,
                      frameShow = T,
                      labels=labels,
                      labelsSize = labelsSize,
                      panelLabel = paste(names(datax)[!names(datax) %in% c("lat","lon")],sep=""),
                      legendTitle =paste(legendTitle,sep=""),legendTitleSize = legendTitleSizeAnim,legendTextSize =  legendTextSizeAnim,
                      legendStyle="kmeans",
                      legendFixedBreaks=legendFixedBreaks,
                      legendDigits = legendDigits,
                      legendOutsidePosition = legendOutsidePosition,
                      legendPosition = legendPositionS,
                      fillPalette = fillPalette,
                      bgColor = bgColorChosen,
                      figWidth=figWidth,figHeight=figHeight,
                      fileName = paste("map_",boundaryRegionsSelect,"_raster_",param_i,"_",scenario_i,nameAppend,"_MEAN_FREESCALE",sep=""),
                      dirOutputs = paste(dirOutputs,"/Maps/",boundaryRegionsSelect,"/raster/", scenario_i,sep = ""))


        } # if(nrow(datax)>1){
        } # If number of classes == 1

     } # If nrow greater than 0
      } # close Params
  } # Close scenario loop
} # Close if gridTbl is Null

# -------------------
# Create Polygon Plots for Each scenario
# -------------------

if(!is.null(shapeTbl)){

for (scenario_i in unique(shapeTbl$scenario)){
for (subRegType_i in unique(shapeTbl$subRegType)){
for (param_i in unique(shapeTbl$param)){

  if(nrow(shapeTbl%>%dplyr::filter(subRegType==subRegType_i,scenario==scenario_i,param==param_i))>0){

  animScalePoly<-(shapeTbl%>%dplyr::filter(subRegType==subRegType_i,scenario==scenario_i,param==param_i))$value
  animPrettyBreaksPoly<-scales::pretty_breaks(n=legendFixedBreaks)(animScalePoly)
  animKmeanBreaksPoly<-sort(as.vector((stats::kmeans(animScalePoly,centers=legendFixedBreaks))$centers[,1]))

  if((max(range(animScalePoly))-min(range(animScalePoly)))<1E-10 &
     (max(range(animScalePoly))-min(range(animScalePoly)))>-1E-10){animScalePolyRange=min(animScalePoly)}else{
       animScalePolyRange=range(animScalePoly)
     }

  if(mean(animScalePolyRange,na.rm = T)<0.01 & mean(animScalePolyRange,na.rm = T)>(-0.01)){animLegendDigits<-4}else{
    if(mean(animScalePolyRange,na.rm = T)<0.1 & mean(animScalePolyRange,na.rm = T)>(-0.1)){animLegendDigits<-3}else{
      if(mean(animScalePolyRange,na.rm = T)<1 & mean(animScalePolyRange,na.rm = T)>(-1)){animLegendDigits<-2}else{
        if(mean(animScalePolyRange,na.rm = T)<10 & mean(animScalePolyRange,na.rm = T)>(-10)){animLegendDigits<-1}else{animLegendDigits<-0}}}}


# Figure 1 : each param: If class > 1 { (Map x Class) x Selected Years}

  shapeTblx<-shapeTbl%>%dplyr::filter(region==boundaryRegionsSelect,scenario==scenario_i,subRegType==subRegType_i,
                                  param==param_i)

for (x_i in unique(shapeTbl$x)){

  datax<-shapeTblx%>%dplyr::filter(x==x_i)

  if(nrow(datax)>1){
  legendTitle<-unique(datax$units)
  fillPalette<-as.character(unique(datax$classPalette))

  datax<-datax%>%dplyr::select(subRegion,class,value)%>%
  tidyr::spread(key=class,value=value)

scaleData<-datax%>%dplyr::select(-subRegion)
if(mean(range(scaleData,na.rm=T),na.rm = T)<0.01 & mean(range(scaleData,na.rm=T),na.rm = T)>(-0.01)){legendDigits<-4}else{
if(mean(range(scaleData,na.rm=T),na.rm = T)<0.1 & mean(range(scaleData,na.rm=T),na.rm = T)>(-0.1)){legendDigits<-3}else{
if(mean(range(scaleData,na.rm=T),na.rm = T)<1 & mean(range(scaleData,na.rm=T),na.rm = T)>(-1)){legendDigits<-2}else{
if(mean(range(scaleData,na.rm=T),na.rm = T)<10 & mean(range(scaleData,na.rm=T),na.rm = T)>(-10)){legendDigits<-1}else{legendDigits<-0}}}}

mapx<-shape
mapx@data<-mapx@data%>%dplyr::left_join(datax)%>%
  dplyr::select(names(datax))


if(length(names(mapx@data%>%dplyr::select(-subRegion)))==1){
  legendOutsideAnimated=legendOutsideSingle
  legendTitleAnimated=legendTitle
  panelLabelAnimated=paste(x_i)
  legendAnimatedPosition=legendPositionS
  legendTitleSizeAnim = legendTitleSizeS
  legendTextSizeAnim = legendTextSizeS
  legendBreaksAnim = animKmeanBreaksGrid
  legendStyleAnim="fixed"}else{
    legendStyleAnim="fixed"
    legendBreaksAnim = animKmeanBreaksGrid
    legendOutsideAnimated=T
    legendTitleAnimated=paste(x_i,"\n",legendTitle,sep="")
    panelLabelAnimated=NULL
    legendAnimatedPosition=legendPositionS
    legendTitleSizeAnim = legendTitleSizeS
    legendTextSizeAnim = legendTextSizeS
  }

metis.map(panelLabel=panelLabelAnimated,
          underLayer=underLayer, dataPolygon=mapx,
          fillColumn = names(mapx@data%>%dplyr::select(-subRegion)),
          legendShow = T,
          legendOutside = legendOutsideAnimated,
          facetFreeScale = F,
          frameShow = T,
          labels=labels,
          labelsSize = labelsSize,
          legendTitleSize = legendTitleSizeAnim,legendTextSize = legendTextSizeAnim,
          legendTitle =legendTitleAnimated,
          legendStyle=legendStyleAnim,
          legendBreaks = animKmeanBreaksPoly,
          legendFixedBreaks=legendFixedBreaks,
          legendDigits = animLegendDigits,
          legendOutsidePosition = legendOutsidePosition,
          legendPosition = legendAnimatedPosition,
          fillPalette = fillPalette,
          bgColor = bgColorChosen,
          figWidth=figWidth,
          figHeight=figHeight,
        fileName = paste("map_",boundaryRegionsSelect,"_",subRegType_i,"_",param_i,"_",x_i,"_",scenario_i,nameAppend,"_KMEANS",sep=""),
        dirOutputs = paste(dirOutputs,"/Maps/",boundaryRegionsSelect,"/",subRegion_i,"/", scenario_i,"/byYear",sep = ""))


if(length(names(mapx@data%>%dplyr::select(-subRegion)))==1){
  legendBreaksAnim = animPrettyBreaksGrid
  legendStyleAnim="fixed"}else{
    legendStyleAnim="fixed"
    legendBreaksAnim = animPrettyBreaksGrid
  }

metis.map(panelLabel=panelLabelAnimated,
          underLayer=underLayer, dataPolygon=mapx,
          fillColumn = names(mapx@data%>%dplyr::select(-subRegion)),
          legendShow = T,
          legendOutside = legendOutsideAnimated,
          facetFreeScale = F,
          frameShow = T,
          labels=labels,
          labelsSize = labelsSize,
          legendTitle =legendTitleAnimated,
          legendTitleSize = legendTitleSizeAnim,legendTextSize = legendTextSizeAnim,
          legendStyle=legendStyleAnim,
          legendBreaks = animPrettyBreaksPoly,
          legendFixedBreaks=legendFixedBreaks,
          legendDigits = animLegendDigits,
          legendOutsidePosition = legendOutsidePosition,
          legendPosition = legendAnimatedPosition,
          fillPalette = fillPalette,
          bgColor = bgColorChosen,
          figWidth=figWidth,
          figHeight=figHeight,
        fileName = paste("map_",boundaryRegionsSelect,"_",subRegType_i,"_",param_i,"_",x_i,"_",scenario_i,nameAppend,"_PRETTY",sep=""),
        dirOutputs = paste(dirOutputs,"/Maps/",boundaryRegionsSelect,"/",subRegion_i,"/", scenario_i,"/byYear",sep = ""))

if(length(names(mapx@data%>%dplyr::select(-subRegion)))==1){
  legendOutsideAnimated=legendOutsideSingle
  legendAnimatedPosition
  legendTitleSizeAnim = legendTitleSizeS
  legendTextSizeAnim = legendTextSizeS}else{
    legendOutsideAnimated=F
    legendAnimatedPosition=legendPosition
    legendTitleSizeAnim = legendTitleSizeI
    legendTextSizeAnim = legendTextSizeI
  }

metis.map(panelLabel= panelLabelAnimated,underLayer=underLayer, dataPolygon=mapx,
          fillColumn = names(mapx@data%>%dplyr::select(-subRegion)),
          legendShow = T,
          legendOutside = legendOutsideAnimated,
          facetFreeScale = T,
          frameShow = T,
          labels=labels,
          labelsSize = labelsSize,
          legendTitle =legendTitleAnimated,
          legendTitleSize = legendTitleSizeAnim,legendTextSize = legendTextSizeAnim,
          legendStyle="kmeans",
          legendFixedBreaks=legendFixedBreaks,
          legendDigits = animLegendDigits,
          legendOutsidePosition = legendOutsidePosition,
          legendPosition = legendAnimatedPosition,
          fillPalette = fillPalette,
          bgColor = bgColorChosen,
          figWidth=figWidth,
          figHeight=figHeight,
        fileName = paste("map_",boundaryRegionsSelect,"_",subRegType_i,"_",param_i,"_",x_i,"_",scenario_i,nameAppend,"_FREESCALE",sep=""),
        dirOutputs = paste(dirOutputs,"/Maps/",boundaryRegionsSelect,"/",subRegion_i,"/", scenario_i,"/byYear",sep = ""))


}# if(nrow(datax)>1){
}# Close years loop

# Animate 1 : each param: If class > 1 { (Map x Class) x Anim Years}

  if(animateOn==T){

    checkIM <- system("cmd.exe",input="magick -version")
    if (checkIM!=0) stop("Could not find ImageMagick. Make sure it is installed and included in the systems PATH")

    animName<-paste("anim_",boundaryRegionsSelect,"_",subRegType_i,"_",param_i,"_",scenario_i,nameAppend,"_PRETTY.gif",sep="")
    processed <- system("cmd.exe",input=paste("magick -delay ",
                                              delay=delay,
                                              " ", paste(dirOutputs,"/Maps/",boundaryRegionsSelect,"/",subRegion_i,"/", scenario_i,"/byYear",
                                                         "/*",param_i,"*PRETTY.png ",sep = ""),
                                              paste(dirOutputs,"/Maps/",boundaryRegionsSelect,"/",subRegion_i,"/", scenario_i,"/",
                                                    animName,sep = ""),
                                              "\"",sep=""))

    animName<-paste("anim_",boundaryRegionsSelect,"_",subRegType_i,"_",param_i,"_",scenario_i,nameAppend,"_KMEANS.gif",sep="")
    processed <- system("cmd.exe",input=paste("magick -delay ",
                                              delay=delay,
                                              " ", paste(dirOutputs,"/Maps/",boundaryRegionsSelect,"/",subRegion_i,"/", scenario_i,"/byYear",
                                                         "/*",param_i,"*KMEANS.png ",sep = ""),
                                              paste(dirOutputs,"/Maps/",boundaryRegionsSelect,"/",subRegion_i,"/", scenario_i,"/",
                                                    animName,sep = ""),
                                              "\"",sep=""))

    animName<-paste("anim_",boundaryRegionsSelect,"_",subRegType_i,"_",param_i,"_",scenario_i,nameAppend,"_FREESCALE.gif",sep="")
    processed <- system("cmd.exe",input=paste("magick -delay ",
                                              delay=delay,
                                              " ", paste(dirOutputs,"/Maps/",boundaryRegionsSelect,"/",subRegion_i,"/", scenario_i,"/byYear",
                                                         "/*",param_i,"*FREESCALE.png ",sep = ""),
                                              paste(dirOutputs,"/Maps/",boundaryRegionsSelect,"/",subRegion_i,"/", scenario_i,"/",
                                                    animName,sep = ""),
                                              "\"",sep=""))

    #unlink(paste(dirOutputs,"/Maps/",boundaryRegionsSelect,"/",subRegion_i,"/", scenario_i,"/byYear/animate_",param_i,sep = ""), recursive = TRUE) #-------------- cleaning up plots and temporary variables
  } # If Animate ON==t


#------------------------------
# Figure 2 : each param: If class ==1 { Map x years}
#-----------------------------

  checkTbl<-shapeTbl%>%dplyr::filter(region==boundaryRegionsSelect,scenario==scenario_i,subRegType==subRegType_i,param==param_i)
  checkTbl<-droplevels(checkTbl)
if(length(unique(checkTbl$class))==1){

  rm(checkTbl)

      datax<-shapeTbl%>%dplyr::filter(region==boundaryRegionsSelect,scenario==scenario_i,subRegType==subRegType_i,param==param_i)
      if(nrow(datax)>1){
      legendTitle<-paste(unique(datax$units),sep="")
      fillPalette<-as.character(unique(datax$classPalette))

      datax<-datax%>%dplyr::select(subRegion,x,value)%>%
        tidyr::spread(key=x,value=value)

      scaleData<-datax%>%dplyr::select(-subRegion)
      if(mean(range(scaleData,na.rm=T),na.rm = T)<0.01 & mean(range(scaleData,na.rm=T),na.rm = T)>(-0.01)){legendDigits<-4}else{
        if(mean(range(scaleData,na.rm=T),na.rm = T)<0.1 & mean(range(scaleData,na.rm=T),na.rm = T)>(-0.1)){legendDigits<-3}else{
          if(mean(range(scaleData,na.rm=T),na.rm = T)<1 & mean(range(scaleData,na.rm=T),na.rm = T)>(-1)){legendDigits<-2}else{
            if(mean(range(scaleData,na.rm=T),na.rm = T)<10 & mean(range(scaleData,na.rm=T),na.rm = T)>(-10)){legendDigits<-1}else{legendDigits<-0}}}}

      mapx<-shape
      mapx@data<-mapx@data%>%dplyr::left_join(datax)%>%
        dplyr::select(names(datax))

      metis.map(underLayer=underLayer, dataPolygon=mapx,
              fillColumn = names(mapx@data%>%dplyr::select(-subRegion)),
              legendShow = T,
              legendOutside = legendOutsideSingle,
              facetFreeScale = F,
              frameShow = T,
              labels=labels,
              labelsSize = labelsSize,
              legendTitle =legendTitle,legendTitleSize = legendTitleSizeS,legendTextSize = legendTextSizeS,
              legendStyle="kmeans",
              legendFixedBreaks=legendFixedBreaks,
              legendDigits = legendDigits,
              legendOutsidePosition = legendOutsidePosition,
              legendPosition = NULL,
              fillPalette = fillPalette,
              bgColor = bgColorChosen,figWidth=figWidth,figHeight=figHeight,
              fileName = paste("map_",boundaryRegionsSelect,"_",subRegType_i,"_",param_i,"_",scenario_i,nameAppend,"_KMEANS",sep=""),
              dirOutputs = paste(dirOutputs,"/Maps/",boundaryRegionsSelect,"/",subRegion_i,"/", scenario_i,sep = ""))

      metis.map(underLayer=underLayer, dataPolygon=mapx,
              fillColumn = names(mapx@data%>%dplyr::select(-subRegion)),
              legendShow = T,
              legendOutside = legendOutsideSingle,
              facetFreeScale = F,
              frameShow = T,
              labels=labels,
              labelsSize = labelsSize,
              legendTitle =legendTitle,legendTitleSize = legendTitleSizeS,legendTextSize = legendTextSizeS,
              legendStyle="pretty",
              legendFixedBreaks=legendFixedBreaks,
              legendDigits = legendDigits,
              legendOutsidePosition = legendOutsidePosition,
              legendPosition = NULL,
              fillPalette = fillPalette,
              bgColor = bgColorChosen,figWidth=figWidth,figHeight=figHeight,
              fileName = paste("map_",boundaryRegionsSelect,"_",subRegType_i,"_",param_i,"_",scenario_i,nameAppend,"_PRETTY",sep=""),
              dirOutputs = paste(dirOutputs,"/Maps/",boundaryRegionsSelect,"/",subRegion_i,"/", scenario_i,sep = ""))

      if(length(names(mapx@data))==1){
        legendOutsideAnimated=legendOutsideSingle
        legendTitleSizeAnim = legendTitleSizeS
        legendTextSizeAnim = legendTextSizeS}else{
          legendOutsideAnimated=F
          legendTitleSizeAnim = legendTitleSizeI
          legendTextSizeAnim = legendTextSizeI
        }

      metis.map(underLayer=underLayer, dataPolygon=mapx,
              fillColumn = names(mapx@data%>%dplyr::select(-subRegion)),
              legendShow = T,
              legendOutside = legendOutsideAnimated,
              facetFreeScale = T,
              frameShow = T,
              labels=labels,
              labelsSize = labelsSize,
              legendTitle =legendTitle,legendTitleSize = legendTitleSizeAnim,legendTextSize = legendTextSizeAnim,
              legendStyle="kmeans",
              legendFixedBreaks=legendFixedBreaks,
              legendDigits = legendDigits,
              legendOutsidePosition = legendOutsidePosition,
              legendPosition = legendPosition,
              fillPalette = fillPalette,
              bgColor = bgColorChosen,figWidth=figWidth,figHeight=figHeight,
              fileName = paste("map_",boundaryRegionsSelect,"_",subRegType_i,"_",param_i,"_",scenario_i,nameAppend,"_FREESCALE",sep=""),
              dirOutputs = paste(dirOutputs,"/Maps/",boundaryRegionsSelect,"/",subRegion_i,"/", scenario_i,sep = ""))

# Animate 2 : each param: If class == 1 { (Map x Anim Years}

}  #if(nrow(datax)>1){


      # Calculate Mean

      datax<-shapeTbl%>%dplyr::filter(region==boundaryRegionsSelect,scenario==scenario_i,subRegType==subRegType_i,param==param_i)
      if(nrow(datax)>1){
        legendTitle<-paste(unique(datax$units),sep="")
        fillPalette<-as.character(unique(datax$classPalette))

        datax<-datax%>%dplyr::select(subRegion,x,value)%>%
          dplyr::group_by(subRegion)%>%
          dplyr::summarize(!!paste("Mean_",min(xRange),"to",max(xRange),sep=""):=mean(value))%>%
          dplyr::ungroup()

        scaleData<-datax%>%dplyr::select(-subRegion)
        if(mean(range(scaleData,na.rm=T),na.rm = T)<0.01 & mean(range(scaleData,na.rm=T),na.rm = T)>(-0.01)){legendDigits<-4}else{
          if(mean(range(scaleData,na.rm=T),na.rm = T)<0.1 & mean(range(scaleData,na.rm=T),na.rm = T)>(-0.1)){legendDigits<-3}else{
            if(mean(range(scaleData,na.rm=T),na.rm = T)<1 & mean(range(scaleData,na.rm=T),na.rm = T)>(-1)){legendDigits<-2}else{
              if(mean(range(scaleData,na.rm=T),na.rm = T)<10 & mean(range(scaleData,na.rm=T),na.rm = T)>(-10)){legendDigits<-1}else{legendDigits<-0}}}}

        mapx<-shape
        mapx@data<-mapx@data%>%dplyr::left_join(datax)%>%
          dplyr::select(names(datax))

        metis.map(underLayer=underLayer, dataPolygon=mapx,
                  fillColumn = names(mapx@data%>%dplyr::select(-subRegion)),
                  legendShow = T,
                  legendOutside = legendOutsideSingle,
                  facetFreeScale = F,
                  frameShow = T,
                  labels=labels,
                  labelsSize = labelsSize,
                  panelLabel = paste((names(datax%>%dplyr::select(-subRegion))[!names(datax%>%dplyr::select(-subRegion)) %in% c("lat","lon")]),sep=""),
                  legendTitle =paste(legendTitle,sep=""),
                  legendTitleSize = legendTitleSizeS,legendTextSize = legendTextSizeS,
                  legendStyle="kmeans",
                  legendFixedBreaks=legendFixedBreaks,
                  legendDigits = legendDigits,
                  legendOutsidePosition = legendOutsidePosition,
                  legendPosition = legendPositionS,
                  fillPalette = fillPalette,
                  bgColor = bgColorChosen,figWidth=figWidth,figHeight=figHeight,
                  fileName = paste("map_",boundaryRegionsSelect,"_",subRegType_i,"_",param_i,"_",scenario_i,nameAppend,"_MEAN_KMEANS",sep=""),
                  dirOutputs = paste(dirOutputs,"/Maps/",boundaryRegionsSelect,"/",subRegion_i,"/", scenario_i,sep = ""))

        metis.map(underLayer=underLayer, dataPolygon=mapx,
                  fillColumn = names(mapx@data%>%dplyr::select(-subRegion)),
                  legendShow = T,
                  legendOutside = legendOutsideSingle,
                  facetFreeScale = F,
                  frameShow = T,
                  labels=labels,
                  labelsSize = labelsSize,
                  panelLabel = paste((names(datax%>%dplyr::select(-subRegion))[!names(datax%>%dplyr::select(-subRegion)) %in% c("lat","lon")]),sep=""),
                  legendTitle =paste(legendTitle,sep=""),
                  legendTitleSize = legendTitleSizeS,legendTextSize = legendTextSizeS,
                  legendStyle="pretty",
                  legendFixedBreaks=legendFixedBreaks,
                  legendDigits = legendDigits,
                  legendOutsidePosition = legendOutsidePosition,
                  legendPosition = legendPositionS,
                  fillPalette = fillPalette,
                  bgColor = bgColorChosen,figWidth=figWidth,figHeight=figHeight,
                  fileName = paste("map_",boundaryRegionsSelect,"_",subRegType_i,"_",param_i,"_",scenario_i,nameAppend,"_MEAN_PRETTY",sep=""),
                  dirOutputs = paste(dirOutputs,"/Maps/",boundaryRegionsSelect,"/",subRegion_i,"/", scenario_i,sep = ""))

        if(length(names(mapx@data))==1){
          legendOutsideAnimated=legendOutsideSingle
          legendTitleSizeAnim = legendTitleSizeS
          legendTextSizeAnim = legendTextSizeS}else{
            legendOutsideAnimated=F
            legendTitleSizeAnim = legendTitleSizeI
            legendTextSizeAnim = legendTextSizeI
          }

        metis.map(underLayer=underLayer, dataPolygon=mapx,
                  fillColumn = names(mapx@data%>%dplyr::select(-subRegion)),
                  legendShow = T,
                  legendOutside = legendOutsideAnimated,
                  facetFreeScale = T,
                  frameShow = T,
                  labels=labels,
                  labelsSize = labelsSize,
                  panelLabel = paste((names(datax%>%dplyr::select(-subRegion))[!names(datax%>%dplyr::select(-subRegion)) %in% c("lat","lon")]),sep=""),
                  legendTitle =paste(legendTitle,sep=""),
                  legendTitleSize = legendTitleSizeAnim,legendTextSize = legendTextSizeAnim,
                  legendStyle="kmeans",
                  legendFixedBreaks=legendFixedBreaks,
                  legendDigits = legendDigits,
                  legendOutsidePosition = legendOutsidePosition,
                  legendPosition = legendPositionS,
                  fillPalette = fillPalette,
                  bgColor = bgColorChosen,figWidth=figWidth,figHeight=figHeight,
                  fileName = paste("map_",boundaryRegionsSelect,"_",subRegType_i,"_",param_i,"_",scenario_i,nameAppend,"_MEAN_FREESCALE",sep=""),
                  dirOutputs = paste(dirOutputs,"/Maps/",boundaryRegionsSelect,"/",subRegion_i,"/", scenario_i,sep = ""))

        # Animate 2 : each param: If class == 1 { (Map x Anim Years}


      }  #if(nrow(datax)>1){

  } # If number of classes == 1
  } # If nrow greater than 0
} # close Params
} # Close subRegType loop
} # Close scenario loop

} # Clsoe if shapeTbl is NUll
} # Close Function
