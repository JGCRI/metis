#' srn.mapProcess
#'
#' This function produce different kinds of maps for the srn package.
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
#' @param regionsSelect Default = NULL,
#' @param subRegShape Default = NULL,
#' @param subRegShpFolder Default = paste(getwd(),"/dataFiles/gis/admin_gadm36",sep=""),
#' @param subRegShpFile Default = paste("gadm36_1",sep=""),
#' @param subRegCol Default ="NAME_1",
#' @param subRegType Default ="subRegType",
#' @param aggType Default = NULL,
#' @param nameAppend Default =""
#' @param rasterCoverNegShape Default =T
#' @param legendOutsidePosition Default = NULL, # "right","left","top","bottom", "center"
#' @param legendPosition Default = NULL, # c("RIGHT','top') - RIGHT LEFT TOP BOTTOM
#' @export


srn.mapProcess<-function(polygonDataTables=NULL,
                         gridDataTables=NULL,
                         dirOutputs=paste(getwd(),"/outputs",sep=""),
                         xRange="All",
                         labels=F,
                         labelsSize=1.2,
                         regionsSelect=NULL,
                         subRegShape=NULL,
                         subRegShpFolder=paste(getwd(),"/dataFiles/gis/admin_gadm36",sep=""),
                         subRegShpFile=paste("gadm36_1",sep=""),
                         subRegCol="NAME_1",
                         subRegType="subRegType",
                         aggType=NULL,
                         nameAppend="",
                         rasterCoverNegShape=T,
                         legendOutsidePosition=NULL,
                         legendPosition=NULL){

#------------------
# Load required Libraries
# -----------------
  requireNamespace("tibble",quietly = T)
  requireNamespace("dplyr",quietly = T)
  requireNamespace("utils",quietly = T)
  requireNamespace("tidyr",quietly = T)
  requireNamespace("rlang",quietly = T)
  requireNamespace("magrittr",quietly = T)

#------------------
# Initialize variables to remove binding errors
# -----------------

  NULL->lat->lon->param->region->scenario->subRegion->
  value->x->year

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
        gridTblNew<-utils::read.csv(paste(grid_i), stringsAsFactors = F)%>%tibble::as.tibble()
        gridTbl<-dplyr::bind_rows(gridTbl,gridTblNew)
      } else {stop(paste(grid_i," does not exist"))}
    }

    # Join relevant colors and classes using the mapping file if it exists
    if(!"classPalette" %in% names(shapeTblNew)){
    if(file.exists(paste(getwd(),"/dataFiles/mapping/template_subRegional_mapping.csv", sep = ""))){
      map<-utils::read.csv(paste(getwd(),"/dataFiles/mapping/template_subRegional_mapping.csv", sep = ""), stringsAsFactors = F)%>%tibble::as.tibble()
      gridTbl<-gridTbl%>%dplyr::left_join(map,by=c("param","units","class"))
    }}

    # Add missing columns
    gridTbl<-addMissing(gridTbl)
   }else{gridTbl<-gridDataTables}

  if(!"subRegType" %in% names(gridTbl)){
      paste("'subRegType' column not present in grid data provided. Creating class column 'subRegType'",sep="")
      gridTbl<-gridTbl%>%dplyr::mutate(subRegType="raster")
    }
  if(!"class" %in% names(gridTbl)){
    paste("'class' column not present in grid data provided. Creating class column 'class'",sep="")
    gridTbl<-gridTbl%>%dplyr::mutate(class="class")
  }
  if(!"x" %in% names(gridTbl)){
    paste("'x' column not present in grid data provided. Creating x column 'x'",sep="")
    gridTbl<-gridTbl%>%dplyr::mutate(x="x")
  }
  if(!"param" %in% names(gridTbl)){
    paste("'param' column not present in grid data provided. Creating param column 'param'",sep="")
    gridTbl<-gridTbl%>%dplyr::mutate(param="param")
  }
  if(!"classPalette" %in% names(gridTbl)){
      paste("'classPalette' column not present in polygon data provided. Creating classPalette column 'classPalette'",sep="")
      gridTbl<-gridTbl%>%dplyr::mutate(classPalette="pal_hot")
    }
  if(!"value" %in% names(gridTbl)){
    stop("'value' column not present in polygon data provided. Need to have values. Check data.",sep="")
  }

  if(!"lat" %in% names(gridTbl)){stop("'lat' column not present in grid data provided. Need to have lat. Check data.",sep="")}
  if(!"lon" %in% names(gridTbl)){stop("'lon' column not present in grid data provided. Need to have lat. Check data.",sep="")}

  }

#------------------
# Read in shape Tables (Either csv tables or R table
#------------------

  shapeTbl<-tibble::tibble()

  if(!is.null(polygonDataTables)){

    if(all(!class(polygonDataTables) %in% c("tbl_df","tbl","data.frame"))){
      if(class(polygonDataTables)!="character"){stop("polygonDataTables neither .csv file path nor dataframe or tibble")}
    for(i in polygonDataTables){
      if(file.exists(i)){
        shapeTblNew<-utils::read.csv(paste(i), stringsAsFactors = F)%>%tibble::as.tibble()

        # Join relevant colors and classes using the mapping file if it exists
        if(!"classPalette" %in% names(shapeTblNew)){
        if(file.exists(paste(getwd(),"/dataFiles/mapping/template_subRegional_mapping.csv", sep = ""))){
          map<-utils::read.csv(paste(getwd(),"/dataFiles/mapping/template_subRegional_mapping.csv", sep = ""), stringsAsFactors = F)%>%tibble::as.tibble()
          shapeTblNew<-shapeTblNew%>%dplyr::left_join(map,by=c("param","units","class"))
        }else{"subregional mapping not found. Using defaults."}}

        shapeTbl<-dplyr::bind_rows(shapeTbl,shapeTblNew)

        if(!"subRegion" %in% names(shapeTbl)){stop(paste("SubRegCol: ",subRegCol," not present in polygonDataTables ",i,sep=""))}

      } else {stop(paste(i," does not exist"))}
    }


    # Add missing columns
    shapeTbl<-addMissing(shapeTbl)
    }else{shapeTbl<-polygonDataTables}}

  if(!"class" %in% names(shapeTbl)){
    paste("'class' column not present in polygon data provided. Creating class column 'class'",sep="")
    shapeTbl<-shapeTbl%>%dplyr::mutate(class="class")
  }
  if(!"x" %in% names(shapeTbl)){
    paste("'x' column not present in polygon data provided. Creating x column 'x'",sep="")
    shapeTbl<-shapeTbl%>%dplyr::mutate(x="x")
  }
  if(!"param" %in% names(shapeTbl)){
    paste("'param' column not present in polygon data provided. Creating param column 'param'",sep="")
    shapeTbl<-shapeTbl%>%dplyr::mutate(param="param")
  }
  if(!"classPalette" %in% names(shapeTbl)){
      paste("'classPalette' column not present in polygon data provided. Creating classPalette column 'classPalette'",sep="")
      shapeTbl<-shapeTbl%>%dplyr::mutate(classPalette="pal_hot")
    }
  if(!"value" %in% names(shapeTbl)){
    stop("'value' column not present in polygon data provided. Need to have values. Check data.",sep="")
  }

#------------------
# Read in shape files
#------------------

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

  if(!subRegCol %in% names(subRegShape)){stop(paste("SubRegCol: ",subRegCol," not present in subRegShape",sep=""))}

subRegShape@data<-subRegShape@data%>%dplyr::mutate(subRegion=get(subRegCol))

#------------------
# Subset Data
#------------------

if(any(!regionsSelect %in% unique(shapeTbl$region))){
  stop(paste("regionsSelect: ",regionsSelect," not in shapeTbl regions"))}

for(regionsSelect_i in regionsSelect){

shapeTbl<-shapeTbl%>%unique()%>%dplyr::filter(region %in% regionsSelect_i)
if(any(xRange!="All")){if(is.numeric(shapeTbl$x)){shapeTbl<-shapeTbl%>%dplyr::filter(x %in% xRange)}}
if(any(xRange!="All")){if(is.numeric(gridTbl$x)){gridTbl<-gridTbl%>%dplyr::filter(x %in% xRange)}}


#------------------
# Create Folders if needed
#------------------

# Check to see if correct columns are present in data
if(!"scenario" %in% names(shapeTbl)){
  paste("'scenario' column not present in polygon data provided. Creating scenario 'scenario'",sep="")
  shapeTbl<-shapeTbl%>%dplyr::mutate(scenario="scenario")
  }
if(!"subRegType" %in% names(shapeTbl)){
  paste("'subRegType' column not present in polygon data provided. Creating subRegType 'subRegion'",sep="")
  shapeTbl<-shapeTbl%>%dplyr::mutate(subRegType="subRegion")
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


  if (!dir.exists(dirOutputs)){dir.create(dirOutputs)}
  if (!dir.exists(paste(dirOutputs, "/Maps", sep = ""))){
    dir.create(paste(dirOutputs, "/Maps", sep = ""))}

    if (!dir.exists(paste(dirOutputs, "/Maps/",regionsSelect_i,sep = ""))){
      dir.create(paste(dirOutputs, "/Maps/",regionsSelect_i,sep = ""))}

      if (!dir.exists(paste(dirOutputs, "/Maps/",regionsSelect_i,"/raster",sep = ""))){
        dir.create(paste(dirOutputs, "/Maps/",regionsSelect_i,"/raster",sep = ""))}
        for (scenario_i in unique(gridTbl$scenario)) {
          if (!dir.exists(paste(dirOutputs, "/Maps/",regionsSelect_i,"/raster/", scenario_i,sep = ""))){
            dir.create(paste(dirOutputs,  "/Maps/",regionsSelect_i,"/raster/",scenario_i,sep = ""))}

          if (!dir.exists(paste(dirOutputs, "/Maps/",regionsSelect_i,"/raster/", scenario_i,"/byYear",sep = ""))){
            dir.create(paste(dirOutputs,  "/Maps/",regionsSelect_i,"/raster/",scenario_i,"/byYear",sep = ""))}

        } # Close for scenario i

      for (subRegion_i in unique(shapeTbl$subRegType)) {
        if (!dir.exists(paste(dirOutputs, "/Maps/",regionsSelect_i,"/",subRegion_i,sep = ""))){
          dir.create(paste(dirOutputs, "/Maps/",regionsSelect_i,"/",subRegion_i,sep = ""))

          for (scenario_i in unique(shapeTbl$scenario)) {
            if (!dir.exists(paste(dirOutputs, "/Maps/",regionsSelect_i,"/",subRegion_i,"/", scenario_i,sep = ""))){
              dir.create(paste(dirOutputs,  "/Maps/",regionsSelect_i,"/",subRegion_i,"/", scenario_i,sep = ""))}

            if (!dir.exists(paste(dirOutputs, "/Maps/",regionsSelect_i,"/",subRegion_i,"/",scenario_i,"/byYear",sep = ""))){
              dir.create(paste(dirOutputs,  "/Maps/",regionsSelect_i,"/",subRegion_i,"/",scenario_i,"/byYear",sep = ""))}

          } # Close for scenario i

        } # Close subRegion directory
      } # Close subRegion

# -------------------
# Create Raster Plots
# -------------------

  for (scenario_i in unique(gridTbl$scenario)){
    for (param_i in unique(gridTbl$param)){

        # Figure 1 : each param: If class > 1 { (Map x Class) x Selected Years}

        if(length(unique(shapeTbl$class))>1){

          for (x_i in unique(shapeTbl$x)){

            datax<-gridTbl%>%dplyr::filter(scenario==scenario_i,x==x_i,param==param_i)
        if(nrow(datax)>1){
            legendTitle<-unique(datax$units)
            fillPalette<-unique(datax$classPalette)

            datax<-datax%>%dplyr::select(lat,lon,class,value)%>%
              tidyr::spread(key=class,value=value)

            shapeExpandEtxent<-as.data.frame(sp::bbox(subRegShape))   # Get Bounding box
            expandbboxPercent<-0.5; shapeExpandEtxent$min;shapeExpandEtxent$max
            shapeExpandEtxent$min[1]<-if(shapeExpandEtxent$min[1]<0){(1+expandbboxPercent/100)*shapeExpandEtxent$min[1]}else{(1-expandbboxPercent/100)*shapeExpandEtxent$min[1]};
            shapeExpandEtxent$min[2]<-if(shapeExpandEtxent$min[2]<0){(1+expandbboxPercent/100)*shapeExpandEtxent$min[2]}else{(1-expandbboxPercent/100)*shapeExpandEtxent$min[2]};
            shapeExpandEtxent$max[1]<-if(shapeExpandEtxent$max[1]<0){(1-expandbboxPercent/100)*shapeExpandEtxent$max[1]}else{(1+expandbboxPercent/100)*shapeExpandEtxent$max[1]};
            shapeExpandEtxent$max[2]<-if(shapeExpandEtxent$max[2]<0){(1-expandbboxPercent/100)*shapeExpandEtxent$max[2]}else{(1+expandbboxPercent/100)*shapeExpandEtxent$max[2]};
            shapeExpandEtxent$min;shapeExpandEtxent$max;
            shapeExpandEtxent<-methods::as(raster::extent(as.vector(t(shapeExpandEtxent))), "SpatialPolygons")
            sp::proj4string(shapeExpandEtxent)<-sp::CRS(sp::proj4string(subRegShape)) # ASSIGN COORDINATE SYSTEM

            rasterx<-sp::SpatialPointsDataFrame(sp::SpatialPoints(coords=(cbind(datax$lon,datax$lat))),data=datax)
            sp::proj4string(rasterx)<-sp::proj4string(subRegShape)
            rasterx<-raster::intersect(rasterx,shapeExpandEtxent)
            sp::gridded(rasterx)<-T

            scaleData<-datax%>%dplyr::select(-lat,-lon)
            if(mean(scaleData%>%as.matrix(),na.rm = T)<0.01 & mean(scaleData%>%as.matrix(),na.rm = T)>(-0.01)){legendDigits<-4}else{
              if(mean(scaleData%>%as.matrix(),na.rm = T)<0.1 & mean(scaleData%>%as.matrix(),na.rm = T)>(-0.1)){legendDigits<-3}else{
                if(mean(scaleData%>%as.matrix(),na.rm = T)<1 & mean(scaleData%>%as.matrix(),na.rm = T)>(-1)){legendDigits<-2}else{
                  if(mean(scaleData%>%as.matrix(),na.rm = T)<10 & mean(scaleData%>%as.matrix(),na.rm = T)>(-10)){legendDigits<-1}else{legendDigits<-0}}}}

            mapx<-rasterx
            mapx@data<-mapx@data%>%dplyr::select(-lat,-lon)

            srn.map(dataPolygon=subRegShape,
                    dataGrid=mapx,
                    fillColumn = names(mapx@data),
                    legendShow = T,
                    legendOutside = T,
                    facetFreeScale = F,
                    frameShow = T,
                    labels=labels,
                    labelsSize = labelsSize,
                    legendTitle =legendTitle,
                    legendStyle="kmeans",
                    legendFixedBreaks = 10,
                    legendDigits = legendDigits,
                    legendOutsidePosition = legendOutsidePosition,
                    legendPosition = NULL,
                    fillPalette = fillPalette,
                    rasterCoverNegShape=rasterCoverNegShape,
                    mapName = paste("map_",regionsSelect_i,"_raster_",param_i,"_",x_i,"_",scenario_i,nameAppend,"_KMEANS",sep=""),
                    dirOutputs = paste(dirOutputs,"/Maps/",regionsSelect_i,"/raster/", scenario_i,"/byYear",sep = ""))


            srn.map(dataPolygon=subRegShape,
                    dataGrid=mapx,
                    fillColumn = names(mapx@data),
                    legendShow = T,
                    legendOutside = T,
                    facetFreeScale = F,
                    frameShow = T,
                    labels=labels,
                    labelsSize = labelsSize,
                    legendTitle =legendTitle,
                    legendStyle="pretty",
                    legendFixedBreaks = 10,
                    legendDigits = legendDigits,
                    legendOutsidePosition = legendOutsidePosition,
                    legendPosition = NULL,
                    fillPalette = fillPalette,
                    rasterCoverNegShape=rasterCoverNegShape,
                    mapName = paste("map_",regionsSelect_i,"_raster_",param_i,"_",x_i,"_",scenario_i,nameAppend,"_PRETTY",sep=""),
                    dirOutputs = paste(dirOutputs,"/Maps/",regionsSelect_i,"/raster/", scenario_i,"/byYear",sep = ""))

            srn.map(dataPolygon=subRegShape,
                    dataGrid=mapx,
                    fillColumn = names(mapx@data),
                    legendShow = T,
                    legendOutside = F,
                    facetFreeScale = T,
                    frameShow = T,
                    labels=labels,
                    labelsSize = labelsSize,
                    legendTitle =legendTitle,
                    legendStyle="kmeans",
                    legendFixedBreaks = 9,
                    legendDigits = NULL,
                    legendOutsidePosition = legendOutsidePosition,
                    legendPosition = legendPosition,
                    fillPalette = fillPalette,
                    rasterCoverNegShape=rasterCoverNegShape,
                    mapName = paste("map_",regionsSelect_i,"_raster_",param_i,"_",x_i,"_",scenario_i,nameAppend,"_FREESCALE",sep=""),
                    dirOutputs = paste(dirOutputs,"/Maps/",regionsSelect_i,"/raster/", scenario_i,"/byYear",sep = ""))

            } # if nrow(datax) > 1
          }# Close years loop

          # Animate 1 : each param: If class > 1 { (Map x Class) x Anim Years}

        } # If number of classes > 1


        #------------------------------
        # Figure 2 : each param: If class ==1 { Map x years}
        #-----------------------------

        if(length(unique(gridTbl$class))==1){

          datax<-gridTbl%>%dplyr::filter(scenario==scenario_i,param==param_i)
        if(nrow(datax)>1){
          legendTitle<-unique(datax$units)
          fillPalette<-unique(datax$classPalette)

          datax<-datax%>%dplyr::select(lat,lon,x,value)%>%
            tidyr::spread(key=x,value=value)

          shapeExpandEtxent<-as.data.frame(sp::bbox(subRegShape))   # Get Bounding box
          expandbboxPercent<-0.5; shapeExpandEtxent$min;shapeExpandEtxent$max
          shapeExpandEtxent$min[1]<-if(shapeExpandEtxent$min[1]<0){(1+expandbboxPercent/100)*shapeExpandEtxent$min[1]}else{(1-expandbboxPercent/100)*shapeExpandEtxent$min[1]};
          shapeExpandEtxent$min[2]<-if(shapeExpandEtxent$min[2]<0){(1+expandbboxPercent/100)*shapeExpandEtxent$min[2]}else{(1-expandbboxPercent/100)*shapeExpandEtxent$min[2]};
          shapeExpandEtxent$max[1]<-if(shapeExpandEtxent$max[1]<0){(1-expandbboxPercent/100)*shapeExpandEtxent$max[1]}else{(1+expandbboxPercent/100)*shapeExpandEtxent$max[1]};
          shapeExpandEtxent$max[2]<-if(shapeExpandEtxent$max[2]<0){(1-expandbboxPercent/100)*shapeExpandEtxent$max[2]}else{(1+expandbboxPercent/100)*shapeExpandEtxent$max[2]};
          shapeExpandEtxent$min;shapeExpandEtxent$max;
          shapeExpandEtxent<-methods::as(raster::extent(as.vector(t(shapeExpandEtxent))), "SpatialPolygons")
          sp::proj4string(shapeExpandEtxent)<-sp::CRS(sp::proj4string(subRegShape)) # ASSIGN COORDINATE SYSTEM

          rasterx<-sp::SpatialPointsDataFrame(sp::SpatialPoints(coords=(cbind(datax$lon,datax$lat))),data=datax)
          sp::proj4string(rasterx)<-sp::proj4string(subRegShape)
          rasterx<-raster::intersect(rasterx,shapeExpandEtxent)
          sp::gridded(rasterx)<-T

          scaleData<-datax%>%dplyr::select(-lat,-lon)
          if(mean(scaleData%>%as.matrix(),na.rm = T)<0.01 & mean(scaleData%>%as.matrix(),na.rm = T)>(-0.01)){legendDigits<-4}else{
            if(mean(scaleData%>%as.matrix(),na.rm = T)<0.1 & mean(scaleData%>%as.matrix(),na.rm = T)>(-0.1)){legendDigits<-3}else{
              if(mean(scaleData%>%as.matrix(),na.rm = T)<1 & mean(scaleData%>%as.matrix(),na.rm = T)>(-1)){legendDigits<-2}else{
                if(mean(scaleData%>%as.matrix(),na.rm = T)<10 & mean(scaleData%>%as.matrix(),na.rm = T)>(-10)){legendDigits<-1}else{legendDigits<-0}}}}

          mapx<-rasterx
          mapx@data<-mapx@data%>%dplyr::select(-lat,-lon)
          names(mapx@data)<-paste("X",names(mapx@data),sep="")

          srn.map(dataPolygon=subRegShape,
                  dataGrid=mapx,
                  fillColumn = names(mapx@data),
                  legendShow = T,
                  legendOutside = T,
                  facetFreeScale = F,
                  frameShow = T,
                  labels=labels,
                  labelsSize = labelsSize,
                  legendTitle =legendTitle,
                  legendStyle="kmeans",
                  legendFixedBreaks = 10,
                  legendDigits = legendDigits,
                  legendOutsidePosition = legendOutsidePosition,
                  legendPosition = NULL,
                  fillPalette = fillPalette,
                  rasterCoverNegShape=rasterCoverNegShape,
                  mapName = paste("map_",regionsSelect_i,"_raster_",param_i,"_",scenario_i,nameAppend,"_KMEANS",sep=""),
                  dirOutputs = paste(dirOutputs,"/Maps/",regionsSelect_i,"/raster/", scenario_i,sep = ""))

          srn.map(dataPolygon=subRegShape,
                  dataGrid=mapx,
                  fillColumn = names(mapx@data),
                  legendShow = T,
                  legendOutside = T,
                  facetFreeScale = F,
                  frameShow = T,
                  labels=labels,
                  labelsSize = labelsSize,
                  legendTitle =legendTitle,
                  legendStyle="pretty",
                  legendFixedBreaks = 10,
                  legendDigits = legendDigits,
                  legendOutsidePosition = legendOutsidePosition,
                  legendPosition = NULL,
                  fillPalette = fillPalette,
                  rasterCoverNegShape=rasterCoverNegShape,
                  mapName = paste("map_",regionsSelect_i,"_raster_",param_i,"_",scenario_i,nameAppend,"_PRETTY",sep=""),
                  dirOutputs = paste(dirOutputs,"/Maps/",regionsSelect_i,"/raster/", scenario_i,sep = ""))

          srn.map(dataPolygon=subRegShape,
                  dataGrid=mapx,
                  fillColumn = names(mapx@data),
                  legendShow = T,
                  legendOutside = F,
                  facetFreeScale = T,
                  frameShow = T,
                  labels=labels,
                  labelsSize = labelsSize,
                  legendTitle =legendTitle,
                  legendStyle="kmeans",
                  legendFixedBreaks = 9,
                  legendDigits = NULL,
                  legendOutsidePosition = legendOutsidePosition,
                  legendPosition = legendPosition,
                  fillPalette = fillPalette,
                  rasterCoverNegShape=rasterCoverNegShape,
                  mapName = paste("map_",regionsSelect_i,"_raster_",param_i,"_",scenario_i,nameAppend,"_FREESCALE",sep=""),
                  dirOutputs = paste(dirOutputs,"/Maps/",regionsSelect_i,"/raster/", scenario_i,sep = ""))

          # Animate 2 : each param: If class == 1 { (Map x Anim Years}

        } # if(nrow(datax)>1){
        } # If number of classes == 1

      } # close Params
  } # Close scenario loop

# -------------------
# Create Polygon Plots for Each scenario
# -------------------

for (scenario_i in unique(shapeTbl$scenario)){
for (subRegType_i in unique(shapeTbl$subRegType)){
for (param_i in unique(shapeTbl$param)){

# Figure 1 : each param: If class > 1 { (Map x Class) x Selected Years}

if(length(unique(shapeTbl$class))>1){

for (x_i in unique(shapeTbl$x)){

  datax<-shapeTbl%>%dplyr::filter(region==regionsSelect_i,scenario==scenario_i,subRegType==subRegType_i,
                                     x==x_i,param==param_i)
  if(nrow(datax)>1){
  legendTitle<-unique(datax$units)
  fillPalette<-unique(datax$classPalette)

  datax<-datax%>%dplyr::select(subRegion,class,value)%>%
  tidyr::spread(key=class,value=value)

scaleData<-datax%>%dplyr::select(-subRegion)
if(mean(scaleData%>%as.matrix(),na.rm = T)<0.01 & mean(scaleData%>%as.matrix(),na.rm = T)>(-0.01)){legendDigits<-4}else{
if(mean(scaleData%>%as.matrix(),na.rm = T)<0.1 & mean(scaleData%>%as.matrix(),na.rm = T)>(-0.1)){legendDigits<-3}else{
if(mean(scaleData%>%as.matrix(),na.rm = T)<1 & mean(scaleData%>%as.matrix(),na.rm = T)>(-1)){legendDigits<-2}else{
if(mean(scaleData%>%as.matrix(),na.rm = T)<10 & mean(scaleData%>%as.matrix(),na.rm = T)>(-10)){legendDigits<-1}else{legendDigits<-0}}}}

mapx<-subRegShape
mapx@data<-mapx@data%>%dplyr::left_join(datax)%>%
  dplyr::select(names(datax))

srn.map(dataPolygon=mapx,
        fillColumn = names(mapx@data%>%dplyr::select(-subRegion)),
        legendShow = T,
        legendOutside = T,
        facetFreeScale = F,
        frameShow = T,
        labels=labels,
        labelsSize = labelsSize,
        legendTitle =legendTitle,
        legendStyle="kmeans",
        legendFixedBreaks = 10,
        legendDigits = legendDigits,
        legendOutsidePosition = legendOutsidePosition,
        legendPosition = NULL,
        fillPalette = fillPalette,
        mapName = paste("map_",regionsSelect_i,"_",subRegType_i,"_",param_i,"_",x_i,"_",scenario_i,nameAppend,"_KMEANS",sep=""),
        dirOutputs = paste(dirOutputs,"/Maps/",regionsSelect_i,"/",subRegion_i,"/", scenario_i,"/byYear",sep = ""))

srn.map(dataPolygon=mapx,
        fillColumn = names(mapx@data%>%dplyr::select(-subRegion)),
        legendShow = T,
        legendOutside = T,
        facetFreeScale = F,
        frameShow = T,
        labels=labels,
        labelsSize = labelsSize,
        legendTitle =legendTitle,
        legendStyle="pretty",
        legendFixedBreaks = 10,
        legendDigits = legendDigits,
        legendOutsidePosition = legendOutsidePosition,
        legendPosition = NULL,
        fillPalette = fillPalette,
        mapName = paste("map_",regionsSelect_i,"_",subRegType_i,"_",param_i,"_",x_i,"_",scenario_i,nameAppend,"_PRETTY",sep=""),
        dirOutputs = paste(dirOutputs,"/Maps/",regionsSelect_i,"/",subRegion_i,"/", scenario_i,"/byYear",sep = ""))

srn.map(dataPolygon=mapx,
        fillColumn = names(mapx@data%>%dplyr::select(-subRegion)),
        legendShow = T,
        legendOutside = F,
        facetFreeScale = T,
        frameShow = T,
        labels=labels,
        labelsSize = labelsSize,
        legendTitle =legendTitle,
        legendStyle="kmeans",
        legendFixedBreaks = 9,
        legendDigits = NULL,
        legendOutsidePosition = legendOutsidePosition,
        legendPosition = legendPosition,
        fillPalette = fillPalette,
        mapName = paste("map_",regionsSelect_i,"_",subRegType_i,"_",param_i,"_",x_i,"_",scenario_i,nameAppend,"_FREESCALE",sep=""),
        dirOutputs = paste(dirOutputs,"/Maps/",regionsSelect_i,"/",subRegion_i,"/", scenario_i,"/byYear",sep = ""))

}# if(nrow(datax)>1){
}# Close years loop

# Animate 1 : each param: If class > 1 { (Map x Class) x Anim Years}

} # If number of classes > 1


#------------------------------
# Figure 2 : each param: If class ==1 { Map x years}
#-----------------------------

if(length(unique(shapeTbl$class))==1){

      datax<-shapeTbl%>%dplyr::filter(region==regionsSelect_i,scenario==scenario_i,subRegType==subRegType_i,param==param_i)
      if(nrow(datax)>1){
      legendTitle<-paste(unique(datax$units),sep="")
      fillPalette<-unique(datax$classPalette)

      datax<-datax%>%dplyr::select(subRegion,x,value)%>%
        tidyr::spread(key=x,value=value)

      scaleData<-datax%>%dplyr::select(-subRegion)
      if(mean(scaleData%>%as.matrix(),na.rm = T)<0.01 & mean(scaleData%>%as.matrix(),na.rm = T)>(-0.01)){legendDigits<-4}else{
        if(mean(scaleData%>%as.matrix(),na.rm = T)<0.1 & mean(scaleData%>%as.matrix(),na.rm = T)>(-0.1)){legendDigits<-3}else{
          if(mean(scaleData%>%as.matrix(),na.rm = T)<1 & mean(scaleData%>%as.matrix(),na.rm = T)>(-1)){legendDigits<-2}else{
            if(mean(scaleData%>%as.matrix(),na.rm = T)<10 & mean(scaleData%>%as.matrix(),na.rm = T)>(-10)){legendDigits<-1}else{legendDigits<-0}}}}

      mapx<-subRegShape
      mapx@data<-mapx@data%>%dplyr::left_join(datax)%>%
        dplyr::select(names(datax))

      srn.map(dataPolygon=mapx,
              fillColumn = names(mapx@data%>%dplyr::select(-subRegion)),
              legendShow = T,
              legendOutside = T,
              facetFreeScale = F,
              frameShow = T,
              labels=labels,
              labelsSize = labelsSize,
              legendTitle =legendTitle,
              legendStyle="kmeans",
              legendFixedBreaks = 5,
              legendDigits = legendDigits,
              legendOutsidePosition = legendOutsidePosition,
              legendPosition = NULL,
              fillPalette = fillPalette,
              mapName = paste("map_",regionsSelect_i,"_",subRegType_i,"_",param_i,"_",x_i,"_",scenario_i,nameAppend,"_KMEANS",sep=""),
              dirOutputs = paste(dirOutputs,"/Maps/",regionsSelect_i,"/",subRegion_i,"/", scenario_i,sep = ""))

      srn.map(dataPolygon=mapx,
              fillColumn = names(mapx@data%>%dplyr::select(-subRegion)),
              legendShow = T,
              legendOutside = T,
              facetFreeScale = F,
              frameShow = T,
              labels=labels,
              labelsSize = labelsSize,
              legendTitle =legendTitle,
              legendStyle="pretty",
              legendFixedBreaks = 5,
              legendDigits = legendDigits,
              legendOutsidePosition = legendOutsidePosition,
              legendPosition = NULL,
              fillPalette = fillPalette,
              mapName = paste("map_",regionsSelect_i,"_",subRegType_i,"_",param_i,"_",x_i,"_",scenario_i,nameAppend,"_PRETTY",sep=""),
              dirOutputs = paste(dirOutputs,"/Maps/",regionsSelect_i,"/",subRegion_i,"/", scenario_i,sep = ""))

      srn.map(dataPolygon=mapx,
              fillColumn = names(mapx@data%>%dplyr::select(-subRegion)),
              legendShow = T,
              legendOutside = F,
              facetFreeScale = T,
              frameShow = T,
              labels=labels,
              labelsSize = labelsSize,
              legendTitle =legendTitle,
              legendStyle="kmeans",
              legendFixedBreaks = 9,
              legendDigits = NULL,
              legendOutsidePosition = legendOutsidePosition,
              legendPosition = legendPosition,
              fillPalette = fillPalette,
              mapName = paste("map_",regionsSelect_i,"_",subRegType_i,"_",param_i,"_",x_i,"_",scenario_i,nameAppend,"_FREESCALE",sep=""),
              dirOutputs = paste(dirOutputs,"/Maps/",regionsSelect_i,"/",subRegion_i,"/", scenario_i,sep = ""))

# Animate 2 : each param: If class == 1 { (Map x Anim Years}
}  #if(nrow(datax)>1){
  } # If number of classes == 1

} # close Params
} # Close subRegType loop
} # Close scenario loop

} # Close RegionsSelect
} # Close Function
