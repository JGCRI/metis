#' metis.map
#'
#' This function produce different kinds of maps for the metis package.
#' Each figure is accompanied with a csv table.
#'
#' @param dataPolygon Default = NULL,
#' @param dataGrid Default = NULL,
#' @param fileName Default = "map",
#' @param dataRaster Default = NULL,
#' @param shpFolder Default = paste(getwd(),"/dataFiles/gis/admin_gadm36_1",sep Default = ""),
#' @param shpFile Default = paste("gadm36_1",sep Default = ""),
#' @param shpName Default = "NAME_0",
#' @param dirOutputs Default = paste(getwd(),"/outputs",sep Default = ""),
#' @param fillPalette Default = "Spectral",
#' @param borderColor Default = "gray20",
#' @param lwd Default = 1,
#' @param lty Default = 1,
#' @param bgColor Default = "white",
#' @param frameShow Default = F,
#' @param fillColumn Default = NULL, # Or give column data with
#' @param labels Default = F,
#' @param labelsSize Default = 1.2,
#' @param labelsColor Default = "black",
#' @param labelsAutoPlace Default = F,
#' @param figWidth Default = 9,
#' @param figHeight Default = 7,
#' @param legendWidth Default = -1,
#' @param legendShow Default = F,
#' @param legendOutside Default = T,
#' @param legendTextSize Default = 0.8,
#' @param legendTitleSize Default = 1,
#' @param legendOutsidePosition Default = NULL, # "right","left","top","bottom", "center"
#' @param legendPosition Default = NULL, # c("RIGHT','top') - RIGHT LEFT TOP BOTTOM
#' @param legendDigits Default = NULL,
#' @param legendTitle Default = "Legend",
#' @param legendStyle Default = "pretty",
#' @param legendFixedBreaks Default = "5",
#' @param legendBreaks Default = NULL,
#' @param pdfpng Default = "png",
#' @param underLayer Default = NULL,
#' @param overLayer Default = NULL,
#' @param printFig Default = T,
#' @param facetFreeScale Default = F,
#' @param facetRows Default = NA,
#' @param facetCols Default = 3,
#' @param facetLabelColor Default = "grey75",
#' @param facetLabelSize Default = 1.5,
#' @param alpha Default = 1
#' @param rasterCoverNegShape Default =T
#' @keywords charts, diffplots
#' @return Returns the formatted data used to produce chart
#' @export

metis.map<-function(dataPolygon=NULL,
                  dataGrid=NULL,
                  dataRaster=NULL,
                  shpFolder=paste(getwd(),"/dataFiles/gis/admin_gadm36",sep=""),
                  shpFile=paste("gadm36_1",sep=""),
                  shpName="NAME_0",
                  fillPalette="Spectral",
                  borderColor="gray20",
                  lwd=1,
                  lty=1,
                  bgColor="white",
                  frameShow=F,
                  fillColumn=NULL, # Or give column data with
                  labels=F,
                  labelsSize=1.2,
                  labelsColor="black",
                  labelsAutoPlace=F,
                  figWidth=9,
                  figHeight=7,
                  legendWidth=-1,
                  legendShow=F,
                  legendOutside=T,
                  legendTextSize=1,
                  legendTitleSize=2,
                  legendOutsidePosition=NULL,
                  legendPosition=NULL,
                  legendDigits=NULL,
                  legendTitle="Legend",
                  legendStyle="pretty",
                  legendFixedBreaks=5,
                  legendBreaks=NULL,
                  pdfpng="png",
                  underLayer=NULL,
                  overLayer=NULL,
                  printFig=T,
                  fileName="map",
                  dirOutputs=paste(getwd(),"/outputs",sep=""),
                  facetFreeScale=F,
                  facetRows=NA,
                  facetCols=3,
                  facetLabelColor="grey75",
                  facetLabelSize=1.5,
                  alpha=1,
                  rasterCoverNegShape=F
                  ){


  # dataPolygon=NULL
  # dataGrid=NULL
  # fileName="map"
  # dataRaster=NULL
  # shpFolder=paste(getwd(),"/dataFiles/gis/admin_gadm36_1",sep="")
  # shpFile=paste("gadm36_1",sep="")
  # shpName="NAME_0"
  # dirOutputs=paste(getwd(),"/outputs",sep="")
  # fillPalette="Spectral"
  # borderColor="gray20"
  # lwd=1
  # lty=1
  # bgColor="white"
  # frameShow=F
  # fillColumn=NULL # Or give column data with
  # labels=F
  # labelsSize=1.2
  # labelsColor="black"
  # labelsAutoPlace=F
  # figWidth=9
  # figHeight=7
  # legendWidth=-1
  # legendShow=F
  # legendOutside=T
  # legendTextSize=0.8
  # legendTitleSize=1
  # legendOutsidePosition=NULL
  # legendPosition=NULL
  # legendDigits=NULL
  # legendTitle="Legend"
  # legendStyle="pretty"
  # legendFixedBreaks="5"
  # pdfpng="png"
  # underLayer=NULL
  # overLayer=NULL
  # printFig=T
  # facetFreeScale=F
  # facetRows=NA
  # facetCols=3
  # facetLabelColor="grey75"
  # facetLabelSize=1.5
  # alpha=1

#----------------
# Load Libraries
#---------------
  requireNamespace("tmap",quietly = T)
  requireNamespace("tidyr",quietly = T)
  requireNamespace("dplyr",quietly = T)
  requireNamespace("tibble",quietly = T)
  requireNamespace("rgeos",quietly = T)
#------------------
# Initialize variables to remove binding errors if needed
# -----------------

NULL->raster->shape->map->checkFacets

#------------------------------------------
# Read data and check inputs
#------------------------------------------

if (!dir.exists(dirOutputs)){dir.create(dirOutputs)}

if(!is.null(dataPolygon)){
  print("Using given dataPolygon file as shape.")
  if(!is.null(shpFolder) & !is.null(shpFile)){print(paste("NOT reading shapefile '",shpFile,"' from folder '",shpFolder,"'",sep=""))}
    shape<-dataPolygon
  }else{
if(!is.null(shpFolder) & !is.null(shpFile)){
  if(!dir.exists(shpFolder)){
    stop("Shapefile folder: ", shpFolder ," is incorrect or doesn't exist.",sep="")}
  if(!file.exists(paste(shpFolder,"/",shpFile,".shp",sep=""))){
    stop("Shape file: ", paste(shpFolder,"/",shpFile,".shp",sep="")," is incorrect or doesn't exist.",sep="")}
    print("Reading shapefile '",shpFile,"' from folder '",shpFolder,"'",sep="")
    shape=rgdal::readOGR(dsn=shpFolder,layer=shpFile,use_iconv=T,encoding='UTF-8')
    }
  }

if(!is.null(dataGrid)){
   if(!grepl("SpatialPixelsDataFrame",class(dataGrid)[1],ignore.case=T)){
     stop("dataGrid must be of class 'SpatialPixelsDataFrame'")}
    raster<-dataGrid
  }


# if(!is.null(shape)){
#   if(any(!fillColumn %in% names(shape))){
#         stop(paste("One or more columns in 'fillColumn' specified: ",paste(fillColumn,collapse=", ")," are not any of the columns of shape: ",
#                    paste(names(shape),collapse=", "),
#                    sep=""))}}
# if(!is.null(raster)){
#   if(any(!fillColumn %in% names(raster))){
#     stop(paste("One or more columns in 'fillColumn' specified: ",paste(fillColumn,collapse=", ")," are not any of the columns of raster: ",
#                paste(names(raster),collapse=", "),
#                sep=""))}
#           }

if(length(fillPalette)==1){
 if(fillPalette %in% names(metis.colors())){
            fillPalette<-metis.colors()[[fillPalette]]}}else{
             fillPalette<-fillPalette}

#-----------------
#----------------

if(!is.null(raster)){

  if(is.null(legendBreaks)){legendBreaks=scales::pretty_breaks(n=legendFixedBreaks)(raster@data%>%dplyr::select(fillColumn)%>%as.matrix())}
  map<-tm_shape(raster) + tm_raster(col=fillColumn,palette = fillPalette, title=legendTitle,
                                  style=legendStyle,n=legendFixedBreaks,breaks=legendBreaks)

  if(!is.null(raster)){checkFacets=length(names(raster))}else{
  }
  if(!is.null(checkFacets) & checkFacets>1 & !is.null(fillColumn)){
    map<- map + tm_facets(free.scales.fill=facetFreeScale,
                          nrow=facetRows,
                          ncol=min(facetCols,length(fillColumn))) +
      tm_layout(panel.labels=gsub("X","",fillColumn),
                panel.label.bg.color = facetLabelColor,
                panel.label.size = facetLabelSize)
    figWidth=figWidth*1.2
  }

  if(rasterCoverNegShape==T){
  if(!is.null(shape) & !is.null(raster)){
  # Add Extent to hide rasters outside shape
  shapeExpandEtxent<-as.data.frame(sp::bbox(shape))   # Get Bounding box
  expandbboxPercent<-1; shapeExpandEtxent$min;shapeExpandEtxent$max
  shapeExpandEtxent$min[1]<-if(shapeExpandEtxent$min[1]<0){(1+expandbboxPercent/100)*shapeExpandEtxent$min[1]}else{(1-expandbboxPercent/100)*shapeExpandEtxent$min[1]};
  shapeExpandEtxent$min[2]<-if(shapeExpandEtxent$min[2]<0){(1+expandbboxPercent/100)*shapeExpandEtxent$min[2]}else{(1-expandbboxPercent/100)*shapeExpandEtxent$min[2]};
  shapeExpandEtxent$max[1]<-if(shapeExpandEtxent$max[1]<0){(1-expandbboxPercent/100)*shapeExpandEtxent$max[1]}else{(1+expandbboxPercent/100)*shapeExpandEtxent$max[1]};
  shapeExpandEtxent$max[2]<-if(shapeExpandEtxent$max[2]<0){(1-expandbboxPercent/100)*shapeExpandEtxent$max[2]}else{(1+expandbboxPercent/100)*shapeExpandEtxent$max[2]};
  shapeExpandEtxent$min;shapeExpandEtxent$max;
  shapeExpandEtxent<-methods::as(raster::extent(as.vector(t(shapeExpandEtxent))), "SpatialPolygons")
  sp::proj4string(shapeExpandEtxent)<-sp::CRS(sp::proj4string(shape)) # ASSIGN COORDINATE SYSTEM
  neg <- rgeos::gDifference(shapeExpandEtxent,shape)
  map<-map+tm_shape(neg)+tm_fill(col=bgColor)}
    }
}




if(is.null(underLayer)){
  if(grepl("tmap",class(shape)[1],ignore.case=T)){
    if(!is.null(map)){map<-map+shape}else{map<-shape}
    }else
      if(!is.null(map)){map<-map+tm_shape(shape)}else{map<-tm_shape(shape)}
  }else{
    if(grepl("tmap",class(shape)[1],ignore.case=T)){
      if(!is.null(map)){map<-map+underLayer+shape}else{map<-underLayer+shape}
      }else
        if(!is.null(map)){map<-map+underLayer+tm_shape(shape)}else{map<-underLayer+tm_shape(shape)}
  }

if(!is.null(shape)){

if(grepl("line",class(shape)[1],ignore.case=T)){
  map=map +  tm_lines(col=borderColor,lwd=lwd, lty=lty)}

if(grepl("polygon",class(shape)[1],ignore.case=T) | grepl("tmap",class(shape)[1],ignore.case=T)){
  if(is.null(fillColumn)){
    map= map + tm_borders(col=borderColor,lwd=lwd, lty=lty)
  }else{
if(is.null(raster)){
if(is.null(legendBreaks)){legendBreaks=scales::pretty_breaks(n=legendFixedBreaks)(shape@data%>%dplyr::select(fillColumn)%>%as.matrix())}
#names(shape)[names(shape) %in% fillColumn]<-gsub(" ","_",names(shape)[names(shape) %in% fillColumn])
map<-map + tm_fill(col=fillColumn, palette = fillPalette, title=legendTitle,
                   style=legendStyle,n=legendFixedBreaks,breaks=legendBreaks,alpha=alpha,colorNA=NULL) +
           tm_borders(col=borderColor,lwd=lwd, lty=lty)
}else{
  map<-map + tm_borders(col=borderColor,lwd=lwd, lty=lty)
}
  }

}

  if(labels!=F){
    if(is.null(raster)){
      if(!is.null(fillColumn)){
      map= map + tm_text(fillColumn,scale=labelsSize,auto.placement=labelsAutoPlace, col=labelsColor)}else{
        print("For labels text need to define fillColumn. Ignoring text labels for now.")}
    }
  }

  } # Close Polygon Maps


if(!is.null(legendOutsidePosition)){map <- map + tm_layout(legend.outside.position = legendOutsidePosition)}
if(!is.null(legendPosition)){map <- map + tm_layout(legend.position = legendPosition)}


if(is.null(raster)){if(!is.null(shape)){checkFacets=length(names(shape))-1}
if(!is.null(checkFacets) & checkFacets>1 & !is.null(fillColumn)){
  map<- map + tm_facets(free.scales.fill=facetFreeScale,
                        nrow=facetRows,
                        ncol=min(facetCols,length(fillColumn))) +
              tm_layout(panel.labels=gsub("X","",fillColumn),
                        panel.label.bg.color = facetLabelColor,
                        panel.label.size = facetLabelSize)
  figWidth=figWidth*1.2
}}

  map<- map +
    tm_layout(legend.show = legendShow,
              legend.outside=legendOutside,
              legend.title.size = legendTitleSize,
              legend.text.size = legendTextSize)+
    tm_layout(frame = frameShow, bg.color=bgColor)+
    tm_layout(main.title.position="left",main.title.size=1.5,
              inner.margins = rep(0,4),outer.margins=rep(0.01,4))

if(!is.null(legendDigits)){map<- map + tm_layout(legend.format = list(digits = legendDigits))}


if(!is.null(overLayer)){
  map<-map+overLayer
}

print(map)

if(printFig!=F){
fname<-paste(fileName,sep="")
if(!dir.exists(dirOutputs)){
  print(paste("dirOutputs provided: ",dirOutputs," does not exist. Saving to: ", getwd(),sep=""))
  diroutputs=getwd()}else{
metis.printPdfPng(figure=map,
                dir=dirOutputs,
                filename=fname,
                figWidth=figWidth,
                figHeight=figHeight,
                pdfpng=pdfpng)

print(paste("Figure saved as: ",fileName,".",pdfpng," in folder: ", paste(dirOutputs,sep=""),sep=""))
}}else{print("printFig set to F so no figure will be saved.")}


  return(map)
}


