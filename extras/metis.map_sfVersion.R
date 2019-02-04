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
#' @param facetBGColor Default = "grey75",
#' @param facetLabelColor Default = "black",
#' @param facetLabelSize Default = 1.5,
#' @param alpha Default = 1
#' @param fillcolorNA Default =NULL
#' @param fillcolorNULL Default =NULL
#' @param facetsON Default =F,
#' @param panelLabel Default = NULL,
#' @param multiFacetRows Default=NULL,
#' @param multiFacetCols Default=NULL,
#' @param mapTitle Default=NULL
#' @keywords charts, diffplots
#' @return Returns the formatted data used to produce chart
#' @export

metis.map<-function(dataPolygon=NULL,
                  dataGrid=NULL,
                  dataRaster=NULL,
                  shpFolder=NULL,
                  shpFile=NULL,
                  fillPalette="Spectral",
                  borderColor="gray20",
                  lwd=0.75,
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
                  facetBGColor="grey30",
                  facetLabelColor = "white",
                  facetLabelSize=1.5,
                  alpha=1,
                  fillcolorNA="grey30",
                  fillshowNA=NA,
                  fillcolorNULL="grey30",
                  facetsON=T,
                  panelLabel=NULL,
                  multiFacetRows=NULL,
                  multiFacetCols=NULL,
                  mapTitle=NULL
                  ){


  # dataPolygon=NULL
  # dataGrid=NULL
  # dataRaster=NULL
  # shpFolder=NULL
  # shpFile=NULL
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
  # legendTextSize=1
  # legendTitleSize=2
  # legendOutsidePosition=NULL
  # legendPosition=NULL
  # legendDigits=NULL
  # legendTitle="Legend"
  # legendStyle="pretty"
  # legendFixedBreaks=5
  # legendBreaks=NULL
  # pdfpng="png"
  # underLayer=NULL
  # overLayer=NULL
  # printFig=T
  # fileName="map"
  # dirOutputs=paste(getwd(),"/outputs",sep="")
  # facetFreeScale=F
  # facetRows=NA
  # facetCols=3
  # facetBGColor="grey30"
  # facetLabelColor = "white"
  # facetLabelSize=1.5
  # alpha=1
  # fillcolorNA=NULL
  # facetsON=T
  # panelLabel=NULL
  # multiFacetRows=NULL
  # multiFacetCols=NULL

#------------------
# Initialize variables to remove binding errors if needed
# -----------------

NULL->raster->shape->map->checkFacets

legendTitle=gsub(" ","\n",legendTitle)
tmap::tmap_mode(mode = c("plot"))

#------------------------------------------
# Read data and check inputs
#------------------------------------------

if (!dir.exists(dirOutputs)){dir.create(dirOutputs)}

if(!is.null(dataPolygon)){
  print("Using given dataPolygon file as shape.")
  if(!is.null(shpFolder) & !is.null(shpFile)){print(paste("NOT reading shapefile '",shpFile,"' from folder '",shpFolder,"'",sep=""))}
  shape<-dataPolygon
  if(!"gg" %in% class(shape)){
  if(!"sf" %in% class(shape)){shape<-sf::st_as_sf(shape)}}
  }else{
if(!is.null(shpFolder) & !is.null(shpFile)){
  if(!dir.exists(shpFolder)){
    stop("Shapefile folder: ", shpFolder ," is incorrect or doesn't exist.",sep="")}
  if(!file.exists(paste(shpFolder,"/",shpFile,".shp",sep=""))){
    stop("Shape file: ", paste(shpFolder,"/",shpFile,".shp",sep="")," is incorrect or doesn't exist.",sep="")}
    print("Reading shapefile '",shpFile,"' from folder '",shpFolder,"'",sep="")
    shape=rgdal::readOGR(dsn=shpFolder,layer=shpFile,use_iconv=T,encoding='UTF-8')
    if(!"sf" %in% class(shape)){shape<-sf::st_as_sf(shape)}
    }
  }

if(!is.null(dataGrid)){
   if(!grepl("SpatialPixelsDataFrame",class(dataGrid)[1],ignore.case=T)){
     stop("dataGrid must be of class 'SpatialPixelsDataFrame'")}
    raster<-dataGrid
    if(!is.null(shape)){
    raster<-raster::stack(raster)
    raster::projection(raster)<-sp::proj4string(shape)
    raster<-raster::mask(raster,shape)
    raster<-methods::as(raster, "SpatialPixelsDataFrame")
    raster@data<-Filter(function(x)!all(is.na(x)), raster@data)
    fillColumn<-fillColumn[c(fillColumn %in% names(raster@data))]
    }
  }


if(length(fillPalette)==1){
 if(fillPalette %in% names(metis.colors())){
            fillPalette<-metis.colors()[[fillPalette]]}}else{
             fillPalette<-fillPalette}

#-----------------
#----------------

if(!is.null(raster)){

  if(is.null(legendBreaks)){legendBreaks=scales::pretty_breaks(n=legendFixedBreaks)(dataGrid@data%>%dplyr::select(fillColumn)%>%as.matrix())}
  map<-tmap::tm_shape(raster) + tmap::tm_raster(col=fillColumn,palette = fillPalette, title=legendTitle,
                                  style=legendStyle,n=legendFixedBreaks,breaks=legendBreaks,legend.show = legendShow)

  if(!is.null(raster)){checkFacets=length(names(raster))}else{
  }
  if(!is.null(checkFacets) & checkFacets>1 & !is.null(fillColumn)){
    map<- map + tmap::tm_facets(free.scales.fill=facetFreeScale,
                          nrow=facetRows,
                          ncol=min(facetCols,length(fillColumn))) +
      tmap::tm_layout(panel.labels=gsub("X","",fillColumn),
                panel.label.bg.color = facetBGColor,
                panel.label.color = facetLabelColor,
                panel.label.size = facetLabelSize)
    figWidth=figWidth*1.2
  }

}

if(is.null(underLayer)){
  if(any(grepl("gg",class(shape),ignore.case=T))){
    if(!is.null(map)){map<-map+shape}else{map<-shape}
    }else
      if(!is.null(map)){map<-map+ggplot2::ggplot(shape)}else{map<-ggplot2::ggplot(shape)}
  }else{
    if(any(grepl("gg",class(shape),ignore.case=T))){
      if(!is.null(map)){map<-map+underLayer+shape}else{map<-underLayer+shape}
      }else
        if(!is.null(map)){map<-underLayer+map+ggplot2::ggplot(shape)}else{map<-underLayer+ggplot2::ggplot(shape)}
  }

if(!is.null(shape)){

if(grepl("line",class(shape$geometry)[1],ignore.case=T)){
  map=map +  tmap::tm_lines(col=borderColor,lwd=lwd, lty=lty)}

if(any(grepl("polygon",class(shape$geometry),ignore.case=T))){
  if(is.null(fillColumn)){
    map= map + tmap::tm_borders(col=borderColor,lwd=lwd, lty=lty)
  }else{
if(is.null(raster)){
if(is.null(legendBreaks)){
  if(length(scales::pretty_breaks(n=legendFixedBreaks)(shape[[fillColumn]]%>%as.matrix()))>1){
    legendBreaks=scales::pretty_breaks(n=legendFixedBreaks)(shape[[fillColumn]]%>%as.matrix())
  }else{legendBreaks=NULL}
  }

#map<-ggplot(shape)
map<- map + geom_sf(data=shape,
                    color=borderColor,
                    alpha=alpha,lwd=lwd,lty=lty)
if(length(fillPalette)==1){map<-map+geom_sf(fill=fillPalette)}else{
  map<-map+geom_sf(aes_string(fill=fillColumn))+scale_fill_manual(values=fillPalette)}
}else{
  map<-map + geom_sf(data=shape,color=borderColor,alpha=alpha,lwd=lwd,lty=lty)
}
  }

}

  if(labels!=F){
    if(is.null(raster)){
      if(!is.null(fillColumn)){
      map <- map + geom_sf_text(aes_string(label = fillColumn),size=4*labelsSize,color=labelsColor)
      }else{
        print("For labels text need to define fillColumn. Ignoring text labels for now.")}
    }
  }

  } # Close Polygon Maps



if(!is.null(legendOutsidePosition)){map <- map + tmap::tm_layout(legend.outside.position = legendOutsidePosition)}
if(!is.null(legendPosition)){map <- map + tmap::tm_layout(legend.position = legendPosition)}

if(!is.null(multiFacetRows) & !is.null(multiFacetCols)){
  map<- map + tmap::tm_facets(by=c(multiFacetRows,multiFacetCols),free.coords = FALSE,drop.units=T,free.scales=facetFreeScale)
}else{
  if(!is.null(multiFacetRows) & is.null(multiFacetCols)){
    map<- map + tmap::tm_facets(by=c(multiFacetRows),free.coords = FALSE,drop.units=T,free.scales=facetFreeScale)
  }else{
    if(is.null(multiFacetRows) & !is.null(multiFacetCols)){
      map<- map + tmap::tm_facets(by=c(multiFacetCols),free.coords = FALSE,drop.units=T,free.scales=facetFreeScale)
    }
  } # Close multifacetRows
} # Close multiFacetCols

if(is.null(multiFacetRows) & is.null(multiFacetCols)){
if(facetsON==T){
if(is.null(raster)){if(!is.null(shape)){checkFacets=length(names(shape))-1}
if(!is.null(checkFacets) & checkFacets>1 & !is.null(fillColumn)){
  map<- map + tmap::tm_facets(free.scales.fill=facetFreeScale,
                        nrow=facetRows,
                        ncol=min(facetCols,length(fillColumn))) +
              tmap::tm_layout(panel.labels=gsub("X","",fillColumn),
                        panel.label.bg.color = facetBGColor,
                        panel.label.color = facetLabelColor,
                        panel.label.size = facetLabelSize)
  figWidth=figWidth*1.2
}}}

  if(!is.null(panelLabel)){
    map<- map + tmap::tm_facets(nrow=1,ncol=1) +
      tmap::tm_layout(panel.labels=gsub("X","",panelLabel),
                      panel.label.bg.color = facetBGColor,
                      panel.label.color = facetLabelColor,
                      panel.label.size = facetLabelSize)
  }

}


  map<-map + theme_bw()
  map<-map + theme(axis.text.x = element_blank(),axis.text.y = element_blank(),axis.ticks = element_blank())
  map<-map + theme(plot.title = element_blank(),axis.title.x = element_blank(),axis.title.y = element_blank())
  map <- map + coord_sf(expand=F, datum=NA) +
                    theme(panel.background = element_rect(fill = bgColor),
                     panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  if(frameShow==F){map <- map + theme(panel.border = element_blank())}


  # if(!is.null(multiFacetRows) | !is.null(multiFacetCols)){map<-map+tm_layout(asp=1)}else{
  #   map<-map+tm_layout(asp=NA)
  # }

#if(!is.null(legendDigits)){map<- map + tmap::tm_layout(legend.format = list(digits = legendDigits))}
#  if(!is.null(mapTitle)){map<- map + tmap::tm_layout(main.title = mapTitle)}


if(!is.null(overLayer)){
  map<-map+overLayer
}

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
  }}else{
    print("printFig set to F so no figure will be saved.")
    print(map)}


  return(map)
}
