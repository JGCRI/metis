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
#' @param fillshowNA Default =NA
#' @param facetsON Default =F,
#' @param panelLabel Default = NULL,
#' @param multiFacetRows Default=NULL,
#' @param multiFacetCols Default=NULL,
#' @param mapTitle Default=NULL
#' @param mapTitleSize Default=1
#' @param numeric2Cat_list Default=NULL,
#' @param catParam Default=NULL
#' @param innerMargins Default =c(0,0,0,0), # bottom, left, top, right
#' @param outerMargins Default =c(0.01,0.01,0.01,0.01) # bottom, left, top, right
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
                  lwd=1,
                  lty=1,
                  bgColor="white",
                  frameShow=F,
                  fillColumn=NULL, # Or give column data with
                  labels=F,
                  labelsSize=1.2,
                  labelsColor="black",
                  labelsAutoPlace=T,
                  figWidth=9,
                  figHeight=7,
                  legendWidth=-1,
                  legendShow=F,
                  legendOutside=F,
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
                  fillcolorNA="gray",
                  fillshowNA=NA,
                  fillcolorNULL="gray",
                  facetsON=T,
                  panelLabel=NULL,
                  multiFacetRows=NULL,
                  multiFacetCols=NULL,
                  mapTitle=NULL,
                  mapTitleSize=1,
                  numeric2Cat_list=NULL,
                  catParam=NULL,
                  innerMargins=c(0,0,0,0), # bottom, left, top, right
                  outerMargins=c(0.01,0.01,0.01,0.01) # bottom, left, top, right
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
  # fillshowNA=NA
  # fillcolorNULL="grey30"
  # facetsON=T
  # panelLabel=NULL
  # multiFacetRows=NULL
  # multiFacetCols=NULL
  # mapTitle=NULL
  # mapTitleSize=1
  # numeric2Cat_list=NULL
  # catParam=NULL
  # innerMargins=c(0,0,0,0)
  # outerMargins=c(0.01,0.01,0.01,0.01)

#------------------
# Initialize variables to remove binding errors if needed
# -----------------

NULL->raster->shape->map->checkFacets->catBreaks->catLabels->catPalette

legendTitle=gsub(" ","\n",legendTitle)
tmap::tmap_mode(mode = c("plot"))
tmap::tmap_options(max.categories=10000)


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
    if(!is.null(shape)){
    raster<-raster::stack(raster)
    raster::projection(raster)<-sp::proj4string(shape)
    shape_ras <- raster::rasterize(shape, raster[[1]], getCover=TRUE)
    shape_ras[shape_ras==0] <- NA
    raster<-raster::mask(raster,shape_ras)
    raster<-methods::as(raster, "SpatialPixelsDataFrame")
    raster@data<-Filter(function(x)!all(is.na(x)), raster@data)
    # Replace spaces because raster::stack(raster) will add periods which then don't correspond to fillColumn names
    fillColumn<-gsub("\\ ",".",fillColumn)
    fillColumn<-fillColumn[c(fillColumn %in% names(raster@data))]
   }
}


if(!is.null(numeric2Cat_list)){
     if(all(c("numeric2Cat_param","numeric2Cat_breaks","numeric2Cat_labels","numeric2Cat_palette","numeric2Cat_legendTextSize") %in% names(numeric2Cat_list))){
       if(catParam %in% unique(unlist(numeric2Cat_list$numeric2Cat_param))) {
       list_index <- which(numeric2Cat_list$numeric2Cat_param==catParam)
       catBreaks <- numeric2Cat_list$numeric2Cat_breaks[[list_index]]
       catLabels <- numeric2Cat_list$numeric2Cat_labels[[list_index]]
       if(grepl("c\\(",numeric2Cat_list$numeric2Cat_palette[[list_index]])){
         catPalette <- eval(parse(text=paste(numeric2Cat_list$numeric2Cat_palette[[list_index]])))}else{
           catPalette <- numeric2Cat_list$numeric2Cat_palette[[list_index]]}

       legendTextSize <- numeric2Cat_list$numeric2Cat_legendTextSize[[list_index]]
       }
     } else {print("numerc2Cat_list does not contain the appropriate sublists: 'numeric2Cat_param','numeric2Cat_breaks','numeric2Cat_labels','numeric2Cat_catPalette'. Skipping conversion to Categorical")}
   } else {print("numerc2Cat_list is not a list. Skipping conversion to Categorical")}


# If categorical data then set as factor
if(!is.null(raster)){
  if(!is.null(catBreaks) & !is.null(catLabels)){

    if(!is.null(catPalette)){
      if(length(catPalette)>1){
        fillPalette <- c(catPalette,metis.colors()$pal_16)
      }else{
      if(catPalette %in% names(metis.colors())){
        fillPalette <- metis.colors()[[catPalette]]}
      }
  }


    for(i in 1:length(fillColumn)){
      fillColumn_i <- fillColumn[i]

      if(is.numeric(raster@data[[fillColumn_i]])){

        legendStyleOrig <- legendStyle; legendBreaksOrig <- legendBreaks;
        legendStyle <- "cat"
        legendBreaks <- NULL

        raster@data[[fillColumn_i]] <- cut( raster@data[[fillColumn_i]],
                                           breaks=catBreaks,
                                           labels=catLabels)
      }


      if(any(unique(raster@data[[fillColumn_i]]) %in% names(fillPalette))){
        raster@data %>%
          dplyr::mutate(!!fillColumn_i := factor(raster@data[[fillColumn_i]],
                                                 levels = names(fillPalette)[1:max(length(unique(raster@data[[fillColumn_i]])),
                                                                                    length(names(fillPalette)))]))->
          raster@data
      } else { raster@data %>%
          dplyr::mutate(!!fillColumn_i := as.factor(raster@data[[fillColumn_i]])) -> raster@data}
    }
  }
} else{
if(!is.null(shape)){
  if(!is.null(catBreaks) & !is.null(catLabels)){

    if(!is.null(catPalette)){
      if(length(catPalette)>1){
        fillPalette <- c(catPalette,metis.colors()$pal_16)
      }else{
        if(catPalette %in% names(metis.colors())){
          fillPalette <- metis.colors()[[catPalette]]}
      }
    }

      for(i in 1:length(fillColumn)){
        fillColumn_i <- fillColumn[i]

        if(is.numeric(shape@data[[fillColumn_i]])){

            legendStyle <- "cat"
            legendBreaks <- NULL

          shape@data[[fillColumn_i]] <- cut( shape@data[[fillColumn_i]],
                                              breaks=catBreaks,
                                              labels=catLabels)
        } else {
          legendStyleOrig -> legendStyle
          legendBreaksOrig -> legendBreaks;
        }


        if(any(unique(shape@data[[fillColumn_i]]) %in% names(fillPalette))){
          shape@data %>%
            dplyr::mutate(!!fillColumn_i := factor(shape@data[[fillColumn_i]],
                                                   levels = names(fillPalette)[1:max(length(unique(shape@data[[fillColumn_i]])),
                                                                                     length(names(fillPalette)))])) ->
            shape@data
        } else { shape@data %>%
            dplyr::mutate(!!fillColumn_i := as.factor(shape@data[[fillColumn_i]])) -> shape@data}
      }
  }
}
}


if(length(fillPalette)==1){
 if(fillPalette %in% names(metis.colors())){
            fillPalette<-metis.colors()[[fillPalette]]}}

#-----------------
#----------------

if(!is.null(raster)){


  if(is.null(legendBreaks)){legendBreaks=scales::pretty_breaks(n=legendFixedBreaks)(raster@data%>%dplyr::select(fillColumn)%>%as.matrix())}

  if(!is.null(shape)){map<-tmap::tm_shape(raster, bbox=shape@bbox)} else {map<-tmap::tm_shape(raster)}

  map<- map + tmap::tm_raster(col=fillColumn,palette = fillPalette, title=legendTitle,
                                  style=legendStyle,n=legendFixedBreaks,breaks=legendBreaks,legend.show = legendShow)

  if(!is.null(raster)){checkFacets=length(names(raster))}

  if(!is.null(checkFacets) & checkFacets>1 & !is.null(fillColumn) & facetsON==T){
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

if(!is.null(shape)){
if(is.null(underLayer)){
  if(grepl("tmap",class(shape)[1],ignore.case=T)){
    if(!is.null(map)){map<-map+shape}else{map<-shape}
    }else
      if(!is.null(map)){map<-map+tmap::tm_shape(shape)}else{map<-tmap::tm_shape(shape)}
  }else{
    if(grepl("tmap",class(shape)[1],ignore.case=T)){
      if(!is.null(map)){map<-map+underLayer+shape}else{map<-underLayer+shape}
      }else
        if(!is.null(map)){map<-underLayer+map+tmap::tm_shape(shape)}else{map<-underLayer+tmap::tm_shape(shape)}
  }}

if(!is.null(shape)){


if(grepl("line",class(shape)[1],ignore.case=T)){
  map=map +  tmap::tm_lines(col=borderColor,lwd=lwd, lty=lty)}

if(grepl("polygon",class(shape)[1],ignore.case=T) | grepl("tmap",class(shape)[1],ignore.case=T)){
  if(is.null(fillColumn)){
    map= map + tmap::tm_borders(col=borderColor,lwd=lwd, lty=lty)
  }else{
if(is.null(raster)){
if(is.null(legendBreaks)){
  if(length(scales::pretty_breaks(n=legendFixedBreaks)(shape@data%>%dplyr::select(fillColumn)%>%as.matrix()))>1){
    legendBreaks=scales::pretty_breaks(n=legendFixedBreaks)(shape@data%>%dplyr::select(fillColumn)%>%as.matrix())
  }else{legendBreaks=NULL}
}

if(length(unique(legendBreaks))==1){legendStyle="kmeans"}
#names(shape)[names(shape) %in% fillColumn]<-gsub(" ","_",names(shape)[names(shape) %in% fillColumn])
map<-map + tmap::tm_fill(col=fillColumn, palette = fillPalette, title=legendTitle,
                   style=legendStyle,n=legendFixedBreaks,breaks=legendBreaks,alpha=alpha,colorNA=fillcolorNA,
                   legend.show = legendShow, showNA=fillshowNA) +
           tmap::tm_borders(col=borderColor,lwd=lwd, lty=lty)
}else{
  map<-map + tmap::tm_borders(col=borderColor,lwd=lwd, lty=lty)
}
  }

}

  if(labels!=F){
    if(is.null(raster)){
      if(!is.null(fillColumn)){
      map= map + tmap::tm_text(fillColumn,scale=labelsSize,auto.placement=labelsAutoPlace, col=labelsColor)}else{
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
              tmap::tm_layout(panel.labels=gsub("X","",fillColumn))
  figWidth=figWidth*1.2
}}}

  if(!is.null(panelLabel)){
    map<- map + tmap::tm_facets(nrow=1,ncol=1) +
      tmap::tm_layout(panel.labels=gsub("X","",panelLabel))
  }

}


  map<- map +
    tmap::tm_layout(
              legend.outside=legendOutside,
              legend.title.size = legendTitleSize,
              legend.text.size = legendTextSize)+
    tmap::tm_layout(frame = frameShow,bg.color=bgColor)+
    tmap::tm_layout(main.title.position="left",main.title.size=1.5,
              inner.margins = innerMargins,outer.margins=outerMargins) +
    tmap::tm_layout(panel.label.bg.color = facetBGColor,
                    panel.label.color = facetLabelColor,
                    panel.label.size = facetLabelSize)

  if(!is.null(multiFacetRows) | !is.null(multiFacetCols)){map<-map+tmap::tm_layout(asp=1)}else{
    map<-map+tmap::tm_layout(asp=NA)
  }

if(!is.null(legendDigits)){map<- map + tmap::tm_layout(legend.format = list(digits = legendDigits))}
  if(!is.null(mapTitle)){map<- map + tmap::tm_layout(main.title = mapTitle, main.title.size = mapTitleSize)}


if(!is.null(overLayer)){
  map<-map+overLayer
}


if(printFig!=F){
fname<-paste(fileName,sep="")

if(nchar(paste(dirOutputs,"/",fname,sep=""))>250){
  print("Save path for figure larger than 250 characters. Clipping name.")
  print(paste("Orig name: ",dirOutputs,"/",fname,sep=""))
  print(paste("New name: ", dirOutputs,"/",strtrim(fname, (250-nchar(paste(dirOutputs,"/",sep="")))),sep=""))
  fname<-strtrim(fname, (250-nchar(paste(dirOutputs,"/",sep=""))))
}

if(!dir.exists(dirOutputs)){
  print(paste("dirOutputs provided: ",dirOutputs," does not exist. Saving to: ", getwd(),sep=""))

  if (!dir.exists(paste(dirOutputs, "/outputsTemp", sep = ""))){
    dir.create(paste(dirOutputs, "/outputstemp", sep = ""))}

  metis.printPdfPng(figure=map,
                    dir=dirOutputs,
                    filename=fname,
                    figWidth=figWidth,
                    figHeight=figHeight,
                    pdfpng=pdfpng)
  }else{
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
