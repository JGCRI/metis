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
#' @param folderName Default = NULL,
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
#' @param mapTitleOn Default=T
#' @param mapTitle Default=NULL
#' @param mapTitleSize Default=1
#' @param numeric2Cat_list Default=NULL,
#' @param catParam Default=NULL
#' @param innerMargins Default =c(0,0,0,0), # bottom, left, top, right
#' @param outerMargins Default =c(0.01,0.01,0.01,0.01) # bottom, left, top, right
#' @param legendSingleColorOn Default=F
#' @param legendSingleValue Default=NULL
#' @param legendSingleColor Default="white"
#' @param legendDigitsOverride Default=NULL
#' @param compassScale Default=F
#' @param scalePos Default = c("RIGHT","BOTTOM")
#' @param compassPos Default = c("LEFT","BOTTOM")
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
                  labelsAutoPlace=F,
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
                  dirOutputs=NULL,
                  folderName=NULL,
                  facetFreeScale=F,
                  facetRows=NA,
                  facetCols=4,
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
                  mapTitleOn=T,
                  mapTitle=NULL,
                  mapTitleSize=1,
                  numeric2Cat_list=NULL,
                  catParam=NULL,
                  innerMargins=c(0,0,0,0), # bottom, left, top, right
                  outerMargins=c(0.01,0.01,0.01,0.01),# bottom, left, top, right
                  legendSingleColorOn=T,
                  legendSingleValue=NULL,
                  legendSingleColor="white",
                  legendDigitsOverride=NULL,
                  compassScale=F,
                  scalePos = c("right","bottom"),
                  compassPos = c("left","bottom")
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
  # folderName=NULL
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
  # panelLabel=NULL
  # multiFacetRows=NULL
  # multiFacetCols=NULL
  # mapTitle=NULL
  # mapTitleSize=1
  # numeric2Cat_list=NULL
  # catParam=NULL
  # innerMargins=c(0,0,0,0)
  # outerMargins=c(0.01,0.01,0.01,0.01)
  # legendSingleColorOn=T
  # legendSingleValue=NULL
  # legendSingleColor="white"
  # legendDigitsOverride=NULL
  # compassScale=F
  # scalePos = c("right","bottom")
  # compassPos = c("left","bottom")

#------------------
# Initialize variables to remove binding errors if needed
# -----------------

NULL->raster->shape->map->checkFacets->catBreaks->catLabels->catPalette->legendSinglecolorOn

legendTitle=gsub(" ","\n",legendTitle)
tmap::tmap_mode(mode = c("plot"))
tmap::tmap_options(max.categories=10000)

fillPaletteOrig <- fillPalette


#------------------
# Custom functions and fixes
# -----------------

# Fix to tmap to fix width of row facet labels
# From answer on stackoverflow
# https://stackoverflow.com/questions/57697351/how-do-you-adjust-facet-row-label-height-in-faceted-tmap-plots

process_facet_layout <- function(gm) {
  panel.mode <- outer.margins <- attr.outside.position <- legend.outside.position <- NULL
  fpi <- gm$shape.fpi

  if (gm$panel.mode=="none") {
    dh2 <- gm$shape.dh - fpi$legH - fpi$attrH - fpi$mainH - (gm$nrow - 1) * fpi$between.margin.in - fpi$xlabHin - gm$nrow * fpi$xgridHin
    dw2 <- gm$shape.dw - fpi$legW - (gm$ncol - 1) * fpi$between.margin.in - fpi$ylabWin - gm$ncol * fpi$ygridWin
  } else if (gm$panel.mode=="one") {
    dh2 <- gm$shape.dh - fpi$legH - fpi$attrH - fpi$mainH - gm$nrow * fpi$pSH - (gm$nrow - 1) * fpi$between.margin.in - fpi$xlabHin - gm$nrow * fpi$xgridHin
    dw2 <- gm$shape.dw - fpi$legW - (gm$ncol - 1) * fpi$between.margin.in - fpi$ylabWin - gm$ncol * fpi$ygridWin
  } else {
    dh2 <- gm$shape.dh - fpi$legH - fpi$attrH - fpi$mainH - fpi$pSH - fpi$between.margin.in * gm$nrow - fpi$xlabHin - gm$nrow * fpi$xgridHin
    dw2 <- gm$shape.dw - fpi$legW - fpi$pSW - fpi$between.margin.in * gm$ncol - fpi$ylabWin - gm$ncol * fpi$ygridWin+1
  }

  dasp2 <- dw2/dh2
  hasp <- gm$shape.sasp * gm$ncol / gm$nrow

  if (hasp>dasp2) {
    fW <- dw2
    fH <- dw2 / hasp
  } else {
    fH <- dh2
    fW <- dh2 * hasp
  }

  gasp <- fW/fH
  if (gasp>dasp2) {
    xs <- 0
    ys <- grid::convertHeight(unit(dh2-(dw2 / gasp), "inch"), "npc", valueOnly=TRUE)
  } else {
    xs <- grid::convertWidth(unit(dw2-(gasp * dh2), "inch"), "npc", valueOnly=TRUE)
    ys <- 0
  }

  outerx <- sum(gm$outer.margins[c(2,4)])
  outery <- sum(gm$outer.margins[c(1,3)])
  spc <- 1e-5

  gm <- within(gm, {
    between.margin.y <- grid::convertHeight(unit(fpi$between.margin.in, "inch"), "npc", valueOnly=TRUE)
    between.margin.x <- grid::convertWidth(unit(fpi$between.margin.in, "inch"), "npc", valueOnly=TRUE)
    panelh <- grid::convertHeight(unit(fpi$pSH, "inch"), "npc", valueOnly=TRUE)
    panelw <- grid::convertWidth(unit(fpi$pSW, "inch"), "npc", valueOnly=TRUE)

    ylabWnpc <- grid::convertWidth(unit(fpi$ylabWin, "inch"), "npc", valueOnly=TRUE)
    xlabHnpc <- grid::convertHeight(unit(fpi$xlabHin, "inch"), "npc", valueOnly=TRUE)

    ygridWnpc <- grid::convertWidth(unit(fpi$ygridWin, "inch"), "npc", valueOnly=TRUE)
    xgridHnpc <- grid::convertHeight(unit(fpi$xgridHin, "inch"), "npc", valueOnly=TRUE)

    attr.between.legend.and.map <- attr.outside.position %in% c("top", "bottom")

    if (panel.mode=="none") {
      colrange <- (1:ncol)*3 + 3
      rowrange <- (1:nrow)*3 + 3
      facetw <- ((1-spc-outerx)-xs-fpi$legmarx-ylabWnpc-between.margin.x*(ncol-1))/ncol-ygridWnpc
      faceth <- ((1-spc-outery)-ys-fpi$legmary-fpi$attrmary-fpi$mainmary-xlabHnpc-between.margin.y*(nrow-1))/nrow-xgridHnpc
      colws <- c(outer.margins[2], xs/2, fpi$legmar[2], ylabWnpc, rep(c(ygridWnpc, facetw, between.margin.x), ncol-1), ygridWnpc, facetw, fpi$legmar[4], xs/2, outer.margins[4])

      if (attr.between.legend.and.map) {
        rowhs <- c(outer.margins[3], ys/2, fpi$mainmary, fpi$legmar[3], fpi$attrmar[3], rep(c(faceth, xgridHnpc, between.margin.y), nrow-1), faceth, xgridHnpc, xlabHnpc, fpi$attrmar[1], fpi$legmar[1], ys/2, outer.margins[1])
      } else {
        rowhs <- c(outer.margins[3], ys/2, fpi$mainmary, fpi$attrmar[3], fpi$legmar[3], rep(c(faceth, xgridHnpc, between.margin.y), nrow-1), faceth, xgridHnpc, xlabHnpc, fpi$legmar[1], fpi$attrmar[1], ys/2, outer.margins[1])
      }

    } else if (panel.mode=="one") {
      colrange <- (1:ncol)*3 + 3
      rowrange <- (1:nrow)*4 + 3

      facetw <- ((1-spc-outerx)-xs-fpi$legmarx-ylabWnpc-between.margin.x*(ncol-1))/ncol-ygridWnpc
      faceth <- ((1-spc-outery)-ys-fpi$legmary-fpi$attrmary-fpi$mainmary-xlabHnpc-between.margin.y*(nrow-1))/nrow - panelh-xgridHnpc

      colws <- c(outer.margins[2], xs/2, fpi$legmar[2], ylabWnpc, ygridWnpc, rep(c(facetw, between.margin.x, ygridWnpc), ncol-1), facetw, fpi$legmar[4], xs/2, outer.margins[4])
      if (attr.between.legend.and.map) {
        rowhs <- c(outer.margins[3], ys/2, fpi$mainmary, fpi$legmar[3], fpi$attrmar[3], rep(c(panelh, faceth, xgridHnpc, between.margin.y), nrow-1), panelh, faceth, xgridHnpc, xlabHnpc, fpi$attrmar[1], fpi$legmar[1], ys/2, outer.margins[1])
      } else {
        rowhs <- c(outer.margins[3], ys/2, fpi$mainmary, fpi$attrmar[3], fpi$legmar[3], rep(c(panelh, faceth, xgridHnpc, between.margin.y), nrow-1), panelh, faceth, xgridHnpc, xlabHnpc, fpi$legmar[1], fpi$attrmar[1], ys/2, outer.margins[1])
      }

    } else {
      colrange <- (1:ncol)*3 + 5
      rowrange <- (1:nrow)*3 + 5

      colpanelrow <- 6
      rowpanelcol <- 6 #5

      facetw <- ((1-spc-outerx)-xs-fpi$legmarx-ylabWnpc-between.margin.x*ncol-panelw)/ncol-ygridWnpc
      faceth <- ((1-spc-outery)-ys-fpi$legmary-fpi$attrmary-fpi$mainmary-xlabHnpc-between.margin.y*nrow-panelh)/nrow-xgridHnpc

      # Here is the modified code
      colws <- c(outer.margins[2], xs/2, fpi$legmar[2], ylabWnpc, panelw, c(panelw, ygridWnpc, facetw), rep(c(between.margin.x, ygridWnpc, facetw), ncol-1), fpi$legmar[4], xs/2, outer.margins[4])

      if (attr.between.legend.and.map) {
        rowhs <- c(outer.margins[3], ys/2, fpi$mainmary, fpi$legmar[3], fpi$attrmar[3], panelh, rep(c(between.margin.y, faceth, xgridHnpc), nrow), xlabHnpc, fpi$attrmar[1],fpi$legmar[1], ys/2, outer.margins[1])
      } else {
        rowhs <- c(outer.margins[3], ys/2, fpi$mainmary, fpi$attrmar[3], fpi$legmar[3], panelh, rep(c(between.margin.y, faceth, xgridHnpc), nrow), xgridHnpc, xlabHnpc, fpi$legmar[1], fpi$attrmar[1], ys/2, outer.margins[1])
      }

    }
    if (legend.outside.position[1] == "left") {
      legx <- 3
      legy <- 5:(length(rowhs)-5)
    } else if (legend.outside.position[1] == "right") {
      legx <- length(colws)-2
      legy <- 5:(length(rowhs)-5)
    } else if (legend.outside.position[1] == "top") {
      legy <- 4- attr.between.legend.and.map
      legx <- 5:(length(colws)-3)
    } else if (legend.outside.position[1] == "bottom") {
      legy <- length(rowhs)-3 + attr.between.legend.and.map
      legx <- 5:(length(colws)-3)
    }

    if (tolower(attr.outside.position[1]) == "top") {
      attry <- 3 + attr.between.legend.and.map
      attrx <- 5:(length(colws)-3)
    } else {
      attry <- length(rowhs)-2 - attr.between.legend.and.map
      attrx <- 5:(length(colws)-3)
    }

    xlaby <- length(rowhs)-4
    xlabx <- 5:(length(colws)-3)

    ylaby <- 5:(length(rowhs)-5)
    ylabx <- 4

  })
  gm$gasp <- unname(gasp)
  gm
}

x="process_facet_layout";
value=process_facet_layout;
ns="tmap"

utils::assignInNamespace(x="process_facet_layout", value=process_facet_layout, ns="tmap")



#------------------
# Create Directories
# -----------------

if(printFig!=F){
if(!is.null(dirOutputs)){
  if(grepl("/",dirOutputs)){
    if(!dir.exists(dirOutputs)){
      print(paste("dirOutputs entered: ", dirOutputs, " is invalid.",sep=""))
      stop("Please enter a valid directory path or set to NULL for default 'output' folder.")
    }
  }else{
  dirOutputs = paste(getwd(),"/",gsub(paste(getwd(),"/",sep=""),"",dirOutputs),sep="")
  if (!dir.exists(paste(dirOutputs,sep=""))){dir.create(paste(dirOutputs,sep=""))}
  if(!is.null(folderName)){
    if (!dir.exists(paste(dirOutputs,"/",folderName,sep=""))){dir.create(paste(dirOutputs,"/",folderName,sep=""))}
    if(dirOutputs==paste(dirOutputs,sep="")){dirOutputs=paste(dirOutputs,"/",folderName,sep="")}
  }
}
}else{
  if(!is.null(folderName)){
    dirOutputs=paste(getwd(),"/",folderName,sep="")
  }else{
    dirOutputs=paste(getwd(),sep="")
  }
  if(!is.null(folderName)){
    if (!dir.exists(paste(getwd(),"/",folderName,sep=""))){dir.create(paste(getwd(),"/",folderName,sep=""))}
  }
}
}

#------------------------------------------
# Read data and check inputs
#------------------------------------------


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
            fillPalette<-metis.colors()[[fillPalette]]}else{
              if(!is.na(RColorBrewer::brewer.pal.info[fillPalette,]$maxcolors)){
                fillPalette <- RColorBrewer::brewer.pal(RColorBrewer::brewer.pal.info[fillPalette,]$maxcolors,fillPalette)}

            }}; graphics::pie(rep(1,length(fillPalette)),label=names(fillPalette),col=fillPalette)



#-----------------
# Remove Inf values
#-----------------

if(!is.null(shape)){
  if(class(shape)!="tmap"){
  if(nrow(shape@data)>0){
    shape@data <- shape@data %>% dplyr::mutate_if(is.numeric, list(~dplyr::na_if(abs(.), Inf)))
    }
   }# If not tmap
  }

#--------------
# Plot
#-------------


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

  if(!is.null(legendDigitsOverride)){
  if(!is.null(legendDigits)){
    legendDigits <- legendDigitsOverride;
  }}

# Adding in a single value (eg. 0 to be set to a single color eg. white)
  if(is.null(catPalette)){
  if(!is.null(legendDigits)){
  if(legendSingleColorOn){

    # For Testing
    # legendBreaks=seq(from=0,to=2,by=0.5)
    # legendSingleValue=1
    # legendSingleColor="white"
    # legendSingleColorOn=T
    # legendDigits=1
    # fillPaletteOrig="pal_div_RdBlu"
    # fillPalette=metis.colors()[[fillPaletteOrig]]

    legendBreaksX <- legendBreaks; legendBreaksX

    # New Breaks
    if(is.null(legendSingleValue)){
      if(min(legendBreaksX)*max(legendBreaksX)<=0){legendSingleValuex=0}else{
        legendSingleValuex=floor(min(legendBreaksX))}
    }else{legendSingleValuex=legendSingleValue}


  # Find location of single value provided.
  if(min(legendBreaksX)>=legendSingleValuex){
  singlevalLoc<-match(min(legendBreaksX[legendBreaksX>=legendSingleValuex]),legendBreaksX)}else{
    if(max(legendBreaksX)<=legendSingleValuex){
      singlevalLoc<-match(max(legendBreaksX[legendBreaksX<=legendSingleValuex]),legendBreaksX)
    }else{
      singlevalLoc<-match(max(legendBreaksX[legendBreaksX<legendSingleValuex]),legendBreaksX)+1
    }
  };singlevalLoc

  # Legend Labels
  a<-c()
  for(i in 1:(length(legendBreaksX)-1)){
    if(i!=1){lower<-upperLast}else{lower <- round(legendBreaksX[i],(legendDigits))};lower
    upper <- round(legendBreaksX[i+1],legendDigits); upper
    countDig <- 1
    while(upper==lower & countDig<6){upper <- round(legendBreaksX[i+1],(legendDigits+countDig)); countDig=countDig+1};upper
    upperLast <- upper; upperLast
    a[i]=paste(lower," to ",upper,sep="")};a

  if(min(legendBreaksX)>=legendSingleValuex){
    legendLabelsX=c(a)
    legendLabelsX<-c(paste(legendSingleValuex,sep=""),
                    legendLabelsX[(singlevalLoc):length(legendLabelsX)])
    }else{
      if(max(legendBreaksX)<=legendSingleValuex){
        legendLabelsX=c(a)
        legendLabelsX<-c(legendLabelsX[1:(singlevalLoc-1)],
                        paste(legendSingleValuex,sep=""))
      }else{
        legendLabelsX=c(a)
        legendLabelsX<-c(legendLabelsX[1:(singlevalLoc-1)],
                        paste(legendSingleValuex,sep=""),
                        legendLabelsX[(singlevalLoc):length(legendLabelsX)])
      }}; legendLabelsX

  # Fill palette
  if(T){

    graphics::pie(rep(1,length(fillPalette)),label=names(fillPalette),col=fillPalette);fillPalette
    fillColUp<-fillPalette[(round(length(fillPalette)/2,0)+2):length(fillPalette)];fillColUp
    fillColUp <- grDevices::colorRampPalette(c("white",fillColUp))(11)[-1];fillColUp
    graphics::pie(rep(1,length(fillColUp)),label=names(fillColUp),col=fillColUp)
    fillColDown<-rev(fillPalette[1:(round(length(fillPalette)/2,0)-1)])
    fillColDown <- grDevices::colorRampPalette(c("white",fillColDown))(11)[-1];fillColDown
    graphics::pie(rep(1,length(fillColDown)),label=names(fillColDown),col=fillColDown)

    if(max(legendBreaksX)<=legendSingleValuex){
      if(grepl("diff|div|ratio",fillPaletteOrig,ignore.case=T)){
        fillPaletteX<-grDevices::colorRampPalette(fillColDown)(length(legendLabelsX))
      }else{
        fillPaletteX<-grDevices::colorRampPalette(fillPalette)(length(legendLabelsX))
      }
      }else{
      if(min(legendBreaksX)>=legendSingleValuex){
        if(grepl("diff|div|ratio",fillPaletteOrig,ignore.case=T)){
          fillPaletteX<-grDevices::colorRampPalette(fillColUp)(length(legendLabelsX))
        }else{
        fillPaletteX<-grDevices::colorRampPalette(fillPalette)(length(legendLabelsX))
        }
        }else{
        if(singlevalLoc==length(legendLabelsX)){fillPaletteXUp<-c()}else{
          fillPaletteXUp <- grDevices::colorRampPalette(fillColUp)(round((length(legendLabelsX)-singlevalLoc),0))};fillPaletteXUp
        if(singlevalLoc==1){fillPaletteXDown<-c()}else{
          fillPaletteXDown <- rev(grDevices::colorRampPalette(fillColDown)(singlevalLoc))};fillPaletteXDown
        fillPaletteX <-c(fillPaletteXDown,fillPaletteXUp)
      }
    }
    graphics::pie(rep(1,length(fillPaletteX)),label=names(fillPaletteX),col=fillPaletteX)

   if(min(legendBreaksX)>=legendSingleValuex){
     fillPaletteX<-c(paste(legendSingleColor,sep=""),
                    fillPaletteX[(singlevalLoc):length(fillPaletteX)])
  }else{
    if(max(legendBreaksX)<=legendSingleValuex){
      fillPaletteX<-c(fillPaletteX[1:(singlevalLoc-1)],
                      paste(legendSingleColor,sep=""))
    }else{
      fillPaletteX<-c(fillPaletteX[1:(singlevalLoc-1)],
                      paste(legendSingleColor,sep=""),
                      fillPaletteX[(singlevalLoc+1):length(fillPaletteX)])
    }
  };fillPaletteX;graphics::pie(rep(1,length(fillPaletteX)),label=legendLabelsX,col=fillPaletteX)
    }


  if(legendSingleValuex %in% legendBreaksX){
    if(max(legendBreaksX)==legendSingleValuex){
    legendAdder = (legendSingleValuex+(legendBreaksX[singlevalLoc]-legendBreaksX[singlevalLoc-1])/1000)}else{
      legendAdder = (legendSingleValuex+(legendBreaksX[singlevalLoc+1]-legendBreaksX[singlevalLoc])/1000)
    }

  }else{legendAdder=NULL}

  if(min(legendBreaksX)>legendSingleValuex){
    legendBreaksX<- sort(c(legendSingleValuex[!legendSingleValuex %in% legendBreaksX],
                     legendBreaksX[singlevalLoc:length(legendBreaksX)]))
  }else{
    if(max(legendBreaksX)<legendSingleValuex){
      legendBreaksX<- sort(c(legendBreaksX[1:(singlevalLoc)],
                       legendSingleValuex[!legendSingleValuex %in% legendBreaksX]))
    }else{
      if(min(legendBreaksX)==legendSingleValuex){
      legendBreaksX<- sort(c(legendSingleValuex,
                             legendAdder,
                        legendBreaksX[(singlevalLoc+1):length(legendBreaksX)]))
    }else{
      if(max(legendBreaksX)==legendSingleValuex){
        legendBreaksX<- sort(c(legendBreaksX[1:(singlevalLoc)],
                           legendSingleValuex[!legendSingleValuex %in% legendBreaksX],
                           legendAdder))
      }else{
        legendBreaksX<- sort(c(legendBreaksX[1:(singlevalLoc-1)],
                       legendSingleValuex[!legendSingleValuex %in% legendBreaksX],
                       legendAdder,
                       legendBreaksX[(singlevalLoc):length(legendBreaksX)]))
    }}}};legendBreaksX

  legendFixedBreaksX = length(legendBreaksX)
  length(legendBreaksX);length(legendLabelsX);legendBreaksX;legendLabelsX

if(length(legendBreaksX)-1!=length(legendLabelsX)){
  print("Length of legend breaks must be length of legend labels +1. Skipping setting singleValeColor.")
  legendFixedBreaksX=legendFixedBreaks
  legendBreaksX=legendBreaks
  legendLabelsX=NULL
  fillPaletteX=fillPalette
  }
  } else{
    legendFixedBreaksX=legendFixedBreaks
    legendBreaksX=legendBreaks
    legendLabelsX=NULL
    fillPaletteX=fillPalette
  }
    }else {
    legendFixedBreaksX=legendFixedBreaks
    legendBreaksX=legendBreaks
    legendLabelsX=NULL
    fillPaletteX=fillPalette
  }
    }else{
    legendFixedBreaksX=legendFixedBreaks
    legendBreaksX=legendBreaks
    legendLabelsX=NULL
    fillPaletteX=fillPalette
}

if(is.null(legendLabelsX)){if(length(unique(legendBreaksX))==1){legendStyle="kmeans"}}
#names(shape)[names(shape) %in% fillColumn]<-gsub(" ","_",names(shape)[names(shape) %in% fillColumn])
map<-map + tmap::tm_fill(col=fillColumn, palette = fillPaletteX, title=legendTitle,
                   style=legendStyle,n=legendFixedBreaksX,breaks=legendBreaksX,labels=legendLabelsX,alpha=alpha,colorNA=fillcolorNA,
                   colorNULL = fillcolorNULL,
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
                    panel.label.size = facetLabelSize);map

  if(!is.null(multiFacetRows) | !is.null(multiFacetCols)){map<-map+tmap::tm_layout(asp=1)}else{
    map<-map+tmap::tm_layout(asp=NA)
  };map


if(legendStyle!="cat"){
if(!is.null(dataGrid)){
  if(all(fillColumn %in% names(raster@data))){
  if(length(unique(raster@data%>%dplyr::select(fillColumn)))>1){
    if(is.null(catPalette)){
    if(!is.null(legendDigits)){map<- map + tmap::tm_layout(legend.format = list(digits = legendDigits))}
  }}
}} else {
  if(!is.null(shape)){
    if(all(fillColumn %in% names(shape@data))){
    if(length(unique(shape@data%>%dplyr::select(fillColumn)))>1){
      if(is.null(catPalette)){
      if(!is.null(legendDigits)){
        if(!legendSingleColorOn){
        map<- map + tmap::tm_layout(legend.format = list(digits = legendDigits))
        }
        }
     }}
    }
  }
}
}; map
if(mapTitleOn==T){
if(!is.null(mapTitle)){map<- map + tmap::tm_layout(main.title = mapTitle, main.title.size = mapTitleSize)}}


if(!is.null(overLayer)){
  map<-map+overLayer
}

if(compassScale){
  if(!is.null(scalePos)){
  if(scalePos!=F & scalePos!="none"){
  map <- map +  tmap::tm_scale_bar(position=scalePos)}}
  if(!is.null(compassPos)){
  if(compassPos!=F & compassPos!="none"){
    map <- map +  tmap::tm_compass(position=compassPos)}}
}


if(printFig!=F){
fname<-paste(fileName,sep="")

if(nchar(paste(dirOutputs,"/",fname,sep=""))>250){
  print("Save path for figure larger than 250 characters. Clipping name.")
  print(paste("Orig name: ",dirOutputs,"/",fname,sep=""))
  print(paste("New name: ", dirOutputs,"/",strtrim(fname, (250-min(249,nchar(paste(dirOutputs,"/",sep=""))))),sep=""))
  fname<-strtrim(fname, (250-nchar(paste(dirOutputs,"/",sep=""))))
}

if(!dir.exists(dirOutputs)){
  print(paste("dirOutputs provided: ",dirOutputs," does not exist. Saving to: ", getwd(), "/outputsTemp",sep=""))

  if (!dir.exists(paste(getwd(), "/outputsTemp", sep = ""))){
    dir.create(paste(getwd(), "/outputstemp", sep = ""))}

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
