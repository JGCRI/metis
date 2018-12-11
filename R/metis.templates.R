#' metis.templates
#'
#' This script holds various templates used for different scripts.
#'
#' List of Templates in this script:
#' \itemize{
#' \item metis.printPdfPng: Function used to print charts to a pdf or png or both.
#' \item metis.chartsThemeLight: A light ggplot theme for charts
#' \item metis.tmapAnimate: A function to animate tmaps across a variable.
#' \item metis.tmapLayout: A fucntion to define tmap layouts
#' }
#' @rdname metis.templates
#' @name metis.templates
#' @param figure Figure to be printed in function metis.printPdfPng
#' @param dir Directory to print figure to in function metis.printPdfPng
#' @param filename Filename for figure printed in function metis.printPdfPng
#' @param figWidth Figure Width in inches for figures to be printed in function metis.printPdfPng
#' @param figHeight Figure height in inches for figures to be printed in function metis.printPdfPng
#' @param pdfpng  Either "pdf", "png" or "both" to define the format of output
#' @keywords templates, charts, maps, print
#' @return A list of different templates
#' @export

#-------------
# Print to PDF or PNG
#-------------

metis.printPdfPng <- function(figure,dir,filename, figWidth = 13, figHeight = 9,pdfpng="png"){

    if(pdfpng=='pdf'){grDevices::pdf(paste(dir,"/",filename,".pdf",sep=""),width=figWidth,height=figHeight)
      print(figure)
      grDevices::dev.off()}
    if(pdfpng=='png'){grDevices::png(paste(dir,"/",filename,".png",sep=""),width=figWidth,height=figHeight, units="in",res=300)
      print(figure)
      grDevices::dev.off()}
    if(pdfpng=='both'){
      grDevices::pdf(paste(dir,"/",filename,".pdf",sep=""),width=figWidth,height=figHeight)
      print(figure)
      grDevices::dev.off()
      grDevices::png(paste(dir,"/",filename,".png",sep=""),width=figWidth,height=figHeight, units="in",res=300)
      print(figure)
      grDevices::dev.off()
    }
}

#-------------
#' @rdname metis.templates
#' @export
#' @import ggplot2
# A light theme for ggplot charts
#-------------

metis.chartsThemeLight <- function(){

#------------------
# Load required Libraries
# -----------------
requireNamespace("ggplot2",quietly = T)

    x<- theme_bw() +
        theme(
        text =                element_text(family = NULL, face = "plain",colour = "black", size = 24,
                                           hjust = 0.5, vjust = 0.5, angle = 0, lineheight = 0.9)
        , axis.text.x =       element_text(size=24)
        , axis.text.y =       element_text(size=24)
        ,axis.title.x =       element_text(vjust = -1, margin=margin(t=1,unit="line"))
        ,axis.title.y =       element_text(angle = 90, vjust = 2, margin=margin(r=1,unit="line"))
        ,legend.key =         element_blank()
        ,legend.key.size =    unit(1.5, 'lines')
        ,legend.text =        element_text(size = rel(1.0), colour = "black")
        ,legend.title =       element_text(size = rel(1.2), face = NULL, hjust = 0, colour = "black")
        #,strip.background =   element_rect(fill = NA, colour = "black")
        ,plot.margin =        unit(c(1, 1, 1, 1), "lines")
        ,plot.title=          element_text(face="bold", hjust=0,size=20,margin = margin(b=20))
        )
  return(x)
}

#-------------
#' @rdname metis.templates
#' @param map A tmap object with facets which will be converted to animations
#' @param width Width of map in inches.
#' @param height Hieght of map
#' @param delay Delay. Time between animations = delay/100. Default is 60 or 0.6 seconds.
#' @export
#' @import tmap
# Creating tmap Animations
#-------------
#-------- ANIMATED TMAPS animation_tmap https://rdrr.io/cran/tmap/man/animation_tmap.html

metis.tmapAnimate <- function(map, filename="animation.gif", width, height, delay=60){

#------------------
# Load required Libraries
# -----------------
  requireNamespace("tmap",quietly = T)

  checkIM <- system("cmd.exe",input="magick -version")
  if (checkIM!=0) stop("Could not find ImageMagick. Make sure it is installed and included in the systems PATH")
  d <- paste(getwd(), "/tmap_plots", sep="")   #------------ Create Temporary Folder for plots
  dir.create(d, showWarnings = FALSE)
  save_tmap(map+tm_facets(nrow=1,ncol=1,free.scales=F) +
              tm_layout(outer.margins=c(0,0,0,0),inner.margins = c(0,0,0,0), between.margin = 0),
            filename = paste(d, "plot%03d.png", sep="/"), width=width*0.5, height=height*0.5) #------------ In tmap tm_facets MAKE SURE nrow/ncol=1, tm_facets(free.scales=FALSE,nrow=1,ncol=1)
  processed <- system("cmd.exe",input=paste("magick -delay ", delay, " ", d, "/*.png \"", filename, "\"", sep=""))
  unlink(d, recursive = TRUE) #-------------- cleaning up plots and temporary variables
  invisible()
}


